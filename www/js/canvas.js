/**
 * ShinyLabel Annotation Canvas Engine
 * ------------------------------------
 * Handles: draw, select, move, resize (8 handles), delete, undo,
 *          class coloring, coordinate reporting to Shiny.
 *
 * Coordinate contract:
 *   All coords stored in CANVAS DISPLAY space while drawing,
 *   but normalized to IMAGE PIXEL space before sending to Shiny.
 *   The R server does the final YOLO normalization.
 */

(function() {
  "use strict";

  // ── State ────────────────────────────────────────────────────────────────
  const state = {
    canvas:       null,
    ctx:          null,
    image:        null,          // HTMLImageElement
    imgNaturalW:  0,
    imgNaturalH:  0,
    scale:        1,             // canvas display scale vs natural image size
    offsetX:      0,             // canvas padding offset X
    offsetY:      0,             // canvas padding offset Y

    boxes:        [],            // Array of box objects
    selectedIdx:  -1,            // index into boxes[]
    activeClass:  { id: 1, name: "person", color: "#FF6B6B" },
    classes:      [],

    // Drawing state
    isDrawing:    false,
    drawStart:    { x: 0, y: 0 },
    drawCurrent:  { x: 0, y: 0 },

    // Resize/move state
    isDragging:   false,
    dragHandle:   null,          // null | 'move' | 'nw'|'n'|'ne'|'e'|'se'|'s'|'sw'|'w'
    dragStart:    { x: 0, y: 0 },
    dragBoxSnapshot: null,

    history:      [],            // undo stack, array of boxes[] snapshots
    maxHistory:   50,

    HANDLE_SIZE:  8,
    MIN_BOX_PX:   10,
  };

  // ── Public API (attached to window.ShinyLabel) ───────────────────────────
  window.ShinyLabel = {
    init,
    loadImage,
    loadBoxes,
    setActiveClass,
    updateClasses,
    clearBoxes,
    deleteSelected,
    undo,
    getBoxes,
    redraw,
  };

  // ── Initialization ────────────────────────────────────────────────────────
  function init(canvasId) {
    state.canvas = document.getElementById(canvasId);
    if (!state.canvas) { console.error("Canvas not found:", canvasId); return; }
    state.ctx = state.canvas.getContext("2d");

    state.canvas.addEventListener("mousedown",  onMouseDown);
    state.canvas.addEventListener("mousemove",  onMouseMove);
    state.canvas.addEventListener("mouseup",    onMouseUp);
    state.canvas.addEventListener("mouseleave", onMouseLeave);
    state.canvas.addEventListener("dblclick",   onDblClick);

    document.addEventListener("keydown", onKeyDown);
    state.canvas.style.cursor = "crosshair";
    redraw();
  }

  // ── Image loading ─────────────────────────────────────────────────────────
  function loadImage(src, naturalW, naturalH) {
    state.imgNaturalW = naturalW;
    state.imgNaturalH = naturalH;
    state.boxes       = [];
    state.selectedIdx = -1;
    state.history     = [];
    state.isDrawing   = false;

    const img = new Image();
    img.onload = function() {
      state.image = img;
      fitCanvas();
      redraw();
      reportBoxes();
    };
    img.onerror = function() {
      console.error("[ShinyLabel] Failed to load image src");
    };
    img.src = src;
  }

  function fitCanvas() {
    const container = state.canvas.parentElement;
    const maxW = container ? container.clientWidth  - 4 : 900;
    const maxH = Math.min(window.innerHeight * 0.65, 700);

    const scaleW = maxW / state.imgNaturalW;
    const scaleH = maxH / state.imgNaturalH;
    state.scale  = Math.min(scaleW, scaleH, 1);  // never upscale past 1:1

    state.canvas.width  = Math.round(state.imgNaturalW * state.scale);
    state.canvas.height = Math.round(state.imgNaturalH * state.scale);
    state.offsetX = 0;
    state.offsetY = 0;
  }

  // ── Box data helpers ──────────────────────────────────────────────────────
  // Boxes stored as { x, y, w, h } in IMAGE pixel coordinates (not canvas).
  // Canvas coords = image coords * scale.

  function imgToCanvas(x, y) {
    return { x: x * state.scale + state.offsetX,
             y: y * state.scale + state.offsetY };
  }
  function canvasToImg(cx, cy) {
    return { x: (cx - state.offsetX) / state.scale,
             y: (cy - state.offsetY) / state.scale };
  }
  function clampImg(x, y) {
    return {
      x: Math.max(0, Math.min(state.imgNaturalW, x)),
      y: Math.max(0, Math.min(state.imgNaturalH, y))
    };
  }

  // ── Load existing boxes from Shiny (on image switch) ─────────────────────
  function loadBoxes(boxArray) {
    // boxArray: [{class_id, class_name, color_hex, x_pixel, y_pixel, w_pixel, h_pixel}, ...]
    state.boxes = (boxArray || []).map(b => ({
      x:         b.x_pixel,
      y:         b.y_pixel,
      w:         b.w_pixel,
      h:         b.h_pixel,
      classId:   b.class_id,
      className: b.class_name,
      color:     b.color_hex || "#FF6B6B",
    }));
    state.selectedIdx = -1;
    state.history     = [];
    redraw();
  }

  // ── Class management ──────────────────────────────────────────────────────
  function setActiveClass(classObj) {
    // classObj: { id, name, color }
    state.activeClass = classObj;
  }

  function updateClasses(classArray) {
    state.classes = classArray;
  }

  // ── Canvas event handlers ─────────────────────────────────────────────────
  function getCanvasPos(e) {
    const rect = state.canvas.getBoundingClientRect();
    return {
      x: (e.clientX - rect.left) * (state.canvas.width  / rect.width),
      y: (e.clientY - rect.top)  * (state.canvas.height / rect.height),
    };
  }

  function onMouseDown(e) {
    e.preventDefault();
    if (!state.image) return;
    const pos = getCanvasPos(e);

    // Check if clicking a handle on selected box
    if (state.selectedIdx >= 0) {
      const handle = getHandleAt(pos, state.selectedIdx);
      if (handle) {
        pushHistory();
        state.isDragging      = true;
        state.dragHandle      = handle;
        state.dragStart       = pos;
        state.dragBoxSnapshot = { ...state.boxes[state.selectedIdx] };
        return;
      }
    }

    // Check if clicking inside any box (select / move)
    const hitIdx = getBoxAt(pos);
    if (hitIdx >= 0) {
      state.selectedIdx     = hitIdx;
      pushHistory();
      state.isDragging      = true;
      state.dragHandle      = "move";
      state.dragStart       = pos;
      state.dragBoxSnapshot = { ...state.boxes[hitIdx] };
      redraw();
      return;
    }

    // Start drawing a new box
    state.selectedIdx = -1;
    state.isDrawing   = true;
    state.drawStart   = pos;
    state.drawCurrent = pos;
    redraw();
  }

  function onMouseMove(e) {
    if (!state.image) return;
    const pos = getCanvasPos(e);

    if (state.isDragging) {
      applyDrag(pos);
      redraw();
      return;
    }

    if (state.isDrawing) {
      state.drawCurrent = pos;
      redraw();
      return;
    }

    // Update cursor based on hover
    updateCursor(pos);
  }

  function onMouseUp(e) {
    if (!state.image) return;
    const pos = getCanvasPos(e);

    if (state.isDragging) {
      applyDrag(pos);
      state.isDragging = false;
      state.dragHandle = null;
      clampBox(state.selectedIdx);
      redraw();
      reportBoxes();
      return;
    }

    if (state.isDrawing) {
      state.isDrawing = false;
      finalizeBox(state.drawStart, pos);
      return;
    }
  }

  function onMouseLeave() {
    if (state.isDrawing) {
      state.isDrawing = false;
      finalizeBox(state.drawStart, state.drawCurrent);
    }
    if (state.isDragging) {
      state.isDragging = false;
      clampBox(state.selectedIdx);
      redraw();
      reportBoxes();
    }
  }

  function onDblClick(e) {
    // Double-click deselects
    state.selectedIdx = -1;
    redraw();
  }

  function onKeyDown(e) {
    if (e.key === "Delete" || e.key === "Backspace") {
      if (state.selectedIdx >= 0) {
        pushHistory();
        state.boxes.splice(state.selectedIdx, 1);
        state.selectedIdx = -1;
        redraw();
        reportBoxes();
      }
    }
    if ((e.ctrlKey || e.metaKey) && e.key === "z") {
      undo();
    }
    if (e.key === "Escape") {
      state.isDrawing   = false;
      state.selectedIdx = -1;
      redraw();
    }
  }

  // ── Drawing logic ─────────────────────────────────────────────────────────
  function finalizeBox(start, end) {
    // Convert canvas → image coords
    const s = clampImg(...Object.values(canvasToImg(start.x, start.y)));
    const f = clampImg(...Object.values(canvasToImg(end.x,   end.y)));

    const x = Math.min(s.x, f.x);
    const y = Math.min(s.y, f.y);
    const w = Math.abs(f.x - s.x);
    const h = Math.abs(f.y - s.y);

    if (w < state.MIN_BOX_PX || h < state.MIN_BOX_PX) {
      redraw();
      return;  // too small, discard
    }

    pushHistory();
    const box = {
      x, y, w, h,
      classId:   state.activeClass.id,
      className: state.activeClass.name,
      color:     state.activeClass.color,
    };
    state.boxes.push(box);
    state.selectedIdx = state.boxes.length - 1;
    redraw();
    reportBoxes();
  }

  // ── Drag / resize logic ───────────────────────────────────────────────────
  function applyDrag(pos) {
    const dx = (pos.x - state.dragStart.x) / state.scale;
    const dy = (pos.y - state.dragStart.y) / state.scale;
    const snap = state.dragBoxSnapshot;
    const box  = state.boxes[state.selectedIdx];
    if (!box) return;

    switch (state.dragHandle) {
      case "move":
        box.x = snap.x + dx;
        box.y = snap.y + dy;
        break;
      case "nw": box.x = snap.x+dx; box.y = snap.y+dy; box.w = snap.w-dx; box.h = snap.h-dy; break;
      case "n":                      box.y = snap.y+dy;                    box.h = snap.h-dy; break;
      case "ne":                     box.y = snap.y+dy; box.w = snap.w+dx; box.h = snap.h-dy; break;
      case "e":                                         box.w = snap.w+dx;                    break;
      case "se":                                        box.w = snap.w+dx; box.h = snap.h+dy; break;
      case "s":                                                             box.h = snap.h+dy; break;
      case "sw": box.x = snap.x+dx;                    box.w = snap.w-dx; box.h = snap.h+dy; break;
      case "w":  box.x = snap.x+dx;                    box.w = snap.w-dx;                    break;
    }

    // Ensure positive dimensions
    if (box.w < state.MIN_BOX_PX) { box.w = state.MIN_BOX_PX; }
    if (box.h < state.MIN_BOX_PX) { box.h = state.MIN_BOX_PX; }
  }

  function clampBox(idx) {
    if (idx < 0 || idx >= state.boxes.length) return;
    const b = state.boxes[idx];
    b.x = Math.max(0, b.x);
    b.y = Math.max(0, b.y);
    if (b.x + b.w > state.imgNaturalW) b.w = state.imgNaturalW - b.x;
    if (b.y + b.h > state.imgNaturalH) b.h = state.imgNaturalH - b.y;
  }

  // ── Handle detection ──────────────────────────────────────────────────────
  const HANDLE_POSITIONS = ["nw","n","ne","e","se","s","sw","w"];

  function getHandleCoords(box) {
    const c = imgToCanvas(box.x, box.y);
    const cw = box.w * state.scale;
    const ch = box.h * state.scale;
    return {
      nw: { x: c.x,        y: c.y        },
      n:  { x: c.x+cw/2,   y: c.y        },
      ne: { x: c.x+cw,     y: c.y        },
      e:  { x: c.x+cw,     y: c.y+ch/2   },
      se: { x: c.x+cw,     y: c.y+ch     },
      s:  { x: c.x+cw/2,   y: c.y+ch     },
      sw: { x: c.x,        y: c.y+ch     },
      w:  { x: c.x,        y: c.y+ch/2   },
    };
  }

  function getHandleAt(pos, idx) {
    if (idx < 0) return null;
    const box = state.boxes[idx];
    if (!box) return null;
    const handles = getHandleCoords(box);
    const hs = state.HANDLE_SIZE + 2;
    for (const name of HANDLE_POSITIONS) {
      const h = handles[name];
      if (Math.abs(pos.x - h.x) <= hs && Math.abs(pos.y - h.y) <= hs) return name;
    }
    return null;
  }

  function getBoxAt(pos) {
    // Returns index of topmost box containing canvas point (last drawn = top)
    const imgPos = canvasToImg(pos.x, pos.y);
    for (let i = state.boxes.length - 1; i >= 0; i--) {
      const b = state.boxes[i];
      if (imgPos.x >= b.x && imgPos.x <= b.x + b.w &&
          imgPos.y >= b.y && imgPos.y <= b.y + b.h) return i;
    }
    return -1;
  }

  function updateCursor(pos) {
    if (state.selectedIdx >= 0) {
      const handle = getHandleAt(pos, state.selectedIdx);
      if (handle) {
        const cursors = {
          nw:"nw-resize",n:"n-resize",ne:"ne-resize",e:"e-resize",
          se:"se-resize",s:"s-resize",sw:"sw-resize",w:"w-resize"
        };
        state.canvas.style.cursor = cursors[handle] || "pointer";
        return;
      }
    }
    const hit = getBoxAt(pos);
    state.canvas.style.cursor = hit >= 0 ? "move" : "crosshair";
  }

  // ── Rendering ─────────────────────────────────────────────────────────────
  function redraw() {
    const ctx = state.ctx;
    const cw  = state.canvas.width;
    const ch  = state.canvas.height;

    ctx.clearRect(0, 0, cw, ch);

    // Draw image
    if (state.image) {
      ctx.drawImage(state.image, 0, 0, cw, ch);
    } else {
      ctx.fillStyle = "#1a1a2e";
      ctx.fillRect(0, 0, cw, ch);
      ctx.fillStyle = "#555";
      ctx.font = "16px monospace";
      ctx.textAlign = "center";
      ctx.fillText("Load an image to begin annotating", cw / 2, ch / 2);
    }

    // Draw committed boxes
    state.boxes.forEach((box, idx) => drawBox(box, idx === state.selectedIdx));

    // Draw in-progress box
    if (state.isDrawing) {
      drawGhostBox(state.drawStart, state.drawCurrent);
    }
  }

  function drawBox(box, isSelected) {
    const ctx = state.ctx;
    const c   = imgToCanvas(box.x, box.y);
    const cw  = box.w * state.scale;
    const ch  = box.h * state.scale;
    const color = box.color || "#FF6B6B";

    // Box fill (semi-transparent)
    ctx.fillStyle = hexToRgba(color, isSelected ? 0.18 : 0.10);
    ctx.fillRect(c.x, c.y, cw, ch);

    // Box border
    ctx.strokeStyle = color;
    ctx.lineWidth   = isSelected ? 2.5 : 1.5;
    ctx.setLineDash(isSelected ? [] : []);
    ctx.strokeRect(c.x, c.y, cw, ch);

    // Label badge
    const label = box.className;
    ctx.font = "bold 11px 'JetBrains Mono', monospace";
    const tw  = ctx.measureText(label).width;
    const badgeH = 18;
    const badgeY = c.y > badgeH ? c.y - badgeH : c.y + 1;
    ctx.fillStyle = color;
    ctx.fillRect(c.x, badgeY, tw + 8, badgeH);
    ctx.fillStyle = "#fff";
    ctx.fillText(label, c.x + 4, badgeY + 13);

    // Resize handles (only for selected)
    if (isSelected) {
      const handles = getHandleCoords(box);
      const hs = state.HANDLE_SIZE;
      ctx.fillStyle   = "#fff";
      ctx.strokeStyle = color;
      ctx.lineWidth   = 1.5;
      for (const name of HANDLE_POSITIONS) {
        const h = handles[name];
        ctx.beginPath();
        ctx.rect(h.x - hs/2, h.y - hs/2, hs, hs);
        ctx.fill();
        ctx.stroke();
      }
    }
  }

  function drawGhostBox(start, end) {
    const ctx   = state.ctx;
    const color = state.activeClass.color || "#FF6B6B";
    const x = Math.min(start.x, end.x);
    const y = Math.min(start.y, end.y);
    const w = Math.abs(end.x - start.x);
    const h = Math.abs(end.y - start.y);

    ctx.fillStyle   = hexToRgba(color, 0.12);
    ctx.fillRect(x, y, w, h);
    ctx.strokeStyle = color;
    ctx.lineWidth   = 1.5;
    ctx.setLineDash([5, 3]);
    ctx.strokeRect(x, y, w, h);
    ctx.setLineDash([]);

    // Size label
    const imgW = Math.round(w / state.scale);
    const imgH = Math.round(h / state.scale);
    ctx.font      = "10px monospace";
    ctx.fillStyle = color;
    ctx.fillText(`${imgW}×${imgH}px`, x + 4, y + h - 4);
  }

  // ── History (undo) ────────────────────────────────────────────────────────
  function pushHistory() {
    state.history.push(JSON.parse(JSON.stringify(state.boxes)));
    if (state.history.length > state.maxHistory) state.history.shift();
  }

  function undo() {
    if (state.history.length === 0) return;
    state.boxes       = state.history.pop();
    state.selectedIdx = -1;
    redraw();
    reportBoxes();
  }

  // ── Shiny communication ───────────────────────────────────────────────────
  function reportBoxes() {
    if (!window.Shiny) return;

    const payload = state.boxes.map(b => ({
      class_id:   b.classId,
      class_name: b.className,
      color_hex:  b.color,
      // Image pixel coordinates (top-left origin)
      x_pixel:    Math.round(Math.max(0, b.x)),
      y_pixel:    Math.round(Math.max(0, b.y)),
      w_pixel:    Math.round(Math.max(1, b.w)),
      h_pixel:    Math.round(Math.max(1, b.h)),
      // YOLO normalized (pre-computed here for convenience; R recomputes authoritatively)
      x_center_norm: (b.x + b.w / 2) / state.imgNaturalW,
      y_center_norm: (b.y + b.h / 2) / state.imgNaturalH,
      w_norm:        b.w / state.imgNaturalW,
      h_norm:        b.h / state.imgNaturalH,
    }));

    Shiny.setInputValue("canvas_boxes", payload, { priority: "event" });
  }

  // ── Public helpers ────────────────────────────────────────────────────────
  function clearBoxes() {
    pushHistory();
    state.boxes       = [];
    state.selectedIdx = -1;
    redraw();
    reportBoxes();
  }

  function deleteSelected() {
    if (state.selectedIdx < 0) return;
    pushHistory();
    state.boxes.splice(state.selectedIdx, 1);
    state.selectedIdx = -1;
    redraw();
    reportBoxes();
  }

  function getBoxes() { return state.boxes; }

  // ── Utility ───────────────────────────────────────────────────────────────
  function hexToRgba(hex, alpha) {
    hex = hex.replace("#", "");
    if (hex.length === 3) hex = hex.split("").map(c => c+c).join("");
    const r = parseInt(hex.substring(0,2), 16);
    const g = parseInt(hex.substring(2,4), 16);
    const b = parseInt(hex.substring(4,6), 16);
    return `rgba(${r},${g},${b},${alpha})`;
  }

})();
