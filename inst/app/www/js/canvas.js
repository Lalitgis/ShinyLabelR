/**
 * ShinyLabel Annotation Canvas Engine — FIXED
 *
 * BUGS FIXED:
 *  1. onMouseLeave: drag end was missing reportBoxes() — Shiny state desync
 *  2. finalizeBox: Object.values() spread replaced with explicit destructuring
 *  3. onKeyDown: now guards against firing inside text/select inputs
 */

(function() {
  "use strict";

  const state = {
    canvas: null, ctx: null, image: null,
    imgNaturalW: 0, imgNaturalH: 0,
    scale: 1, offsetX: 0, offsetY: 0,
    boxes: [], selectedIdx: -1,
    activeClass: { id: 1, name: "person", color: "#FF6B6B" },
    classes: [],
    isDrawing: false, drawStart: {x:0,y:0}, drawCurrent: {x:0,y:0},
    isDragging: false, dragHandle: null, dragStart: {x:0,y:0}, dragBoxSnapshot: null,
    history: [], maxHistory: 50,
    HANDLE_SIZE: 8, MIN_BOX_PX: 10,
  };

  window.ShinyLabel = { init, loadImage, loadBoxes, setActiveClass, updateClasses, clearBoxes, deleteSelected, undo, getBoxes, redraw };

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

  function loadImage(src, naturalW, naturalH) {
    state.imgNaturalW = naturalW; state.imgNaturalH = naturalH;
    state.boxes = []; state.selectedIdx = -1; state.history = [];
    state.isDrawing = false; state.isDragging = false;
    const img = new Image();
    img.onload = function() { state.image = img; fitCanvas(); redraw(); reportBoxes(); };
    img.onerror = function() { console.error("[ShinyLabel] Failed to load image src"); };
    img.src = src;
  }

  function fitCanvas() {
    const container = state.canvas.parentElement;
    const maxW = container ? container.clientWidth - 4 : 900;
    const maxH = Math.min(window.innerHeight * 0.65, 700);
    state.scale = Math.min(maxW / state.imgNaturalW, maxH / state.imgNaturalH, 1);
    state.canvas.width  = Math.round(state.imgNaturalW * state.scale);
    state.canvas.height = Math.round(state.imgNaturalH * state.scale);
    state.offsetX = 0; state.offsetY = 0;
  }

  function imgToCanvas(x, y) { return { x: x*state.scale+state.offsetX, y: y*state.scale+state.offsetY }; }
  function canvasToImg(cx, cy) { return { x: (cx-state.offsetX)/state.scale, y: (cy-state.offsetY)/state.scale }; }
  function clampImg(x, y) { return { x: Math.max(0,Math.min(state.imgNaturalW,x)), y: Math.max(0,Math.min(state.imgNaturalH,y)) }; }

  function loadBoxes(boxArray) {
    state.boxes = (boxArray||[]).map(b => ({ x:b.x_pixel, y:b.y_pixel, w:b.w_pixel, h:b.h_pixel, classId:b.class_id, className:b.class_name, color:b.color_hex||"#FF6B6B" }));
    state.selectedIdx = -1; state.history = [];
    redraw();
  }

  function setActiveClass(c) { state.activeClass = c; }
  function updateClasses(a) { state.classes = a; }

  function getCanvasPos(e) {
    const rect = state.canvas.getBoundingClientRect();
    return { x: (e.clientX-rect.left)*(state.canvas.width/rect.width), y: (e.clientY-rect.top)*(state.canvas.height/rect.height) };
  }

  function onMouseDown(e) {
    e.preventDefault();
    if (!state.image) return;
    const pos = getCanvasPos(e);
    if (state.selectedIdx >= 0) {
      const handle = getHandleAt(pos, state.selectedIdx);
      if (handle) { pushHistory(); state.isDragging=true; state.dragHandle=handle; state.dragStart=pos; state.dragBoxSnapshot={...state.boxes[state.selectedIdx]}; return; }
    }
    const hitIdx = getBoxAt(pos);
    if (hitIdx >= 0) { state.selectedIdx=hitIdx; pushHistory(); state.isDragging=true; state.dragHandle="move"; state.dragStart=pos; state.dragBoxSnapshot={...state.boxes[hitIdx]}; redraw(); return; }
    state.selectedIdx=-1; state.isDrawing=true; state.drawStart=pos; state.drawCurrent=pos; redraw();
  }

  function onMouseMove(e) {
    if (!state.image) return;
    const pos = getCanvasPos(e);
    if (state.isDragging) { applyDrag(pos); redraw(); return; }
    if (state.isDrawing)  { state.drawCurrent=pos; redraw(); return; }
    updateCursor(pos);
  }

  function onMouseUp(e) {
    if (!state.image) return;
    const pos = getCanvasPos(e);
    if (state.isDragging) { applyDrag(pos); state.isDragging=false; state.dragHandle=null; clampBox(state.selectedIdx); redraw(); reportBoxes(); return; }
    if (state.isDrawing)  { state.isDrawing=false; finalizeBox(state.drawStart, pos); return; }
  }

  // BUG FIX 1: drag end on mouseleave was missing reportBoxes() — Shiny state desync
  function onMouseLeave() {
    if (state.isDrawing) {
      state.isDrawing = false;
      finalizeBox(state.drawStart, state.drawCurrent); // calls reportBoxes internally
    }
    if (state.isDragging) {
      state.isDragging = false;
      state.dragHandle = null;
      clampBox(state.selectedIdx);
      redraw();
      reportBoxes(); // BUG FIX: this was missing — drag-end on leave lost data
    }
  }

  function onDblClick() { state.selectedIdx=-1; redraw(); }

  // BUG FIX 3: Delete key was firing while user typed in class name text inputs
  function onKeyDown(e) {
    const tag = document.activeElement ? document.activeElement.tagName : "";
    if (tag === "INPUT" || tag === "TEXTAREA" || tag === "SELECT") return;
    if (e.key === "Delete" || e.key === "Backspace") {
      if (state.selectedIdx >= 0) { pushHistory(); state.boxes.splice(state.selectedIdx,1); state.selectedIdx=-1; redraw(); reportBoxes(); }
    }
    if ((e.ctrlKey||e.metaKey) && e.key==="z") { undo(); }
    if (e.key === "Escape") { state.isDrawing=false; state.selectedIdx=-1; redraw(); }
  }

  // BUG FIX 2: was using spread of Object.values() — property order not guaranteed
  function finalizeBox(start, end) {
    const imgS = canvasToImg(start.x, start.y);
    const imgE = canvasToImg(end.x, end.y);
    const s = clampImg(imgS.x, imgS.y);
    const f = clampImg(imgE.x, imgE.y);
    const x=Math.min(s.x,f.x), y=Math.min(s.y,f.y), w=Math.abs(f.x-s.x), h=Math.abs(f.y-s.y);
    if (w < state.MIN_BOX_PX || h < state.MIN_BOX_PX) { redraw(); return; }
    pushHistory();
    state.boxes.push({ x,y,w,h, classId:state.activeClass.id, className:state.activeClass.name, color:state.activeClass.color });
    state.selectedIdx = state.boxes.length - 1;
    redraw(); reportBoxes();
  }

  function applyDrag(pos) {
    const dx=(pos.x-state.dragStart.x)/state.scale, dy=(pos.y-state.dragStart.y)/state.scale;
    const snap=state.dragBoxSnapshot, box=state.boxes[state.selectedIdx];
    if (!box) return;
    switch(state.dragHandle) {
      case "move": box.x=snap.x+dx; box.y=snap.y+dy; break;
      case "nw": box.x=snap.x+dx; box.y=snap.y+dy; box.w=snap.w-dx; box.h=snap.h-dy; break;
      case "n":                    box.y=snap.y+dy;                  box.h=snap.h-dy; break;
      case "ne":                   box.y=snap.y+dy; box.w=snap.w+dx; box.h=snap.h-dy; break;
      case "e":                                     box.w=snap.w+dx;                  break;
      case "se":                                    box.w=snap.w+dx; box.h=snap.h+dy; break;
      case "s":                                                      box.h=snap.h+dy; break;
      case "sw": box.x=snap.x+dx;                  box.w=snap.w-dx; box.h=snap.h+dy; break;
      case "w":  box.x=snap.x+dx;                  box.w=snap.w-dx;                  break;
    }
    if (box.w < state.MIN_BOX_PX) box.w = state.MIN_BOX_PX;
    if (box.h < state.MIN_BOX_PX) box.h = state.MIN_BOX_PX;
  }

  function clampBox(idx) {
    if (idx<0||idx>=state.boxes.length) return;
    const b=state.boxes[idx];
    b.x=Math.max(0,b.x); b.y=Math.max(0,b.y);
    if (b.x+b.w>state.imgNaturalW) b.w=state.imgNaturalW-b.x;
    if (b.y+b.h>state.imgNaturalH) b.h=state.imgNaturalH-b.y;
  }

  const HANDLE_POSITIONS = ["nw","n","ne","e","se","s","sw","w"];

  function getHandleCoords(box) {
    const c=imgToCanvas(box.x,box.y), cw=box.w*state.scale, ch=box.h*state.scale;
    return { nw:{x:c.x,y:c.y}, n:{x:c.x+cw/2,y:c.y}, ne:{x:c.x+cw,y:c.y}, e:{x:c.x+cw,y:c.y+ch/2}, se:{x:c.x+cw,y:c.y+ch}, s:{x:c.x+cw/2,y:c.y+ch}, sw:{x:c.x,y:c.y+ch}, w:{x:c.x,y:c.y+ch/2} };
  }

  function getHandleAt(pos, idx) {
    if (idx<0) return null;
    const box=state.boxes[idx]; if (!box) return null;
    const handles=getHandleCoords(box), hs=state.HANDLE_SIZE+2;
    for (const name of HANDLE_POSITIONS) { const h=handles[name]; if (Math.abs(pos.x-h.x)<=hs&&Math.abs(pos.y-h.y)<=hs) return name; }
    return null;
  }

  function getBoxAt(pos) {
    const ip=canvasToImg(pos.x,pos.y);
    for (let i=state.boxes.length-1;i>=0;i--) { const b=state.boxes[i]; if (ip.x>=b.x&&ip.x<=b.x+b.w&&ip.y>=b.y&&ip.y<=b.y+b.h) return i; }
    return -1;
  }

  function updateCursor(pos) {
    if (state.selectedIdx>=0) { const h=getHandleAt(pos,state.selectedIdx); if (h) { state.canvas.style.cursor={nw:"nw-resize",n:"n-resize",ne:"ne-resize",e:"e-resize",se:"se-resize",s:"s-resize",sw:"sw-resize",w:"w-resize"}[h]||"pointer"; return; } }
    state.canvas.style.cursor = getBoxAt(pos)>=0 ? "move" : "crosshair";
  }

  function redraw() {
    const ctx=state.ctx; if (!ctx) return;
    const cw=state.canvas.width, ch=state.canvas.height;
    ctx.clearRect(0,0,cw,ch);
    if (state.image) { ctx.drawImage(state.image,0,0,cw,ch); }
    else { ctx.fillStyle="#1a1a2e"; ctx.fillRect(0,0,cw,ch); ctx.fillStyle="#555"; ctx.font="16px monospace"; ctx.textAlign="center"; ctx.fillText("Load an image to begin annotating",cw/2,ch/2); }
    state.boxes.forEach((box,idx) => drawBox(box, idx===state.selectedIdx));
    if (state.isDrawing) drawGhostBox(state.drawStart, state.drawCurrent);
  }

  function drawBox(box, isSelected) {
    const ctx=state.ctx, c=imgToCanvas(box.x,box.y), cw=box.w*state.scale, ch=box.h*state.scale, color=box.color||"#FF6B6B";
    ctx.fillStyle=hexToRgba(color,isSelected?0.18:0.10); ctx.fillRect(c.x,c.y,cw,ch);
    ctx.strokeStyle=color; ctx.lineWidth=isSelected?2.5:1.5; ctx.setLineDash([]); ctx.strokeRect(c.x,c.y,cw,ch);
    const label=box.className||""; ctx.font="bold 11px 'JetBrains Mono',monospace";
    const tw=ctx.measureText(label).width, badgeH=18, badgeY=c.y>badgeH?c.y-badgeH:c.y+1;
    ctx.fillStyle=color; ctx.fillRect(c.x,badgeY,tw+8,badgeH); ctx.fillStyle="#fff"; ctx.fillText(label,c.x+4,badgeY+13);
    if (isSelected) {
      const handles=getHandleCoords(box), hs=state.HANDLE_SIZE;
      ctx.fillStyle="#fff"; ctx.strokeStyle=color; ctx.lineWidth=1.5;
      for (const name of HANDLE_POSITIONS) { const h=handles[name]; ctx.beginPath(); ctx.rect(h.x-hs/2,h.y-hs/2,hs,hs); ctx.fill(); ctx.stroke(); }
    }
  }

  function drawGhostBox(start, end) {
    const ctx=state.ctx, color=state.activeClass.color||"#FF6B6B";
    const x=Math.min(start.x,end.x), y=Math.min(start.y,end.y), w=Math.abs(end.x-start.x), h=Math.abs(end.y-start.y);
    ctx.fillStyle=hexToRgba(color,0.12); ctx.fillRect(x,y,w,h);
    ctx.strokeStyle=color; ctx.lineWidth=1.5; ctx.setLineDash([5,3]); ctx.strokeRect(x,y,w,h); ctx.setLineDash([]);
    ctx.font="10px monospace"; ctx.fillStyle=color; ctx.fillText(`${Math.round(w/state.scale)}×${Math.round(h/state.scale)}px`,x+4,y+h-4);
  }

  function pushHistory() { state.history.push(JSON.parse(JSON.stringify(state.boxes))); if (state.history.length>state.maxHistory) state.history.shift(); }
  function undo() { if (!state.history.length) return; state.boxes=state.history.pop(); state.selectedIdx=-1; redraw(); reportBoxes(); }

  function reportBoxes() {
    if (!window.Shiny) return;
    Shiny.setInputValue("canvas_boxes", state.boxes.map(b => ({
      class_id:b.classId, class_name:b.className, color_hex:b.color,
      x_pixel:Math.round(Math.max(0,b.x)), y_pixel:Math.round(Math.max(0,b.y)),
      w_pixel:Math.round(Math.max(1,b.w)), h_pixel:Math.round(Math.max(1,b.h)),
      x_center_norm:(b.x+b.w/2)/state.imgNaturalW, y_center_norm:(b.y+b.h/2)/state.imgNaturalH,
      w_norm:b.w/state.imgNaturalW, h_norm:b.h/state.imgNaturalH,
    })), { priority:"event" });
  }

  function clearBoxes() { pushHistory(); state.boxes=[]; state.selectedIdx=-1; redraw(); reportBoxes(); }
  function deleteSelected() { if (state.selectedIdx<0) return; pushHistory(); state.boxes.splice(state.selectedIdx,1); state.selectedIdx=-1; redraw(); reportBoxes(); }
  function getBoxes() { return state.boxes; }

  function hexToRgba(hex, alpha) {
    hex=hex.replace("#",""); if (hex.length===3) hex=hex.split("").map(c=>c+c).join("");
    return `rgba(${parseInt(hex.substring(0,2),16)},${parseInt(hex.substring(2,4),16)},${parseInt(hex.substring(4,6),16)},${alpha})`;
  }

})();
