/**
 * ShinyLabel — Shiny Message Handlers + UI Behaviour
 * Handles: canvas bridge, theme toggle, tab switching, color picker
 */

// ── Theme Toggle ─────────────────────────────────────────────────────────────
function toggleTheme(checkbox) {
  const isDark = !checkbox.checked;  // unchecked = dark, checked = light
  document.body.classList.toggle("light-mode", !isDark);

  // Sync both toggles (login + main navbar)
  document.querySelectorAll("input[type=checkbox][onchange='toggleTheme(this)']")
    .forEach(el => { el.checked = checkbox.checked; });

  // Update label
  const lbl = document.getElementById("theme_label");
  if (lbl) lbl.textContent = isDark ? "Dark mode" : "Light mode";

  // Persist preference
  try { localStorage.setItem("sl_theme", isDark ? "dark" : "light"); } catch(e) {}
}

// Restore theme on load
(function() {
  try {
    const saved = localStorage.getItem("sl_theme");
    if (saved === "light") {
      document.body.classList.add("light-mode");
      // Will sync checkboxes after DOM ready
      document.addEventListener("DOMContentLoaded", function() {
        document.querySelectorAll("input[type=checkbox][onchange='toggleTheme(this)']")
          .forEach(el => { el.checked = true; });
        const lbl = document.getElementById("theme_label");
        if (lbl) lbl.textContent = "Light mode";
      });
    }
  } catch(e) {}
})();

// ── Tab Switching ─────────────────────────────────────────────────────────────
function switchTab(name) {
  // Hide all panels
  document.querySelectorAll(".sl-tab-panel").forEach(el => {
    el.classList.remove("active");
  });
  // Deactivate all tab buttons
  document.querySelectorAll(".sl-nav-tab").forEach(el => {
    el.classList.remove("active");
  });

  // Show target panel
  const panel = document.getElementById("tab-" + name);
  if (panel) panel.classList.add("active");

  // Activate button
  const btn = document.getElementById("tab-btn-" + name);
  if (btn) btn.classList.add("active");

  // Notify R (for dashboard refresh)
  if (window.Shiny) {
    Shiny.setInputValue("main_tabs", name, { priority: "event" });
  }
}

// ── Shiny Canvas Handlers ────────────────────────────────────────────────────
$(document).ready(function() {

  Shiny.addCustomMessageHandler("sl_init_canvas", function(msg) {
    ShinyLabel.init(msg.canvasId);
  });

  Shiny.addCustomMessageHandler("sl_load_image", function(msg) {
    ShinyLabel.loadImage(msg.src, msg.width, msg.height);
  });

  Shiny.addCustomMessageHandler("sl_load_boxes", function(boxArray) {
    ShinyLabel.loadBoxes(boxArray);
  });

  Shiny.addCustomMessageHandler("sl_update_classes", function(classArray) {
    ShinyLabel.updateClasses(classArray);
  });

  Shiny.addCustomMessageHandler("sl_set_active_class", function(classObj) {
    ShinyLabel.setActiveClass(classObj);
  });

  Shiny.addCustomMessageHandler("sl_undo",           function(_) { ShinyLabel.undo(); });
  Shiny.addCustomMessageHandler("sl_delete_selected",function(_) { ShinyLabel.deleteSelected(); });
  Shiny.addCustomMessageHandler("sl_clear_boxes",    function(_) { ShinyLabel.clearBoxes(); });

  // ── Color picker bridge ────────────────────────────────────────────────────
  // The native <input type="color"> isn't a Shiny input, so we relay its value
  $(document).on("change input", "#new_class_color", function() {
    if (window.Shiny) {
      Shiny.setInputValue("new_class_color_val", this.value, { priority: "event" });
    }
  });

  // ── Keyboard navigation ────────────────────────────────────────────────────
  $(document).on("keydown", function(e) {
    const tag = e.target.tagName.toLowerCase();
    if (tag === "input" || tag === "textarea" || tag === "select") return;

    if (e.key === "ArrowRight") $("#btn_next").click();
    if (e.key === "ArrowLeft")  $("#btn_prev").click();
  });

  // ── Export download trigger ──────────────────────────────────────────────
  // Creates a hidden <a> and clicks it — works in all browsers
  // bypasses Shiny downloadHandler which produces download.htm in some setups
  Shiny.addCustomMessageHandler("sl_trigger_download", function(msg) {
    var a = document.createElement("a");
    a.href     = msg.url;
    a.download = msg.filename;
    a.style.display = "none";
    document.body.appendChild(a);
    a.click();
    setTimeout(function() { document.body.removeChild(a); }, 1000);
  });
  Shiny.addCustomMessageHandler("sl_toast", function(msg) {
    const toast = $('<div style="position:fixed;bottom:24px;right:24px;background:var(--bg-card);border:1px solid var(--border-light);border-radius:6px;padding:12px 18px;font-family:var(--font-mono);font-size:13px;color:var(--text-primary);box-shadow:var(--shadow-lg);z-index:9999;">'
      + msg.text + '</div>');
    $("body").append(toast);
    setTimeout(function() {
      toast.fadeOut(300, function() { $(this).remove(); });
    }, msg.duration || 2000);
  });

});
