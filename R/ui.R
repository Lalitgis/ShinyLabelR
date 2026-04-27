library(shiny)
library(bslib)
library(shinyjs)

#' ShinyLabel UI
#' @export
sl_ui <- function() {
  tagList(
    useShinyjs(),
    tags$head(
      tags$link(rel = "stylesheet", href = "css/style.css"),
      tags$script(src = "js/canvas.js"),
      tags$script(src = "js/shiny_handlers.js"),
      tags$title("ShinyLabel — R Annotation Tool")
    ),

    # ── Login Screen ──────────────────────────────────────────────────────────
    div(id = "login-screen",
      div(class = "login-card",
        div(class = "login-logo", "ShinyLabel v0.1"),
        h1(class = "login-title", "Welcome"),
        p(class = "login-subtitle", "R-native YOLO annotation tool"),

        div(style = "text-align:left; margin-bottom:16px;",
          tags$label("Your name", class = "sl-form-label"),
          textInput("login_name", NULL, placeholder = "e.g. alice", width = "100%")
        ),

        div(style = "display:flex; align-items:center; gap:10px; margin-bottom:22px;",
          span(class = "sl-form-label", style = "margin:0; flex-shrink:0;", "Theme:"),
          tags$label(class = "sl-toggle-wrap",
            tags$input(type = "checkbox", id = "theme_toggle",
                       onchange = "toggleTheme(this)"),
            div(class = "sl-toggle-slider",
              span(class = "sl-toggle-icon", "🌙"),
              span(class = "sl-toggle-icon", "☀️")
            )
          ),
          span(id = "theme_label", style = "font-size:13px; color:var(--text-muted);",
               "Dark mode")
        ),

        actionButton("btn_login", "Start Annotating →",
                     class = "sl-btn sl-btn-primary",
                     style = "width:100%; height:46px; font-size:15px;")
      )
    ),

    # ── Main App ──────────────────────────────────────────────────────────────
    shinyjs::hidden(
      div(id = "main-app",

        # ── Navbar ─────────────────────────────────────────────────────────
        div(class = "sl-navbar",
          # Brand
          div(class = "sl-navbar-brand",
            div(class = "sl-navbar-logo", "SL"),
            span(class = "sl-navbar-title", "ShinyLabel")
          ),

          # Center tab navigation (in navbar)
          div(class = "sl-navbar-tabs",
            tags$button(class = "sl-nav-tab active", id = "tab-btn-annotate",
                        onclick = "switchTab('annotate')", "✏ Annotate"),
            tags$button(class = "sl-nav-tab", id = "tab-btn-export",
                        onclick = "switchTab('export')", "📦 Export"),
            tags$button(class = "sl-nav-tab", id = "tab-btn-dashboard",
                        onclick = "switchTab('dashboard')", "📊 Dashboard")
          ),

          # Right: stats + user + theme toggle
          div(class = "sl-navbar-meta",
            uiOutput("navbar_progress"),
            uiOutput("navbar_user"),
            tags$label(class = "sl-toggle-wrap sl-toggle-small",
              tags$input(type = "checkbox", id = "main_theme_toggle",
                         onchange = "toggleTheme(this)"),
              div(class = "sl-toggle-slider",
                span(class = "sl-toggle-icon", "🌙"),
                span(class = "sl-toggle-icon", "☀️")
              )
            )
          )
        ),

        # ═══ ANNOTATE PANEL ═════════════════════════════════════════════════
        div(id = "tab-annotate", class = "sl-tab-panel active",
          div(class = "sl-layout",

            # ── LEFT SIDEBAR ──────────────────────────────────────────────
            div(class = "sl-sidebar-left",

              div(class = "sl-panel",
                div(class = "sl-panel-header",
                  span(class = "sl-panel-title", "Load Images")
                ),
                div(class = "sl-panel-body",

                  # --- Upload files ---
                  tags$label("From Computer", class = "sl-form-label"),
                  div(class = "sl-file-drop-zone",
                    fileInput("file_upload", NULL,
                              multiple    = TRUE,
                              accept      = c("image/jpeg","image/png",
                                              "image/gif","image/bmp","image/webp"),
                              width       = "100%",
                              buttonLabel = "📁 Browse Files…",
                              placeholder = "No files chosen")
                  ),
                  actionButton("btn_load_upload", "⬆ Load Selected Files",
                               class = "sl-btn sl-btn-primary",
                               style = "width:100%;"),

                  tags$hr(class = "sl-divider"),

                  # --- URL ---
                  tags$label("From URL", class = "sl-form-label"),
                  textInput("url_input", NULL,
                            placeholder = "https://example.com/image.jpg",
                            width = "100%"),
                  actionButton("btn_load_url", "⬆ Load from URL",
                               class = "sl-btn sl-btn-primary",
                               style = "width:100%;")
                )
              ),

              # Image list
              div(class = "sl-panel",
                  style = "flex:1; overflow:hidden; display:flex; flex-direction:column;",
                div(class = "sl-panel-header",
                  span(class = "sl-panel-title", "Images"),
                  uiOutput("image_count_badge")
                ),
                div(style = "flex:1; overflow-y:auto; padding:6px;",
                  uiOutput("image_list_ui")
                )
              )
            ),

            # ── CENTER: Canvas ────────────────────────────────────────────
            div(class = "sl-main",
              div(class = "sl-canvas-toolbar",
                div(class = "sl-nav-controls",
                  actionButton("btn_prev", "◀", class = "sl-btn sl-btn-icon"),
                  uiOutput("img_counter_ui"),
                  actionButton("btn_next", "▶", class = "sl-btn sl-btn-icon")
                ),
                div(class = "sl-toolbar-divider"),
                actionButton("btn_undo",   "↩ Undo",   class = "sl-btn"),
                actionButton("btn_delete", "✕ Delete", class = "sl-btn sl-btn-danger"),
                actionButton("btn_clear",  "⬜ Clear",  class = "sl-btn"),
                div(style = "flex:1;"),
                uiOutput("quick_class_selector"),
                div(class = "sl-toolbar-divider"),
                actionButton("btn_save_now", "💾 Save", class = "sl-btn sl-btn-success")
              ),

              div(class = "sl-canvas-wrapper",
                tags$canvas(id = "annotation-canvas", width = "800", height = "500")
              ),

              div(class = "sl-canvas-status",
                uiOutput("canvas_status_ui")
              )
            ),

            # ── RIGHT SIDEBAR ─────────────────────────────────────────────
            div(class = "sl-sidebar-right",

              # Classes — empty by default, user adds their own
              div(class = "sl-panel",
                div(class = "sl-panel-header",
                  span(class = "sl-panel-title", "Label Classes")
                ),
                div(class = "sl-panel-body", style = "padding:10px;",

                  # Add class form
                  div(class = "sl-add-class-form",
                    tags$label("Class name", class = "sl-form-label"),
                    textInput("new_class_name", NULL,
                              placeholder = "e.g. person, car, drone…",
                              width = "100%"),
                    tags$label("Color", class = "sl-form-label",
                               style = "margin-top:6px;"),
                    # Preset color swatches — no OS picker, no layout overflow
                    div(id = "color-swatches", class = "sl-color-swatches",
                      lapply(c("#FF6B6B","#4ECDC4","#FFE66D","#A8E6CF",
                               "#4f8ef7","#B4A7D6","#82B366","#FF8B94",
                               "#F6A623","#50C878","#FF69B4","#00BFFF"),
                        function(hex) {
                          tags$div(class = "sl-swatch",
                            `data-color` = hex,
                            style = paste0("background:", hex, ";"),
                            onclick = paste0(
                              "document.querySelectorAll('.sl-swatch').forEach(s=>s.classList.remove('selected'));",
                              "this.classList.add('selected');",
                              "Shiny.setInputValue('new_class_color_val','", hex,
                              "',{priority:'event'});"
                            )
                          )
                        })
                    ),
                    actionButton("btn_confirm_class", "＋ Add Class",
                                 class = "sl-btn sl-btn-primary",
                                 style = "width:100%; margin-top:10px;")
                  ),

                  uiOutput("class_list_separator"),
                  uiOutput("class_list_ui"),
                  uiOutput("class_empty_state")
                )
              ),

              # Box list
              div(class = "sl-panel",
                  style = "flex:1; overflow:hidden; display:flex; flex-direction:column;",
                div(class = "sl-panel-header",
                  span(class = "sl-panel-title", "Boxes on this image"),
                  uiOutput("box_count_badge")
                ),
                div(style = "flex:1; overflow-y:auto; padding:8px;",
                  uiOutput("box_list_ui")
                )
              ),

              # Shortcuts
              div(class = "sl-panel",
                div(class = "sl-panel-header",
                  span(class = "sl-panel-title", "Shortcuts")
                ),
                div(class = "sl-panel-body",
                  tags$table(class = "sl-shortcuts-table",
                    tags$tr(tags$td(class="sl-key","Del / ⌫"),
                            tags$td("Delete selected box")),
                    tags$tr(tags$td(class="sl-key","Ctrl+Z"),
                            tags$td("Undo")),
                    tags$tr(tags$td(class="sl-key","Esc"),
                            tags$td("Deselect")),
                    tags$tr(tags$td(class="sl-key","◀  ▶"),
                            tags$td("Prev / Next image"))
                  )
                )
              )
            )
          )
        ), # end annotate

        # ═══ EXPORT PANEL ═══════════════════════════════════════════════════
        div(id = "tab-export", class = "sl-tab-panel",
          div(class = "sl-page-content",
            div(class = "sl-page-header",
              h2(class = "sl-page-title", "Export Annotations"),
              p(class = "sl-page-subtitle",
                "Download your dataset ready for YOLO training or Python CV tools.")
            ),
            div(class = "sl-export-grid",
              div(class = "sl-panel",
                div(class = "sl-panel-header",
                  span(class = "sl-panel-title", "🎯 YOLO Ultralytics Format"),
                  span(class = "sl-badge sl-badge-success", "Recommended")
                ),
                div(class = "sl-panel-body",
                  p(class = "sl-help-text",
                    "Generates images/train, images/val, labels/train, labels/val and data.yaml"),
                  tags$label("Train / Val Split", class = "sl-form-label"),
                  sliderInput("export_split", NULL,
                              min = 0.5, max = 0.95, value = 0.8,
                              step = 0.05, width = "100%", ticks = FALSE),
                  uiOutput("export_yolo_summary"),
                  br(),
                  actionButton("btn_export_yolo", "⚙ Build YOLO Dataset",
                               class = "sl-btn sl-btn-primary",
                               style = "width:100%; height:46px; font-size:15px;"),
                  uiOutput("export_yolo_link")
                )
              ),
              div(class = "sl-panel",
                div(class = "sl-panel-header",
                  span(class = "sl-panel-title", "🐍 COCO JSON Format"),
                  span(class = "sl-badge sl-badge-accent", "Python interop")
                ),
                div(class = "sl-panel-body",
                  p(class = "sl-help-text",
                    "Compatible with Detectron2, MMDetection and most Python CV libraries."),
                  br(), br(),
                  actionButton("btn_export_coco", "⚙ Build COCO JSON",
                               class = "sl-btn",
                               style = "width:100%; height:46px; font-size:15px;"),
                  uiOutput("export_coco_link")
                )
              )
            )
          )
        ), # end export

        # ═══ DASHBOARD PANEL ════════════════════════════════════════════════
        div(id = "tab-dashboard", class = "sl-tab-panel",
          div(class = "sl-page-content",
            div(class = "sl-page-header",
              h2(class = "sl-page-title", "Annotation Progress"),
              p(class = "sl-page-subtitle",
                "Live overview of your team's annotation work.")
            ),
            uiOutput("dashboard_stats"),
            div(class = "sl-dashboard-grid",
              div(class = "sl-panel",
                div(class = "sl-panel-header",
                  span(class = "sl-panel-title", "Boxes per Class")
                ),
                div(class = "sl-panel-body",
                  plotOutput("plot_class_dist", height = "260px")
                )
              ),
              div(class = "sl-panel",
                div(class = "sl-panel-header",
                  span(class = "sl-panel-title", "Annotator Contributions")
                ),
                div(class = "sl-panel-body",
                  plotOutput("plot_annotator", height = "260px")
                )
              )
            ),
            div(class = "sl-panel", style = "margin-top:20px;",
              div(class = "sl-panel-header",
                span(class = "sl-panel-title", "Annotator Detail")
              ),
              div(class = "sl-panel-body",
                DT::dataTableOutput("tbl_annotators")
              )
            ),
            div(class = "sl-panel", style = "margin-top:20px;",
              div(class = "sl-panel-header",
                span(class = "sl-panel-title", "Images Needing Annotation"),
                uiOutput("todo_count_badge")
              ),
              div(class = "sl-panel-body",
                DT::dataTableOutput("tbl_todo_images")
              )
            )
          )
        ) # end dashboard

      ) # end main-app
    ) # end hidden
  )
}
