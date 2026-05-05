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
      tags$title("ShinyLabelR — R Annotation Tool"),
      # Enter key submits the name form
      tags$script(HTML("
        document.addEventListener('keydown', function(e) {
          if (e.key === 'Enter') {
            var screen = document.getElementById('screen-name');
            if (screen && screen.style.display !== 'none') {
              document.getElementById('btn_enter').click();
            }
          }
        });
      "))
    ),

    # ══ ENTRY SCREEN — just enter your name ═══════════════════════════════════
    div(id = "screen-name", class = "auth-screen", style = "display:flex;",
      div(class = "auth-card",
        div(class = "auth-logo",
          div(class = "auth-logo-mark", "SL"),
          span("ShinyLabelR")
        ),
        h1(class = "auth-title", "Welcome"),
        p(class = "auth-subtitle", "Enter your name to start annotating."),
        div(class = "auth-field",
          tags$label("Your name", class = "sl-form-label"),
          textInput("user_name", NULL,
                    placeholder = "e.g. Alice Chen",
                    width = "100%")
        ),
        actionButton("btn_enter", "Start annotating →",
                     class = "sl-btn sl-btn-primary auth-submit"),
        div(style = "margin-top:20px;text-align:center;",
          tags$label(class = "sl-toggle-wrap",
            tags$input(type = "checkbox", id = "theme_toggle",
                       onchange = "toggleTheme(this)"),
            div(class = "sl-toggle-slider",
              span(class = "sl-toggle-icon", "🌙"),
              span(class = "sl-toggle-icon", "☀️")
            )
          ),
          span(id = "theme_label",
               style = "font-size:12px;color:var(--text-muted);margin-left:8px;",
               "Dark mode")
        )
      )
    ),

    # ══ MAIN APP ══════════════════════════════════════════════════════════════
    div(id = "main-app", style = "display:none;",

      # ── Navbar ────────────────────────────────────────────────────────────
      div(class = "sl-navbar",
        div(class = "sl-navbar-brand",
          div(class = "sl-navbar-logo", "SL"),
          span(class = "sl-navbar-title", "ShinyLabelR")
        ),
        div(class = "sl-navbar-tabs",
          tags$button(class = "sl-nav-tab active", id = "tab-btn-annotate",
                      onclick = "switchTab('annotate')", "Annotate"),
          tags$button(class = "sl-nav-tab", id = "tab-btn-export",
                      onclick = "switchTab('export')", "Export"),
          tags$button(class = "sl-nav-tab", id = "tab-btn-dashboard",
                      onclick = "switchTab('dashboard');Shiny.setInputValue('load_dashboard_tab',Math.random(),{priority:'event'});",
                      "Dashboard")
        ),
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

      # ═══ ANNOTATE ═══════════════════════════════════════════════════════════
      div(id = "tab-annotate", class = "sl-tab-panel active",
        div(class = "sl-layout",

          div(class = "sl-sidebar-left",
            div(class = "sl-panel",
              div(class = "sl-panel-header",
                span(class = "sl-panel-title", "Load Images")
              ),
              div(class = "sl-panel-body",
                tags$label("From computer", class = "sl-form-label"),
                div(class = "sl-file-drop-zone",
                  fileInput("file_upload", NULL, multiple = TRUE,
                            accept = c("image/jpeg","image/png",
                                       "image/gif","image/bmp","image/webp"),
                            width = "100%",
                            buttonLabel = "Browse files…",
                            placeholder = "No files chosen")
                ),
                actionButton("btn_load_upload", "Load selected files",
                             class = "sl-btn sl-btn-primary",
                             style = "width:100%;"),
                tags$hr(class = "sl-divider"),
                tags$label("From URL", class = "sl-form-label"),
                textInput("url_input", NULL,
                          placeholder = "https://example.com/image.jpg",
                          width = "100%"),
                actionButton("btn_load_url", "Load from URL",
                             class = "sl-btn sl-btn-primary",
                             style = "width:100%;")
              )
            ),
            div(class = "sl-panel",
                style = "flex:1;overflow:hidden;display:flex;flex-direction:column;",
              div(class = "sl-panel-header",
                span(class = "sl-panel-title", "Images"),
                uiOutput("image_count_badge")
              ),
              div(style = "flex:1;overflow-y:auto;padding:6px;",
                uiOutput("image_list_ui")
              )
            )
          ),

          div(class = "sl-main",
            div(class = "sl-canvas-toolbar",
              div(class = "sl-nav-controls",
                actionButton("btn_prev", "←", class = "sl-btn sl-btn-icon"),
                uiOutput("img_counter_ui"),
                actionButton("btn_next", "→", class = "sl-btn sl-btn-icon")
              ),
              actionButton("btn_undo",   "Undo",   class = "sl-btn"),
              actionButton("btn_delete", "Delete", class = "sl-btn sl-btn-danger"),
              actionButton("btn_clear",  "Clear",  class = "sl-btn"),
              div(style = "flex:1;"),
              uiOutput("quick_class_selector"),
              actionButton("btn_save_now", "Save", class = "sl-btn sl-btn-success")
            ),
            div(class = "sl-canvas-wrapper",
              tags$canvas(id = "annotation-canvas", width = "800", height = "500")
            ),
            div(class = "sl-canvas-status", uiOutput("canvas_status_ui"))
          ),

          div(class = "sl-sidebar-right",
            div(class = "sl-panel",
              div(class = "sl-panel-header",
                span(class = "sl-panel-title", "Label Classes")
              ),
              div(class = "sl-panel-body", style = "padding:10px;",
                div(class = "sl-add-class-form",
                  tags$label("Class name", class = "sl-form-label"),
                  textInput("new_class_name", NULL,
                            placeholder = "e.g. person, car…",
                            width = "100%"),
                  tags$label("Color", class = "sl-form-label",
                             style = "margin-top:6px;"),
                  div(id = "color-swatches", class = "sl-color-swatches",
                    lapply(c("#FF6B6B","#4ECDC4","#FFE66D","#A8E6CF",
                             "#4f8ef7","#B4A7D6","#82B366","#FF8B94",
                             "#F6A623","#50C878","#FF69B4","#00BFFF"),
                      function(hex)
                        tags$div(class = "sl-swatch", `data-color` = hex,
                          style = paste0("background:", hex, ";"),
                          onclick = paste0(
                            "document.querySelectorAll('.sl-swatch').forEach(s=>s.classList.remove('selected'));",
                            "this.classList.add('selected');",
                            "Shiny.setInputValue('new_class_color_val','", hex,
                            "',{priority:'event'});"))
                    )
                  ),
                  actionButton("btn_confirm_class", "+ Add Class",
                               class = "sl-btn sl-btn-primary",
                               style = "width:100%;margin-top:10px;")
                ),
                uiOutput("class_list_separator"),
                uiOutput("class_list_ui"),
                uiOutput("class_empty_state")
              )
            ),
            div(class = "sl-panel",
                style = "flex:1;overflow:hidden;display:flex;flex-direction:column;",
              div(class = "sl-panel-header",
                span(class = "sl-panel-title", "Boxes on this image"),
                uiOutput("box_count_badge")
              ),
              div(style = "flex:1;overflow-y:auto;padding:8px;",
                uiOutput("box_list_ui")
              )
            ),
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
                  tags$tr(tags$td(class="sl-key","← →"),
                          tags$td("Prev / Next image"))
                )
              )
            )
          )
        )
      ), # end annotate

      # ═══ EXPORT ══════════════════════════════════════════════════════════════
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
                span(class = "sl-panel-title", "YOLO Ultralytics Format"),
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
                actionButton("btn_export_yolo", "Build YOLO Dataset",
                             class = "sl-btn sl-btn-primary",
                             style = "width:100%;height:42px;font-size:14px;"),
                uiOutput("export_yolo_link")
              )
            ),
            div(class = "sl-panel",
              div(class = "sl-panel-header",
                span(class = "sl-panel-title", "COCO JSON Format"),
                span(class = "sl-badge sl-badge-accent", "Python interop")
              ),
              div(class = "sl-panel-body",
                p(class = "sl-help-text",
                  "Compatible with Detectron2, MMDetection and most Python CV libraries."),
                br(), br(),
                actionButton("btn_export_coco", "Build COCO JSON",
                             class = "sl-btn",
                             style = "width:100%;height:42px;font-size:14px;"),
                uiOutput("export_coco_link")
              )
            )
          )
        )
      ), # end export

      # ═══ DASHBOARD ═══════════════════════════════════════════════════════════
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
          )
        )
      ) # end dashboard

    ) # end main-app
  )
}
