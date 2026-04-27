library(shiny)
library(shinyjs)
library(ggplot2)
library(DT)

#' ShinyLabel Server
#' @param db_path Path to SQLite database file
#' @export
sl_server <- function(db_path = "shinylabel.db") {
  # Resolve www/ folder relative to db_path location
  app_dir <- normalizePath(dirname(db_path), mustWork = FALSE)

  function(input, output, session) {

    # ── DB connection ─────────────────────────────────────────────────────────
    con <- sl_init_db(db_path)
    session$onSessionEnded(function() {
      tryCatch(DBI::dbDisconnect(con), error = function(e) NULL)
    })

    # ── Reactive state ────────────────────────────────────────────────────────
    rv <- reactiveValues(
      annotator    = NULL,
      images       = data.frame(),
      current_idx  = 0L,
      current_img  = NULL,       # stored as named list via as.list(row)
      classes      = data.frame(),
      active_class = NULL,
      canvas_boxes = list(),     # list of named R lists, one per box
      save_counter = 0L          # increments on every save to refresh UI
    )

    # ════════════════════════════════════════════════════════════════════════
    # HELPERS
    # ════════════════════════════════════════════════════════════════════════

    # Coerce one JS box to a clean named R list.
    # JS may send named vectors OR named lists depending on R version.
    coerce_box <- function(b) {
      tryCatch({
        if (is.null(b)) return(NULL)
        if (!is.list(b)) b <- as.list(b)
        list(
          class_id      = as.integer(b[["class_id"]]),
          class_name    = as.character(b[["class_name"]]),
          color_hex     = as.character(b[["color_hex"]]  %||% "#999999"),
          x_pixel       = as.numeric(b[["x_pixel"]]),
          y_pixel       = as.numeric(b[["y_pixel"]]),
          w_pixel       = as.numeric(b[["w_pixel"]]),
          h_pixel       = as.numeric(b[["h_pixel"]]),
          x_center_norm = as.numeric(b[["x_center_norm"]]),
          y_center_norm = as.numeric(b[["y_center_norm"]]),
          w_norm        = as.numeric(b[["w_norm"]]),
          h_norm        = as.numeric(b[["h_norm"]])
        )
      }, error = function(e) {
        message("[ShinyLabel] coerce_box: ", e$message)
        NULL
      })
    }

    # Count boxes per class_name from rv$canvas_boxes.
    # Returns a named integer vector.  Safe on empty / NULL input.
    canvas_class_counts <- function(boxes) {
      out <- setNames(integer(0), character(0))
      if (length(boxes) == 0L) return(out)
      for (b in boxes) {
        cn <- tryCatch(as.character(b[["class_name"]]), error = function(e) NA_character_)
        if (!is.null(cn) && !is.na(cn) && nzchar(cn)) {
          out[cn] <- (out[cn] %||% 0L) + 1L
        }
      }
      out
    }

    # ════════════════════════════════════════════════════════════════════════
    # LOGIN
    # ════════════════════════════════════════════════════════════════════════
    observeEvent(input$btn_login, {
      name <- trimws(input$login_name)
      if (!nzchar(name)) {
        showNotification("Please enter your name.", type = "error")
        return()
      }
      rv$annotator <- name
      DBI::dbExecute(con,
        "INSERT INTO sessions (annotator_name) VALUES (?)", params = list(name))

      shinyjs::hide("login-screen")
      shinyjs::show("main-app")

      rv$images  <- sl_get_images(con)
      rv$classes <- sl_get_classes(con)

      if (nrow(rv$classes) > 0L) {
        rv$active_class <- list(
          id    = rv$classes$class_id[1],
          name  = rv$classes$class_name[1],
          color = rv$classes$color_hex[1]
        )
      }
      session$sendCustomMessage("sl_init_canvas", list(canvasId = "annotation-canvas"))
      push_classes_to_canvas()
    })

    # ════════════════════════════════════════════════════════════════════════
    # CANVAS HELPERS
    # ════════════════════════════════════════════════════════════════════════
    push_classes_to_canvas <- function() {
      cls <- rv$classes
      if (is.null(cls) || nrow(cls) == 0L) return()
      class_list <- lapply(seq_len(nrow(cls)), function(i)
        list(id = cls$class_id[i], name = cls$class_name[i], color = cls$color_hex[i]))
      session$sendCustomMessage("sl_update_classes", class_list)
      if (!is.null(rv$active_class))
        session$sendCustomMessage("sl_set_active_class", rv$active_class)
    }

    # ════════════════════════════════════════════════════════════════════════
    # IMAGE LOADING
    # ════════════════════════════════════════════════════════════════════════
    img_dest_dir <- function() {
      d <- file.path(dirname(normalizePath(db_path, mustWork = FALSE)), "images")
      dir.create(d, showWarnings = FALSE, recursive = TRUE)
      d
    }

    # Upload from computer
    observeEvent(input$btn_load_upload, {
      req(input$file_upload)
      files  <- input$file_upload
      added  <- 0L
      dest_d <- img_dest_dir()

      withProgress(message = "Loading images…", value = 0, {
        for (i in seq_len(nrow(files))) {
          # Warn if image is very large — will load slowly via base64
          file_mb <- file.info(files$datapath[i])$size / 1024^2
          if (file_mb > 20) {
            showNotification(
              paste0(files$name[i], " is ", round(file_mb, 1),
                     "MB — large images may load slowly."),
              type = "warning", duration = 6)
          }

          info <- sl_image_info(files$datapath[i])
          if (info$width == 0L) next
          dest <- file.path(dest_d, files$name[i])
          file.copy(files$datapath[i], dest, overwrite = FALSE)
          sl_add_image(con, dest, files$name[i],
                       info$width, info$height, "upload", rv$annotator)
          added <- added + 1L
          incProgress(1 / nrow(files))
        }
      })

      rv$images <- sl_get_images(con)
      showNotification(paste0("Loaded ", added, " image(s) ✓"), type = "message")
      if (nrow(rv$images) > 0L && rv$current_idx == 0L) navigate_to(1L)
    })

    # Load from URL
    observeEvent(input$btn_load_url, {
      url <- trimws(input$url_input)
      if (!nzchar(url)) {
        showNotification("Please enter a URL.", type = "warning")
        return()
      }
      tryCatch({
        showNotification("Downloading…", type = "message", duration = 2)
        result <- sl_fetch_url_image(url)
        dest   <- file.path(img_dest_dir(), result$filename)
        file.copy(result$local_path, dest, overwrite = FALSE)
        sl_add_image(con, dest, result$filename,
                     result$width, result$height, "url", rv$annotator)
        rv$images <- sl_get_images(con)
        updateTextInput(session, "url_input", value = "")
        showNotification(paste0("Loaded: ", result$filename, " ✓"), type = "message")
        if (rv$current_idx == 0L) navigate_to(nrow(rv$images))
      }, error = function(e)
        showNotification(paste("Download failed:", e$message), type = "error"))
    })

    # ════════════════════════════════════════════════════════════════════════
    # NAVIGATION
    # ════════════════════════════════════════════════════════════════════════
    navigate_to <- function(idx) {
      imgs <- rv$images
      if (nrow(imgs) == 0L) return()
      idx <- max(1L, min(as.integer(idx), nrow(imgs)))

      if (rv$current_idx > 0L) auto_save(silent = TRUE)

      rv$current_idx <- idx
      rv$current_img <- as.list(imgs[idx, ])

      img_path <- imgs$filepath[idx]
      img_w    <- as.integer(imgs$img_width[idx])
      img_h    <- as.integer(imgs$img_height[idx])

      if (file.exists(img_path)) {
        tryCatch({
          b64 <- sl_image_b64(img_path)
          session$sendCustomMessage("sl_load_image",
            list(src = b64, width = img_w, height = img_h))
        }, error = function(e)
          showNotification(paste("Cannot read image:", e$message), type = "error"))
      } else {
        showNotification(paste("Image not found:", img_path), type = "warning")
      }

      img_id   <- imgs$id[idx]
      existing <- sl_load_annotations(con, img_id)

      if (nrow(existing) > 0L) {
        box_list <- lapply(seq_len(nrow(existing)), function(i) {
          r <- existing[i, ]
          list(class_id      = as.integer(r$class_id),
               class_name    = as.character(r$class_name),
               color_hex     = as.character(r$color_hex),
               x_pixel       = as.numeric(r$x_pixel),
               y_pixel       = as.numeric(r$y_pixel),
               w_pixel       = as.numeric(r$w_pixel),
               h_pixel       = as.numeric(r$h_pixel),
               x_center_norm = as.numeric(r$x_center_norm),
               y_center_norm = as.numeric(r$y_center_norm),
               w_norm        = as.numeric(r$w_norm),
               h_norm        = as.numeric(r$h_norm))
        })
        session$sendCustomMessage("sl_load_boxes", box_list)
        rv$canvas_boxes <- box_list
      } else {
        session$sendCustomMessage("sl_load_boxes", list())
        rv$canvas_boxes <- list()
      }
    }

    observeEvent(input$btn_next, {
      if (nrow(rv$images) == 0L || rv$current_idx >= nrow(rv$images)) return()
      navigate_to(rv$current_idx + 1L)
    })
    observeEvent(input$btn_prev, {
      if (rv$current_idx <= 1L) return()
      navigate_to(rv$current_idx - 1L)
    })
    observeEvent(input$img_list_click, {
      idx <- suppressWarnings(as.integer(input$img_list_click))
      if (!is.na(idx)) navigate_to(idx)
    })

    # ════════════════════════════════════════════════════════════════════════
    # CANVAS → R  (boxes sent from JS on every draw/edit)
    # ════════════════════════════════════════════════════════════════════════
    observeEvent(input$canvas_boxes, {
      raw <- input$canvas_boxes
      if (is.null(raw)) { rv$canvas_boxes <- list(); return() }
      if (!is.list(raw)) raw <- list(raw)
      rv$canvas_boxes <- Filter(Negate(is.null), lapply(raw, coerce_box))
    })

    # ════════════════════════════════════════════════════════════════════════
    # SAVE
    # ════════════════════════════════════════════════════════════════════════
    auto_save <- function(silent = FALSE) {
      img_meta <- rv$current_img
      if (is.null(img_meta)) return()
      img_id <- as.integer(img_meta[["id"]])
      img_w  <- as.integer(img_meta[["img_width"]])
      img_h  <- as.integer(img_meta[["img_height"]])
      if (is.na(img_id) || img_id == 0L) return()

      boxes_raw <- rv$canvas_boxes
      if (length(boxes_raw) == 0L) {
        sl_save_annotations(con, img_id, NULL, rv$annotator)
      } else {
        box_df <- do.call(rbind, lapply(boxes_raw, function(b) {
          norm <- px_to_yolo_norm(
            as.numeric(b[["x_pixel"]]), as.numeric(b[["y_pixel"]]),
            as.numeric(b[["w_pixel"]]), as.numeric(b[["h_pixel"]]),
            img_w, img_h)
          data.frame(
            class_id      = as.integer(b[["class_id"]]),
            class_name    = as.character(b[["class_name"]]),
            x_pixel       = as.numeric(b[["x_pixel"]]),
            y_pixel       = as.numeric(b[["y_pixel"]]),
            w_pixel       = as.numeric(b[["w_pixel"]]),
            h_pixel       = as.numeric(b[["h_pixel"]]),
            x_center_norm = norm$x_center_norm,
            y_center_norm = norm$y_center_norm,
            w_norm        = norm$w_norm,
            h_norm        = norm$h_norm,
            stringsAsFactors = FALSE)
        }))
        sl_save_annotations(con, img_id, box_df, rv$annotator)
      }

      rv$images <- sl_get_images(con)
      if (rv$current_idx > 0L && rv$current_idx <= nrow(rv$images))
        rv$current_img <- as.list(rv$images[rv$current_idx, ])
      rv$save_counter <- rv$save_counter + 1L

      if (!silent) showNotification("Saved ✓", type = "message", duration = 1.5)
    }

    observeEvent(input$btn_save_now, auto_save(silent = FALSE))

    # ════════════════════════════════════════════════════════════════════════
    # CANVAS TOOLBAR
    # ════════════════════════════════════════════════════════════════════════
    observeEvent(input$btn_undo,   session$sendCustomMessage("sl_undo",            list()))
    observeEvent(input$btn_delete, session$sendCustomMessage("sl_delete_selected", list()))
    observeEvent(input$btn_clear,  session$sendCustomMessage("sl_clear_boxes",     list()))

    observeEvent(input$quick_class_select, {
      cid <- suppressWarnings(as.integer(input$quick_class_select))
      if (is.na(cid)) return()
      cls <- rv$classes[rv$classes$class_id == cid, ]
      if (nrow(cls) == 0L) return()
      rv$active_class <- list(id = cls$class_id[1], name = cls$class_name[1],
                               color = cls$color_hex[1])
      session$sendCustomMessage("sl_set_active_class", rv$active_class)
    })

    observeEvent(input$class_click, {
      cid <- suppressWarnings(as.integer(input$class_click))
      if (is.na(cid)) return()
      cls <- rv$classes[rv$classes$class_id == cid, ]
      if (nrow(cls) == 0L) return()
      rv$active_class <- list(id = cls$class_id[1], name = cls$class_name[1],
                               color = cls$color_hex[1])
      session$sendCustomMessage("sl_set_active_class", rv$active_class)
    })

    # ════════════════════════════════════════════════════════════════════════
    # CLASS MANAGEMENT
    # ════════════════════════════════════════════════════════════════════════
    observeEvent(input$btn_confirm_class, {
      name <- trimws(input$new_class_name)
      if (!nzchar(name)) {
        showNotification("Please enter a class name.", type = "error")
        return()
      }
      # Color from native <input type="color"> relayed via shiny_handlers.js
      color <- tryCatch({
        v <- input$new_class_color_val
        if (!is.null(v) && nzchar(v) && grepl("^#[0-9A-Fa-f]{6}$", v)) v
        else {
          pal <- c("#FF6B6B","#4ECDC4","#FFE66D","#A8E6CF","#FF8B94",
                   "#B4A7D6","#D5E8D4","#FFD966","#82B366","#DAE8FC")
          n <- DBI::dbGetQuery(con, "SELECT COUNT(*) as n FROM classes")$n
          pal[(n %% length(pal)) + 1L]
        }
      }, error = function(e) "#4f8ef7")

      sl_add_class(con, name, color)
      rv$classes <- sl_get_classes(con)

      new_cls <- rv$classes[rv$classes$class_name == name, ]
      if (nrow(new_cls) > 0L)
        rv$active_class <- list(id    = new_cls$class_id[1],
                                name  = new_cls$class_name[1],
                                color = new_cls$color_hex[1])
      push_classes_to_canvas()
      updateTextInput(session, "new_class_name", value = "")
      showNotification(paste0("Class '", name, "' added ✓"), type = "message", duration = 2)
    })

    # ════════════════════════════════════════════════════════════════════════
    # EXPORTS
    # Strategy: actionButton BUILDS the zip, downloadHandler SERVES it.
    # This gives the browser a proper Content-Disposition: attachment header
    # which triggers the "Save As" dialog on all browsers.
    # ════════════════════════════════════════════════════════════════════════

    on_cloud <- nzchar(Sys.getenv("SHINYAPPS_TOKEN")) || nzchar(Sys.getenv("SHINY_HOST"))
    exports_dir <- if (on_cloud) file.path(tempdir(), "sl_exports") else file.path(app_dir, "www", "exports")
    dir.create(exports_dir, showWarnings = FALSE, recursive = TRUE)
    shiny::addResourcePath("exports", exports_dir)

    # Store the built zip/json paths so downloadHandler can serve them
    yolo_zip_path <- reactiveVal(NULL)
    coco_json_path <- reactiveVal(NULL)

    # Step 1: BUILD the zip
    observeEvent(input$btn_export_yolo, {
      auto_save(silent = TRUE)

      if (nrow(rv$classes) == 0L) {
        showNotification("Add at least one class before exporting.", type = "error")
        return()
      }
      if (sum(rv$images$box_count > 0, na.rm = TRUE) == 0L) {
        showNotification("Annotate at least one image before exporting.", type = "error")
        return()
      }

      yolo_zip_path(NULL)   # reset

      withProgress(message = "Building YOLO dataset…", value = 0, {
        incProgress(0.6, detail = "Writing .txt label files and zipping…")
        zp <- tryCatch(
          sl_export_yolo(con, output_dir = exports_dir,
                         split_ratio = input$export_split %||% 0.8),
          error = function(e) {
            showNotification(paste("Export failed:", e$message),
                             type = "error", duration = 15)
            NULL
          }
        )
        incProgress(0.4, detail = "Ready!")
      })

      if (is.null(zp) || !file.exists(zp)) return()

      yolo_zip_path(zp)
      showNotification("✓ Dataset built! Click the green button to download.",
                       type = "message", duration = 4)
    })

    # Step 2: DOWNLOAD via proper downloadHandler (triggers Save As dialog)
    output$dl_yolo_ready <- downloadHandler(
      filename = function() {
        paste0("yolo_annotations_", format(Sys.Date(), "%Y%m%d"), ".zip")
      },
      content = function(file) {
        zp <- yolo_zip_path()
        if (is.null(zp) || !file.exists(zp)) {
          showNotification("Please click 'Build' first.", type = "error")
          return()
        }
        file.copy(zp, file, overwrite = TRUE)
      },
      contentType = "application/zip"
    )

    output$export_yolo_link <- renderUI({
      zp <- yolo_zip_path()
      if (is.null(zp)) return(NULL)
      div(style = "margin-top:10px;",
        downloadButton("dl_yolo_ready",
                       "⬇ Save YOLO Dataset (.zip)",
                       class = "sl-btn sl-btn-success",
                       style = "width:100%; height:46px; font-size:15px;
                                justify-content:center;")
      )
    })

    # COCO export
    observeEvent(input$btn_export_coco, {
      auto_save(silent = TRUE)

      if (nrow(rv$classes) == 0L) {
        showNotification("Add at least one class before exporting.", type = "error")
        return()
      }

      coco_json_path(NULL)

      jp <- tryCatch(
        sl_export_coco(con, output_dir = exports_dir),
        error = function(e) {
          showNotification(paste("COCO export failed:", e$message),
                           type = "error", duration = 12)
          NULL
        }
      )
      if (is.null(jp) || !file.exists(jp)) return()
      coco_json_path(jp)
      showNotification("✓ COCO JSON ready! Click the green button to download.",
                       type = "message", duration = 4)
    })

    output$dl_coco_ready <- downloadHandler(
      filename = function() {
        paste0("coco_annotations_", format(Sys.Date(), "%Y%m%d"), ".json")
      },
      content = function(file) {
        jp <- coco_json_path()
        if (is.null(jp) || !file.exists(jp)) return()
        file.copy(jp, file, overwrite = TRUE)
      },
      contentType = "application/json"
    )

    output$export_coco_link <- renderUI({
      jp <- coco_json_path()
      if (is.null(jp)) return(NULL)
      div(style = "margin-top:10px;",
        downloadButton("dl_coco_ready",
                       "⬇ Save COCO JSON",
                       class = "sl-btn sl-btn-success",
                       style = "width:100%; height:46px; font-size:15px;
                                justify-content:center;")
      )
    })

    # ════════════════════════════════════════════════════════════════════════
    # UI OUTPUTS
    # ════════════════════════════════════════════════════════════════════════

    output$navbar_progress <- renderUI({
      rv$save_counter
      imgs <- rv$images
      if (nrow(imgs) == 0L) return(NULL)
      done  <- sum(imgs$status == "done", na.rm = TRUE)
      total <- nrow(imgs)
      tagList(
        span(class = "sl-badge sl-badge-accent",
             paste0(done, "/", total, " images")),
        span(class = "sl-badge sl-badge-success",
             paste0(round(done/total*100), "% done"))
      )
    })

    output$navbar_user <- renderUI({
      req(rv$annotator)
      span(class = "sl-badge sl-badge-warning", paste0("👤 ", rv$annotator))
    })

    output$image_list_ui <- renderUI({
      rv$save_counter
      imgs <- rv$images
      if (nrow(imgs) == 0L) {
        return(div(style="text-align:center;padding:28px 12px;",
          tags$span(style="font-size:32px;","🖼"),
          p(style="color:var(--text-dim);font-size:14px;margin-top:10px;",
            "No images loaded yet.", br(), "Use Upload or URL above.")))
      }
      tags$ul(class = "sl-image-list",
        lapply(seq_len(nrow(imgs)), function(i) {
          img  <- imgs[i, ]
          done <- !is.na(img$status) && img$status == "done"
          tags$li(
            class = paste("sl-image-item",
                          if (i == rv$current_idx) "active" else "",
                          if (done) "done" else "todo"),
            onclick = sprintf(
              "Shiny.setInputValue('img_list_click',%d,{priority:'event'});", i),
            span(class = "sl-img-status", if (done) "✓" else "○"),
            span(class = "sl-img-name",   img$filename),
            span(class = "sl-img-count",  paste0(img$box_count, "b"))
          )
        })
      )
    })

    output$image_count_badge <- renderUI({
      span(class = "sl-badge sl-badge-accent", nrow(rv$images))
    })

    output$img_counter_ui <- renderUI({
      span(class = "sl-img-counter",
           if (nrow(rv$images) == 0L) "—/—"
           else paste0(rv$current_idx, "/", nrow(rv$images)))
    })

    output$class_empty_state <- renderUI({
      if (nrow(rv$classes) > 0L) return(NULL)
      div(style="text-align:center;padding:14px 8px;color:var(--text-dim);font-size:13px;",
          "⬆ Type a name and click Add Class to start.")
    })

    output$class_list_separator <- renderUI({
      if (nrow(rv$classes) == 0L) return(NULL)
      tags$hr(class = "sl-divider")
    })

    # ── Class list with LIVE annotation counts ────────────────────────────
    # Counts = (all DB annotations) - (saved for current image) + (live canvas)
    # This gives an accurate running total that updates on every box drawn.
    output$class_list_ui <- renderUI({
      rv$canvas_boxes   # invalidate when canvas changes
      rv$save_counter   # invalidate when saved

      cls <- rv$classes
      if (nrow(cls) == 0L) return(NULL)
      active_id <- rv$active_class$id

      # All saved annotations in DB
      db_all <- tryCatch(
        DBI::dbGetQuery(con,
          "SELECT class_name, COUNT(*) as n FROM annotations GROUP BY class_name"),
        error = function(e) data.frame(class_name=character(), n=integer()))
      db_all_map <- setNames(as.integer(db_all$n), as.character(db_all$class_name))

      # Saved annotations for CURRENT image only (to subtract before adding canvas)
      cur_id <- tryCatch(as.integer(rv$current_img[["id"]]),
                         error = function(e) NA_integer_)
      db_cur_map <- setNames(integer(0), character(0))
      if (!is.null(cur_id) && !is.na(cur_id) && cur_id > 0L) {
        res <- tryCatch(
          DBI::dbGetQuery(con,
            "SELECT class_name, COUNT(*) as n FROM annotations WHERE image_id=? GROUP BY class_name",
            params = list(cur_id)),
          error = function(e) data.frame(class_name=character(), n=integer()))
        if (nrow(res) > 0L)
          db_cur_map <- setNames(as.integer(res$n), as.character(res$class_name))
      }

      # Live canvas boxes (not yet saved)
      canvas_map <- canvas_class_counts(rv$canvas_boxes)

      div(class = "sl-class-list",
        lapply(seq_len(nrow(cls)), function(i) {
          cr        <- cls[i, ]
          is_active <- !is.null(active_id) && !is.na(cr$class_id) &&
                       cr$class_id == active_id
          cn        <- as.character(cr$class_name)

          n_db_all  <- if (cn %in% names(db_all_map)) db_all_map[cn] else 0L
          n_db_cur  <- if (cn %in% names(db_cur_map)) db_cur_map[cn] else 0L
          n_canvas  <- if (cn %in% names(canvas_map)) canvas_map[cn] else 0L
          total     <- as.integer(max(0L, n_db_all - n_db_cur + n_canvas))

          div(class = paste("sl-class-item", if (is_active) "active" else ""),
            onclick = sprintf(
              "Shiny.setInputValue('class_click',%d,{priority:'event'});",
              as.integer(cr$class_id)),
            div(class = "sl-class-dot",
                style = paste0("background:", cr$color_hex, ";")),
            span(class = "sl-class-name", cn),
            span(class = paste("sl-class-count",
                               if (total > 0L) "has-annotations" else ""),
                 total)
          )
        })
      )
    })

    output$quick_class_selector <- renderUI({
      cls <- rv$classes
      if (nrow(cls) == 0L)
        return(span(style="font-size:12px;color:var(--text-muted);font-family:var(--font-mono);",
                    "← Add a class first"))
      selectInput("quick_class_select", NULL,
                  choices  = setNames(as.integer(cls$class_id), cls$class_name),
                  selected = rv$active_class$id,
                  width    = "150px")
    })

    output$box_list_ui <- renderUI({
      boxes <- rv$canvas_boxes
      if (length(boxes) == 0L) {
        return(div(style="text-align:center;padding:24px 8px;",
          tags$span(style="font-size:28px;","⬜"),
          p(style="color:var(--text-dim);font-size:13px;margin-top:8px;",
            "Drag on the image to draw a box.")))
      }
      div(class = "sl-box-list",
        lapply(seq_along(boxes), function(i) {
          b <- boxes[[i]]
          div(class = "sl-box-item",
            div(class = "sl-class-dot",
                style = paste0("background:", as.character(b[["color_hex"]] %||% "#999"), ";")),
            div(class = "sl-box-label",  as.character(b[["class_name"]] %||% "?")),
            div(class = "sl-box-coords",
                sprintf("%d × %d px", round(as.numeric(b[["w_pixel"]])),
                                      round(as.numeric(b[["h_pixel"]]))))
          )
        })
      )
    })

    output$box_count_badge <- renderUI({
      n <- length(rv$canvas_boxes)
      span(class = if (n > 0L) "sl-badge sl-badge-success" else "sl-badge sl-badge-accent", n)
    })

    output$canvas_status_ui <- renderUI({
      rv$save_counter
      img <- rv$current_img
      if (is.null(img))
        return(span(style="color:var(--text-dim);","Load an image to begin"))
      n      <- length(rv$canvas_boxes)
      fname  <- as.character(img[["filename"]]  %||% "—")
      iw     <- as.integer(img[["img_width"]]   %||% 0L)
      ih     <- as.integer(img[["img_height"]]  %||% 0L)
      aname  <- as.character(rv$active_class$name %||% "none")
      tagList(
        span(style="color:var(--text-primary);", paste0("📷 ", fname)),
        span(style="margin:0 10px;color:var(--border);","|"),
        span(paste0(iw, " × ", ih, " px")),
        span(style="margin:0 10px;color:var(--border);","|"),
        span(paste0(n, " box", if (n != 1L) "es" else "")),
        span(style="margin:0 10px;color:var(--border);","|"),
        span(style="color:var(--accent);", paste0("Class: ", aname))
      )
    })

    output$export_yolo_summary <- renderUI({
      rv$save_counter
      imgs      <- rv$images
      annotated <- sum(imgs$box_count > 0L, na.rm = TRUE)
      split     <- input$export_split %||% 0.8
      n_train   <- floor(annotated * split)
      n_val     <- annotated - n_train
      div(style="display:flex;gap:10px;flex-wrap:wrap;margin-top:8px;",
        span(class="sl-badge sl-badge-success", paste0(annotated, " annotated images")),
        span(class="sl-badge sl-badge-accent",  paste0(nrow(rv$classes), " classes")),
        span(class="sl-badge sl-badge-warning",
             paste0("train: ", n_train, "  /  val: ", n_val))
      )
    })

    # ════════════════════════════════════════════════════════════════════════
    # DASHBOARD
    # ════════════════════════════════════════════════════════════════════════
    observeEvent(input$main_tabs, {
      if (!is.null(input$main_tabs) && input$main_tabs == "dashboard") {
        auto_save(silent = TRUE)
        rv$images <- sl_get_images(con)
      }
    })

    dash_stats <- reactive({
      rv$save_counter
      sl_get_stats(con)
    })

    output$dashboard_stats <- renderUI({
      s     <- dash_stats()$totals
      done  <- s$done_images  %||% 0L
      total <- s$total_images %||% 0L
      todo  <- s$todo_images  %||% 0L
      boxes <- s$total_boxes  %||% 0L
      pct   <- if (total > 0L) round(done / total * 100L) else 0L
      tagList(
        div(class="sl-stat-grid",
          div(class="sl-stat-card",
            div(class="sl-stat-value",style="color:var(--accent);",done),
            div(class="sl-stat-label","Images Done")),
          div(class="sl-stat-card",
            div(class="sl-stat-value",style="color:var(--warning);",todo),
            div(class="sl-stat-label","Remaining")),
          div(class="sl-stat-card",
            div(class="sl-stat-value",style="color:var(--success);",boxes),
            div(class="sl-stat-label","Total Boxes")),
          div(class="sl-stat-card",
            div(class="sl-stat-value",paste0(pct,"%")),
            div(class="sl-stat-label","Complete"))
        ),
        div(class="sl-progress-bar",
          div(class="sl-progress-fill",style=paste0("width:",pct,"%;")))
      )
    })

    pt <- function() {
      theme_minimal(base_size = 13) +
        theme(plot.background  = element_rect(fill="transparent",color=NA),
              panel.background = element_rect(fill="transparent",color=NA),
              panel.grid.major = element_line(color="#2a3050"),
              panel.grid.minor = element_blank(),
              axis.text  = element_text(color="#6b7a9e"),
              axis.title = element_text(color="#6b7a9e"))
    }

    output$plot_class_dist <- renderPlot({
      df <- dash_stats()$by_class
      if (is.null(df) || nrow(df) == 0L) return(NULL)
      ggplot(df, aes(x=reorder(class_name,box_count), y=box_count, fill=color_hex)) +
        geom_col(width=0.65) + scale_fill_identity() + coord_flip() +
        labs(x=NULL,y="Boxes") + pt()
    }, bg="transparent")

    output$plot_annotator <- renderPlot({
      df <- dash_stats()$by_annotator
      if (is.null(df) || nrow(df) == 0L) return(NULL)
      ggplot(df, aes(x=reorder(annotator_name,boxes_drawn), y=boxes_drawn)) +
        geom_col(fill="#4f8ef7",width=0.65,alpha=0.85) +
        geom_text(aes(label=boxes_drawn),hjust=-0.2,color="#e8ecf5",size=4) +
        coord_flip() + labs(x=NULL,y="Boxes") + pt()
    }, bg="transparent")

    output$tbl_annotators <- DT::renderDataTable({
      df <- dash_stats()$by_annotator
      if (is.null(df) || nrow(df)==0L) return(NULL)
      DT::datatable(df, colnames=c("Annotator","Images","Boxes","Last Active"),
                    options=list(pageLength=10,dom="tp"),rownames=FALSE,class="compact")
    })

    output$tbl_todo_images <- DT::renderDataTable({
      imgs <- rv$images
      if (nrow(imgs)==0L) return(NULL)
      todo <- imgs[imgs$status=="unannotated",
                   c("filename","status","added_by","added_at"),drop=FALSE]
      if (nrow(todo)==0L) return(NULL)
      DT::datatable(todo,colnames=c("Filename","Status","Added By","Added At"),
                    options=list(pageLength=10,dom="tp"),rownames=FALSE,class="compact")
    })

    output$todo_count_badge <- renderUI({
      n <- sum(rv$images$status=="unannotated", na.rm=TRUE)
      span(class=if(n>0L)"sl-badge sl-badge-warning" else "sl-badge sl-badge-success",
           paste0(n," remaining"))
    })

  } # end server function
}

# Null-coalescing
`%||%` <- function(a, b) if (!is.null(a) && length(a) > 0L && !is.na(a[1])) a else b
