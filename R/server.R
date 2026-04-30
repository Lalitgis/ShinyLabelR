library(shiny)
library(shinyjs)
library(ggplot2)
library(DT)
library(httr2)

#' ShinyLabel Server — Supabase edition
#' @export
sl_server <- function(db_path = NULL) {  # db_path kept for back-compat, ignored

  function(input, output, session) {

    # ── Validate Supabase config on startup ───────────────────────────────────
    tryCatch(sb_check_env(), error = function(e) {
      showModal(modalDialog(
        title = "Configuration Error",
        pre(e$message),
        footer = NULL, easyClose = FALSE
      ))
    })

    # ── Reactive state ────────────────────────────────────────────────────────
    rv <- reactiveValues(
      user         = NULL,   # list: id, email, display_name, role
      images       = data.frame(),
      current_idx  = 0L,
      current_img  = NULL,
      classes      = data.frame(),
      active_class = NULL,
      canvas_boxes = list(),
      save_counter = 0L,
      team_members = data.frame(),
      invites      = data.frame()
    )

    # ════════════════════════════════════════════════════════════════════════
    # HELPERS
    # ════════════════════════════════════════════════════════════════════════

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
      }, error = function(e) { message("[ShinyLabel] coerce_box: ", e$message); NULL })
    }

    canvas_class_counts <- function(boxes) {
      out <- setNames(integer(0), character(0))
      for (b in boxes) {
        cn <- tryCatch(as.character(b[["class_name"]]), error = function(e) NA_character_)
        if (!is.null(cn) && !is.na(cn) && nzchar(cn))
          out[cn] <- (out[cn] %||% 0L) + 1L
      }
      out
    }

    push_classes_to_canvas <- function() {
      cls <- rv$classes
      if (is.null(cls) || nrow(cls) == 0L) return()
      class_list <- lapply(seq_len(nrow(cls)), function(i)
        list(id = cls$class_id[i], name = cls$class_name[i], color = cls$color_hex[i]))
      session$sendCustomMessage("sl_update_classes", class_list)
      if (!is.null(rv$active_class))
        session$sendCustomMessage("sl_set_active_class", rv$active_class)
    }

    is_admin <- reactive({ !is.null(rv$user) && rv$user$role == "admin" })

    # ════════════════════════════════════════════════════════════════════════
    # SCREEN ROUTING
    # ════════════════════════════════════════════════════════════════════════

    show_screen <- function(id) {
      for (s in c("screen-login", "screen-register", "main-app"))
        shinyjs::hide(s)
      shinyjs::show(id)
    }

    # ════════════════════════════════════════════════════════════════════════
    # LOGIN — returning user
    # ════════════════════════════════════════════════════════════════════════
    observeEvent(input$btn_login, {
      email <- tolower(trimws(input$login_email))
      if (!nzchar(email) || !grepl("@", email)) {
        showNotification("Please enter a valid email.", type = "error")
        return()
      }

      user <- tryCatch(sb_get_user(email), error = function(e) {
        showNotification(paste("Connection error:", e$message), type = "error")
        NULL
      })

      if (is.null(user)) {
        showNotification("No account found. Use an invite code to join.", type = "warning")
        # Pre-fill email on register screen
        updateTextInput(session, "reg_email", value = email)
        show_screen("screen-register")
        return()
      }

      launch_app(user)
    })

    # ════════════════════════════════════════════════════════════════════════
    # REGISTER — new user with invite code
    # ════════════════════════════════════════════════════════════════════════
    observeEvent(input$btn_go_register, {
      show_screen("screen-register")
    })

    observeEvent(input$btn_back_login, {
      show_screen("screen-login")
    })

    observeEvent(input$btn_register, {
      email   <- tolower(trimws(input$reg_email))
      name    <- trimws(input$reg_name)
      code    <- toupper(trimws(input$reg_invite_code))

      # Validate inputs
      if (!nzchar(email) || !grepl("@", email)) {
        showNotification("Please enter a valid email.", type = "error"); return()
      }
      if (!nzchar(name)) {
        showNotification("Please enter your name.", type = "error"); return()
      }

      # Check if email already registered
      existing <- tryCatch(sb_get_user(email), error = function(e) NULL)
      if (!is.null(existing)) {
        showNotification("This email is already registered. Please log in.", type = "warning")
        show_screen("screen-login")
        return()
      }

      # Determine role: first user ever = admin (no invite code needed)
      user_count <- tryCatch(sb_user_count(), error = function(e) -1L)

      if (user_count == 0L) {
        # First user — becomes admin automatically, no invite needed
        new_user <- tryCatch(
          sb_create_user(email, name, role = "admin"),
          error = function(e) {
            showNotification(paste("Failed to create account:", e$message), type = "error")
            NULL
          })
        if (is.null(new_user)) return()

        user <- tryCatch(sb_get_user(email), error = function(e) NULL)
        if (is.null(user)) {
          showNotification("Account created but could not retrieve profile. Please log in.", type = "warning")
          show_screen("screen-login")
          return()
        }
        showNotification(paste0("Welcome, ", name, "! You are the project admin."),
                         type = "message", duration = 4)
        launch_app(user)

      } else {
        # Subsequent users — need a valid invite code
        if (!nzchar(code)) {
          showNotification("Please enter your invite code.", type = "error"); return()
        }

        invite_row <- tryCatch(sb_validate_invite(code), error = function(e) {
          showNotification(paste("Error checking code:", e$message), type = "error")
          NULL
        })

        if (is.null(invite_row)) {
          showNotification("Invalid or expired invite code. Ask your admin for a new one.",
                           type = "error")
          return()
        }

        # Create the user
        new_user <- tryCatch(
          sb_create_user(email, name, role = "annotator"),
          error = function(e) {
            showNotification(paste("Failed to create account:", e$message), type = "error")
            NULL
          })
        if (is.null(new_user)) return()

        user <- tryCatch(sb_get_user(email), error = function(e) NULL)
        if (is.null(user)) {
          showNotification("Account created. Please log in.", type = "message")
          show_screen("screen-login")
          return()
        }

        # Mark invite as used
        tryCatch(sb_use_invite(invite_row$id, user$id), error = function(e) NULL)

        showNotification(paste0("Welcome, ", name, "! You've joined the project."),
                         type = "message", duration = 4)
        launch_app(user)
      }
    })

    # ════════════════════════════════════════════════════════════════════════
    # LAUNCH APP after successful auth
    # ════════════════════════════════════════════════════════════════════════
    launch_app <- function(user) {
      rv$user <- if (is.data.frame(user)) as.list(user[1,]) else user

      show_screen("main-app")

      rv$images  <- tryCatch(sb_get_images(),  error = function(e) data.frame())
      rv$classes <- tryCatch(sb_get_classes(), error = function(e) data.frame())

      if (is.data.frame(rv$classes) && nrow(rv$classes) > 0L) {
        rv$active_class <- list(
          id    = rv$classes$class_id[1],
          name  = rv$classes$class_name[1],
          color = rv$classes$color_hex[1]
        )
      }
      session$sendCustomMessage("sl_init_canvas", list(canvasId = "annotation-canvas"))
      push_classes_to_canvas()

      if (nrow(rv$images) > 0L) navigate_to(1L)
    }

    # ════════════════════════════════════════════════════════════════════════
    # IMAGE LOADING
    # ════════════════════════════════════════════════════════════════════════
    img_dest_dir <- function() {
      d <- file.path(tempdir(), "sl_images")
      dir.create(d, showWarnings = FALSE, recursive = TRUE)
      d
    }

    observeEvent(input$btn_load_upload, {
      req(input$file_upload)
      files  <- input$file_upload
      added  <- 0L
      dest_d <- img_dest_dir()

      withProgress(message = "Loading images...", value = 0, {
        for (i in seq_len(nrow(files))) {
          file_mb <- file.info(files$datapath[i])$size / 1024^2
          if (file_mb > 20)
            showNotification(paste0(files$name[i], " is ", round(file_mb,1),
                                    "MB — large images may load slowly."),
                             type = "warning", duration = 6)

          info <- sl_image_info(files$datapath[i])
          if (info$width == 0L) next
          dest <- file.path(dest_d, files$name[i])
          file.copy(files$datapath[i], dest, overwrite = FALSE)
          tryCatch(
            sb_add_image(dest, files$name[i], info$width, info$height,
                         "upload", rv$user$email),
            error = function(e) message("[ShinyLabel] sb_add_image error: ", e$message))
          added <- added + 1L
          incProgress(1 / nrow(files))
        }
      })

      rv$images <- tryCatch(sb_get_images(), error = function(e) rv$images)
      showNotification(paste0("Loaded ", added, " image(s)"), type = "message")
      if (nrow(rv$images) > 0L && rv$current_idx == 0L) navigate_to(1L)
    })

    observeEvent(input$btn_load_url, {
      url <- trimws(input$url_input)
      if (!nzchar(url)) { showNotification("Please enter a URL.", type = "warning"); return() }
      tryCatch({
        showNotification("Downloading...", type = "message", duration = 2)
        result <- sl_fetch_url_image(url)
        dest   <- file.path(img_dest_dir(), result$filename)
        file.copy(result$local_path, dest, overwrite = FALSE)
        sb_add_image(dest, result$filename, result$width, result$height,
                     "url", rv$user$email)
        rv$images <- tryCatch(sb_get_images(), error = function(e) rv$images)
        updateTextInput(session, "url_input", value = "")
        showNotification(paste0("Loaded: ", result$filename), type = "message")
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
        showNotification(paste("Image not found on server:", img_path), type = "warning")
      }

      img_id   <- imgs$id[idx]
      existing <- tryCatch(sb_load_annotations(img_id),
                           error = function(e) data.frame())

      if (is.data.frame(existing) && nrow(existing) > 0L) {
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
    # CANVAS → R
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
        tryCatch(sb_save_annotations(img_id, NULL, rv$user$email),
                 error = function(e) message("[ShinyLabel] save error: ", e$message))
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
        tryCatch(sb_save_annotations(img_id, box_df, rv$user$email),
                 error = function(e) message("[ShinyLabel] save error: ", e$message))
      }

      rv$images <- tryCatch(sb_get_images(), error = function(e) rv$images)
      if (rv$current_idx > 0L && rv$current_idx <= nrow(rv$images))
        rv$current_img <- as.list(rv$images[rv$current_idx, ])
      rv$save_counter <- rv$save_counter + 1L
      if (!silent) showNotification("Saved", type = "message", duration = 1.5)
    }

    observeEvent(input$btn_save_now, auto_save(silent = FALSE))
    observeEvent(input$btn_undo,   session$sendCustomMessage("sl_undo",            list()))
    observeEvent(input$btn_delete, session$sendCustomMessage("sl_delete_selected", list()))
    observeEvent(input$btn_clear,  session$sendCustomMessage("sl_clear_boxes",     list()))

    observeEvent(input$quick_class_select, {
      cid <- suppressWarnings(as.integer(input$quick_class_select))
      if (is.na(cid)) return()
      cls <- rv$classes[rv$classes$class_id == cid, ]
      if (nrow(cls) == 0L) return()
      rv$active_class <- list(id=cls$class_id[1], name=cls$class_name[1], color=cls$color_hex[1])
      session$sendCustomMessage("sl_set_active_class", rv$active_class)
    })

    observeEvent(input$class_click, {
      cid <- suppressWarnings(as.integer(input$class_click))
      if (is.na(cid)) return()
      cls <- rv$classes[rv$classes$class_id == cid, ]
      if (nrow(cls) == 0L) return()
      rv$active_class <- list(id=cls$class_id[1], name=cls$class_name[1], color=cls$color_hex[1])
      session$sendCustomMessage("sl_set_active_class", rv$active_class)
    })

    # ════════════════════════════════════════════════════════════════════════
    # CLASS MANAGEMENT
    # ════════════════════════════════════════════════════════════════════════
    observeEvent(input$btn_confirm_class, {
      name <- trimws(input$new_class_name)
      if (!nzchar(name)) { showNotification("Please enter a class name.", type = "error"); return() }

      color <- tryCatch({
        v <- input$new_class_color_val
        if (!is.null(v) && nzchar(v) && grepl("^#[0-9A-Fa-f]{6}$", v)) v
        else { n <- nrow(rv$classes); PALETTE[(n %% length(PALETTE)) + 1L] }
      }, error = function(e) "#4f8ef7")

      tryCatch(sb_add_class(name, color), error = function(e)
        showNotification(paste("Failed to add class:", e$message), type = "error"))

      rv$classes <- tryCatch(sb_get_classes(), error = function(e) rv$classes)
      new_cls    <- rv$classes[rv$classes$class_name == name, ]
      if (nrow(new_cls) > 0L)
        rv$active_class <- list(id=new_cls$class_id[1], name=new_cls$class_name[1],
                                color=new_cls$color_hex[1])
      push_classes_to_canvas()
      updateTextInput(session, "new_class_name", value = "")
      showNotification(paste0("Class '", name, "' added"), type = "message", duration = 2)
    })

    # ════════════════════════════════════════════════════════════════════════
    # TEAM MANAGEMENT (admin only)
    # ════════════════════════════════════════════════════════════════════════

    # Load team data when Team tab is opened
    observeEvent(input$main_tabs, {
      if (!is.null(input$main_tabs) && input$main_tabs == "team" && is_admin()) {
        rv$team_members <- tryCatch(sb_get_all_users(), error = function(e) data.frame())
        rv$invites      <- tryCatch(sb_get_invites(),   error = function(e) data.frame())
      }
      if (!is.null(input$main_tabs) && input$main_tabs == "dashboard") {
        auto_save(silent = TRUE)
        rv$images <- tryCatch(sb_get_images(), error = function(e) rv$images)
      }
    })

    # Generate invite code
    observeEvent(input$btn_gen_invite, {
      req(is_admin())
      code <- tryCatch(
        sb_create_invite(rv$user$id, expires_hours = 72L),
        error = function(e) {
          showNotification(paste("Failed to generate code:", e$message), type = "error")
          NULL
        })
      if (is.null(code)) return()
      rv$invites <- tryCatch(sb_get_invites(), error = function(e) rv$invites)
      showNotification(
        paste0("Invite code generated: ", code, " (valid 72 hrs)"),
        type = "message", duration = 8)
      # Display in UI
      output$latest_invite_code <- renderUI({
        div(class = "sl-invite-code-display",
          span(class = "sl-code-text", code),
          tags$button(class = "sl-btn sl-btn-icon",
            onclick = paste0("navigator.clipboard.writeText('", code, "');"),
            "⧉")
        )
      })
    })

    # Remove a team member (admin only, cannot remove self)
    observeEvent(input$btn_remove_member, {
      req(is_admin())
      uid <- suppressWarnings(as.integer(input$member_to_remove))
      if (is.na(uid) || uid == rv$user$id) {
        showNotification("You cannot remove yourself.", type = "warning"); return()
      }
      tryCatch({
        sb_remove_user(uid)
        rv$team_members <- sb_get_all_users()
        showNotification("Member removed.", type = "message")
      }, error = function(e)
        showNotification(paste("Failed:", e$message), type = "error"))
    })

    # ════════════════════════════════════════════════════════════════════════
    # EXPORTS
    # ════════════════════════════════════════════════════════════════════════
    exports_dir    <- file.path(tempdir(), "sl_exports")
    dir.create(exports_dir, showWarnings = FALSE, recursive = TRUE)

    yolo_zip_path  <- reactiveVal(NULL)
    coco_json_path <- reactiveVal(NULL)

    # For export we need a local SQLite for the export functions (they use DBI)
    # We materialise the Supabase data into a temp SQLite just for export.
    get_export_con <- function() {
      tmp_db <- file.path(tempdir(), "sl_export_tmp.db")
      con    <- DBI::dbConnect(RSQLite::SQLite(), tmp_db)
      # Write current data
      DBI::dbWriteTable(con, "images",      rv$images,                 overwrite = TRUE)
      DBI::dbWriteTable(con, "classes",     rv$classes,                overwrite = TRUE)
      all_ann <- tryCatch({
        req <- sb_req("annotations") |>
          httr2::req_url_query(select = "*")
        sb_perform(req, empty_df_cols = c("id","image_id","class_id","class_name",
          "x_pixel","y_pixel","w_pixel","h_pixel","x_center_norm","y_center_norm",
          "w_norm","h_norm","annotator_email","created_at","updated_at"))
      }, error = function(e) data.frame())
      if (is.data.frame(all_ann) && nrow(all_ann) > 0)
        DBI::dbWriteTable(con, "annotations", all_ann, overwrite = TRUE)
      con
    }

    observeEvent(input$btn_export_yolo, {
      req(is_admin())
      auto_save(silent = TRUE)
      if (nrow(rv$classes) == 0L) { showNotification("Add at least one class first.", type="error"); return() }

      yolo_zip_path(NULL)
      withProgress(message = "Building YOLO dataset...", value = 0, {
        incProgress(0.3, detail = "Fetching annotations from Supabase...")
        con <- tryCatch(get_export_con(), error = function(e) {
          showNotification(paste("Export error:", e$message), type = "error"); NULL })
        if (is.null(con)) return()
        incProgress(0.5, detail = "Writing label files...")
        zp <- tryCatch(
          sl_export_yolo(con, output_dir = exports_dir,
                         split_ratio = input$export_split %||% 0.8),
          error = function(e) {
            showNotification(paste("Export failed:", e$message), type="error", duration=15); NULL })
        DBI::dbDisconnect(con)
        incProgress(0.2, detail = "Done!")
      })
      if (is.null(zp) || !file.exists(zp)) return()
      yolo_zip_path(zp)
      showNotification("Dataset built! Click the green button to download.", type="message", duration=4)
    })

    output$dl_yolo_ready <- downloadHandler(
      filename = function() paste0("yolo_annotations_", format(Sys.Date(), "%Y%m%d"), ".zip"),
      content  = function(file) {
        zp <- yolo_zip_path()
        if (!is.null(zp) && file.exists(zp)) file.copy(zp, file, overwrite = TRUE)
      },
      contentType = "application/zip"
    )

    output$export_yolo_link <- renderUI({
      if (is.null(yolo_zip_path())) return(NULL)
      div(style="margin-top:10px;",
        downloadButton("dl_yolo_ready", "Save YOLO Dataset (.zip)",
                       class="sl-btn sl-btn-success",
                       style="width:100%; height:42px; font-size:14px; justify-content:center;"))
    })

    observeEvent(input$btn_export_coco, {
      req(is_admin())
      auto_save(silent = TRUE)
      if (nrow(rv$classes) == 0L) { showNotification("Add at least one class first.", type="error"); return() }
      coco_json_path(NULL)
      con <- tryCatch(get_export_con(), error = function(e) {
        showNotification(paste("Export error:", e$message), type = "error"); NULL })
      if (is.null(con)) return()
      jp <- tryCatch(sl_export_coco(con, output_dir = exports_dir),
        error = function(e) { showNotification(paste("COCO export failed:", e$message), type="error"); NULL })
      DBI::dbDisconnect(con)
      if (is.null(jp) || !file.exists(jp)) return()
      coco_json_path(jp)
      showNotification("COCO JSON ready! Click the green button to download.", type="message", duration=4)
    })

    output$dl_coco_ready <- downloadHandler(
      filename = function() paste0("coco_annotations_", format(Sys.Date(), "%Y%m%d"), ".json"),
      content  = function(file) {
        jp <- coco_json_path()
        if (!is.null(jp) && file.exists(jp)) file.copy(jp, file, overwrite = TRUE)
      },
      contentType = "application/json"
    )

    output$export_coco_link <- renderUI({
      if (is.null(coco_json_path())) return(NULL)
      div(style="margin-top:10px;",
        downloadButton("dl_coco_ready", "Save COCO JSON",
                       class="sl-btn sl-btn-success",
                       style="width:100%; height:42px; font-size:14px; justify-content:center;"))
    })

    # ════════════════════════════════════════════════════════════════════════
    # UI OUTPUTS
    # ════════════════════════════════════════════════════════════════════════

    output$navbar_progress <- renderUI({
      rv$save_counter
      imgs  <- rv$images
      if (nrow(imgs) == 0L) return(NULL)
      done  <- sum(imgs$status == "done",  na.rm = TRUE)
      total <- nrow(imgs)
      tagList(
        span(class="sl-badge sl-badge-accent",  paste0(done, "/", total, " images")),
        span(class="sl-badge sl-badge-success", paste0(round(done/total*100), "% done"))
      )
    })

    output$navbar_user <- renderUI({
      req(rv$user)
      role_badge <- if (rv$user$role == "admin")
        span(class="sl-badge sl-badge-warning", "Admin")
      else NULL
      tagList(
        role_badge,
        span(class="sl-badge sl-badge-accent", rv$user$display_name)
      )
    })

    output$image_list_ui <- renderUI({
      rv$save_counter
      imgs <- rv$images
      if (nrow(imgs) == 0L)
        return(div(style="text-align:center;padding:28px 12px;",
          p(style="color:var(--text-dim);font-size:14px;", "No images loaded yet.",
            br(), "Use Upload or URL above.")))
      tags$ul(class="sl-image-list",
        lapply(seq_len(nrow(imgs)), function(i) {
          img  <- imgs[i, ]
          done <- !is.na(img$status) && img$status == "done"
          tags$li(
            class   = paste("sl-image-item", if(i==rv$current_idx)"active" else "", if(done)"done" else "todo"),
            onclick = sprintf("Shiny.setInputValue('img_list_click',%d,{priority:'event'});", i),
            span(class="sl-img-status", if(done) "✓" else "○"),
            span(class="sl-img-name",   img$filename),
            span(class="sl-img-count",  paste0(img$box_count, "b"))
          )
        })
      )
    })

    output$image_count_badge <- renderUI(span(class="sl-badge sl-badge-accent", nrow(rv$images)))

    output$img_counter_ui <- renderUI(
      span(class="sl-img-counter",
           if(nrow(rv$images)==0L) "—/—" else paste0(rv$current_idx,"/",nrow(rv$images))))

    output$class_empty_state <- renderUI({
      if (nrow(rv$classes) > 0L) return(NULL)
      div(style="text-align:center;padding:14px 8px;color:var(--text-dim);font-size:13px;",
          "Type a name and click Add Class to start.")
    })

    output$class_list_separator <- renderUI({
      if (nrow(rv$classes) == 0L) return(NULL)
      tags$hr(class="sl-divider")
    })

    output$class_list_ui <- renderUI({
      rv$canvas_boxes; rv$save_counter
      cls <- rv$classes
      if (nrow(cls) == 0L) return(NULL)
      active_id <- rv$active_class$id

      canvas_map <- canvas_class_counts(rv$canvas_boxes)

      div(class="sl-class-list",
        lapply(seq_len(nrow(cls)), function(i) {
          cr        <- cls[i, ]
          is_active <- !is.null(active_id) && !is.na(cr$class_id) && cr$class_id == active_id
          cn        <- as.character(cr$class_name)
          n_canvas  <- if (cn %in% names(canvas_map)) canvas_map[cn] else 0L

          div(class=paste("sl-class-item", if(is_active)"active" else ""),
            onclick=sprintf("Shiny.setInputValue('class_click',%d,{priority:'event'});",
                            as.integer(cr$class_id)),
            div(class="sl-class-dot", style=paste0("background:",cr$color_hex,";")),
            span(class="sl-class-name", cn),
            span(class=paste("sl-class-count", if(n_canvas>0L)"has-annotations" else ""),
                 n_canvas)
          )
        })
      )
    })

    output$quick_class_selector <- renderUI({
      cls <- rv$classes
      if (nrow(cls) == 0L)
        return(span(style="font-size:12px;color:var(--text-muted);", "Add a class first"))
      selectInput("quick_class_select", NULL,
                  choices  = setNames(as.integer(cls$class_id), cls$class_name),
                  selected = rv$active_class$id,
                  width    = "150px")
    })

    output$box_list_ui <- renderUI({
      boxes <- rv$canvas_boxes
      if (length(boxes) == 0L)
        return(div(style="text-align:center;padding:24px 8px;",
          p(style="color:var(--text-dim);font-size:13px;margin-top:8px;",
            "Drag on the image to draw a box.")))
      div(class="sl-box-list",
        lapply(seq_along(boxes), function(i) {
          b <- boxes[[i]]
          div(class="sl-box-item",
            div(class="sl-class-dot", style=paste0("background:",as.character(b[["color_hex"]] %||% "#999"),";")),
            div(class="sl-box-label", as.character(b[["class_name"]] %||% "?")),
            div(class="sl-box-coords", sprintf("%d × %d px",
              round(as.numeric(b[["w_pixel"]])), round(as.numeric(b[["h_pixel"]]))))
          )
        })
      )
    })

    output$box_count_badge <- renderUI({
      n <- length(rv$canvas_boxes)
      span(class=if(n>0L)"sl-badge sl-badge-success" else "sl-badge sl-badge-accent", n)
    })

    output$canvas_status_ui <- renderUI({
      rv$save_counter
      img <- rv$current_img
      if (is.null(img)) return(span(style="color:var(--text-dim);","Load an image to begin"))
      n     <- length(rv$canvas_boxes)
      fname <- as.character(img[["filename"]] %||% "—")
      iw    <- as.integer(img[["img_width"]]  %||% 0L)
      ih    <- as.integer(img[["img_height"]] %||% 0L)
      aname <- as.character(rv$active_class$name %||% "none")
      tagList(
        span(style="color:var(--text-primary);", fname),
        span(style="margin:0 10px;color:var(--border);","|"),
        span(paste0(iw, " × ", ih, " px")),
        span(style="margin:0 10px;color:var(--border);","|"),
        span(paste0(n, " box", if(n!=1L)"es" else "")),
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
        span(class="sl-badge sl-badge-warning", paste0("train: ",n_train,"  /  val: ",n_val))
      )
    })

    # ── Team tab UI outputs (admin only) ─────────────────────────────────────
    output$team_members_ui <- renderUI({
      req(is_admin())
      members <- rv$team_members
      if (!is.data.frame(members) || nrow(members) == 0L)
        return(p(style="color:var(--text-muted);font-size:13px;", "No team members yet."))

      tags$table(style="width:100%;border-collapse:collapse;",
        tags$thead(
          tags$tr(
            tags$th(style="text-align:left;padding:6px 8px;font-size:11px;color:var(--text-muted);font-weight:600;text-transform:uppercase;", "Name"),
            tags$th(style="text-align:left;padding:6px 8px;font-size:11px;color:var(--text-muted);font-weight:600;text-transform:uppercase;", "Email"),
            tags$th(style="text-align:left;padding:6px 8px;font-size:11px;color:var(--text-muted);font-weight:600;text-transform:uppercase;", "Role"),
            tags$th(style="text-align:left;padding:6px 8px;font-size:11px;color:var(--text-muted);font-weight:600;text-transform:uppercase;", "Joined"),
            tags$th()
          )
        ),
        tags$tbody(
          lapply(seq_len(nrow(members)), function(i) {
            m      <- members[i, ]
            is_me  <- !is.null(rv$user) && m$id == rv$user$id
            tags$tr(style=if(is_me)"background:var(--accent-glow);" else "",
              tags$td(style="padding:8px;font-size:13px;", m$display_name),
              tags$td(style="padding:8px;font-size:12px;color:var(--text-muted);font-family:var(--font-data);", m$email),
              tags$td(style="padding:8px;",
                span(class=if(m$role=="admin")"sl-badge sl-badge-warning" else "sl-badge sl-badge-accent",
                     m$role)),
              tags$td(style="padding:8px;font-size:11px;color:var(--text-muted);",
                      substr(m$created_at, 1, 10)),
              tags$td(style="padding:8px;",
                if (!is_me)
                  tags$button(class="sl-btn sl-btn-danger",
                    style="padding:3px 8px;font-size:11px;",
                    onclick=sprintf(
                      "Shiny.setInputValue('member_to_remove',%d,{priority:'event'});Shiny.setInputValue('btn_remove_member',Math.random(),{priority:'event'});",
                      as.integer(m$id)),
                    "Remove")
                else span(style="font-size:11px;color:var(--text-dim);","(you)")
              )
            )
          })
        )
      )
    })

    output$invites_ui <- renderUI({
      req(is_admin())
      inv <- rv$invites
      if (!is.data.frame(inv) || nrow(inv) == 0L)
        return(p(style="color:var(--text-muted);font-size:13px;",
                 "No invite codes generated yet."))

      now <- Sys.time()
      tags$table(style="width:100%;border-collapse:collapse;",
        tags$thead(tags$tr(
          tags$th(style="text-align:left;padding:6px 8px;font-size:11px;color:var(--text-muted);font-weight:600;", "Code"),
          tags$th(style="text-align:left;padding:6px 8px;font-size:11px;color:var(--text-muted);font-weight:600;", "Expires"),
          tags$th(style="text-align:left;padding:6px 8px;font-size:11px;color:var(--text-muted);font-weight:600;", "Status")
        )),
        tags$tbody(lapply(seq_len(min(nrow(inv), 10L)), function(i) {
          r       <- inv[i, ]
          is_used <- !is.null(r$used_at) && nzchar(r$used_at) && r$used_at != "NA"
          expires <- tryCatch(as.POSIXct(r$expires_at, format="%Y-%m-%dT%H:%M:%SZ", tz="UTC"),
                              error=function(e) NA)
          is_expired <- !is.na(expires) && expires < now

          tags$tr(
            tags$td(style="padding:7px 8px;",
              span(class="sl-code-inline", r$code)
            ),
            tags$td(style="padding:7px 8px;font-size:11px;color:var(--text-muted);",
              if (!is.na(expires)) format(expires, "%b %d, %H:%M UTC") else "—"),
            tags$td(style="padding:7px 8px;",
              if (is_used)
                span(class="sl-badge sl-badge-success", "Used")
              else if (is_expired)
                span(class="sl-badge sl-badge-danger", "Expired")
              else
                span(class="sl-badge sl-badge-accent", "Active")
            )
          )
        }))
      )
    })

    output$latest_invite_code <- renderUI(NULL)  # placeholder

    # ── Dashboard ────────────────────────────────────────────────────────────
    dash_stats <- reactive({
      rv$save_counter
      tryCatch(sb_get_stats(), error = function(e) list(
        totals       = data.frame(total_images=0,done_images=0,todo_images=0,total_boxes=0),
        by_class     = data.frame(),
        by_annotator = data.frame()))
    })

    output$dashboard_stats <- renderUI({
      s     <- dash_stats()$totals
      done  <- s$done_images  %||% 0L
      total <- s$total_images %||% 0L
      todo  <- s$todo_images  %||% 0L
      boxes <- s$total_boxes  %||% 0L
      pct   <- if (total > 0L) round(done/total*100L) else 0L
      tagList(
        div(class="sl-stat-grid",
          div(class="sl-stat-card",
            div(class="sl-stat-value",style="color:var(--accent);", done),
            div(class="sl-stat-label","Images Done")),
          div(class="sl-stat-card",
            div(class="sl-stat-value",style="color:var(--warning);", todo),
            div(class="sl-stat-label","Remaining")),
          div(class="sl-stat-card",
            div(class="sl-stat-value",style="color:var(--success);", boxes),
            div(class="sl-stat-label","Total Boxes")),
          div(class="sl-stat-card",
            div(class="sl-stat-value", paste0(pct,"%")),
            div(class="sl-stat-label","Complete"))
        ),
        div(class="sl-progress-bar",
            div(class="sl-progress-fill", style=paste0("width:",pct,"%;"))
        )
      )
    })

    pt <- function() {
      theme_minimal(base_size=13) +
        theme(plot.background=element_rect(fill="transparent",color=NA),
              panel.background=element_rect(fill="transparent",color=NA),
              panel.grid.major=element_line(color="#2a3050"),
              panel.grid.minor=element_blank(),
              axis.text=element_text(color="#6b7a9e"),
              axis.title=element_text(color="#6b7a9e"))
    }

    output$plot_class_dist <- renderPlot({
      df <- dash_stats()$by_class
      if (is.null(df) || nrow(df)==0L) return(NULL)
      ggplot(df, aes(x=reorder(class_name,box_count), y=box_count, fill=color_hex)) +
        geom_col(width=0.65) + scale_fill_identity() + coord_flip() +
        labs(x=NULL, y="Boxes") + pt()
    }, bg="transparent")

    output$plot_annotator <- renderPlot({
      df <- dash_stats()$by_annotator
      if (is.null(df) || nrow(df)==0L) return(NULL)
      ggplot(df, aes(x=reorder(annotator_name,boxes_drawn), y=boxes_drawn)) +
        geom_col(fill="#4f8ef7", width=0.65, alpha=0.85) +
        geom_text(aes(label=boxes_drawn), hjust=-0.2, color="#e8ecf5", size=4) +
        coord_flip() + labs(x=NULL, y="Boxes") + pt()
    }, bg="transparent")

    output$tbl_annotators <- DT::renderDataTable({
      df <- dash_stats()$by_annotator
      if (is.null(df)||nrow(df)==0L) return(NULL)
      DT::datatable(df, colnames=c("Annotator","Images","Boxes","Last Active"),
                    options=list(pageLength=10,dom="tp"), rownames=FALSE, class="compact")
    })

    output$tbl_todo_images <- DT::renderDataTable({
      imgs <- rv$images
      if (nrow(imgs)==0L) return(NULL)
      todo <- imgs[imgs$status=="unannotated", c("filename","status","added_by","added_at"), drop=FALSE]
      if (nrow(todo)==0L) return(NULL)
      DT::datatable(todo, colnames=c("Filename","Status","Added By","Added At"),
                    options=list(pageLength=10,dom="tp"), rownames=FALSE, class="compact")
    })

    output$todo_count_badge <- renderUI({
      n <- sum(rv$images$status=="unannotated", na.rm=TRUE)
      span(class=if(n>0L)"sl-badge sl-badge-warning" else "sl-badge sl-badge-success",
           paste0(n," remaining"))
    })

  } # end server function
}
