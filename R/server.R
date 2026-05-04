library(shiny)
library(shinyjs)
library(ggplot2)
library(DT)
library(digest)

`%||%` <- function(a, b) if (!is.null(a) && length(a) > 0L && !is.na(a[1])) a else b

#' ShinyLabel Server
#' @param db_path Path to SQLite database file
#' @export
sl_server <- function(db_path = "shinylabel.db") {

  function(input, output, session) {

    con <- sl_init_db(db_path)
    session$onSessionEnded(function() {
      tryCatch(DBI::dbDisconnect(con), error = function(e) NULL)
    })

    rv <- reactiveValues(
      user         = NULL,   # list: id, first_name, last_name, email, role
      annotator    = NULL,   # display name (first + last)
      user_role    = "annotator",
      reset_email  = NULL,   # used during password reset flow
      images       = data.frame(),
      current_idx  = 0L,
      current_img  = NULL,
      classes      = data.frame(),
      active_class = NULL,
      canvas_boxes = list(),
      save_counter = 0L,
      team_members = data.frame(),
      invite_list  = data.frame()
    )

    # ── Helpers ───────────────────────────────────────────────────────────────
    all_auth_screens <- c("screen-signin","screen-register","screen-forgot",
                          "screen-reset","screen-check-email")

    show_screen <- function(id) {
      for (s in c(all_auth_screens, "main-app")) shinyjs::hide(s)
      shinyjs::show(id)
    }

    is_admin  <- reactive({ rv$user_role == "admin" })
    resend_ok <- nzchar(Sys.getenv("RESEND_API_KEY"))

    display_name <- function(user) {
      paste(trimws(user$first_name), trimws(user$last_name))
    }

    launch_app <- function(user) {
      rv$user      <- user
      rv$annotator <- display_name(user)
      rv$user_role <- user$role

      show_screen("main-app")

      rv$images  <- sl_get_images(con)
      rv$classes <- sl_get_classes(con)

      if (nrow(rv$classes) > 0L)
        rv$active_class <- list(
          id    = rv$classes$class_id[1],
          name  = rv$classes$class_name[1],
          color = rv$classes$color_hex[1])

      session$sendCustomMessage("sl_init_canvas", list(canvasId = "annotation-canvas"))
      push_classes_to_canvas()
      if (nrow(rv$images) > 0L) navigate_to(1L)
    }

    # Check URL params for verify/invite/reset tokens on load
    observe({
      query <- parseQueryString(session$clientData$url_search)

      # ── Email verification link ──────────────────────────────────────────
      if (!is.null(query$verify) && nzchar(query$verify)) {
        token_row <- sl_validate_token(con, query$verify, "verify")
        if (!is.null(token_row)) {
          sl_verify_user(con, token_row$email)
          sl_use_token(con, query$verify)
          showNotification(
            "Email verified! You can now sign in.",
            type = "message", duration = 6)
        } else {
          showNotification(
            "This verification link has expired. Please register again.",
            type = "error", duration = 6)
        }
        show_screen("screen-signin")
      }

      # ── Invite link — pre-fill register form ────────────────────────────
      if (!is.null(query$invite) && nzchar(query$invite)) {
        token_row <- sl_validate_token(con, query$invite, "invite")
        if (!is.null(token_row)) {
          updateTextInput(session, "reg_email", value = token_row$email)
          showNotification(
            paste0("You've been invited! Fill in your details to join."),
            type = "message", duration = 5)
          show_screen("screen-register")
        } else {
          showNotification(
            "This invite link has expired. Ask your admin to resend.",
            type = "error", duration = 6)
          show_screen("screen-signin")
        }
      }

      # ── Password reset link ──────────────────────────────────────────────
      if (!is.null(query$reset) && nzchar(query$reset)) {
        token_row <- sl_validate_token(con, query$reset, "reset")
        if (!is.null(token_row)) {
          rv$reset_email <- token_row$email
          rv$reset_token <- query$reset
          show_screen("screen-reset")
        } else {
          showNotification(
            "This reset link has expired. Please request a new one.",
            type = "error", duration = 6)
          show_screen("screen-signin")
        }
      }
    })

    # ════════════════════════════════════════════════════════════════════════
    # SIGN IN
    # ════════════════════════════════════════════════════════════════════════
    observeEvent(input$btn_signin, {
      email    <- tolower(trimws(input$signin_email))
      password <- input$signin_password

      if (!nzchar(email) || !grepl("@", email)) {
        showNotification("Please enter a valid email.", type = "error"); return()
      }
      if (!nzchar(password)) {
        showNotification("Please enter your password.", type = "error"); return()
      }

      # No users yet — tell them to register first
      if (sl_user_count(con) == 0L) {
        showNotification(
          "No accounts exist yet. Click 'Create one' to register as the first admin.",
          type = "warning", duration = 7)
        return()
      }

      user <- sl_verify_login(con, email, password)

      if (is.null(user)) {
        showNotification(
          "Incorrect email or password. Please try again.",
          type = "error", duration = 5)
        return()
      }

      # Auto-verify if email service not configured (shinyapps.io SQLite mode)
      if (user$verified == 0L && !resend_ok) {
        sl_verify_user(con, email)
        user$verified <- 1L
      }

      if (user$verified == 0L) {
        showNotification(
          "Please verify your email before signing in. Check your inbox.",
          type = "warning", duration = 7)
        return()
      }

      showNotification(
        paste0("Welcome back, ", user$first_name, "!"),
        type = "message", duration = 3)

      launch_app(user)   # FIX 1: was missing — sign-in never launched the app
    })                   # FIX 1: closing }) for btn_signin was missing

    # ════════════════════════════════════════════════════════════════════════
    # REGISTER
    # ════════════════════════════════════════════════════════════════════════
    observeEvent(input$btn_register, {
      first  <- trimws(input$reg_first_name)
      last   <- trimws(input$reg_last_name)
      email  <- tolower(trimws(input$reg_email))
      pass1  <- input$reg_password
      pass2  <- input$reg_password2

      # Validate all fields
      if (!nzchar(first)) {
        showNotification("Please enter your first name.", type = "error"); return()
      }
      if (!nzchar(last)) {
        showNotification("Please enter your last name.", type = "error"); return()
      }
      if (!nzchar(email) || !grepl("@", email)) {
        showNotification("Please enter a valid email address.", type = "error"); return()
      }
      if (nchar(pass1) < 8) {
        showNotification("Password must be at least 8 characters.", type = "error"); return()
      }
      if (!identical(pass1, pass2)) {
        showNotification("Passwords do not match.", type = "error"); return()
      }

      # Check if email already registered
      existing <- sl_get_user_by_email(con, email)
      if (!is.null(existing)) {
        showNotification(
          "An account with this email already exists. Please sign in.",
          type = "warning", duration = 5)
        show_screen("screen-signin")
        return()
      }

      # Check for valid invite token if this is NOT the first user
      user_count <- sl_user_count(con)
      invite_token_row <- NULL

      if (user_count > 0L) {
        # Not the first user — need a valid invite token
        # Check if the email has a pending invite
        invite_token_row <- tryCatch(
          sl_validate_token(con, "", "invite"),  # will be NULL
          error = function(e) NULL)

        # Actually check by email
        inv_check <- DBI::dbGetQuery(con,
          "SELECT * FROM email_tokens
           WHERE type='invite' AND email=? AND used_at IS NULL
             AND expires_at > datetime('now')
           LIMIT 1",
          params = list(email))

        if (nrow(inv_check) == 0L) {
          showNotification(
            paste0("This email was not invited. Ask your project admin to invite ",
                   email, " from the Team tab."),
            type = "error", duration = 8)
          return()
        }
        invite_token_row <- as.list(inv_check[1, ])
      }

      # Determine role
      role <- if (user_count == 0L) "admin" else "annotator"

      # Create user
      tryCatch({
        sl_create_user(con, first, last, email, pass1, role)
      }, error = function(e) {
        showNotification(paste("Failed to create account:", e$message),
                         type = "error"); return()
      })

      # Mark invite as used
      if (!is.null(invite_token_row)) {
        sl_use_token(con, invite_token_row$token)
      }

      # Send verification email
      verify_token <- sl_create_token(con, "verify", email, hours_valid = 24L)

      # Auto-verify immediately
      sl_verify_user(con, email)
      user <- sl_get_user_by_email(con, email)
      if (role == "admin") {
        showNotification(
          paste0("Welcome, ", first, "! You are the project admin."),
          type = "message", duration = 5)
      } else {
        showNotification(
          paste0("Welcome, ", first, "! You've joined the project."),
          type = "message", duration = 3)
      }
      launch_app(user)
    })

    # ════════════════════════════════════════════════════════════════════════
    # FORGOT PASSWORD
    # ════════════════════════════════════════════════════════════════════════
    observeEvent(input$btn_forgot, {
      email <- tolower(trimws(input$forgot_email))
      if (!nzchar(email) || !grepl("@", email)) {
        showNotification("Please enter a valid email.", type = "error"); return()
      }

      user <- sl_get_user_by_email(con, email)
      # Always show success to prevent email enumeration
      output$check_email_msg <- renderUI(
        p(class = "auth-subtitle", style = "color:var(--text-muted);",
          paste0("If an account exists for ", email,
                 ", we've sent a password reset link. Check your inbox."))
      )
      show_screen("screen-check-email")

      if (!is.null(user) && resend_ok) {
        reset_token <- sl_create_token(con, "reset", email, hours_valid = 1L)
        auth_send_reset_email(email, user$first_name, reset_token)
      }
    })

    # ════════════════════════════════════════════════════════════════════════
    # RESET PASSWORD
    # ════════════════════════════════════════════════════════════════════════
    observeEvent(input$btn_reset_password, {
      pass1 <- input$reset_password
      pass2 <- input$reset_password2
      email <- rv$reset_email

      if (is.null(email)) {
        showNotification("Invalid reset session. Please request a new link.",
                         type = "error"); return()
      }
      if (nchar(pass1) < 8) {
        showNotification("Password must be at least 8 characters.", type = "error"); return()
      }
      if (!identical(pass1, pass2)) {
        showNotification("Passwords do not match.", type = "error"); return()
      }

      sl_update_password(con, email, pass1)
      sl_use_token(con, rv$reset_token)
      rv$reset_email <- NULL
      rv$reset_token <- NULL

      showNotification("Password updated! Please sign in.", type = "message", duration = 4)
      show_screen("screen-signin")
    })

    # ════════════════════════════════════════════════════════════════════════
    # CANVAS HELPERS
    # ════════════════════════════════════════════════════════════════════════
    coerce_box <- function(b) {
      tryCatch({
        if (is.null(b)) return(NULL)
        if (!is.list(b)) b <- as.list(b)
        list(
          class_id      = as.integer(b[["class_id"]]),
          class_name    = as.character(b[["class_name"]]),
          color_hex     = as.character(b[["color_hex"]] %||% "#999999"),
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
      if (length(boxes) == 0L) return(out)
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

    # ════════════════════════════════════════════════════════════════════════
    # IMAGE LOADING
    # ════════════════════════════════════════════════════════════════════════
    img_dest_dir <- function() {
      d <- file.path(dirname(normalizePath(db_path, mustWork = FALSE)), "images")
      dir.create(d, showWarnings = FALSE, recursive = TRUE)
      d
    }

    observeEvent(input$btn_load_upload, {
      req(input$file_upload)
      files <- input$file_upload; added <- 0L; dest_d <- img_dest_dir()
      withProgress(message = "Loading images…", value = 0, {
        for (i in seq_len(nrow(files))) {
          file_mb <- file.info(files$datapath[i])$size / 1024^2
          if (file_mb > 20)
            showNotification(paste0(files$name[i], " is ", round(file_mb,1),
                                    "MB — may load slowly."),
                             type = "warning", duration = 6)
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
      showNotification(paste0("Loaded ", added, " image(s)"), type = "message")
      if (nrow(rv$images) > 0L && rv$current_idx == 0L) navigate_to(1L)
    })

    observeEvent(input$btn_load_url, {
      url <- trimws(input$url_input)
      if (!nzchar(url)) { showNotification("Please enter a URL.", type = "warning"); return() }
      tryCatch({
        showNotification("Downloading…", type = "message", duration = 2)
        result <- sl_fetch_url_image(url)
        dest   <- file.path(img_dest_dir(), result$filename)
        file.copy(result$local_path, dest, overwrite = FALSE)
        sl_add_image(con, dest, result$filename,
                     result$width, result$height, "url", rv$annotator)
        rv$images <- sl_get_images(con)
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
        showNotification(paste("Image not found:", img_path), type = "warning")
      }
      img_id   <- imgs$id[idx]
      existing <- sl_load_annotations(con, img_id)
      if (nrow(existing) > 0L) {
        box_list <- lapply(seq_len(nrow(existing)), function(i) {
          r <- existing[i, ]
          list(class_id=as.integer(r$class_id), class_name=as.character(r$class_name),
               color_hex=as.character(r$color_hex),
               x_pixel=as.numeric(r$x_pixel), y_pixel=as.numeric(r$y_pixel),
               w_pixel=as.numeric(r$w_pixel), h_pixel=as.numeric(r$h_pixel),
               x_center_norm=as.numeric(r$x_center_norm),
               y_center_norm=as.numeric(r$y_center_norm),
               w_norm=as.numeric(r$w_norm), h_norm=as.numeric(r$h_norm))
        })
        session$sendCustomMessage("sl_load_boxes", box_list)
        rv$canvas_boxes <- box_list
      } else {
        session$sendCustomMessage("sl_load_boxes", list())
        rv$canvas_boxes <- list()
      }
    }

    observeEvent(input$btn_next, {
      if (nrow(rv$images)==0L || rv$current_idx>=nrow(rv$images)) return()
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
            class_id=as.integer(b[["class_id"]]),
            class_name=as.character(b[["class_name"]]),
            x_pixel=as.numeric(b[["x_pixel"]]), y_pixel=as.numeric(b[["y_pixel"]]),
            w_pixel=as.numeric(b[["w_pixel"]]), h_pixel=as.numeric(b[["h_pixel"]]),
            x_center_norm=norm$x_center_norm, y_center_norm=norm$y_center_norm,
            w_norm=norm$w_norm, h_norm=norm$h_norm,
            stringsAsFactors=FALSE)
        }))
        sl_save_annotations(con, img_id, box_df, rv$annotator)
      }
      rv$images <- sl_get_images(con)
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
      rv$active_class <- list(id=cls$class_id[1],name=cls$class_name[1],color=cls$color_hex[1])
      session$sendCustomMessage("sl_set_active_class", rv$active_class)
    })

    observeEvent(input$class_click, {
      cid <- suppressWarnings(as.integer(input$class_click))
      if (is.na(cid)) return()
      cls <- rv$classes[rv$classes$class_id == cid, ]
      if (nrow(cls) == 0L) return()
      rv$active_class <- list(id=cls$class_id[1],name=cls$class_name[1],color=cls$color_hex[1])
      session$sendCustomMessage("sl_set_active_class", rv$active_class)
    })

    # ════════════════════════════════════════════════════════════════════════
    # CLASS MANAGEMENT
    # ════════════════════════════════════════════════════════════════════════
    observeEvent(input$btn_confirm_class, {
      name <- trimws(input$new_class_name)
      if (!nzchar(name)) { showNotification("Please enter a class name.", type="error"); return() }
      color <- tryCatch({
        v <- input$new_class_color_val
        if (!is.null(v) && nzchar(v) && grepl("^#[0-9A-Fa-f]{6}$", v)) v else NULL
      }, error = function(e) NULL)
      sl_add_class(con, name, color)
      rv$classes <- sl_get_classes(con)
      new_cls <- rv$classes[rv$classes$class_name == name, ]
      if (nrow(new_cls) > 0L)
        rv$active_class <- list(id=new_cls$class_id[1],name=new_cls$class_name[1],
                                color=new_cls$color_hex[1])
      push_classes_to_canvas()
      updateTextInput(session, "new_class_name", value = "")
      showNotification(paste0("Class '", name, "' added"), type="message", duration=2)
    })

    # ════════════════════════════════════════════════════════════════════════
    # TEAM TAB
    # ════════════════════════════════════════════════════════════════════════
    observeEvent(input$load_team_tab, {
      rv$team_members <- tryCatch(sl_get_all_users(con), error = function(e) data.frame())
      rv$invite_list  <- tryCatch(sl_get_invites(con),   error = function(e) data.frame())
    })

    # Admin invites by email
    observeEvent(input$btn_send_invite, {
      req(is_admin())
      invite_email <- tolower(trimws(input$invite_email))

      if (!nzchar(invite_email) || !grepl("@", invite_email)) {
        showNotification("Please enter a valid email address.", type = "error"); return()
      }

      # Check if already a member
      existing <- sl_get_user_by_email(con, invite_email)
      if (!is.null(existing)) {
        showNotification(paste0(invite_email, " already has an account."),
                         type = "warning"); return()
      }

      # Create invite token (72 hours)
      token <- sl_create_token(con, "invite", invite_email,
                               created_by = rv$user$email, hours_valid = 72L)

      if (resend_ok) {
        sent <- auth_send_invite_email(invite_email, rv$annotator, token)
        if (sent) {
          updateTextInput(session, "invite_email", value = "")
          rv$invite_list <- sl_get_invites(con)
          showNotification(
            paste0("Invite sent to ", invite_email),
            type = "message", duration = 5)
        } else {
          showNotification(
            "Email failed to send. Check your RESEND_API_KEY.",
            type = "error", duration = 6)
        }
      } else {
        # No Resend — show the link so admin can share manually
        invite_url <- paste0(auth_app_url(), "?invite=", token)
        rv$invite_list <- sl_get_invites(con)
        updateTextInput(session, "invite_email", value = "")
        showNotification(
          paste0("RESEND_API_KEY not set. Share this link manually: ", invite_url),
          type = "warning", duration = 20)
      }
    })

    # ── Team UI outputs ───────────────────────────────────────────────────────
    output$team_member_count_badge <- renderUI({
      n <- if (is.data.frame(rv$team_members)) nrow(rv$team_members) else 0L
      span(class = "sl-badge sl-badge-accent",
           paste0(n, " member", if (n != 1L) "s" else ""))
    })

    output$team_members_table_ui <- renderUI({
      members <- rv$team_members
      if (!is.data.frame(members) || nrow(members) == 0L) {
        return(div(style = "text-align:center;padding:40px 20px;",
          p(style = "color:var(--text-muted);font-size:14px;",
            "No team members yet."),
          p(style = "color:var(--text-dim);font-size:13px;",
            "Invite teammates from the panel below.")))
      }
      tagList(lapply(seq_len(nrow(members)), function(i) {
        m     <- members[i, ]
        is_me <- !is.null(rv$user) && m$email == rv$user$email
        full  <- paste(m$first_name, m$last_name)
        initials <- toupper(paste0(
          substr(m$first_name, 1, 1), substr(m$last_name, 1, 1)))
        avatar_colors <- c("#4f8ef7","#FF6B6B","#4ECDC4","#FFE66D",
                           "#B4A7D6","#82B366","#F6A623","#50C878")
        bg <- avatar_colors[(utf8ToInt(substr(m$first_name,1,1)) %% length(avatar_colors)) + 1L]

        div(style = paste0(
              "display:flex;align-items:center;gap:14px;padding:14px 16px;",
              "border-bottom:1px solid var(--border);",
              if (is_me) "background:var(--accent-glow);" else ""),
          div(style = paste0(
                "width:42px;height:42px;border-radius:50%;flex-shrink:0;",
                "background:", bg, "22;border:2px solid ", bg, ";",
                "display:flex;align-items:center;justify-content:center;",
                "font-weight:700;font-size:15px;color:", bg, ";"),
              initials),
          div(style = "flex:1;min-width:0;",
            div(style = "display:flex;align-items:center;gap:8px;",
              span(style = "font-size:14px;font-weight:600;color:var(--text-primary);", full),
              if (is_me) span(style = "font-size:11px;color:var(--text-muted);background:var(--bg-card);border:1px solid var(--border);border-radius:4px;padding:1px 6px;", "you"),
              if (m$role == "admin") span(class = "sl-badge sl-badge-warning", "Admin")
            ),
            div(style = "font-size:12px;color:var(--text-muted);margin-top:2px;",
              m$email,
              if (m$verified == 0L)
                span(style = "margin-left:8px;color:var(--warning);font-size:11px;",
                     "⚠ unverified")
            )
          )
        )
      }))
    })

    output$team_invite_panel <- renderUI({
      div(class = "sl-panel",
        div(class = "sl-panel-header",
          span(class = "sl-panel-title", "Invite by Email"),
          if (is_admin())
            span(class = "sl-badge sl-badge-warning", "Admin")
          else
            span(class = "sl-badge sl-badge-accent", "Admin only")
        ),
        div(class = "sl-panel-body",
          if (!is_admin()) {
            div(style = "text-align:center;padding:20px;",
              p(style = "color:var(--text-muted);font-size:13px;",
                "Only the project admin can invite new members."))
          } else {
            tagList(
              p(class = "sl-help-text",
                "Enter your teammate's email address. They'll receive an invitation email with a link to create their account and join the project. Invite links expire after 72 hours."),

              div(style = "display:flex;gap:10px;align-items:flex-end;margin-bottom:16px;",
                div(style = "flex:1;",
                  tags$label("Email address", class = "sl-form-label"),
                  textInput("invite_email", NULL,
                            placeholder = "teammate@example.com",
                            width = "100%")
                ),
                actionButton("btn_send_invite", "Send Invite",
                             class = "sl-btn sl-btn-primary",
                             style = "height:38px;white-space:nowrap;flex-shrink:0;")
              ),

              if (is.data.frame(rv$invite_list) && nrow(rv$invite_list) > 0L) {
                tagList(
                  tags$hr(class = "sl-divider"),
                  div(style = "font-size:11px;font-weight:600;text-transform:uppercase;letter-spacing:0.06em;color:var(--text-muted);margin-bottom:8px;",
                      "Recent invites"),
                  tags$table(style = "width:100%;border-collapse:collapse;font-size:13px;",
                    tags$thead(tags$tr(
                      tags$th(style="text-align:left;padding:5px 8px;color:var(--text-muted);font-size:11px;font-weight:600;","Email"),
                      tags$th(style="text-align:left;padding:5px 8px;color:var(--text-muted);font-size:11px;font-weight:600;","Sent"),
                      tags$th(style="text-align:left;padding:5px 8px;color:var(--text-muted);font-size:11px;font-weight:600;","Status")
                    )),
                    tags$tbody(lapply(seq_len(nrow(rv$invite_list)), function(i) {
                      r <- rv$invite_list[i, ]
                      is_used <- !is.null(r$used_at) && nzchar(r$used_at %||% "")
                      tags$tr(
                        tags$td(style="padding:6px 8px;font-size:12px;color:var(--text-primary);", r$email),
                        tags$td(style="padding:6px 8px;font-size:11px;color:var(--text-muted);", substr(r$created_at %||% "—", 1, 16)),
                        tags$td(style="padding:6px 8px;",
                          if (is_used) span(class="sl-badge sl-badge-success", "Joined")
                          else span(class="sl-badge sl-badge-accent", "Pending"))
                      )
                    }))
                  )
                )
              }
            )
          }
        )
      )
    })

    # ════════════════════════════════════════════════════════════════════════
    # EXPORTS
    # ════════════════════════════════════════════════════════════════════════
    exports_dir <- file.path(tempdir(), "sl_exports")
    dir.create(exports_dir, showWarnings = FALSE, recursive = TRUE)
    yolo_zip_path  <- reactiveVal(NULL)
    coco_json_path <- reactiveVal(NULL)

    observeEvent(input$btn_export_yolo, {
      auto_save(silent = TRUE)
      if (nrow(rv$classes)==0L) { showNotification("Add at least one class first.",type="error"); return() }
      if (sum(rv$images$box_count>0,na.rm=TRUE)==0L) { showNotification("Annotate at least one image first.",type="error"); return() }
      yolo_zip_path(NULL)
      withProgress(message="Building YOLO dataset…", value=0, {
        incProgress(0.6, detail="Writing label files…")
        zp <- tryCatch(sl_export_yolo(con,output_dir=exports_dir,
                         split_ratio=input$export_split%||%0.8),
          error=function(e){ showNotification(paste("Export failed:",e$message),type="error",duration=15); NULL })
        incProgress(0.4, detail="Done!")
      })
      if (is.null(zp)||!file.exists(zp)) return()
      yolo_zip_path(zp)
      showNotification("Dataset ready! Click the green button to download.",type="message",duration=4)
    })

    output$dl_yolo_ready <- downloadHandler(
      filename = function() paste0("yolo_annotations_",format(Sys.Date(),"%Y%m%d"),".zip"),
      content  = function(file) { zp<-yolo_zip_path(); if(!is.null(zp)&&file.exists(zp)) file.copy(zp,file,overwrite=TRUE) },
      contentType = "application/zip")

    output$export_yolo_link <- renderUI({
      if (is.null(yolo_zip_path())) return(NULL)
      div(style="margin-top:10px;",
        downloadButton("dl_yolo_ready","Save YOLO Dataset (.zip)",
                       class="sl-btn sl-btn-success",
                       style="width:100%;height:42px;font-size:14px;justify-content:center;"))
    })

    observeEvent(input$btn_export_coco, {
      auto_save(silent=TRUE)
      if (nrow(rv$classes)==0L) { showNotification("Add at least one class first.",type="error"); return() }
      coco_json_path(NULL)
      jp <- tryCatch(sl_export_coco(con,output_dir=exports_dir),
        error=function(e){ showNotification(paste("COCO export failed:",e$message),type="error"); NULL })
      if (is.null(jp)||!file.exists(jp)) return()
      coco_json_path(jp)
      showNotification("COCO JSON ready! Click the green button to download.",type="message",duration=4)
    })

    output$dl_coco_ready <- downloadHandler(
      filename=function() paste0("coco_annotations_",format(Sys.Date(),"%Y%m%d"),".json"),
      content=function(file){ jp<-coco_json_path(); if(!is.null(jp)&&file.exists(jp)) file.copy(jp,file,overwrite=TRUE) },
      contentType="application/json")

    output$export_coco_link <- renderUI({
      if (is.null(coco_json_path())) return(NULL)
      div(style="margin-top:10px;",
        downloadButton("dl_coco_ready","Save COCO JSON",
                       class="sl-btn sl-btn-success",
                       style="width:100%;height:42px;font-size:14px;justify-content:center;"))
    })

    # ════════════════════════════════════════════════════════════════════════
    # UI OUTPUTS
    # ════════════════════════════════════════════════════════════════════════
    output$navbar_progress <- renderUI({
      rv$save_counter; imgs<-rv$images; if(nrow(imgs)==0L) return(NULL)
      done<-sum(imgs$status=="done",na.rm=TRUE); total<-nrow(imgs)
      tagList(
        span(class="sl-badge sl-badge-accent",  paste0(done,"/",total," images")),
        span(class="sl-badge sl-badge-success", paste0(round(done/total*100),"% done")))
    })

    output$navbar_user <- renderUI({
      req(rv$user)
      tagList(
        if (rv$user_role == "admin") span(class="sl-badge sl-badge-warning", "Admin"),
        span(class="sl-badge sl-badge-accent", rv$annotator))
    })

    output$image_list_ui <- renderUI({
      rv$save_counter; imgs<-rv$images
      if (nrow(imgs)==0L)
        return(div(style="text-align:center;padding:28px 12px;",
          p(style="color:var(--text-dim);font-size:14px;margin-top:10px;",
            "No images loaded yet.", br(), "Use Upload or URL above.")))
      tags$ul(class="sl-image-list",
        lapply(seq_len(nrow(imgs)), function(i) {
          img<-imgs[i,]; done<-!is.na(img$status)&&img$status=="done"
          tags$li(
            class = paste("sl-image-item", if (i == rv$current_idx) "active" else "", if (done) "done" else "todo"),
            onclick=sprintf("Shiny.setInputValue('img_list_click',%d,{priority:'event'});",i),
            span(class="sl-img-status", if (done) "✓" else "○"),
            span(class="sl-img-name", img$filename),
            span(class="sl-img-count", paste0(img$box_count,"b")))
        }))
    })

    output$image_count_badge <- renderUI(span(class="sl-badge sl-badge-accent",nrow(rv$images)))

    output$img_counter_ui <- renderUI(
      span(class="sl-img-counter",
           if (nrow(rv$images) == 0L) "—/—" else paste0(rv$current_idx,"/",nrow(rv$images))))

    output$class_empty_state <- renderUI({
      if(nrow(rv$classes)>0L) return(NULL)
      div(style="text-align:center;padding:14px 8px;color:var(--text-dim);font-size:13px;",
          "Type a name and click Add Class to start.")
    })

    output$class_list_separator <- renderUI({
      if(nrow(rv$classes)==0L) return(NULL)
      tags$hr(class="sl-divider")
    })

    output$class_list_ui <- renderUI({
      rv$canvas_boxes; rv$save_counter
      cls<-rv$classes; if(nrow(cls)==0L) return(NULL)
      active_id<-rv$active_class$id
      db_all<-tryCatch(DBI::dbGetQuery(con,"SELECT class_name,COUNT(*) as n FROM annotations GROUP BY class_name"),
        error=function(e) data.frame(class_name=character(),n=integer()))
      db_all_map<-setNames(as.integer(db_all$n),as.character(db_all$class_name))
      cur_id<-tryCatch(as.integer(rv$current_img[["id"]]),error=function(e) NA_integer_)
      db_cur_map<-setNames(integer(0),character(0))
      if(!is.null(cur_id)&&!is.na(cur_id)&&cur_id>0L){
        res<-tryCatch(DBI::dbGetQuery(con,"SELECT class_name,COUNT(*) as n FROM annotations WHERE image_id=? GROUP BY class_name",params=list(cur_id)),
          error=function(e) data.frame(class_name=character(),n=integer()))
        if(nrow(res)>0L) db_cur_map<-setNames(as.integer(res$n),as.character(res$class_name))
      }
      canvas_map<-canvas_class_counts(rv$canvas_boxes)
      div(class="sl-class-list",
        lapply(seq_len(nrow(cls)), function(i){
          cr<-cls[i,]; is_active<-!is.null(active_id)&&!is.na(cr$class_id)&&cr$class_id==active_id
          cn<-as.character(cr$class_name)
          total<-as.integer(max(0L,(db_all_map[cn]%||%0L)-(db_cur_map[cn]%||%0L)+(canvas_map[cn]%||%0L)))
          div(
            class = paste("sl-class-item", if (is_active) "active" else ""),   # FIX 2
            onclick=sprintf("Shiny.setInputValue('class_click',%d,{priority:'event'});",as.integer(cr$class_id)),
            div(class="sl-class-dot",style=paste0("background:",cr$color_hex,";")),
            span(class="sl-class-name",cn),
            span(class = paste("sl-class-count", if (total > 0L) "has-annotations" else ""), total))  # FIX 3
        }))
    })

    output$quick_class_selector <- renderUI({
      cls<-rv$classes
      if(nrow(cls)==0L) return(span(style="font-size:12px;color:var(--text-muted);","Add a class first"))
      selectInput("quick_class_select",NULL,
                  choices=setNames(as.integer(cls$class_id),cls$class_name),
                  selected=rv$active_class$id,width="150px")
    })

    output$box_list_ui <- renderUI({
      boxes<-rv$canvas_boxes
      if(length(boxes)==0L)
        return(div(style="text-align:center;padding:24px 8px;",
          p(style="color:var(--text-dim);font-size:13px;margin-top:8px;",
            "Drag on the image to draw a box.")))
      div(class="sl-box-list",
        lapply(seq_along(boxes), function(i){
          b<-boxes[[i]]
          div(class="sl-box-item",
            div(class="sl-class-dot",style=paste0("background:",as.character(b[["color_hex"]]%||%"#999"),";")),
            div(class="sl-box-label",as.character(b[["class_name"]]%||%"?")),
            div(class="sl-box-coords",sprintf("%d × %d px",
              round(as.numeric(b[["w_pixel"]])),round(as.numeric(b[["h_pixel"]])))))
        }))
    })

    output$box_count_badge <- renderUI({
      n <- length(rv$canvas_boxes)
      span(class = if (n > 0L) "sl-badge sl-badge-success" else "sl-badge sl-badge-accent", n)  # FIX 4
    })

    output$canvas_status_ui <- renderUI({
      rv$save_counter; img<-rv$current_img
      if(is.null(img)) return(span(style="color:var(--text-dim);","Load an image to begin"))
      n<-length(rv$canvas_boxes)
      tagList(
        span(style="color:var(--text-primary);",as.character(img[["filename"]]%||%"—")),
        span(style="margin:0 10px;color:var(--border);","|"),
        span(paste0(as.integer(img[["img_width"]]%||%0L)," × ",as.integer(img[["img_height"]]%||%0L)," px")),
        span(style="margin:0 10px;color:var(--border);","|"),
        span(paste0(n, " box", if (n != 1L) "es" else "")),   # FIX 5
        span(style="margin:0 10px;color:var(--border);","|"),
        span(style="color:var(--accent);",paste0("Class: ",as.character(rv$active_class$name%||%"none")))
      )
    })

    output$export_yolo_summary <- renderUI({
      rv$save_counter; imgs<-rv$images
      annotated<-sum(imgs$box_count>0L,na.rm=TRUE)
      split<-input$export_split%||%0.8
      n_train<-floor(annotated*split); n_val<-annotated-n_train
      div(style="display:flex;gap:10px;flex-wrap:wrap;margin-top:8px;",
        span(class="sl-badge sl-badge-success",paste0(annotated," annotated images")),
        span(class="sl-badge sl-badge-accent",paste0(nrow(rv$classes)," classes")),
        span(class="sl-badge sl-badge-warning",paste0("train: ",n_train,"  /  val: ",n_val)))
    })

    # ════════════════════════════════════════════════════════════════════════
    # DASHBOARD
    # ════════════════════════════════════════════════════════════════════════
    observeEvent(input$load_dashboard_tab, {
      auto_save(silent=TRUE); rv$images <- sl_get_images(con)
    })

    dash_stats <- reactive({ rv$save_counter; sl_get_stats(con) })

    output$dashboard_stats <- renderUI({
      s<-dash_stats()$totals
      done<-s$done_images%||%0L; total<-s$total_images%||%0L
      todo<-s$todo_images%||%0L; boxes<-s$total_boxes%||%0L
      pct<-if(total>0L) round(done/total*100L) else 0L
      tagList(
        div(class="sl-stat-grid",
          div(class="sl-stat-card",div(class="sl-stat-value",style="color:var(--accent);",done),div(class="sl-stat-label","Images Done")),
          div(class="sl-stat-card",div(class="sl-stat-value",style="color:var(--warning);",todo),div(class="sl-stat-label","Remaining")),
          div(class="sl-stat-card",div(class="sl-stat-value",style="color:var(--success);",boxes),div(class="sl-stat-label","Total Boxes")),
          div(class="sl-stat-card",div(class="sl-stat-value",paste0(pct,"%")),div(class="sl-stat-label","Complete"))),
        div(class="sl-progress-bar",div(class="sl-progress-fill",style=paste0("width:",pct,"%;"))))
    })

    pt <- function() {
      theme_minimal(base_size=13)+
        theme(plot.background=element_rect(fill="transparent",color=NA),
              panel.background=element_rect(fill="transparent",color=NA),
              panel.grid.major=element_line(color="#2a3050"),
              panel.grid.minor=element_blank(),
              axis.text=element_text(color="#6b7a9e"),
              axis.title=element_text(color="#6b7a9e"))
    }

    output$plot_class_dist <- renderPlot({
      df<-dash_stats()$by_class; if(is.null(df)||nrow(df)==0L) return(NULL)
      ggplot(df,aes(x=reorder(class_name,box_count),y=box_count,fill=color_hex))+
        geom_col(width=0.65)+scale_fill_identity()+coord_flip()+labs(x=NULL,y="Boxes")+pt()
    },bg="transparent")

    output$plot_annotator <- renderPlot({
      df<-dash_stats()$by_annotator; if(is.null(df)||nrow(df)==0L) return(NULL)
      ggplot(df,aes(x=reorder(annotator_name,boxes_drawn),y=boxes_drawn))+
        geom_col(fill="#4f8ef7",width=0.65,alpha=0.85)+
        geom_text(aes(label=boxes_drawn),hjust=-0.2,color="#e8ecf5",size=4)+
        coord_flip()+labs(x=NULL,y="Boxes")+pt()
    },bg="transparent")

    output$tbl_annotators <- DT::renderDataTable({
      df<-dash_stats()$by_annotator; if(is.null(df)||nrow(df)==0L) return(NULL)
      DT::datatable(df,colnames=c("Annotator","Images","Boxes","Last Active"),
                    options=list(pageLength=10,dom="tp"),rownames=FALSE,class="compact")
    })

    output$tbl_todo_images <- DT::renderDataTable({
      imgs<-rv$images; if(nrow(imgs)==0L) return(NULL)
      todo<-imgs[imgs$status=="unannotated",c("filename","status","added_by","added_at"),drop=FALSE]
      if(nrow(todo)==0L) return(NULL)
      DT::datatable(todo,colnames=c("Filename","Status","Added By","Added At"),
                    options=list(pageLength=10,dom="tp"),rownames=FALSE,class="compact")
    })

    output$todo_count_badge <- renderUI({
      n <- sum(rv$images$status == "unannotated", na.rm = TRUE)
      span(class = if (n > 0L) "sl-badge sl-badge-warning" else "sl-badge sl-badge-success",  # FIX 6
           paste0(n, " remaining"))
    })

  } # end inner server function
} # end sl_server
