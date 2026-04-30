# supabase.R — Supabase REST API client for ShinyLabelR
#
# All database operations go through Supabase's PostgREST HTTP API.
# No DBI / SQLite required on the server — just httr2.
#
# Environment variables required (set in shinyapps.io dashboard):
#   SUPABASE_URL   — e.g. https://xyzxyz.supabase.co
#   SUPABASE_KEY   — your project's anon/public key
#
# SQL to run ONCE in Supabase SQL Editor → see inst/sql/schema.sql

# ── Null coalescing (available to all files via this module) ──────────────────
`%||%` <- function(a, b) if (!is.null(a) && length(a) > 0L && !is.na(a[[1]])) a else b

# ── Connection config ─────────────────────────────────────────────────────────

#' Validate that Supabase env vars are set
#' @return invisible TRUE or stops with a helpful message
sb_check_env <- function() {
  url <- Sys.getenv("SUPABASE_URL")
  key <- Sys.getenv("SUPABASE_KEY")
  if (!nzchar(url) || !nzchar(key)) {
    stop(paste0(
      "[ShinyLabel] Supabase credentials missing.\n",
      "Set SUPABASE_URL and SUPABASE_KEY in:\n",
      "  • shinyapps.io → App → Settings → Environment Variables\n",
      "  • Locally: Sys.setenv(SUPABASE_URL='...', SUPABASE_KEY='...')"
    ))
  }
  invisible(TRUE)
}

sb_url <- function() Sys.getenv("SUPABASE_URL")
sb_key <- function() Sys.getenv("SUPABASE_KEY")

# ── Core HTTP helpers ─────────────────────────────────────────────────────────

#' Build a PostgREST request for a table
#' @param table table name
#' @param method HTTP method
sb_req <- function(table, method = "GET") {
  httr2::request(paste0(sb_url(), "/rest/v1/", table)) |>
    httr2::req_headers(
      apikey        = sb_key(),
      Authorization = paste("Bearer", sb_key()),
      `Content-Type`  = "application/json",
      Prefer          = "return=representation"
    ) |>
    httr2::req_method(method)
}

#' Execute request and return parsed JSON as data.frame or list
#' Returns empty data.frame on 404/empty, stops on other errors.
sb_perform <- function(req, empty_df_cols = NULL) {
  resp <- tryCatch(
    httr2::req_perform(req),
    error = function(e) stop("[ShinyLabel] Supabase request failed: ", e$message)
  )
  status <- httr2::resp_status(resp)
  if (status == 204 || httr2::resp_body_string(resp) == "") {
    if (!is.null(empty_df_cols))
      return(as.data.frame(setNames(
        replicate(length(empty_df_cols), character(0), simplify = FALSE),
        empty_df_cols)))
    return(invisible(NULL))
  }
  result <- httr2::resp_body_json(resp, simplifyVector = TRUE)
  if (is.null(result) || length(result) == 0) {
    if (!is.null(empty_df_cols))
      return(as.data.frame(setNames(
        replicate(length(empty_df_cols), character(0), simplify = FALSE),
        empty_df_cols)))
    return(list())
  }
  result
}

# ── USER MANAGEMENT ───────────────────────────────────────────────────────────

#' Get user count (to decide if first user = admin)
sb_user_count <- function() {
  req <- sb_req("users") |>
    httr2::req_url_query(select = "id") |>
    httr2::req_headers(Prefer = "count=exact") |>
    httr2::req_method("HEAD")
  resp <- tryCatch(httr2::req_perform(req), error = function(e) NULL)
  if (is.null(resp)) return(0L)
  ct <- httr2::resp_header(resp, "content-range")
  if (is.null(ct)) return(0L)
  # content-range: 0-0/5  → extract total after /
  as.integer(sub(".*/", "", ct))
}

#' Look up a user by email
#' @return single-row data.frame or NULL
sb_get_user <- function(email) {
  req <- sb_req("users") |>
    httr2::req_url_query(email = paste0("eq.", tolower(trimws(email))),
                         select = "*",
                         limit  = "1")
  result <- sb_perform(req, empty_df_cols = c("id","email","display_name","role","created_at"))
  if (is.data.frame(result) && nrow(result) > 0) return(result[1, ])
  NULL
}

#' Create a new user record
#' @param email character
#' @param display_name character
#' @param role "admin" or "annotator"
#' @return inserted row as list
sb_create_user <- function(email, display_name, role = "annotator") {
  body <- list(
    email        = tolower(trimws(email)),
    display_name = trimws(display_name),
    role         = role
  )
  req <- sb_req("users", "POST") |>
    httr2::req_body_json(body)
  sb_perform(req)
}

#' Get all users (admin view)
sb_get_all_users <- function() {
  req <- sb_req("users") |>
    httr2::req_url_query(select = "id,email,display_name,role,created_at",
                         order  = "created_at.asc")
  sb_perform(req, empty_df_cols = c("id","email","display_name","role","created_at"))
}

#' Remove a user (admin only)
sb_remove_user <- function(user_id) {
  req <- sb_req("users", "DELETE") |>
    httr2::req_url_query(id = paste0("eq.", user_id))
  httr2::req_perform(req)
  invisible(TRUE)
}

# ── INVITE CODES ──────────────────────────────────────────────────────────────

#' Generate a new invite code
#' Format: XXX-0000  (3 letters + dash + 4 digits) — easy to type on mobile
#' @param created_by_id integer user id of admin
#' @param expires_hours how many hours until code expires (default 72)
#' @return the code string
sb_create_invite <- function(created_by_id, expires_hours = 72L) {
  letters_part <- paste0(sample(LETTERS, 3, replace = TRUE), collapse = "")
  digits_part  <- sprintf("%04d", sample(0:9999, 1))
  code         <- paste0(letters_part, "-", digits_part)

  expires_at <- format(Sys.time() + expires_hours * 3600,
                       "%Y-%m-%dT%H:%M:%SZ", tz = "UTC")

  body <- list(
    code           = code,
    created_by     = created_by_id,
    expires_at     = expires_at
  )
  req <- sb_req("invite_codes", "POST") |>
    httr2::req_body_json(body)
  sb_perform(req)
  code
}

#' Validate an invite code — returns the code row or NULL
#' @param code character
sb_validate_invite <- function(code) {
  now <- format(Sys.time(), "%Y-%m-%dT%H:%M:%SZ", tz = "UTC")
  req <- sb_req("invite_codes") |>
    httr2::req_url_query(
      code       = paste0("eq.", toupper(trimws(code))),
      used_at    = "is.null",                # not yet used
      expires_at = paste0("gt.", now),       # not expired
      select     = "*",
      limit      = "1"
    )
  result <- sb_perform(req,
    empty_df_cols = c("id","code","created_by","used_by","expires_at","used_at"))
  if (is.data.frame(result) && nrow(result) > 0) return(result[1, ])
  NULL
}

#' Mark an invite code as used
#' @param code_id integer
#' @param used_by_id integer user id
sb_use_invite <- function(code_id, used_by_id) {
  body <- list(
    used_by = used_by_id,
    used_at = format(Sys.time(), "%Y-%m-%dT%H:%M:%SZ", tz = "UTC")
  )
  req <- sb_req("invite_codes", "PATCH") |>
    httr2::req_url_query(id = paste0("eq.", code_id)) |>
    httr2::req_body_json(body)
  sb_perform(req)
  invisible(TRUE)
}

#' Get all invite codes with creator info (admin view)
sb_get_invites <- function() {
  req <- sb_req("invite_codes") |>
    httr2::req_url_query(
      select = "id,code,expires_at,used_at,created_by,used_by",
      order  = "id.desc"
    )
  sb_perform(req,
    empty_df_cols = c("id","code","expires_at","used_at","created_by","used_by"))
}

# ── IMAGE OPERATIONS ──────────────────────────────────────────────────────────

#' Add image record (skip if filepath already exists)
sb_add_image <- function(filepath, filename, width, height,
                         source_type = "upload", added_by = "unknown") {
  # Check existing
  req_check <- sb_req("images") |>
    httr2::req_url_query(filepath = paste0("eq.", filepath),
                         select   = "id", limit = "1")
  existing <- sb_perform(req_check, empty_df_cols = c("id"))
  if (is.data.frame(existing) && nrow(existing) > 0) return(existing$id[1])

  body <- list(filepath = filepath, filename = filename,
               img_width = width, img_height = height,
               source_type = source_type, added_by = added_by,
               status = "unannotated")
  req <- sb_req("images", "POST") |>
    httr2::req_body_json(body)
  result <- sb_perform(req)
  if (is.list(result) && !is.null(result$id)) return(result$id)
  if (is.data.frame(result) && nrow(result) > 0) return(result$id[1])
  NULL
}

#' Get all images with annotation counts
sb_get_images <- function() {
  # PostgREST supports embedded counts via resource embedding
  req <- sb_req("images") |>
    httr2::req_url_query(
      select = "id,filepath,filename,img_width,img_height,source_type,status,added_by,added_at,annotations(id)",
      order  = "added_at.asc"
    )
  result <- sb_perform(req,
    empty_df_cols = c("id","filepath","filename","img_width","img_height",
                      "source_type","status","added_by","added_at","box_count"))
  if (!is.data.frame(result) || nrow(result) == 0)
    return(data.frame(id=integer(),filepath=character(),filename=character(),
                      img_width=integer(),img_height=integer(),
                      source_type=character(),status=character(),
                      added_by=character(),added_at=character(),
                      box_count=integer(), stringsAsFactors=FALSE))

  # annotations column comes back as a list of data.frames — count rows
  if ("annotations" %in% names(result)) {
    result$box_count <- sapply(result$annotations, function(a) {
      if (is.null(a) || length(a) == 0) 0L
      else if (is.data.frame(a)) nrow(a)
      else length(a)
    })
    result$annotations <- NULL
  } else {
    result$box_count <- 0L
  }
  result
}

#' Update image status
sb_update_image_status <- function(image_id, status = "done") {
  req <- sb_req("images", "PATCH") |>
    httr2::req_url_query(id = paste0("eq.", image_id)) |>
    httr2::req_body_json(list(status = status))
  sb_perform(req)
  invisible(TRUE)
}

# ── CLASS OPERATIONS ──────────────────────────────────────────────────────────

PALETTE <- c("#FF6B6B","#4ECDC4","#FFE66D","#A8E6CF","#4f8ef7",
             "#B4A7D6","#82B366","#FF8B94","#F6A623","#50C878","#FF69B4","#00BFFF")

#' Get all annotation classes
sb_get_classes <- function() {
  req <- sb_req("classes") |>
    httr2::req_url_query(select = "class_id,class_name,color_hex",
                         order  = "class_id.asc")
  sb_perform(req,
    empty_df_cols = c("class_id","class_name","color_hex"))
}

#' Add a new class (skips if name already exists)
sb_add_class <- function(class_name, color_hex = NULL) {
  req_check <- sb_req("classes") |>
    httr2::req_url_query(
      class_name = paste0("ilike.", class_name),
      select     = "class_id", limit = "1")
  existing <- sb_perform(req_check, empty_df_cols = c("class_id"))
  if (is.data.frame(existing) && nrow(existing) > 0) return(existing$class_id[1])

  if (is.null(color_hex)) {
    count_req <- sb_req("classes") |>
      httr2::req_url_query(select = "class_id")
    ct <- sb_perform(count_req, empty_df_cols = c("class_id"))
    n  <- if (is.data.frame(ct)) nrow(ct) else 0L
    color_hex <- PALETTE[(n %% length(PALETTE)) + 1L]
  }

  body <- list(class_name = class_name, color_hex = color_hex)
  req  <- sb_req("classes", "POST") |>
    httr2::req_body_json(body)
  result <- sb_perform(req)
  if (is.list(result) && !is.null(result$class_id)) return(result$class_id)
  if (is.data.frame(result) && nrow(result) > 0) return(result$class_id[1])
  NULL
}

# ── ANNOTATION OPERATIONS ─────────────────────────────────────────────────────

#' Save all boxes for an image + annotator (replaces existing)
sb_save_annotations <- function(image_id, boxes, annotator_email) {
  # Delete existing for this image + annotator
  del_req <- sb_req("annotations", "DELETE") |>
    httr2::req_url_query(
      image_id       = paste0("eq.", image_id),
      annotator_email = paste0("eq.", annotator_email)
    )
  httr2::req_perform(del_req)

  if (!is.null(boxes) && nrow(boxes) > 0) {
    rows <- lapply(seq_len(nrow(boxes)), function(i) {
      b <- boxes[i, ]
      list(
        image_id        = image_id,
        class_id        = as.integer(b$class_id),
        class_name      = as.character(b$class_name),
        x_pixel         = as.numeric(b$x_pixel),
        y_pixel         = as.numeric(b$y_pixel),
        w_pixel         = as.numeric(b$w_pixel),
        h_pixel         = as.numeric(b$h_pixel),
        x_center_norm   = as.numeric(b$x_center_norm),
        y_center_norm   = as.numeric(b$y_center_norm),
        w_norm          = as.numeric(b$w_norm),
        h_norm          = as.numeric(b$h_norm),
        annotator_email = annotator_email
      )
    })
    ins_req <- sb_req("annotations", "POST") |>
      httr2::req_body_json(rows)
    sb_perform(ins_req)
  }

  # Update image status
  count_req <- sb_req("annotations") |>
    httr2::req_url_query(image_id = paste0("eq.", image_id),
                         select   = "id")
  ct     <- sb_perform(count_req, empty_df_cols = c("id"))
  status <- if (is.data.frame(ct) && nrow(ct) > 0) "done" else "unannotated"
  sb_update_image_status(image_id, status)
  invisible(TRUE)
}

#' Load all annotations for an image
sb_load_annotations <- function(image_id) {
  req <- sb_req("annotations") |>
    httr2::req_url_query(
      image_id = paste0("eq.", image_id),
      select   = "id,class_id,class_name,x_pixel,y_pixel,w_pixel,h_pixel,x_center_norm,y_center_norm,w_norm,h_norm,annotator_email,updated_at,classes(color_hex)",
      order    = "id.asc"
    )
  result <- sb_perform(req, empty_df_cols = c("id","class_id","class_name",
    "x_pixel","y_pixel","w_pixel","h_pixel","x_center_norm","y_center_norm",
    "w_norm","h_norm","annotator_email","updated_at","color_hex"))

  if (!is.data.frame(result) || nrow(result) == 0) return(result)

  # Unwrap nested classes.color_hex
  if ("classes" %in% names(result)) {
    result$color_hex <- sapply(result$classes, function(c) {
      if (is.null(c)) "#999999"
      else if (is.list(c)) c$color_hex %||% "#999999"
      else "#999999"
    })
    result$classes <- NULL
  }
  result
}

# ── STATS ─────────────────────────────────────────────────────────────────────

#' Get overall annotation statistics
sb_get_stats <- function() {
  img_req <- sb_req("images") |>
    httr2::req_url_query(select = "id,status")
  imgs <- sb_perform(img_req, empty_df_cols = c("id","status"))

  ann_req <- sb_req("annotations") |>
    httr2::req_url_query(select = "id,class_name,annotator_email,image_id,updated_at")
  anns <- sb_perform(ann_req, empty_df_cols = c("id","class_name","annotator_email","image_id","updated_at"))

  cls_req <- sb_req("annotations") |>
    httr2::req_url_query(
      select = "class_name,classes(color_hex)"
    )
  cls_raw <- sb_perform(cls_req, empty_df_cols = c("class_name","color_hex"))

  total   <- if (is.data.frame(imgs)) nrow(imgs) else 0L
  done    <- if (is.data.frame(imgs)) sum(imgs$status == "done", na.rm = TRUE) else 0L
  todo    <- total - done
  boxes   <- if (is.data.frame(anns)) nrow(anns) else 0L

  by_class <- if (is.data.frame(anns) && nrow(anns) > 0) {
    tbl <- table(anns$class_name)
    data.frame(class_name = names(tbl), box_count = as.integer(tbl),
               color_hex  = "#4f8ef7", stringsAsFactors = FALSE)
  } else data.frame(class_name=character(), box_count=integer(), color_hex=character())

  by_annotator <- if (is.data.frame(anns) && nrow(anns) > 0) {
    do.call(rbind, lapply(split(anns, anns$annotator_email), function(g) {
      data.frame(
        annotator_name   = g$annotator_email[1],
        images_annotated = length(unique(g$image_id)),
        boxes_drawn      = nrow(g),
        last_active      = max(g$updated_at, na.rm = TRUE),
        stringsAsFactors = FALSE)
    }))
  } else data.frame(annotator_name=character(), images_annotated=integer(),
                    boxes_drawn=integer(), last_active=character())

  list(
    totals       = data.frame(total_images=total, done_images=done,
                              todo_images=todo, total_boxes=boxes),
    by_class     = by_class,
    by_annotator = by_annotator
  )
}
