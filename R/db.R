#' Initialize the ShinyLabel SQLite database
#'
#' Creates all required tables and enables WAL mode for concurrent team access.
#' Safe to call multiple times — uses CREATE TABLE IF NOT EXISTS.
#'
#' @param db_path Path to the SQLite .db file
#' @return A DBI connection object
#' @export
sl_init_db <- function(db_path = "shinylabel.db") {
  con <- DBI::dbConnect(RSQLite::SQLite(), db_path)

  # Enable WAL mode for concurrent multi-annotator access
  DBI::dbExecute(con, "PRAGMA journal_mode=WAL;")
  DBI::dbExecute(con, "PRAGMA synchronous=NORMAL;")
  DBI::dbExecute(con, "PRAGMA busy_timeout=10000;")  # 10 second timeout

  # Images table — tracks every image regardless of annotation status
  DBI::dbExecute(con, "
    CREATE TABLE IF NOT EXISTS images (
      id            INTEGER PRIMARY KEY AUTOINCREMENT,
      filepath      TEXT    NOT NULL UNIQUE,
      filename      TEXT    NOT NULL,
      img_width     INTEGER NOT NULL DEFAULT 0,
      img_height    INTEGER NOT NULL DEFAULT 0,
      source_type   TEXT    NOT NULL DEFAULT 'upload',  -- 'upload', 'folder', 'url'
      status        TEXT    NOT NULL DEFAULT 'unannotated',  -- 'unannotated', 'in_progress', 'done'
      added_by      TEXT,
      added_at      TEXT    NOT NULL DEFAULT (datetime('now'))
    )
  ")

  # Classes table — user-defined label classes
  DBI::dbExecute(con, "
    CREATE TABLE IF NOT EXISTS classes (
      class_id    INTEGER PRIMARY KEY AUTOINCREMENT,
      class_name  TEXT    NOT NULL UNIQUE,
      color_hex   TEXT    NOT NULL DEFAULT '#FF6B6B',
      created_at  TEXT    NOT NULL DEFAULT (datetime('now'))
    )
  ")

  # Annotations table — one row per bounding box
  DBI::dbExecute(con, "
    CREATE TABLE IF NOT EXISTS annotations (
      id              INTEGER PRIMARY KEY AUTOINCREMENT,
      image_id        INTEGER NOT NULL REFERENCES images(id) ON DELETE CASCADE,
      class_id        INTEGER NOT NULL REFERENCES classes(class_id),
      class_name      TEXT    NOT NULL,
      -- Raw pixel coordinates (top-left origin)
      x_pixel         REAL    NOT NULL,
      y_pixel         REAL    NOT NULL,
      w_pixel         REAL    NOT NULL,
      h_pixel         REAL    NOT NULL,
      -- YOLO normalized coordinates (center-based, 0-1)
      x_center_norm   REAL    NOT NULL,
      y_center_norm   REAL    NOT NULL,
      w_norm          REAL    NOT NULL,
      h_norm          REAL    NOT NULL,
      -- Team tracking
      annotator_name  TEXT    NOT NULL,
      created_at      TEXT    NOT NULL DEFAULT (datetime('now')),
      updated_at      TEXT    NOT NULL DEFAULT (datetime('now'))
    )
  ")

  # Annotator sessions table — lightweight session log
  DBI::dbExecute(con, "
    CREATE TABLE IF NOT EXISTS sessions (
      id              INTEGER PRIMARY KEY AUTOINCREMENT,
      annotator_name  TEXT NOT NULL,
      started_at      TEXT NOT NULL DEFAULT (datetime('now')),
      last_active_at  TEXT NOT NULL DEFAULT (datetime('now'))
    )
  ")

  # No default classes — users define their own labels

  message("[ShinyLabel] Database initialized at: ", db_path)
  return(con)
}

#' Get a fresh database connection with proper pragmas set
#' @param db_path Path to the SQLite .db file
#' @return DBI connection
sl_get_con <- function(db_path) {
  con <- DBI::dbConnect(RSQLite::SQLite(), db_path)
  DBI::dbExecute(con, "PRAGMA journal_mode=WAL;")
  DBI::dbExecute(con, "PRAGMA busy_timeout=10000;")
  return(con)
}

# ── Image operations ──────────────────────────────────────────────────────────

#' Add an image record to the database
#' @param con DBI connection
#' @param filepath Full path or URL to the image
#' @param filename Display filename
#' @param width Image pixel width
#' @param height Image pixel height
#' @param source_type One of 'upload', 'folder', 'url'
#' @param annotator Current annotator name
#' @return id of inserted (or existing) image row
sl_add_image <- function(con, filepath, filename, width, height,
                         source_type = "upload", annotator = "unknown") {
  existing <- DBI::dbGetQuery(
    con,
    "SELECT id FROM images WHERE filepath = ?",
    params = list(filepath)
  )
  if (nrow(existing) > 0) return(existing$id[1])

  DBI::dbExecute(con,
    "INSERT INTO images (filepath, filename, img_width, img_height, source_type, added_by)
     VALUES (?, ?, ?, ?, ?, ?)",
    params = list(filepath, filename, width, height, source_type, annotator)
  )
  DBI::dbGetQuery(con, "SELECT last_insert_rowid() as id")$id
}

#' Get all images with annotation counts
#' @param con DBI connection
#' @return data.frame
sl_get_images <- function(con) {
  DBI::dbGetQuery(con, "
    SELECT
      i.id, i.filepath, i.filename, i.img_width, i.img_height,
      i.source_type, i.status, i.added_by, i.added_at,
      COUNT(a.id) as box_count
    FROM images i
    LEFT JOIN annotations a ON a.image_id = i.id
    GROUP BY i.id
    ORDER BY i.added_at ASC
  ")
}

#' Update image status based on annotation count
#' @param con DBI connection
#' @param image_id integer
sl_update_image_status <- function(con, image_id) {
  count <- DBI::dbGetQuery(
    con,
    "SELECT COUNT(*) as n FROM annotations WHERE image_id = ?",
    params = list(image_id)
  )$n

  status <- if (count == 0) "unannotated" else "done"
  DBI::dbExecute(
    con,
    "UPDATE images SET status = ? WHERE id = ?",
    params = list(status, image_id)
  )
}

# ── Class operations ──────────────────────────────────────────────────────────

#' Get all annotation classes
#' @param con DBI connection
#' @return data.frame with class_id, class_name, color_hex
sl_get_classes <- function(con) {
  DBI::dbGetQuery(con, "SELECT class_id, class_name, color_hex FROM classes ORDER BY class_id")
}

#' Add a new annotation class
#' @param con DBI connection
#' @param class_name Character
#' @param color_hex Hex color string e.g. "#FF6B6B"
sl_add_class <- function(con, class_name, color_hex = NULL) {
  if (is.null(color_hex)) {
    # Auto-assign from a palette
    palette <- c("#FF6B6B","#4ECDC4","#FFE66D","#A8E6CF","#FF8B94",
                 "#B4A7D6","#D5E8D4","#FFD966","#82B366","#DAE8FC")
    existing_count <- DBI::dbGetQuery(con, "SELECT COUNT(*) as n FROM classes")$n
    color_hex <- palette[(existing_count %% length(palette)) + 1]
  }

  existing <- DBI::dbGetQuery(
    con,
    "SELECT class_id FROM classes WHERE LOWER(class_name) = LOWER(?)",
    params = list(class_name)
  )
  if (nrow(existing) > 0) return(existing$class_id[1])

  DBI::dbExecute(
    con,
    "INSERT INTO classes (class_name, color_hex) VALUES (?, ?)",
    params = list(class_name, color_hex)
  )
  DBI::dbGetQuery(con, "SELECT last_insert_rowid() as id")$id
}

# ── Annotation operations ─────────────────────────────────────────────────────

#' Save all annotations for an image (replaces existing ones for this image)
#'
#' This is the auto-save workhorse. Called whenever user navigates away.
#' Uses IMMEDIATE transaction to avoid deadlocks.
#'
#' @param con DBI connection
#' @param image_id integer
#' @param boxes data.frame with columns: class_id, class_name,
#'   x_pixel, y_pixel, w_pixel, h_pixel,
#'   x_center_norm, y_center_norm, w_norm, h_norm
#' @param annotator_name character
sl_save_annotations <- function(con, image_id, boxes, annotator_name) {
  DBI::dbWithTransaction(con, {
    # Clear existing annotations for this image by this annotator
    DBI::dbExecute(
      con,
      "DELETE FROM annotations WHERE image_id = ? AND annotator_name = ?",
      params = list(image_id, annotator_name)
    )

    if (!is.null(boxes) && nrow(boxes) > 0) {
      for (i in seq_len(nrow(boxes))) {
        b <- boxes[i, ]
        DBI::dbExecute(con, "
          INSERT INTO annotations
            (image_id, class_id, class_name,
             x_pixel, y_pixel, w_pixel, h_pixel,
             x_center_norm, y_center_norm, w_norm, h_norm,
             annotator_name, updated_at)
          VALUES (?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, datetime('now'))
        ", params = list(
          image_id, b$class_id, b$class_name,
          b$x_pixel, b$y_pixel, b$w_pixel, b$h_pixel,
          b$x_center_norm, b$y_center_norm, b$w_norm, b$h_norm,
          annotator_name
        ))
      }
    }
  })
  sl_update_image_status(con, image_id)
}

#' Load all annotations for a given image
#' @param con DBI connection
#' @param image_id integer
#' @return data.frame
sl_load_annotations <- function(con, image_id) {
  DBI::dbGetQuery(con, "
    SELECT
      a.id, a.class_id, a.class_name,
      a.x_pixel, a.y_pixel, a.w_pixel, a.h_pixel,
      a.x_center_norm, a.y_center_norm, a.w_norm, a.h_norm,
      a.annotator_name, a.updated_at,
      c.color_hex
    FROM annotations a
    JOIN classes c ON c.class_id = a.class_id
    WHERE a.image_id = ?
    ORDER BY a.id
  ", params = list(image_id))
}

# ── Dashboard / stats ─────────────────────────────────────────────────────────

#' Get overall annotation statistics
#' @param con DBI connection
#' @return list with summary stats
sl_get_stats <- function(con) {
  totals <- DBI::dbGetQuery(con, "
    SELECT
      COUNT(DISTINCT i.id)                                    as total_images,
      SUM(CASE WHEN i.status = 'done' THEN 1 ELSE 0 END)     as done_images,
      SUM(CASE WHEN i.status = 'unannotated' THEN 1 ELSE 0 END) as todo_images,
      COUNT(a.id)                                             as total_boxes
    FROM images i
    LEFT JOIN annotations a ON a.image_id = i.id
  ")

  by_class <- DBI::dbGetQuery(con, "
    SELECT a.class_name, c.color_hex, COUNT(*) as box_count
    FROM annotations a
    JOIN classes c ON c.class_id = a.class_id
    GROUP BY a.class_name
    ORDER BY box_count DESC
  ")

  by_annotator <- DBI::dbGetQuery(con, "
    SELECT
      annotator_name,
      COUNT(DISTINCT image_id) as images_annotated,
      COUNT(*) as boxes_drawn,
      MAX(updated_at) as last_active
    FROM annotations
    GROUP BY annotator_name
    ORDER BY boxes_drawn DESC
  ")

  list(totals = totals, by_class = by_class, by_annotator = by_annotator)
}
