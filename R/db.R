#' Initialize the ShinyLabel SQLite database
#' @param db_path Path to the SQLite .db file
#' @return A DBI connection object
#' @export
sl_init_db <- function(db_path = "shinylabel.db") {
  con <- DBI::dbConnect(RSQLite::SQLite(), db_path)
  DBI::dbExecute(con, "PRAGMA journal_mode=WAL;")
  DBI::dbExecute(con, "PRAGMA synchronous=NORMAL;")
  DBI::dbExecute(con, "PRAGMA busy_timeout=10000;")

  # ── Users — proper accounts with hashed passwords ─────────────────────────
  DBI::dbExecute(con, "
    CREATE TABLE IF NOT EXISTS users (
      id            INTEGER PRIMARY KEY AUTOINCREMENT,
      first_name    TEXT    NOT NULL,
      last_name     TEXT    NOT NULL,
      email         TEXT    NOT NULL UNIQUE,
      password_hash TEXT    NOT NULL,
      password_salt TEXT    NOT NULL,
      role          TEXT    NOT NULL DEFAULT 'annotator',
      verified      INTEGER NOT NULL DEFAULT 0,
      created_at    TEXT    NOT NULL DEFAULT (datetime('now'))
    )
  ")

  # ── Email tokens — verify | invite | reset ────────────────────────────────
  DBI::dbExecute(con, "
    CREATE TABLE IF NOT EXISTS email_tokens (
      id         INTEGER PRIMARY KEY AUTOINCREMENT,
      token      TEXT    NOT NULL UNIQUE,
      type       TEXT    NOT NULL,
      email      TEXT    NOT NULL,
      created_by TEXT,
      used_at    TEXT,
      expires_at TEXT    NOT NULL,
      created_at TEXT    NOT NULL DEFAULT (datetime('now'))
    )
  ")

  # ── Images ────────────────────────────────────────────────────────────────
  DBI::dbExecute(con, "
    CREATE TABLE IF NOT EXISTS images (
      id            INTEGER PRIMARY KEY AUTOINCREMENT,
      filepath      TEXT    NOT NULL UNIQUE,
      filename      TEXT    NOT NULL,
      img_width     INTEGER NOT NULL DEFAULT 0,
      img_height    INTEGER NOT NULL DEFAULT 0,
      source_type   TEXT    NOT NULL DEFAULT 'upload',
      status        TEXT    NOT NULL DEFAULT 'unannotated',
      added_by      TEXT,
      added_at      TEXT    NOT NULL DEFAULT (datetime('now'))
    )
  ")

  # ── Classes ───────────────────────────────────────────────────────────────
  DBI::dbExecute(con, "
    CREATE TABLE IF NOT EXISTS classes (
      class_id    INTEGER PRIMARY KEY AUTOINCREMENT,
      class_name  TEXT    NOT NULL UNIQUE,
      color_hex   TEXT    NOT NULL DEFAULT '#FF6B6B',
      created_at  TEXT    NOT NULL DEFAULT (datetime('now'))
    )
  ")

  # ── Annotations ───────────────────────────────────────────────────────────
  DBI::dbExecute(con, "
    CREATE TABLE IF NOT EXISTS annotations (
      id              INTEGER PRIMARY KEY AUTOINCREMENT,
      image_id        INTEGER NOT NULL REFERENCES images(id) ON DELETE CASCADE,
      class_id        INTEGER NOT NULL REFERENCES classes(class_id),
      class_name      TEXT    NOT NULL,
      x_pixel         REAL    NOT NULL,
      y_pixel         REAL    NOT NULL,
      w_pixel         REAL    NOT NULL,
      h_pixel         REAL    NOT NULL,
      x_center_norm   REAL    NOT NULL,
      y_center_norm   REAL    NOT NULL,
      w_norm          REAL    NOT NULL,
      h_norm          REAL    NOT NULL,
      annotator_name  TEXT    NOT NULL,
      created_at      TEXT    NOT NULL DEFAULT (datetime('now')),
      updated_at      TEXT    NOT NULL DEFAULT (datetime('now'))
    )
  ")

  # ── Sessions — lightweight activity log ───────────────────────────────────
  DBI::dbExecute(con, "
    CREATE TABLE IF NOT EXISTS sessions (
      id              INTEGER PRIMARY KEY AUTOINCREMENT,
      annotator_name  TEXT NOT NULL,
      started_at      TEXT NOT NULL DEFAULT (datetime('now')),
      last_active_at  TEXT NOT NULL DEFAULT (datetime('now'))
    )
  ")

  message("[ShinyLabel] Database initialized at: ", db_path)
  return(con)
}

# ── User operations ───────────────────────────────────────────────────────────

#' Create a new user account
sl_create_user <- function(con, first_name, last_name, email, password, role = "annotator") {
  salt <- auth_gen_salt()
  hash <- auth_hash_password(password, salt)
  DBI::dbExecute(con,
    "INSERT INTO users (first_name, last_name, email, password_hash, password_salt, role, verified)
     VALUES (?, ?, ?, ?, ?, ?, 0)",
    params = list(first_name, last_name, tolower(trimws(email)), hash, salt, role))
  DBI::dbGetQuery(con, "SELECT last_insert_rowid() as id")$id
}

#' Get user by email
sl_get_user_by_email <- function(con, email) {
  res <- DBI::dbGetQuery(con,
    "SELECT * FROM users WHERE email = ?",
    params = list(tolower(trimws(email))))
  if (nrow(res) == 0L) return(NULL)
  as.list(res[1, ])
}

#' Get all users
sl_get_all_users <- function(con) {
  DBI::dbGetQuery(con,
    "SELECT id, first_name, last_name, email, role, verified, created_at
     FROM users ORDER BY created_at ASC")
}

#' Check if any users exist
sl_user_count <- function(con) {
  DBI::dbGetQuery(con, "SELECT COUNT(*) as n FROM users")$n
}

#' Verify password for a user
sl_verify_login <- function(con, email, password) {
  user <- sl_get_user_by_email(con, email)
  if (is.null(user)) return(NULL)
  if (!auth_verify_password(password, user$password_salt, user$password_hash)) return(NULL)
  user
}

#' Mark user as email-verified
sl_verify_user <- function(con, email) {
  DBI::dbExecute(con,
    "UPDATE users SET verified = 1 WHERE email = ?",
    params = list(tolower(trimws(email))))
}

#' Update user password
sl_update_password <- function(con, email, new_password) {
  salt <- auth_gen_salt()
  hash <- auth_hash_password(new_password, salt)
  DBI::dbExecute(con,
    "UPDATE users SET password_hash = ?, password_salt = ? WHERE email = ?",
    params = list(hash, salt, tolower(trimws(email))))
}

# ── Token operations ──────────────────────────────────────────────────────────

#' Create an email token
#' @param type "verify" | "invite" | "reset"
#' @param email recipient email
#' @param created_by admin email (for invites)
#' @param hours_valid hours until expiry
sl_create_token <- function(con, type, email, created_by = NULL, hours_valid = 24L) {
  token      <- auth_gen_token()
  expires_at <- format(Sys.time() + hours_valid * 3600,
                       "%Y-%m-%d %H:%M:%S", tz = "UTC")
  DBI::dbExecute(con,
    "INSERT INTO email_tokens (token, type, email, created_by, expires_at)
     VALUES (?, ?, ?, ?, ?)",
    params = list(token, type, tolower(trimws(email)), created_by, expires_at))
  token
}

#' Validate a token — returns token row or NULL
sl_validate_token <- function(con, token, type) {
  res <- DBI::dbGetQuery(con,
    "SELECT * FROM email_tokens
     WHERE token = ? AND type = ? AND used_at IS NULL
       AND expires_at > datetime('now')",
    params = list(token, type))
  if (nrow(res) == 0L) return(NULL)
  as.list(res[1, ])
}

#' Mark token as used
sl_use_token <- function(con, token) {
  DBI::dbExecute(con,
    "UPDATE email_tokens SET used_at = datetime('now') WHERE token = ?",
    params = list(token))
}

#' Get recent invite tokens sent by admin
sl_get_invites <- function(con, limit = 10L) {
  DBI::dbGetQuery(con,
    "SELECT token, email, created_by, used_at, expires_at, created_at
     FROM email_tokens WHERE type = 'invite'
     ORDER BY id DESC LIMIT ?",
    params = list(limit))
}

# ── Image operations ──────────────────────────────────────────────────────────

sl_add_image <- function(con, filepath, filename, width, height,
                         source_type = "upload", annotator = "unknown") {
  existing <- DBI::dbGetQuery(con,
    "SELECT id FROM images WHERE filepath = ?", params = list(filepath))
  if (nrow(existing) > 0) return(existing$id[1])
  DBI::dbExecute(con,
    "INSERT INTO images (filepath, filename, img_width, img_height, source_type, added_by)
     VALUES (?, ?, ?, ?, ?, ?)",
    params = list(filepath, filename, width, height, source_type, annotator))
  DBI::dbGetQuery(con, "SELECT last_insert_rowid() as id")$id
}

sl_get_images <- function(con) {
  DBI::dbGetQuery(con, "
    SELECT i.id, i.filepath, i.filename, i.img_width, i.img_height,
           i.source_type, i.status, i.added_by, i.added_at,
           COUNT(a.id) as box_count
    FROM images i
    LEFT JOIN annotations a ON a.image_id = i.id
    GROUP BY i.id ORDER BY i.added_at ASC")
}

sl_update_image_status <- function(con, image_id) {
  count <- DBI::dbGetQuery(con,
    "SELECT COUNT(*) as n FROM annotations WHERE image_id = ?",
    params = list(image_id))$n
  DBI::dbExecute(con,
    "UPDATE images SET status = ? WHERE id = ?",
    params = list(if (count == 0) "unannotated" else "done", image_id))
}

# ── Class operations ──────────────────────────────────────────────────────────

sl_get_classes <- function(con) {
  DBI::dbGetQuery(con, "SELECT class_id, class_name, color_hex FROM classes ORDER BY class_id")
}

sl_add_class <- function(con, class_name, color_hex = NULL) {
  if (is.null(color_hex)) {
    palette <- c("#FF6B6B","#4ECDC4","#FFE66D","#A8E6CF","#FF8B94",
                 "#B4A7D6","#D5E8D4","#FFD966","#82B366","#DAE8FC")
    n <- DBI::dbGetQuery(con, "SELECT COUNT(*) as n FROM classes")$n
    color_hex <- palette[(n %% length(palette)) + 1]
  }
  existing <- DBI::dbGetQuery(con,
    "SELECT class_id FROM classes WHERE LOWER(class_name) = LOWER(?)",
    params = list(class_name))
  if (nrow(existing) > 0) return(existing$class_id[1])
  DBI::dbExecute(con,
    "INSERT INTO classes (class_name, color_hex) VALUES (?, ?)",
    params = list(class_name, color_hex))
  DBI::dbGetQuery(con, "SELECT last_insert_rowid() as id")$id
}

# ── Annotation operations ─────────────────────────────────────────────────────

sl_save_annotations <- function(con, image_id, boxes, annotator_name) {
  DBI::dbWithTransaction(con, {
    DBI::dbExecute(con,
      "DELETE FROM annotations WHERE image_id = ? AND annotator_name = ?",
      params = list(image_id, annotator_name))
    if (!is.null(boxes) && nrow(boxes) > 0) {
      for (i in seq_len(nrow(boxes))) {
        b <- boxes[i, ]
        DBI::dbExecute(con, "
          INSERT INTO annotations
            (image_id, class_id, class_name,
             x_pixel, y_pixel, w_pixel, h_pixel,
             x_center_norm, y_center_norm, w_norm, h_norm,
             annotator_name, updated_at)
          VALUES (?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, datetime('now'))",
          params = list(image_id, b$class_id, b$class_name,
                        b$x_pixel, b$y_pixel, b$w_pixel, b$h_pixel,
                        b$x_center_norm, b$y_center_norm, b$w_norm, b$h_norm,
                        annotator_name))
      }
    }
  })
  sl_update_image_status(con, image_id)
}

sl_load_annotations <- function(con, image_id) {
  DBI::dbGetQuery(con, "
    SELECT a.id, a.class_id, a.class_name,
           a.x_pixel, a.y_pixel, a.w_pixel, a.h_pixel,
           a.x_center_norm, a.y_center_norm, a.w_norm, a.h_norm,
           a.annotator_name, a.updated_at, c.color_hex
    FROM annotations a
    JOIN classes c ON c.class_id = a.class_id
    WHERE a.image_id = ? ORDER BY a.id",
    params = list(image_id))
}

# ── Stats ─────────────────────────────────────────────────────────────────────

sl_get_stats <- function(con) {
  totals <- DBI::dbGetQuery(con, "
    SELECT COUNT(DISTINCT i.id) as total_images,
           SUM(CASE WHEN i.status='done' THEN 1 ELSE 0 END) as done_images,
           SUM(CASE WHEN i.status='unannotated' THEN 1 ELSE 0 END) as todo_images,
           COUNT(a.id) as total_boxes
    FROM images i LEFT JOIN annotations a ON a.image_id = i.id")

  by_class <- DBI::dbGetQuery(con, "
    SELECT a.class_name, c.color_hex, COUNT(*) as box_count
    FROM annotations a JOIN classes c ON c.class_id = a.class_id
    GROUP BY a.class_name ORDER BY box_count DESC")

  by_annotator <- DBI::dbGetQuery(con, "
    SELECT annotator_name,
           COUNT(DISTINCT image_id) as images_annotated,
           COUNT(*) as boxes_drawn,
           MAX(updated_at) as last_active
    FROM annotations GROUP BY annotator_name ORDER BY boxes_drawn DESC")

  list(totals = totals, by_class = by_class, by_annotator = by_annotator)
}
