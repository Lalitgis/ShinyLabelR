#' Export annotations in YOLO Ultralytics format
#'
#' Writes one .txt label file per annotated image:
#'   class_id  x_center  y_center  width  height   (all normalised 0-1)
#' Plus data.yaml with class names. Returns a .zip of the full dataset.
#'
#' @param con        DBI connection to ShinyLabel SQLite DB
#' @param output_dir Temp directory to stage files in
#' @param split_ratio Fraction for train set (default 0.8)
#' @return Absolute path to the produced .zip file
#' @export
sl_export_yolo <- function(con, output_dir = tempdir(), split_ratio = 0.8) {

  classes  <- sl_get_classes(con)
  images   <- sl_get_images(con)
  ann_imgs <- images[images$box_count > 0, ]

  if (nrow(ann_imgs) == 0)
    stop("No annotated images. Please annotate at least one image first.")
  if (nrow(classes) == 0)
    stop("No label classes. Please add at least one class first.")

  # ── Unique staging dir ─────────────────────────────────────────────────────
  stamp    <- format(Sys.time(), "%Y%m%d_%H%M%S")
  root_dir <- file.path(output_dir, paste0("sl_", stamp))
  yolo_dir <- file.path(root_dir,   "yolo_dataset")

  for (d in c(file.path(yolo_dir, "images", "train"),
              file.path(yolo_dir, "images", "val"),
              file.path(yolo_dir, "labels", "train"),
              file.path(yolo_dir, "labels", "val")))
    dir.create(d, recursive = TRUE, showWarnings = FALSE)

  # ── Train / val split ──────────────────────────────────────────────────────
  set.seed(42)
  n         <- nrow(ann_imgs)
  shuf      <- sample(n)
  n_train   <- max(1L, floor(n * split_ratio))
  train_ids <- ann_imgs$id[shuf[seq_len(n_train)]]
  val_ids   <- if (n > 1L) ann_imgs$id[shuf[(n_train + 1L):n]] else integer(0)

  class_map <- setNames(seq_len(nrow(classes)) - 1L, classes$class_name)

  # ── Write label .txt files ─────────────────────────────────────────────────
  write_split <- function(ids, img_dir, lbl_dir) {
    for (img_id in ids) {
      row <- images[images$id == img_id, ]
      if (nrow(row) == 0L) next
      fname     <- as.character(row$filename[1])
      src       <- as.character(row$filepath[1])
      base      <- tools::file_path_sans_ext(fname)
      lbl_path  <- file.path(lbl_dir, paste0(base, ".txt"))

      if (file.exists(src))
        file.copy(src, file.path(img_dir, fname), overwrite = TRUE)

      boxes <- sl_load_annotations(con, img_id)
      if (nrow(boxes) == 0L) {
        file.create(lbl_path)          # valid YOLO empty label
      } else {
        lines <- vapply(seq_len(nrow(boxes)), function(i) {
          b   <- boxes[i, ]
          cid <- class_map[as.character(b$class_name)]
          if (is.na(cid)) cid <- 0L
          sprintf("%d %.6f %.6f %.6f %.6f",
                  as.integer(cid),
                  min(1, max(0, as.numeric(b$x_center_norm))),
                  min(1, max(0, as.numeric(b$y_center_norm))),
                  min(1, max(0, as.numeric(b$w_norm))),
                  min(1, max(0, as.numeric(b$h_norm))))
        }, character(1L))
        writeLines(lines, lbl_path)
      }
    }
  }

  write_split(train_ids,
              file.path(yolo_dir, "images", "train"),
              file.path(yolo_dir, "labels", "train"))
  if (length(val_ids) > 0L)
    write_split(val_ids,
                file.path(yolo_dir, "images", "val"),
                file.path(yolo_dir, "labels", "val"))

  # ── data.yaml ──────────────────────────────────────────────────────────────
  yaml_lines <- c(
    "# ShinyLabel YOLO Export",
    "# Train: yolo train data=data.yaml model=yolov8n.pt epochs=100",
    "", "path: .", "train: images/train", "val:   images/val", "",
    paste0("nc: ", nrow(classes)),
    paste0("names: [", paste(classes$class_name, collapse = ", "), "]"),
    "", "# Class index (zero-indexed):"
  )
  for (i in seq_len(nrow(classes)))
    yaml_lines <- c(yaml_lines,
                    sprintf("#   %d: %s", i - 1L, classes$class_name[i]))
  yaml_lines <- c(yaml_lines, "",
    paste0("# Exported: ",  format(Sys.time(), "%Y-%m-%d %H:%M:%S")),
    paste0("# Train: ",     length(train_ids)),
    paste0("# Val:   ",     length(val_ids)))
  writeLines(yaml_lines, file.path(yolo_dir, "data.yaml"))

  # ── Zip ────────────────────────────────────────────────────────────────────
  # Build zip from root_dir so internal paths are:
  #   yolo_dataset/labels/train/img.txt   (relative, correct)
  # NOT:
  #   /tmp/sl_20250425/yolo_dataset/labels/train/img.txt  (absolute, wrong)

  zip_name <- paste0("yolo_annotations_", format(Sys.Date(), "%Y%m%d"), ".zip")
  zip_abs  <- normalizePath(file.path(root_dir, zip_name), mustWork = FALSE)

  # Helper: verify zip was created and is a real zip (PK magic bytes)
  valid_zip <- function(p) {
    if (!file.exists(p) || file.info(p)$size < 22L) return(FALSE)
    magic <- readBin(p, what = "raw", n = 2L)
    length(magic) == 2L && magic[1] == as.raw(0x50) && magic[2] == as.raw(0x4b)
  }

  zipped <- FALSE

  # ── Attempt 1: zip::zip with root= (zip >= 2.2.0) ─────────────────────────
  if (!zipped && requireNamespace("zip", quietly = TRUE)) {
    tryCatch({
      if ("root" %in% names(formals(zip::zip))) {
        zip::zip(zipfile = zip_abs, files = "yolo_dataset",
                 recurse = TRUE, root = root_dir)
      } else {
        # Older zip package: no root param, use setwd temporarily
        old_wd <- getwd()
        on.exit(setwd(old_wd), add = TRUE)
        setwd(root_dir)
        zip::zip(zipfile = zip_name, files = "yolo_dataset", recurse = TRUE)
        setwd(old_wd)
        on.exit(NULL)
      }
      zipped <- valid_zip(zip_abs)
    }, error = function(e) {
      message("[ShinyLabel] zip::zip attempt failed: ", e$message)
    })
  }

  # ── Attempt 2: utils::zip (base R, calls system zip binary) ───────────────
  if (!zipped) {
    old_wd2 <- getwd()
    tryCatch({
      setwd(root_dir)
      utils::zip(zipfile = zip_name, files = "yolo_dataset")
      setwd(old_wd2)
      zipped <- valid_zip(zip_abs)
    }, error = function(e) {
      try(setwd(old_wd2), silent = TRUE)
      message("[ShinyLabel] utils::zip attempt failed: ", e$message)
    })
  }

  if (!zipped)
    stop(paste0(
      "Cannot create zip file.\n",
      "Please install the zip package:  install.packages('zip')\n",
      "Then restart the app and try again."
    ))

  message(sprintf("[ShinyLabel] YOLO export ready: %s  (train=%d, val=%d)",
                  zip_abs, length(train_ids), length(val_ids)))
  return(zip_abs)
}


#' Export to COCO JSON
#' @param con DBI connection
#' @param output_dir Temp dir
#' @return Path to .json file
#' @export
sl_export_coco <- function(con, output_dir = tempdir()) {
  classes  <- sl_get_classes(con)
  images   <- sl_get_images(con)
  ann_imgs <- images[images$box_count > 0, ]
  if (nrow(ann_imgs) == 0) stop("No annotated images to export.")

  class_map <- setNames(seq_len(nrow(classes)), classes$class_name)
  cats <- lapply(seq_len(nrow(classes)), function(i)
    list(id = i, name = classes$class_name[i], supercategory = "object"))

  imgs_out <- list(); anns_out <- list(); ann_id <- 1L
  for (i in seq_len(nrow(ann_imgs))) {
    img <- ann_imgs[i, ]
    imgs_out[[i]] <- list(id = as.integer(img$id),
                          file_name = as.character(img$filename),
                          width  = as.integer(img$img_width),
                          height = as.integer(img$img_height))
    boxes <- sl_load_annotations(con, img$id)
    for (j in seq_len(nrow(boxes))) {
      b <- boxes[j, ]
      anns_out[[ann_id]] <- list(
        id = ann_id, image_id = as.integer(img$id),
        category_id = unname(as.integer(class_map[as.character(b$class_name)])),
        bbox = list(as.numeric(b$x_pixel), as.numeric(b$y_pixel),
                    as.numeric(b$w_pixel), as.numeric(b$h_pixel)),
        area = as.numeric(b$w_pixel) * as.numeric(b$h_pixel), iscrowd = 0L)
      ann_id <- ann_id + 1L
    }
  }
  out <- list(
    info = list(description = "ShinyLabel COCO Export", version = "1.0",
                year = as.integer(format(Sys.Date(), "%Y")),
                date_created = format(Sys.time(), "%Y/%m/%d")),
    licenses = list(), categories = cats, images = imgs_out, annotations = anns_out)

  path <- file.path(output_dir,
                    paste0("coco_", format(Sys.Date(), "%Y%m%d"), ".json"))
  jsonlite::write_json(out, path, auto_unbox = TRUE, pretty = TRUE)
  return(path)
}


#' Pixel coords → YOLO normalised
#' @export
px_to_yolo_norm <- function(x_tl, y_tl, w_px, h_px, img_w, img_h) {
  list(x_center_norm = (x_tl + w_px / 2) / img_w,
       y_center_norm = (y_tl + h_px / 2) / img_h,
       w_norm = w_px / img_w, h_norm = h_px / img_h)
}
