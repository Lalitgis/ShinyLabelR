# ShinyLabelR

**R based annotation web-app companion to the [ShinyLabel](https://github.com/Lalitgis/ShinyLabel) R package.**

Use `shinylabelR` to read, write, and export ShinyLabel annotation databases 
directly from R scripts — no browser required.

## Installation

```r
# Install from GitHub
devtools::install_github("Lalitgis/shinylabelR")
```

## The Full Workflow

```r
# ── Step 1: Annotate images visually in ShinyLabel ─────────────────────────
shiny::runGitHub("ShinyLabel", "Lalitgis")
# Draw bounding boxes, add classes, save, export

# ── Step 2: Use shinylabelR to work with your annotations in R ─────────────
library(shinylabelR)

# Connect to the database created by ShinyLabel
con <- sl_init_db("C:/Users/Hp/Documents/shinylabel/shinylabel.db")

# Check how many images are annotated
stats <- sl_get_stats(con)
print(stats$totals)
#   total_images done_images todo_images total_boxes
# 1           20          17           3          94

# See annotation counts per class
print(stats$by_class)
#   class_name color_hex box_count
# 1      drone   #4f8ef7        60
# 2     person   #FF6B6B        34

# Load all annotations for image ID 1 as a data.frame
boxes <- sl_load_annotations(con, image_id = 1L)
head(boxes)

# Export to YOLO format (produces a zip with labels/train/, labels/val/, data.yaml)
zip_path <- sl_export_yolo(con, output_dir = "exports/", split_ratio = 0.8)
cat("YOLO dataset saved to:", zip_path)

# Export to COCO JSON
json_path <- sl_export_coco(con, output_dir = "exports/")

# Always disconnect when done
DBI::dbDisconnect(con)
```

## Add Images and Annotations Programmatically

```r
library(shinylabelR)
con <- sl_init_db("new_project.db")

# Add label classes
sl_add_class(con, "drone",  "#4f8ef7")
sl_add_class(con, "person", "#FF6B6B")

# Add an image
img_id <- sl_add_image(con,
  filepath = "images/drone01.jpg",
  filename = "drone01.jpg",
  width    = 1280L,
  height   = 720L)

# Add bounding boxes for that image
boxes <- data.frame(
  class_id      = 1L,
  class_name    = "drone",
  x_pixel       = 200,
  y_pixel       = 100,
  w_pixel       = 300,
  h_pixel       = 200,
  x_center_norm = (200 + 300/2) / 1280,
  y_center_norm = (100 + 200/2) / 720,
  w_norm        = 300 / 1280,
  h_norm        = 200 / 720
)
sl_save_annotations(con, image_id = img_id, boxes = boxes, annotator_name = "Lalit")

DBI::dbDisconnect(con)
```

## Functions

| Function | Description |
|---|---|
| `sl_init_db(path)` | Connect to (or create) a ShinyLabel SQLite database |
| `sl_add_image(...)` | Add an image record |
| `sl_get_images(con)` | Get all images with annotation counts |
| `sl_add_class(con, name, color)` | Add a label class |
| `sl_get_classes(con)` | Get all classes |
| `sl_save_annotations(con, id, boxes, annotator)` | Save bounding boxes for an image |
| `sl_load_annotations(con, image_id)` | Load bounding boxes as a data.frame |
| `sl_get_stats(con)` | Project-wide annotation statistics |
| `sl_export_yolo(con, output_dir)` | Export to YOLO Ultralytics format (.zip) |
| `sl_export_coco(con, output_dir)` | Export to COCO JSON format |
| `px_to_yolo_norm(...)` | Convert pixel coords to YOLO normalised format |

## Related

- **[ShinyLabel](https://github.com/Lalitgis/ShinyLabelR)** — the visual annotation Shiny app
- **[yoloR](https://github.com/Lalitgis/yoloR)** — train YOLO models from R

## License

MIT
