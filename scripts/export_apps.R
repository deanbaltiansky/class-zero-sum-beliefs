# scripts/export_apps.R
# Minimal Pages builder for this repo:
# - Render docs/index.(R)md -> docs/index.html (self-contained)
# - Keep Jekyll out of the way

suppressPackageStartupMessages({
  library(fs)
})

abort <- function(msg) stop(paste0("❌ ", msg), call. = FALSE)
info  <- function(msg) message(paste0("ℹ️  ", msg))
ok    <- function(msg) message(paste0("✅ ", msg))

# --- 0) Preconditions ---
dir_create("docs", recurse = TRUE)
file.create(path("docs", ".nojekyll"))  # Keep GH Pages from processing with Jekyll

input_candidates <- c(path("docs", "index.Rmd"), path("docs", "index.md"))
input <- input_candidates[file_exists(input_candidates)][1]
if (is.na(input)) abort("docs/index.Rmd or docs/index.md not found.")

if (!requireNamespace("rmarkdown", quietly = TRUE)) {
  abort("Package 'rmarkdown' is not installed. install.packages('rmarkdown') and re-run.")
}

# --- 1) Render to self-contained HTML in docs/ ---
# - knit_root_dir/docs ensures relative resource paths (e.g., ./assets/...) resolve
# - pandoc_args adds --self-contained even for plain .md
# - intermediates_dir keeps temp files out of the repo root
out_file <- path("docs", "index.html")
info(paste0("Rendering ", input, " -> ", out_file))

rmarkdown::render(
  input             = input,
  output_file       = "index.html",
  output_dir        = "docs",
  knit_root_dir     = "docs",
  intermediates_dir = "docs/.build",
  envir             = new.env(parent = emptyenv()),
  quiet             = TRUE,
  output_format     = rmarkdown::html_document(self_contained = TRUE)
)


if (!fs::file_exists(out_file)) abort("Render reported success, but docs/index.html was not created.")
ok("Rendered docs/index.html")

# --- 2) Sanity checks helpful during setup ---
if (!dir_exists("docs/assets")) {
  info("No docs/assets/ directory found. If you reference ./assets/... create it under docs/.")
} else {
  ok("Found docs/assets/ directory.")
}

ok("Done. Push to main and GitHub Pages will serve docs/index.html.")
