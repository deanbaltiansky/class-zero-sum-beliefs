# scripts/export_apps.R
# Export Shiny apps and (re)render docs/index.(R)md -> docs/index.html
# - Legacy main app:        study-*/app                      -> docs/studies/<study>/app
# - Sibling apps (flat):    study-*/X/app.R                  -> docs/studies/<study>/X
# - Sibling apps (nested):  study-*/X/app/app.R              -> docs/studies/<study>/X
# - Sub-apps under app/:    study-*/app/Y[/app].R            -> docs/studies/<study>/Y

suppressPackageStartupMessages({
  library(fs)
  library(shinylive)
})

is_shiny_app_dir <- function(p) {
  file_exists(path(p, "app.R")) ||
    (file_exists(path(p, "ui.R")) && file_exists(path(p, "server.R")))
}

export_one <- function(app_dir, out_dir) {
  message("🚀 Exporting ", app_dir, "  -->  ", out_dir)
  if (dir_exists(out_dir)) dir_delete(out_dir)
  dir_create(out_dir, recurse = TRUE)
  # NOTE: Match the BSC script API: shinylive uses `appdir` (no underscore)
  shinylive::export(appdir = app_dir, destdir = out_dir)
}

# ---- Find and export all apps ----
study_dirs <- dir_ls(".", type = "directory", glob = "study-*", recurse = FALSE)

if (length(study_dirs) == 0) {
  message("ℹ️  No study-* directories found. Nothing to export.")
} else {
  dir_create("docs", recurse = TRUE)
  file.create(path("docs", ".nojekyll"))  # required for GitHub Pages + ShinyLive
  
  exported <- character(0)
  
  for (study_dir in study_dirs) {
    study_name <- path_file(study_dir)
    
    # 1) Legacy main app at study-*/app
    legacy_app <- path(study_dir, "app")
    if (dir_exists(legacy_app) && is_shiny_app_dir(legacy_app)) {
      out <- path("docs", "studies", study_name, "app")
      export_one(legacy_app, out)
      exported <- c(exported, paste0(study_name, "/app"))
    }
    
    # 2) Sibling apps: study-*/X and study-*/X/app
    if (dir_exists(study_dir)) {
      siblings <- dir_ls(study_dir, type = "directory", recurse = FALSE)
      for (sib in siblings) {
        app_name <- path_file(sib)
        if (identical(app_name, "app")) next  # handled above
        
        if (is_shiny_app_dir(sib)) {
          out <- path("docs", "studies", study_name, app_name)
          export_one(sib, out)
          exported <- c(exported, paste0(study_name, "/", app_name))
          next
        }
        nested_sib <- path(sib, "app")
        if (dir_exists(nested_sib) && is_shiny_app_dir(nested_sib)) {
          out <- path("docs", "studies", study_name, app_name)
          export_one(nested_sib, out)
          exported <- c(exported, paste0(study_name, "/", app_name))
          next
        }
      }
    }
    
    # 3) Sub-apps inside legacy app/: study-*/app/Y and study-*/app/Y/app
    if (dir_exists(legacy_app)) {
      subapps <- dir_ls(legacy_app, type = "directory", recurse = FALSE)
      for (sub in subapps) {
        sub_name <- path_file(sub)
        if (is_shiny_app_dir(sub)) {
          out <- path("docs", "studies", study_name, sub_name)
          export_one(sub, out)
          exported <- c(exported, paste0(study_name, "/app/", sub_name))
          next
        }
        nested_sub <- path(sub, "app")
        if (dir_exists(nested_sub) && is_shiny_app_dir(nested_sub)) {
          out <- path("docs", "studies", study_name, sub_name)
          export_one(nested_sub, out)
          exported <- c(exported, paste0(study_name, "/app/", sub_name))
          next
        }
      }
    }
  }
  
  if (length(exported)) {
    message("✅ Exported apps:\n  - ", paste(exported, collapse = "\n  - "))
  } else {
    message("ℹ️  Found no Shiny apps to export under study-*/")
  }
  
  message("🧩 Export complete. Rendering homepage…")
  
  # ---- Render index -> docs/index.html ----
  # Prefer docs/index.Rmd or docs/index.md; if missing, accept root index.Rmd/index.md.
  input_candidates <- c(
    path("docs", "index.Rmd"),
    path("docs", "index.md"),
    "index.Rmd",
    "index.md"
  )
  md_path <- input_candidates[file_exists(input_candidates)][1]
  
  if (!is.na(md_path) && nzchar(md_path)) {
    if (!requireNamespace("rmarkdown", quietly = TRUE)) {
      stop("❌ rmarkdown is not installed. Install it with install.packages('rmarkdown') and re-run.")
    }
    out_file <- path("docs", "index.html")
    tryCatch(
      {
        rmarkdown::render(
          input       = md_path,
          # IMPORTANT: no explicit output_format → respect YAML (toc, code_folding, etc.)
          output_file = "index.html",
          output_dir  = "docs",
          envir       = new.env(parent = emptyenv()),
          quiet       = TRUE,
          clean       = TRUE
        )
        if (!file_exists(out_file)) {
          stop("Render reported success but docs/index.html was not created.")
        }
        message("✅ Rendered ", md_path, " -> docs/index.html")
      },
      error = function(e) {
        stop(paste0("❌ Failed to render ", md_path, ": ", conditionMessage(e)))
      }
    )
  } else {
    message("ℹ️  No index.(R)md found in docs/ or repo root; skipping homepage render.")
  }
  
  message("✅ Done. Apps are under docs/studies/<study>/<app_folder>, homepage at docs/index.html")
}
