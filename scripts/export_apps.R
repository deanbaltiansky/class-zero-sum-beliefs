# scripts/export_apps.R
# Export Shiny apps and (re)render index.(R)md/md to docs/index.html

# install.packages(c("fs","shinylive","rmarkdown"))  # run once if needed
suppressPackageStartupMessages({
  library(fs)
  library(shinylive)
  library(rmarkdown)
})

# ---- Helpers ----
is_shiny_app_dir <- function(p) {
  file_exists(path(p, "app.R")) ||
    (file_exists(path(p, "ui.R")) && file_exists(path(p, "server.R")))
}

# Find apps under each study:
# - Legacy main app:        study-*/app
# - Sub-apps under app/:    study-*/app/<X>.R          -> treated as app "<X>"
#                           study-*/app/<X>/app.R      -> treated as app "<X>"
# - Sibling apps (flat):    study-*/<X>/app.R          -> treated as app "<X>"
find_study_apps <- function() {
  studies <- dir_ls(".", type = "directory", regexp = "study-[^/]+$")
  out <- list()
  for (s in studies) {
    s_name <- path_file(s)
    
    # 1) study-*/app (main)
    main_app <- path(s, "app")
    if (dir_exists(main_app) && is_shiny_app_dir(main_app)) {
      out[[length(out) + 1]] <- list(study = s_name, appname = "app", dir = main_app)
    }
    
    # 2) study-*/app/<X>.R  OR  study-*/app/<X>/app.R
    app_children <- if (dir_exists(main_app)) dir_ls(main_app, type = "any") else character(0)
    for (child in app_children) {
      if (is_dir(child) && is_shiny_app_dir(child)) {
        out[[length(out) + 1]] <- list(study = s_name, appname = path_file(child), dir = child)
      } else if (is_file(child) && grepl("\\.R$", child)) {
        # wrap single file app into a temp dir for export
        appname <- path_ext_remove(path_file(child))
        tmp <- dir_create(file_temp(pattern = paste0("appwrap-", appname, "-")))
        file_copy(child, path(tmp, "app.R"))
        out[[length(out) + 1]] <- list(study = s_name, appname = appname, dir = tmp, cleanup = tmp)
      }
    }
    
    # 3) study-*/<X>/app.R (siblings)
    sibs <- dir_ls(s, type = "directory", recurse = FALSE)
    sibs <- sibs[basename(sibs) != "app"]
    for (sib in sibs) {
      if (is_shiny_app_dir(sib)) {
        out[[length(out) + 1]] <- list(study = s_name, appname = path_file(sib), dir = sib)
      }
    }
  }
  out
}

export_one_app <- function(app) {
  dest <- fs::path("docs", "studies", app$study, app$appname)
  fs::dir_create(dest, recurse = TRUE)
  message(sprintf("• Exporting %s/%s -> %s", app$study, app$appname, dest))
  
  # version-agnostic call
  export_formals <- names(formals(shinylive::export))
  args <- list(destdir = dest)
  if ("appdir" %in% export_formals) args$appdir <- app$dir else if ("app_dir" %in% export_formals) args$app_dir <- app$dir
  if ("overwrite" %in% export_formals) args$overwrite <- TRUE
  if ("quiet" %in% export_formals) args$quiet <- TRUE
  if ("verbose" %in% export_formals) args$verbose <- FALSE
  do.call(shinylive::export, args)
  
  # --- sanity checks: data files must be present for shinylive to fetch ---
  required <- fs::path(dest, "data", c("df_czs_elg.csv", "var_info.csv"))
  missing <- required[!fs::file_exists(required)]
  if (length(missing)) {
    warning("After export, missing expected files:\n  - ",
            paste(fs::path_file(missing), collapse = "\n  - "),
            "\nMake sure these exist in study-1/app/data/ before exporting.")
  }
  
  if (!is.null(app$cleanup)) fs::dir_delete(app$cleanup)
}



render_index <- function() {
  input <- if (file_exists("index.Rmd")) {
    "index.Rmd"
  } else if (file_exists("index.md")) {
    "index.md"
  } else if (file_exists(path("docs","index.Rmd"))) {
    path("docs","index.Rmd")
  } else if (file_exists(path("docs","index.md"))) {
    path("docs","index.md")
  } else {
    message("No index.Rmd or index.md found; skipping landing page render.")
    return(invisible(NULL))
  }
  
  dir_create("docs")
  message(sprintf("• Rendering %s -> docs/index.html", input))
  rmarkdown::render(
    input        = input,
    output_file  = "index.html",
    output_dir   = "docs",
    envir        = new.env(parent = globalenv()),
    output_options = list(self_contained = TRUE)
  )
}

# ---- Run ----
apps <- find_study_apps()
if (length(apps)) {
  invisible(lapply(apps, export_one_app))
} else {
  message("No Shiny apps found under study-*/ … skipping shinylive export.")
}
render_index()
message("Done.")
