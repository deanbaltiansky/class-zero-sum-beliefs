# study-1/app/app.R
library(shiny)

`%||%` <- function(x, y) if (is.null(x)) y else x

# -----------------------------
# User-configured variable list
# -----------------------------
numeric_vars_user <- c(
  # Class Zero-Sum: edit as needed; tolerant matching will bridge small diffs
  "zs_class","SDO","zsm","lnktfate","soli","support",
  "ideo_con","ideo_lib","ideo_demsoc","ideo_lbrtn","ideo_prog",
  "man","white","age","income_num","edu_num"
)

# -----------------------------
# Data + var info loaders
# -----------------------------
load_data <- function() {
  # Canonical filename for this project
  p <- "data/df_czs_elg.csv"
  if (!file.exists(p)) {
    stop("Missing data/df_czs_elg.csv in app/data/ . Place the file at study-1/app/data/")
  }
  read.csv(p, check.names = FALSE, stringsAsFactors = FALSE)
}

load_var_info <- function() {
  p <- "data/var_info.csv"
  if (!file.exists(p)) {
    return(data.frame(var = character(), label = character(), description = character(),
                      stringsAsFactors = FALSE))
  }
  vi <- read.csv(p, check.names = FALSE, stringsAsFactors = FALSE)
  names(vi) <- tolower(names(vi))
  if (!"var" %in% names(vi)) vi$var <- character(0)
  if (!"label" %in% names(vi)) vi$label <- vi$var
  if (!"description" %in% names(vi)) vi$description <- ""
  vi$var <- trimws(vi$var)
  vi$label <- ifelse(nzchar(trimws(vi$label)), trimws(vi$label), vi$var)
  vi$description <- trimws(vi$description)
  unique(vi[c("var","label","description")])
}

# -----------------------------
# UI
# -----------------------------
ui <- tagList(
  tags$head(
    tags$title("Correlations App"),
    tags$script(HTML('window.addEventListener("load", function(){ document.title = "Correlations App"; });'))
  ),
  fluidPage(
    title = "Correlations App",
    titlePanel("Class Zero-Sum Beliefs — Correlations"),
    sidebarLayout(
      sidebarPanel(
        helpText("Pick two continuous variables to explore their linear relationship."),
        selectInput("xvar", "X axis", choices = NULL),
        selectInput("yvar", "Y axis", choices = NULL),
        tags$hr(),
        tags$details(
          tags$summary("Debug: detected columns"),
          verbatimTextOutput("debug_cols", placeholder = TRUE)
        )
      ),
      mainPanel(
        plotOutput("scatter", height = 420),
        tags$hr(),
        verbatimTextOutput("stats"),
        tags$hr(),
        tags$h4("Variable descriptions"),
        uiOutput("xdesc"),
        uiOutput("ydesc")
      )
    )
  )
)

# -----------------------------
# Server
# -----------------------------
server <- function(input, output, session) {
  df <- load_data()
  var_info <- load_var_info()
  
  # --- tolerant name normalization (case/space/underscore) ---
  norm_name <- function(x) tolower(gsub("\\s+", "_", trimws(x)))
  
  # Map: normalized df name -> original df name (so we always use real columns)
  df_colmap <- setNames(names(df), nm = norm_name(names(df)))
  
  # Normalize var_info too, for resilient label/description lookup
  if (nrow(var_info)) {
    var_info$var_norm <- norm_name(var_info$var)
  }
  
  # Resolve requested names to actual df columns using normalized map
  resolve_cols <- function(requested) {
    req_norm <- norm_name(requested)
    hits <- df_colmap[intersect(req_norm, names(df_colmap))]
    unname(hits)
  }
  
  # Helpers for labels/descriptions (try exact, then normalized)
  get_label <- function(v) {
    if (is.null(v) || !nzchar(v) || !nrow(var_info)) return(v %||% "")
    hit <- var_info$label[var_info$var == v]
    if (length(hit) == 1 && nzchar(hit)) return(hit)
    if ("var_norm" %in% names(var_info)) {
      hit2 <- var_info$label[var_info$var_norm == norm_name(v)]
      if (length(hit2) == 1 && nzchar(hit2)) return(hit2)
    }
    v
  }
  
  get_desc <- function(v) {
    if (is.null(v) || !nzchar(v) || !nrow(var_info)) return("No description found.")
    hit <- var_info$description[var_info$var == v]
    if (length(hit) == 1 && nzchar(hit)) return(hit)
    if ("var_norm" %in% names(var_info)) {
      hit2 <- var_info$description[var_info$var_norm == norm_name(v)]
      if (length(hit2) == 1 && nzchar(hit2)) return(hit2)
    }
    "No description found."
  }
  
  # -----------------------------
  # Build available choices
  # -----------------------------
  # 1) Try curated list first
  resolved <- resolve_cols(numeric_vars_user)
  
  # 2) Fallback: auto-detect numeric-ish columns so the app is usable immediately
  if (length(resolved) < 2) {
    is_numish <- vapply(df, function(x) {
      if (is.numeric(x)) return(TRUE)
      if (is.character(x)) {
        sup <- suppressWarnings(as.numeric(x))
        return(any(!is.na(sup)))
      }
      FALSE
    }, logical(1))
    resolved <- names(df)[is_numish]
  }
  
  # 3) Keep order stable and unique
  available <- unique(intersect(resolved, names(df)))
  
  validate(
    need(length(available) >= 2,
         paste0(
           "Not enough variables to run.\n\n",
           "CSV columns detected (first 30):\n  - ",
           paste(utils::head(names(df), 30), collapse = "\n  - "), "\n\n",
           "Your requested list (first 30):\n  - ",
           paste(utils::head(numeric_vars_user, 30), collapse = "\n  - "), "\n\n",
           "Tip: check for small name differences (case/underscores/spaces)."
         ))
  )
  
  # Names shown = labels; values = actual df column names
  choice_labels <- vapply(available, get_label, character(1))
  # If any labels are "", fall back to the column name
  choice_labels[!nzchar(choice_labels)] <- available[!nzchar(choice_labels)]
  choices <- setNames(object = available, nm = choice_labels)
  
  updateSelectInput(session, "xvar", choices = choices, selected = available[1])
  updateSelectInput(session, "yvar", choices = choices, selected = available[2])
  
  # -----------------------------
  # Reactive data for selected pair
  # -----------------------------
  pair_data <- reactive({
    req(input$xvar, input$yvar)
    vars <- c(input$xvar, input$yvar)
    vars <- intersect(vars, names(df))
    validate(need(length(vars) == 2, "Pick two valid variables."))
    
    d <- df[, vars, drop = FALSE]
    d[] <- lapply(d, function(x) if (is.character(x)) suppressWarnings(as.numeric(x)) else x)
    d <- stats::na.omit(d)
    validate(
      need(nrow(d) >= 3, "Not enough non-missing pairs to plot."),
      need(stats::sd(d[[1]]) > 0, "X has no variance."),
      need(stats::sd(d[[2]]) > 0, "Y has no variance.")
    )
    d
  })
  
  # -----------------------------
  # Outputs
  # -----------------------------
  output$scatter <- renderPlot({
    d <- pair_data()
    plot(
      d[[1]], d[[2]], pch = 19,
      xlab = get_label(input$xvar),
      ylab = get_label(input$yvar)
    )
    fit <- lm(d[[2]] ~ d[[1]])
    abline(fit, lwd = 2)
  })
  
  output$stats <- renderText({
    d <- pair_data()
    ct <- cor.test(d[[1]], d[[2]], method = "pearson")
    sprintf("Pearson r = %.3f   (p = %.3g,  n = %d)",
            unname(ct$estimate), ct$p.value, nrow(d))
  })
  
  output$xdesc <- renderUI({
    req(input$xvar)
    tags$p(tags$strong("X: "), tags$em(get_desc(input$xvar)))
  })
  
  output$ydesc <- renderUI({
    req(input$yvar)
    tags$p(tags$strong("Y: "), tags$em(get_desc(input$yvar)))
  })
  
  output$debug_cols <- renderText({
    paste0(
      "df columns (first 50):\n  - ",
      paste(utils::head(names(df), 50), collapse = "\n  - "),
      "\n\nMatched available choices:\n  - ",
      paste(available, collapse = "\n  - ")
    )
  })
}

shinyApp(ui, server)
