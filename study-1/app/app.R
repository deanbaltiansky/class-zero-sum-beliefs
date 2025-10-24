# study-1/app/app.R
library(shiny)

# ---- Data sources (shinylive = local files only) ----
numeric_vars_user <- c(
  # << put your numeric/continuous choices here for Class Zero-Sum >>
  # Example placeholders:
  "zs_class","SDO","zsm","lnktfate","soli","support",
  "ideo_con","ideo_lib","ideo_demsoc","ideo_lbrtn","ideo_prog",
  "man","white","age","income_num","edu_num"
)

load_data <- function() {
  p <- "data/df_czs_elg.csv"  # <— rename if you prefer; must be bundled with the app
  if (!file.exists(p)) stop("Missing data/df_czs_elg.csv in app/data/")
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
        selectInput("yvar", "Y axis", choices = NULL)
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

server <- function(input, output, session) {
  df <- load_data()
  var_info <- load_var_info()
  
  get_label <- function(v) {
    if (is.null(v) || !nzchar(v)) return("")
    hit <- var_info$label[var_info$var == v]
    if (length(hit) == 1 && nzchar(hit)) hit else v
  }
  get_desc <- function(v) {
    if (is.null(v) || !nzchar(v)) return("")
    hit <- var_info$description[var_info$var == v]
    if (length(hit) == 1 && nzchar(hit)) hit else "No description found."
  }
  
  available <- intersect(numeric_vars_user, names(df))
  if (length(available) < 2) {
    stop("Fewer than 2 valid numeric columns found. Check df_czs_elg.csv column names.")
  }
  
  choice_labels <- vapply(available, get_label, character(1))
  choices <- setNames(object = available, nm = choice_labels)
  
  updateSelectInput(session, "xvar", choices = choices, selected = available[1])
  updateSelectInput(session, "yvar", choices = choices, selected = available[2])
  
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
}

shinyApp(ui, server)
