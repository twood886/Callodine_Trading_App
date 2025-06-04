# app/main.R

cat(">>>> APP/MAIN.R is now running (with conditional stopApp) <<<<\n")

library(SMAManager)   # ensure positionsModuleUI & plotWeightUI are available
box::use(
  shiny[fluidPage, NS, observe, renderUI, tagList, uiOutput],
  shiny[parseQueryString, actionButton, tags, stopApp, shinyApp],
  shiny.semantic[grid, grid_template, semanticPage],
  shinyjs[useShinyjs],
  waiter[use_waiter, waiter_hide],
)


box::use(
  app/logic/portfolios/bamsf[load_bamsf],
  app/logic/portfolios/bemap[load_bemap],
  app/logic/portfolios/ccmf[load_ccmf],
  app/logic/portfolios/fmap[load_fmap],
  app/logic/utils[loading_screen, waiter_on_load],
  app/view/plot_delta_weights_module_v2[plotWeightUI, plotWeightServer],
  app/view/avail_trade_module[positionsModuleUI, positionsModuleServer]
)

# ────────────────────────────────────────────────────────────────────────────────
# 1) TOP‐LEVEL UI: placeholder that switches between main vs plot views
# ────────────────────────────────────────────────────────────────────────────────
ui <- function(request) {
  semanticPage(
    use_waiter(),
    waiter_on_load(loading_screen()),
    useShinyjs(),
    tags$head(
      tags$script(HTML("
        Shiny.addCustomMessageHandler('open-plot-window', function(message) {
          if (window.electronAPI && window.electronAPI.openPlotWindow) {
            window.electronAPI.openPlotWindow();
          }
        });
      "))
    ),
    uiOutput("page_ui")
  )
}

# ────────────────────────────────────────────────────────────────────────────────
# 2) SERVER: render either positions+button (main) or plotWeight (plot), and register stopApp correctly
# ────────────────────────────────────────────────────────────────────────────────
server <- function(input, output, session) {
  # “main‐or‐plot” is determined reactively. Wrap in observe so `url_search` is accessed
  observe({
    qs0 <- parseQueryString(session$clientData$url_search)
    isPlot <- !is.null(qs0$view) && identical(qs0$view, "plotWeight")
    if (!isPlot) {
      # We are in the *main* window (no ?view=plotWeight), so when it closes, stopApp()
      session$onSessionEnded(function() {
        stopApp()
      })
    } else {
      # If we're in the plot window, do NOT register stopApp()
      # (so closing plot window does NOT kill the main Shiny process)
    }
  })

  # RenderUI: choose which UI to show based on ?view
  output$page_ui <- renderUI({
    qs <- parseQueryString(session$clientData$url_search)
    if (!is.null(qs$view) && identical(qs$view, "plotWeight")) {
      # ───── PLOT‐ONLY VIEW ─────
      ns <- session$ns
      fluidPage(
        plotWeightUI(ns("plot"))
      )
    } else {
      # ───── MAIN VIEW ─────
      ns <- session$ns
      fluidPage(
        tags$div(
          style = "margin: 12px 0;",
          actionButton(ns("openPlotBtn"), "Open Delta Weights in New Window")
        ),
        positionsModuleUI(ns("posMod"))
      )
    }
  })

  # Wire up module servers and the button listener
  observe({
    qs <- parseQueryString(session$clientData$url_search)
    if (!is.null(qs$view) && identical(qs$view, "plotWeight")) {
      # Plot window: run plotWeight server only
      plotWeightServer("plot")
      waiter_hide()
    } else {
      # Main window: run positions server and listen for openPlot button
      load_ccmf()
      load_bamsf()
      load_bemap()
      load_fmap()
      positionsModuleServer("posMod")
      waiter_hide()
      observeEvent(input$openPlotBtn, {
        session$sendCustomMessage(type = "open-plot-window", message = list())
      })
    }
  })
}

# ────────────────────────────────────────────────────────────────────────────────
# 3) Launch the Shiny app
# ────────────────────────────────────────────────────────────────────────────────
shinyApp(ui = ui, server = server)
