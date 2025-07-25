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
  app/view/avail_trade_module[positionsModuleServer, positionsModuleUI],
  app/view/modal_rebal_module[rebalModalServer, rebalModalUI],
  app/view/plot_delta_weights_module_v2[plotWeightServer, plotWeightUI],
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
        Shiny.addCustomMessageHandler('open-rebal-window', function(message) {
          if (window.electronAPI && window.electronAPI.openRebalWindow) {
            window.electronAPI.openRebalWindow();
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
    isChild <- !is.null(qs0$view) && qs0$view %in% c("plotWeight", "rebal")
    if (!isChild) {
      session$onSessionEnded(function() {
        stopApp()
      })
    }
  })

  # RenderUI: choose which UI to show based on ?view
  output$page_ui <- renderUI({
    qs <- parseQueryString(session$clientData$url_search)
    if (!is.null(qs$view) && identical(qs$view, "plotWeight")) {
      # ───── PLOT‐ONLY VIEW ─────
      ns <- session$ns
      fluidPage(plotWeightUI(ns("plot")))
    } else if (!is.null(qs$view) && identical(qs$view, "rebal")) {
      # ───── REBALANCE MODAL VIEW ─────
      ns <- session$ns
      fluidPage(rebalModalUI(ns("rebal")))
    } else {
      # ───── MAIN VIEW ─────
      ns <- session$ns
      fluidPage(
        tags$div(
          style = "margin: 12px 0;",
          actionButton(ns("openPlotBtn"), "Open Delta Weights in New Window"),
          actionButton(ns("openRebalBtn"), "Open Rebalance Window")
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
    } else if (!is.null(qs$view) && identical(qs$view, "rebal")) {
      rebalModalServer("rebal")
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
      observeEvent(input$openRebalBtn, {
        session$sendCustomMessage(type = "open-rebal-window", message = list())
      })
    }
  })
}

# ────────────────────────────────────────────────────────────────────────────────
# 3) Launch the Shiny app
# ────────────────────────────────────────────────────────────────────────────────
shinyApp(ui = ui, server = server)
