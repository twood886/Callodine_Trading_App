library(SMAManager)

# in your main app.R or top‑level module file
box::use(
  shiny.semantic[grid, grid_template, semanticPage],
  shiny[div, h4, HTML, fluidPage, icon, moduleServer, NS, span],
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
  app/view/modal_trade_module[tradeModalServer, tradeModalUI],
  app/view/plot_delta_weights_module_v2[plotWeightServer, plotWeightUI,],
  app/view/plot_lollipop_weights_module[lollipopWeightServer, lollipopWeightUI],
  app/view/trades_table_module[tradesModuleServer, tradesModuleUI],
)

#' @export
ui <- function(id) {
  ns <- NS(id)
  semanticPage(
    use_waiter(),
    waiter_on_load(loading_screen()),
    useShinyjs(),
    positionsModuleUI(ns("posMod"))

  #   shiny::tags$script(shiny::HTML("
  #    Shiny.addCustomMessageHandler('init_modal', function(id) {
  #    $('#' + id).modal('setting', 'observeChanges', true).modal('show');
  #    });
  #   ")),

  #   shiny::tags$style(HTML("
  #    .new-trade-btn {
  #      background-color: #002D57 !important;
  #      color: white !important;
  #      border: none !important;
  #    }
  #    .new-trade-btn:hover {
  #      background-color: #004B7F !important;
  #      color: white !important;
  #    },
  #    .new-trade-btn:focus,
  #    .new-trade-btn:active {
  #      background-color: #004B7F !important;
  #      color: white !important;
  #    }
  #   ")),

  #   title = "Callodine Trading Tool",

  #   shiny::tags$div(
  #    style = "background-color: #002D57; padding: 0px 0px; display: flex; align-items: center;",
  #    shiny::tags$img(
  #      src = "static/Callodine_Capital_vWhite.png",
  #      height = "75px",
  #      style = "margin-right: 12px;"
  #    )
  #   ),
  #   # Second line with lighter divider and button
  #   shiny::tags$div(
  #    style = "background-color: #002D57; padding-bottom: 2px; padding-left: 2px; padding-right:2px; display: flex; align-items: center;",
  #    tradeModalUI(ns("trade_modal")),
  #    shiny::tags$div(style = "width: 2px; height: 32px; background-color: #004B7F; margin: 0 12px;"),
  #    rebalModalUI(ns("rebal_modal")),
  #   ),

  #   div(style = "margin-top: 20px", grid(
  #    grid_template(
  #      default = list(
  #        areas = rbind(c("plot", "table"),c("plot_lollipop")),
  #        cols_widths  = c("70%", "30%"),
  #        rows_heights = c("50%", "50%")
  #      )
  #    ),
  #    plot = div(
  #      class = "ui segment raised",
  #      h4(class = "ui header", icon("chart-bar"), span("Delta Weights")),
  #      plotWeightUI(ns("delta_weights_plot"))
  #    ),
  #    table = div(
  #      class = "ui segment raised",
  #      h4(class = "ui header", icon("table"), span("All Trades Summary")),
  #      tradesModuleUI(ns("trades_tbl"))
  #    ),
  #    plot_lollipop = div(
  #      class = "ui segment raised",
  #      h4(class = "ui header", icon("chart-bar"), span("Lollipop Weights")),
  #      lollipopWeightUI(ns("lollipop_weights_plot"))
  #    )
  #   ))
   )
}

#' @export
server <- function(id) {
  moduleServer(id, function(input, output, session) {
    session$onSessionEnded(function() {shiny::stopApp()})
    load_ccmf()
    load_bemap()
    load_fmap()
    load_bamsf()
    #plotWeightServer("delta_weights_plot")
    waiter_hide()
    #tradeModalServer("trade_modal")
    #rebalModalServer("rebal_modal")
    #tradesModuleServer("trades_tbl")
    #lollipopWeightServer("lollipop_weights_plot")
    positionsModuleServer("posMod")
  })
}