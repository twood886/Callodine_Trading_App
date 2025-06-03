box::use(
  shiny[actionButton, h2, h4, icon, moduleServer, NS],
  shiny[observeEvent, reactiveVal, req, tagList, tags],
  shiny.semantic[create_modal, dropdown_input, modal, remove_all_modals],
  SMAManager[proposed_to_trade, registries],
  waiter[spin_3, spin_loaders, transparent, waiter_hide, waiter_show],
)

box::use(
  app/logic/get_portfolio_ids[derived_portfolio_ids],
  app/logic/trade_trigger[tradeTrigger],
  app/view/proposed_trade_module[proposedTableServer, proposedTableUI],
)

#' @export
rebalModalUI <- function(id) {
  ns <- NS(id)
  actionButton(
    ns("launch"),
    "Rebalance",
    icon("sync"),
    class = "ui basic button new-trade-btn"
  )
}

#' @export
rebalModalServer <- function(id) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns

    trades_rv <- reactiveVal(NULL)
    proposedTableServer("rebalprop", trades_rv)

    observeEvent(input$launch, {
      create_modal(modal(
        id = ns("rebal_modal"),
        header = h2("Reblance Derived Portfolio"),
        content = tagList(
          tags$div(
            style = "margin-bottom: 2rem;",  # Adjust spacing here
            dropdown_input(
              input_id = ns("portfolio"),
              choices = derived_portfolio_ids(),
              default_text = "Select a Derived Portfolio",
              type = "selection fluid"
            )
          ),
          actionButton(
            ns("create"),
            "Create Rebalance Trades",
            icon = shiny::icon("pen-to-square"),
            class = "ui blue button"
          ),
          tags$hr(),
          proposedTableUI(ns("rebalprop")),
        ),
        footer = tagList(
          actionButton(ns("submit"), "Submit", class = "ui primary button"),
          tags$div(class = "ui cancel button", "Cancel")
        )
      ))
    })

    observeEvent(input$create, {
      req(input$portfolio, nzchar(input$portfolio))
      waiter_show(
        html = tagList(
          spin_loaders(id = 3, color = "#002D57"),
          h4("Calculating Rebalance Trades...", style = "color: #002D57;")
        ),
        color = transparent(0.7)
      )
      on.exit(waiter_hide())
      portfolio <- get(input$portfolio, envir = registries$portfolios)
      proposed_trades_all <- portfolio$calc_proposed_rebalance_trade()
      t <- proposed_trades_all$trade
      swap <- proposed_trades_all$swap
      unfilled <- proposed_trades_all$unfilled
      proposed_trades <- data.frame(
        "Portfolio" = rep(input$portfolio, length(t)),
        "Security" = names(t),
        "Trade Quantity" = t,
        "Swap" = swap[names(t)],
        "Unfilled" = unfilled[names(t)],
        row.names = NULL,
        check.names = FALSE
      )
      proposed_trades <- proposed_trades[order(-abs(proposed_trades$`Trade Quantity`)),]
      message("Proposed trades: ", nrow(proposed_trades))
      trades_rv(proposed_trades)
    })

    observeEvent(input$submit, {
      final <- trades_rv()
      req(final, nrow(final) > 0)
      proposed_to_trade(final)
      tradeTrigger(tradeTrigger() + 1)
      trades_rv(NULL)
      remove_all_modals()
    })
  })
}