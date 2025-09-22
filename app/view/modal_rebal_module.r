box::use(
  DT[formatPercentage, formatRound],
  magrittr[`%>%`],
  shiny.semantic[semantic_DT],
  shiny[div, eventReactive, h4, moduleServer],
  shiny[NS, renderUI, req, selectInput, tagList, tags, uiOutput],
  SMAManager[.portfolio],
  waiter[spin_loaders, transparent, waiter_hide, waiter_show],
)

box::use(
  app/logic/get_portfolio_ids[derived_portfolio_ids],
)

#' @export
rebalModalUI <- function(id) {
  ns <- NS(id)
  tagList(
    div(style = "display: flex; align-items: flex-end; gap: 1.5rem;",
      div(
        style = "flex: 1; display: flex; flex-direction: column;",
        h4("Select SMA Portfolio:"),
        selectInput(
          inputId = ns("sma_name"),
          label   = NULL,
          choices = derived_portfolio_ids(),
          selected = NULL
        )
      )
    ),
    tags$hr(),
    uiOutput(ns("rebal_table"))
  )
}

#' @export
rebalModalServer <- function(id) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns

    rebal_df <- eventReactive(
      input$sma_name, {

        waiter_show(
          html = tagList(
            spin_loaders(id = 3, color = "#002D57"),
            h4("Optimizing SMA Given Rules...", style = "color: #002D57;")
          ),
          color = transparent(0.7)
        )
        on.exit(waiter_hide())


        req(input$sma_name)
        portfolio <- .portfolio(input$sma_name)
        base <- portfolio$get_base_portfolio()
        base$update_enfusion()
        portfolio$update_enfusion()
        semantic_DT(
          portfolio$rebalance(),
          options = list(
            dom = "t",
            paging = FALSE,
            searching = FALSE,
            info = FALSE,
            columnDefs = list(
              list(targets = 0, title = "Ticker"),
              list(targets = 1, title = "Tgt Weight"),
              list(targets = 2, title = "Optim Weight"),
              list(targets = 3, title = "Optim Shares"),
              list(targets = 4, title = "Current Shares"),
              list(targets = 5, title = "Trade Amount")
            )
          )
        ) %>%
          formatPercentage(
            columns = c("target_weights", "final_weights"),
            digits = 2
          ) %>%
          formatRound(
            columns = c("final_shares", "current_shares", "trade"),
            digits = 0
          )
      },
      ignoreInit = TRUE
    )

    output$rebal_table <- renderUI({
      req(rebal_df())
      rebal_df()
    })
  })
}