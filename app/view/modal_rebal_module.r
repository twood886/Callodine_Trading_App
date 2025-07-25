box::use(
  shiny.semantic[semantic_DT],
  shiny[div, eventReactive, h4, moduleServer],
  shiny[NS, renderUI, req, selectInput, tagList, uiOutput],
  SMAManager[.portfolio],
  tibble[rownames_to_column],
)

box::use(
  app/logic/get_portfolio_ids[derived_portfolio_ids]
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
          selected = "bamsf"
        )
      )
    ),
    uiOutput(ns("rebal_table"))
  )
}

#' @export
rebalModalServer <- function(id) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns

    rebal_df <- eventReactive(input$sma_name, {
      message("Rebalancing for SMA: ", input$sma_name)
      portfolio <- .portfolio(input$sma_name)
      df <- portfolio$calc_proposed_rebalance_trade()
      df <- rownames_to_column(df, var = "Security")
      semantic_DT(
        df,
        options = list(
          dom = "t",
          paging = FALSE
        )
      )
    })

    output$rebal_table <- renderUI({
      req(rebal_df())
      rebal_df()
    })
  })
}