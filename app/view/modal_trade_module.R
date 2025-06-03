box::use(
  shiny.semantic[actionButton, dropdown_input, multiple_radio, numericInput, textInput],
  shiny.semantic[create_modal, modal,  remove_all_modals],
  shiny[h2, h4, icon, moduleServer, NS, observeEvent, reactiveVal, renderUI],
  shiny[req, tagList, tags, uiOutput],
  SMAManager[create_proposed_trade_qty, create_proposed_trade_tgt_weight, proposed_to_trade],
  SMAManager[registries],
  waiter[spin_loaders, transparent, waiter_hide, waiter_show],
)

box::use(
  app/logic/trade_trigger[tradeTrigger],
  app/view/proposed_trade_module[proposedTableServer, proposedTableUI],
)

#' @export
tradeModalUI <- function(id) {
  ns <- NS(id)
  actionButton(
    inputId = ns("launch"),
    label   = "New Trade",
    icon    = icon("plus"),
    class   = "ui basic button new-trade-btn"
  )
}

#' @export
tradeModalServer <- function(id) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns

    trades_rv <- reactiveVal(NULL)
    proposedTableServer("prop", trades_rv)

    observeEvent(input$launch, {
      create_modal(
        modal(
          id    = ns("trade_modal"),
          class = "trade-modal",
          header = h2("Create a New Trade"),

          content = tagList(
            tags$div(
              class = "content-bucket",
              dropdown_input(
                input_id     = ns("portfolio"),
                choices      = ls(registries$portfolios),
                default_text = "Select a Portfolio",
                type         = "selection fluid"
              )
            ),

            # Order Type radio
            tags$div(
              class = "content-bucket",
              tags$label("Choose order type:", style = "min-width: 140px; margin-bottom: 0;"),
              multiple_radio(
                input_id  = ns("mode"),
                label     = NULL,
                choices   = c("Quantity", "Target Percent"),
                selected  = "Quantity",
                inline    = TRUE
              )
            ),

            # Swap radio
            tags$div(
              class = "content-bucket",
              tags$label("Choose if Swap:", style = "min-width: 140px; margin-bottom: 0;"),
              multiple_radio(
                input_id  = ns("swap"),
                label     = NULL,
                choices   = c(FALSE, TRUE),
                selected  = FALSE,
                inline    = TRUE
              )
            ),

            # Dynamic fields based on mode
            uiOutput(ns("fields")),

            # Create Trade button
            actionButton(
              inputId = ns("create"),
              label   = "Create Trade",
              icon    = icon("pen-to-square"),
              class   = "ui blue button"
            ),

            tags$hr(),

            # Proposed trades table
            tags$div(
              class = "content-bucket",
              proposedTableUI(ns("prop"))
            )
          ),

          # Footer actions
          footer = tagList(
            actionButton(ns("submit"), "Submit", class = "ui primary button"),
            tags$div(class = "ui cancel button", "Cancel")
          ),
          options = list(
            className = "trade-modal",
            closable  = TRUE
          )
        )
      )
    })

    # Render dynamic security/quantity or percent fields
    output$fields <- renderUI({
      req(input$mode)
      tags$div(
        class = "ui form",
        tags$div(
          class = "two fields",
          tags$div(
            class = "field",
            textInput(ns("security"), "Security ID")
          ),
          tags$div(
            class = "field",
            if (input$mode == "Quantity") {
              numericInput(ns("quantity"), "Quantity", value = 1, step = 1)
            } else {
              numericInput(ns("percent"), "Target Percent", value = 0, step = 0.1)
            }
          )
        )
      )
    })

    # Generate proposed trade
    observeEvent(input$create, {
      req(input$portfolio, nzchar(input$portfolio))
      req(nzchar(input$security))
      if (input$mode == "Quantity") req(input$quantity) else req(input$percent)

      waiter_show(
        html = tagList(
          spin_loaders(id = 3, color = "#002D57"),
          h4("Calculating trade...", style = "color: #002D57;")
        ),
        color = transparent(0.7)
      )
      on.exit(waiter_hide())

      new_row <- if (input$mode == "Quantity") {
        create_proposed_trade_qty(
          portfolio_id    = input$portfolio,
          security_id     = input$security,
          trade_qty       = input$quantity,
          swap            = FALSE,
          flow_to_derived = TRUE
        )
      } else {
        create_proposed_trade_tgt_weight(
          portfolio_id    = input$portfolio,
          security_id     = input$security,
          tgt_weight      = input$percent / 100,
          swap            = FALSE,
          flow_to_derived = TRUE
        )
      }
      trades_rv(new_row)
    })

    # Submit the final trade
    observeEvent(input$submit, {
      final <- trades_rv()
      req(nrow(final) > 0)
      proposed_to_trade(final)
      tradeTrigger(tradeTrigger() + 1)
      trades_rv(NULL)
      remove_all_modals()
    })
  })
}
