box::use(
  dplyr[bind_rows],
  DT[datatable, dataTableProxy, DTOutput, renderDataTable, renderDT, replaceData],
  shiny, #nolint
  shiny.semantic[create_modal, dropdown_input, modal, multiple_radio, remove_all_modals, semantic_DT, semantic_DTOutput],
  shinyFeedback[showToast],
  shinyjs[runjs],
  SMAManager[create_proposed_trade_qty, create_proposed_trade_tgt_weight, proposed_to_trade, registries],
  waiter[spin_loaders, use_waiter, waiter_hide, waiter_show, spin_3]
)

#' @export
tradeModalUI <- function(id) { #nolint
  ns <- shiny::NS(id)
  shiny::actionButton(
    ns("launch"),
    "New Trade",
    shiny::icon("plus"),
    class = "ui basic button new-trade-btn"
  )
}

#' @export
tradeModalServer <- function(id) { #nolint
  shiny::moduleServer(id, function(input, output, session) {
    ns <- session$ns
    trades_rv <- shiny::reactiveVal(
      data.frame(
        "Portfolio" = character(),
        "Security" = character(),
        "Trade Quantity" = numeric(),
        "Swap" = logical(),
        "Unfilled" = numeric(),
        stringsAsFactors = FALSE,
        check.names = FALSE
      )
    )

    shiny::observeEvent(input$launch, {
      create_modal(modal(
        id = ns("trade_modal"),
        header  = shiny::h2("Create a New Trade"),
        content = shiny::tagList(
          shiny::tags$div(
            style = "margin-bottom: 2rem;",  # Adjust spacing here
            dropdown_input(
              input_id = ns("portfolio"),
              choices = ls(registries$portfolios),
              default_text = "Select a Portfolio",
              type = "selection fluid"
            )
          ),
          shiny::tags$div(
            style = "display: flex; align-items: center; gap: 1rem; margin-bottom: 1rem;",
            shiny::tags$label("Choose order type:", style = "min-width: 140px; margin-bottom: 0;"),
            shiny::tags$div(
              style = "display: flex; gap: 1.5rem;",  # Spacing between radio buttons
              multiple_radio(
                ns("mode"),
                label = NULL,
                choices = c("Quantity", "Target Percent"),
                selected = "Quantity",
                inline = TRUE
              )
            )
          ),
          shiny::tags$div(
            style = "display: flex; align-items: center; gap: 1rem; margin-bottom: 1rem;",
            shiny::tags$label("Choose if Swap:", style = "min-width: 140px; margin-bottom: 0;"),
            shiny::tags$div(
              style = "display: flex; gap: 1.5rem;",  # Spacing between radio buttons
              multiple_radio(
                ns("swap"),
                label = NULL,
                choices = c(FALSE, TRUE),
                selected = FALSE,
                inline = TRUE
              )
            )
          ),
          shiny::uiOutput(ns("fields")),
          shiny::actionButton(
            ns("create"),
            "Create Trade",
            icon = shiny::icon("pen-to-square"),
            class = "ui blue button",
          ),
          shiny::tags$hr(),
          shiny::uiOutput(ns("proposed_table"))
        ),
        footer = shiny::tagList(
          shiny::actionButton(ns("submit"), "Submit", class = "ui primary button"),
          shiny::tags$div(class = "ui cancel button", "Cancel")
        )
      ))
    })

    output$fields <- shiny::renderUI({
      shiny::req(input$mode)
      shiny::tags$div(
        class = "ui form",
        shiny::tags$div(
          class = "two fields",
          shiny::tags$div(class = "field", shiny::textInput(ns("security"), "Security ID")),
          shiny::tags$div(class = "field",
            if (input$mode == "Quantity") {
              shiny::numericInput(ns("quantity"), "Quantity", value = 1, step = 1)
            } else{
              shiny::numericInput(ns("percent"), "Target Percent", value = 0, step = 0.1)
            }
          )
        )
      )
    })

    shiny::observeEvent(input$create, {
      shiny::req(input$portfolio, nzchar(input$portfolio))
      shiny::req(nzchar(input$security))

      if (input$mode == "Quantity") {
        shiny::req(input$quantity)
      } else {
        shiny::req(input$percent)
      }

      waiter_show(
        html = shiny::tagList(
          spin_loaders(id = 3, color = "#003366"),
          shiny::h4("Calculating trade...", style = "color: black;")
        ),
        color = waiter::transparent(0.7) # Semi-transparent background
      )
      on.exit(waiter_hide())

      new_row <- if (input$mode == "Quantity") {
        create_proposed_trade_qty(
          portfolio_id = input$portfolio,
          security_id = input$security,
          trade_qty = input$quantity,
          swap = FALSE,
          flow_to_derived = TRUE
        )
      } else {
        create_proposed_trade_tgt_weight(
          portfolio_id = input$portfolio,
          security_id = input$security,
          tgt_weight = input$percent/100,
          swap = FALSE,
          flow_to_derived = TRUE
        )
      }
      trades_rv(new_row)
    })

    output$proposed_table <- shiny::renderUI({
      df <- trades_rv()
      shiny::req(nrow(df) > 0)

      df$Delete <- sprintf(
        '<i class="red trash icon delete-btn" style="cursor: pointer;" data-row="%s"></i>',
        seq_len(nrow(df))
      )

      shiny::tags$div(
        style = "width: 100%;",
        shiny::tagList(
          shiny::tags$script(shiny::HTML(sprintf("
            $(document).on('click', '.delete-btn', function() {
            var row = $(this).data('row');
            Shiny.setInputValue('%s', row, {priority: 'event'});
            });
          ", ns("delete_row")))),
          semantic_DT(
            df,
            escape = FALSE,
            options  = list(
              scrolly = "200px",
              scrollCollapse = TRUE,
              paging = FALSE,
              dom = 't', 
              autoWidth = TRUE)
          )
        )
      )
    })

    shiny::observeEvent(input$delete_row, {
      df <- trades_rv()
      row_index <- as.integer(input$delete_row)
      if (!is.na(row_index) && row_index >= 1 && row_index <= nrow(df)) {
        trades_rv(df[-row_index, ])
      }
    }, ignoreInit = TRUE)

    shiny::observeEvent(input$submit, {
      final <- trades_rv()
      shiny::req(nrow(final) > 0)
      proposed_to_trade(final)
      remove_all_modals()
      showToast(
        type = "success",
        message = paste(nrow(final), "trades added")
      )
      trades_rv(
        data.frame(
          portfolio_id = character(),
          security_id = character(),
          trade_qty = numeric(),
          swap = logical(),
          stringsAsFactors = FALSE
        )
      )
    })
  })
}