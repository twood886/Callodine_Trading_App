box::use(
  shiny[HTML, moduleServer, NS, observeEvent, renderUI, req, tagList, tags, uiOutput],
  shiny.semantic[semantic_DT]
)

proposedTableUI <- function(id) {
  ns <- NS(id)
  # we’re going to build the whole table+delete script inside here
  uiOutput(ns("proposed_table"))
}

proposedTableServer <- function(id, trades_rv) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns

    output$proposed_table <- renderUI({
      df <- trades_rv()
      req(df)
      req(nrow(df) > 0)

      df$Delete <- sprintf(
        '<i class="red trash icon delete-btn" style="cursor: pointer;" data-row="%s"></i>',
        seq_len(nrow(df))
      )

      tags$div(
        style = "width: 100%;",
        tagList(
          tags$script(HTML(sprintf("
            $(document).on('click', '.delete-btn', function() {
            var row = $(this).data('row');
            Shiny.setInputValue('%s', row, {priority: 'event'});
            });
          ", ns("delete_row")))),
          semantic_DT(
            df,
            escape = FALSE,
            extensions = "Scroller",
            options  = list(
              searching = FALSE,
              pageLength = 5,
              scrollY = "300px",
              scrollCollapse = TRUE,
              scroller = TRUE,
              deferRender = TRUE,
              paging = TRUE,
              dom = 't',
              autoWidth = TRUE)
          )
        )
      )
    })

    observeEvent(input$delete_row, {
      df <- trades_rv()
      row_index <- as.integer(input$delete_row)
      if (!is.na(row_index) && row_index >= 1 && row_index <= nrow(df)) {
        trades_rv(df[-row_index, ])
      }
    }, ignoreInit = TRUE)
  })
}