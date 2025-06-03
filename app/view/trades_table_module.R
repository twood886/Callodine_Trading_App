# app/view/trades_module.R
box::use(
  shiny[NS, moduleServer, reactive],
  dplyr[bind_rows, cur_data, group_by, mutate, select, summarize, tibble],
  jsonlite[toJSON],
  DT[renderDataTable, JS],
  shiny.semantic[semantic_DT, semantic_DTOutput],
  SMAManager[registries],
  app/logic/trade_trigger[tradeTrigger]
)

#' @export
tradesModuleUI <- function(id) {
  ns <- NS(id)
  semantic_DTOutput(ns("trades_tbl"))
}

#' @export
tradesModuleServer <- function(id) {
  moduleServer(id, function(input, output, session) {
    # reactive that rebuilds whenever you add/remove from registries$trades
    trades_df <- reactive({
      tradeTrigger()
      # pull every trade object and call its to_df()
      lst <- mget(ls(envir = registries$trades), envir = registries$trades)
      if (length(lst) == 0) {
        return(data.frame(
          security_id = character(),
          swap = character(),
          portfolio_short_name = character(),
          shares = numeric(),
          allocation_pct = numeric(),
          stringsAsFactors = FALSE,
          check.names = FALSE
        ))
      }
      binded <- bind_rows(lapply(lst, function(x) x$to_df()))
      binded$allocation_pct <- binded$allocation_pct * 100
      binded
    })

    # parent + JSON details
    nested_df <- reactive({
      df <- trades_df()

      if (nrow(df) == 0 || !all(c("security_id", "swap") %in% names(df))) {
        return(tibble(
          security_id = character(),
          swap = character(),
          total_shares = numeric(),
          shares = numeric(),
          details_json = character()
        ))
      }
      df |>
        group_by(security_id, swap) |>
        summarize(
          total_shares = sum(shares),
          details_json = toJSON(
            select(cur_data(), portfolio_short_name, shares, allocation_pct),
            dataframe = "rows"
          ),
          .groups = "drop"
        )
    })

    output$trades_tbl <- renderDataTable({
      df <- nested_df()
      semantic_DT(
        df,
        callback = JS("
          // add toggle icons in col-0
          table.rows().every(function(i, tab, row) {
            var cell = $(this.node()).find('td').first();
            cell.html('<span class=\"details-toggle\">▶</span>');
          });
          // on-click show/hide child rows
          table.on('click', 'td.details-control', function() {
            var tr  = $(this).closest('tr');
            var row = table.row(tr);
            if (row.child.isShown()) {
              row.child.hide();
              tr.removeClass('shown');
              $(this).find('span').html('▶');
            } else {
              var jsonCol = row.data()[ row.data().length - 1 ];
              var details = JSON.parse(jsonCol);
              var tbl = '<table class=\"ui compact table\"><thead>'
                        +'<tr><th>Portfolio</th><th>Shares</th><th>% Alloc</th></tr>'
                        +'</thead><tbody>';
              details.forEach(function(r){
                tbl += '<tr><td>'+r.portfolio_short_name+'</td>'
                     +  '<td>'+r.shares+'</td>'
                     +  '<td>'+r.allocation_pct+'</td></tr>';
              });
              tbl += '</tbody></table>';
              row.child(tbl).show();
              tr.addClass('shown');
              $(this).find('span').html('▼');
            }
          });
        "),
        options = list(
          paging = FALSE,
          scrollY = '400px',
          scrollCollapse = TRUE,
          columnDefs = list(
            list(targets = 0, orderable = FALSE, className = 'details-control'),
            list(targets = ncol(df) - 1, visible = FALSE)
          ),
          order = list(list(1, 'asc'))
        )
      )
    })
  })
}