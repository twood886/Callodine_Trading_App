box::use(
  dplyr[all_of, full_join, mutate],
  plotly[add_lines, layout, plot_ly, plotlyOutput, renderPlotly],
  purrr[reduce],
  scales[percent],
  shiny[moduleServer, NS, req, selectInput, tagList],
  tidyr[all_of, pivot_longer, replace_na],
)
box::use(app/logic/trade_trigger[tradeTrigger])

#' @export
plotWeightUI <- function(id, portfolio_choices = NULL) {
  ns <- NS(id)
  tagList(
    selectInput(
      ns("portfolio_name"),
      label = "Portfolio Name:",
      choices = c("ccmf")
    ),
    plotlyOutput(
      ns("delta_weights_plot")
    )
  )
}

#' @export
plotWeightServer <- function(id) {
  moduleServer(id, function(input, output, session) {

    compute_plot <- function(portfolio_name) {

      get_delta_weights <- function(portfolio) {
        sapply(
          portfolio$get_target_position(),
          function(pos) {
            d_w <- pos$get_delta_pct_nav()
            names(d_w) <- pos$get_id()
            d_w
          }
        )
      }

      to_df <- function(x) {
        data.frame(
          security_id = names(x),
          weight = x,
          stringsAsFactors = FALSE,
          row.names = NULL
        )
      }

      base <- SMAManager::.portfolio(portfolio_name, create = FALSE)
      base_name <- base$get_short_name()
      smas <- SMAManager::get_tracking_smas(base)
      if (is.null(smas)) stop("No tracking SMAs found.")

      sma_names <- unname(vapply(smas, \(x) x$get_short_name(), character(1)))
      weights_df_wide <- reduce(
        lapply(smas, \(x) to_df(get_delta_weights(x))),
        \(x, y) full_join(x, y, by = "security_id"),
        .init = to_df(get_delta_weights(base))
      )
      names(weights_df_wide) <- c("security", base_name, sma_names)
      weights_df <- weights_df_wide |>
        pivot_longer(
          cols = all_of(sma_names),
          names_to = "sma",
          values_to = "weight"
        ) |>
        mutate(
          sma = factor(sma, levels = sma_names),
          weight = replace_na(weight, 0)
        )

      plot_ly(
        data = weights_df,
        x = ~ .data[[base_name]],
        y = ~ weight,
        color = ~ sma,
        type = "scatter",
        hoverinfo = "text",
        text = ~ paste(
          "Security: ", security,
          "<br>", base_name, ": ",
          percent(.data[[base_name]], accuracy = 0.01),
          "<br>", sma, ": ",
          percent(weight, accuracy = 0.01)
        )
      ) |>
        add_lines(
          x = ~ .data[[base_name]],
          y = ~ .data[[base_name]],
          mode = "lines",
          line = list(color = "black", width = 1),
          inherit = FALSE,
          showlegend = FALSE,
          hoverinfo = "none"
        ) |>
        layout(
          xaxis = list(
            title  = paste(base_name, "Delta Weights"),
            tickformat = ".0%"
          ),
          yaxis = list(
            title       = "SMA Delta Weights",
            tickformat  = ".0%",
            scaleanchor = "x",
            scaleratio  = 1
          ),
          legend = list(
            title = list(text = paste(base_name, "Tracking SMAs"))
          )
        )
    }

    output$delta_weights_plot <- renderPlotly({
      req(input$portfolio_name)
      tradeTrigger()
      compute_plot(input$portfolio_name)
    })
  })
}
