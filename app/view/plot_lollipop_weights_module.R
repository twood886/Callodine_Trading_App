box::use(
  dplyr[all_of, full_join, mutate],
  plotly[add_markers, add_segments, config, layout, plot_ly, plotlyOutput, renderPlotly],
  purrr[reduce],
  scales[percent],
  shiny[moduleServer, NS, req, tagList],
  shiny.semantic[selectInput],
  tidyr[all_of, pivot_longer, replace_na],
)

box::use(
  app/logic/get_portfolio_ids[base_portfolio_ids],
  app/logic/trade_trigger[tradeTrigger],
)

#' @export
lollipopWeightUI <- function(id, portfolio_choices = NULL) {
  ns <- NS(id)
  tagList(
    selectInput(
      ns("portfolio_name"),
      label = "Portfolio Name:",
      choices = c("ccmf")
    ),
    plotlyOutput(
      outputId = ns("delta_weights_plot")
    )
  )
}

#' @export
lollipopWeightServer <- function(id) {
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
          weight      = x,
          stringsAsFactors = FALSE,
          row.names = NULL
        )
      }

      base <- SMAManager::.portfolio(portfolio_name, create = FALSE)
      base_name <- base$get_short_name()
      smas <- SMAManager::get_tracking_smas(base)
      if (is.null(smas)) stop("No tracking SMAs found.")
      sma_names <- unname(vapply(smas, function(x) x$get_short_name(), character(1)))
      weights_df_wide <- reduce(
        lapply(c(list(base), smas), function(obj) to_df(get_delta_weights(obj))),
        full_join,
        by = "security_id"
      )
      names(weights_df_wide) <- c("security", base_name, sma_names)

      weights_df <- weights_df_wide |>
        pivot_longer(
          cols      = all_of(sma_names),
          names_to  = "sma",
          values_to = "weight"
        ) |>
        mutate(
          sma     = factor(sma, levels = sma_names),
          weight  = replace_na(weight, 0),
          base_wt = .data[[base_name]],
          diff    = weight - base_wt
        )

      plot_ly(
        data = weights_df,
        x = ~ base_wt,
        color = ~ sma,
        hoverinfo = "text",
        text = ~ paste0(
          "Security: ", security,
          "<br>", base_name, ": ", percent(base_wt, .01),
          "<br>", sma, ": ", percent(weight, .01),
          "<br>∆ from base: ", percent(diff, .01)
        )
      ) |>
        add_segments(
          x = ~ base_wt,
          xend = ~ base_wt,
          y =  0,
          yend  = ~ diff,
          color = ~ sma,
          split = ~ sma,
          legendgroup = ~ sma,
          showlegend = FALSE,
          inherit = FALSE
        ) |>
        add_markers(
          y     = ~ diff,
          color = ~ sma,
          split = ~ sma,
          legendgroup = ~ sma,
          marker = list(size = 8),
          name   = ~ sma
        ) |>
        layout(
          xaxis = list(
            title      = paste(base_name, "Delta Weights"),
            tickformat = ".0%"
          ),
          yaxis = list(
            title      = paste("Deviation from", base_name),
            tickformat = ".0%"
          ),
          legend = list(
            title = list(text = paste(base_name, "Tracking SMAs"))
          )
        ) |>
        config(displayModeBar = FALSE)
    }

    output$delta_weights_plot <- renderPlotly({
      req(input$portfolio_name)
      tradeTrigger()
      compute_plot(input$portfolio_name)
    })
  })
}