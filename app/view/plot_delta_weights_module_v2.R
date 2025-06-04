box::use(
  dplyr[all_of, full_join, mutate],
  htmltools[HTML, tagList],
  magrittr[`%>%`],
  plotly[add_lines, add_markers, add_segments, config, layout],
  plotly[plot_ly, plotlyOutput, renderPlotly],
  purrr[reduce],
  scales[percent],
  shiny[div, h4, moduleServer, NS, observeEvent, reactiveVal, renderUI],
  shiny[req, selectInput, tags, uiOutput],
  stats[setNames],
  tidyr[all_of, pivot_longer, replace_na],
)

box::use(
  app/logic/trade_trigger[tradeTrigger],
)

#' @export
plotWeightUI <- function(id) {
  ns <- NS(id)
  tagList(
    div(style = "display: flex; align-items: flex-end; gap: 1.5rem;",
      div(
        style = "flex: 1; display: flex; flex-direction: column;",
        h4("Select Base Portfolio:"),
        selectInput(
          inputId = ns("portfolio_name"),
          label   = NULL,
          choices = c("ccmf"),
          selected = "ccmf"
        )
      ),
      uiOutput(ns("view_buttons"))
    ),
    plotlyOutput(
      outputId = ns("delta_weights_plot")
    )
  )
}

#' @export
plotWeightServer <- function(id) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns

    view_type <- reactiveVal("scatter")
    output$view_buttons <- renderUI({
      sel <- view_type()
      tags$div(class = "ui small buttons",
        tags$button(
          id    = ns("btn_scatter"),
          type  = "button",
          class = paste("ui button", if (sel == "scatter") "active" else ""),
          "Scatter"
        ),
        tags$div(class = "or"),
        tags$button(
          id    = ns("btn_lollipop"),
          type  = "button",
          class = paste("ui button", if (sel == "lollipop") "active" else ""),
          "Lollipop"
        )
      )
    })

    observeEvent(input$btn_scatter, view_type("scatter"))
    observeEvent(input$btn_lollipop, view_type("lollipop"))

    # Compute weights_df once
    get_weights_df <- function(portfolio_name) {
      get_delta_weights <- function(portfolio) {
        sapply(
          portfolio$get_target_position(),
          function(pos) setNames(pos$get_delta_pct_nav(), pos$get_id())
        )
      }
      to_df <- function(x) {
        data.frame(
          security_id = names(x),
          weight      = x,
          stringsAsFactors = FALSE,
          row.names   = NULL
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

      weights_df_wide %>%
        pivot_longer(
          cols = all_of(sma_names),
          names_to  = "sma",
          values_to = "weight"
        ) %>%
        mutate(
          sma = factor(sma, levels = sma_names),
          weight  = replace_na(weight, 0),
          base_wt = .data[[base_name]],
          diff = weight - base_wt
        )
    }


    plot_scatter <- function(df, base_name) {
      plot_ly(
        data = df,
        x = ~ .data[[base_name]],
        y = ~ weight,
        color = ~ sma,
        type = "scatter",
        hoverinfo = "text",
        text = ~ paste(
          "Security:", security,
          "<br>", base_name, ":", percent(.data[[base_name]], .01),
          "<br>", sma, ":", percent(weight, .01)
        )
      ) %>%
        add_lines(
          x = ~ .data[[base_name]],
          y = ~ .data[[base_name]],
          mode = "lines",
          line = list(color = "black", width = 1),
          inherit = FALSE,
          showlegend = FALSE,
          hoverinfo = "none"
        ) %>%
        layout(
          xaxis = list(
            title = paste(base_name, "Delta Weights"), tickformat = ".0%"
          ),
          yaxis = list(
            title = "SMA Delta Weights", tickformat = ".0%",
            scaleanchor = "x",
            scaleratio = 1
          ),
          legend = list(title = list(text = paste(base_name, "Tracking SMAs")))
        )
    }

    plot_lollipop <- function(df, base_name) {
      plot_ly(
        data = df,
        x = ~ base_wt,
        hoverinfo = "text",
        text = ~ paste0(
          "Security: ", security,
          "<br>", base_name, ": ", percent(base_wt, .01),
          "<br>", sma, ": ", percent(weight, .01),
          "<br>∆ from base: ", percent(diff, .01)
        )
      ) %>%
        add_segments(
          x = ~ base_wt,
          xend = ~ base_wt,
          y =  0,
          yend = ~ diff,
          color = ~ sma,
          split = ~ sma,
          legendgroup = ~ sma,
          showlegend = FALSE,
          inherit = FALSE,
          line = list(width = 0.5, dash = "solid")
        ) %>%
        add_markers(
          y = ~ diff,
          color = ~ sma,
          split = ~ sma,
          legendgroup = ~ sma,
          marker = list(size = 6),
          name = ~ sma
        ) %>%
        layout(
          xaxis = list(
            title = paste(base_name, "Delta Weights"),
            tickformat = ".0%"
          ),
          yaxis = list(
            title = paste("Deviation from", base_name),
            tickformat = ".0%"
          ),
          legend = list(
            title = list(text = paste(base_name, "Tracking SMAs"))
          )
        )
    }

    # Render based on view selection
    output$delta_weights_plot <- renderPlotly({
      req(input$portfolio_name)
      tradeTrigger()
      df <- get_weights_df(input$portfolio_name)
      base_name <- names(df)[2]

      if (view_type() == "scatter") {
        plt <- plot_scatter(df, base_name)
      } else {
        plt <- plot_lollipop(df, base_name)
      }
      plt %>% config(displayModeBar = FALSE)
    })
  })
}
