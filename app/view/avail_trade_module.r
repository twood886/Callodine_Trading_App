box::use(
  DT[formatPercentage, formatRound],
  htmltools[HTML, tagList],
  magrittr[`%>%`],
  Rblpapi[bdp, defaultConnection, blpConnect],
  shiny.semantic[semantic_DT],
  shiny[actionButton, h4, eventReactive, icon, isolate, moduleServer, NS],
  shiny[renderUI, req, tags, textInput, uiOutput],
  SMAManager[.security, update_bloomberg_fields, update_security_data],
  waiter[spin_loaders, transparent, waiter_hide, waiter_show],
)

box::use(
  app/logic/get_portfolio_ids[portfolios],
)


#' @export
positionsModuleUI <- function(id) {
  ns <- NS(id)
  tagList(
    tags$h3("Maximum Buy/Sell Positions"),
    tags$div(
      style = "display: flex; align-items: center; max-width: 400px;",
      textInput(
        inputId  = ns("bbid"),
        label    = NULL,
        value    = "Enter Bloomberg ID",
        width    = "70%"
      ),
      actionButton(
        ns("refresh"),
        "Refresh",
        icon = icon("sync"),
        class = "ui basic button",
        style = "margin-left: 1px; flex-shrink: 0;"
      )
    ),
    tags$hr(),
    uiOutput(ns("positions_table_ui")),
  )
}

#' @export
positionsModuleServer <- function(id) {
  message(paste0("--- POSITIONS MODULE: positionsModuleServer() CALLED for id '", id, "' ---"))
  moduleServer(id, function(input, output, session) {
    message("--- POSITIONS MODULE: moduleServer INNER FUNCTION EXECUTING ---")
    ns <- session$ns

    allPositionsDF <- eventReactive(input$refresh, {
      message("--- POSITIONS MODULE: eventReactive input$refresh TRIGGERED ---")
      con <- tryCatch(defaultConnection(), error = function(e) NULL)
      if (is.null(con)) con <- blpConnect()

      current_bbid <- tolower(bdp(input$bbid, "DX194")$DX194)
      req(current_bbid, message = "Bloomberg ID is required")
      message(paste(
        ">>> [positionsModule] Refresh triggered for BBID:",
        current_bbid
      ))
      .security(current_bbid)
      pf_list <- portfolios()

      if (!is.list(pf_list) || length(pf_list) == 0) {
        message(paste0(
          ">>> [positionsModule] ",
          "No portfolios found or portfolios() returned an invalid result."
        ))
        # Return an empty data frame with the correct structure
        return(
          data.frame(
            Portfolio = character(0),
            CurrentShares = numeric(0),
            Weight = numeric(0),
            MaxBuy = numeric(0),
            MaxSell = numeric(0),
            stringsAsFactors = FALSE
          )
        )
      }

      # Update Enfusion Data for each portfolio
      lapply(
        pf_list,
        function(portfolio_obj) {
          if (inherits(portfolio_obj, "Portfolio")) {
            portfolio_obj$update_enfusion()
            invisible(TRUE)
          } else {
            warning(paste0(
              ">>> [positionsModule] ",
              "Encountered non-Portfolio object in portfolios list. Skipping."
            ))
          }
        }
      )

      # Get Portfolio Names
      pname <- vapply(
        pf_list,
        function(portfolio) {
          tryCatch({
            portfolio$get_short_name()
          }, error = function(e) {
            warning(paste("Error getting short name for portfolio:", e$message))
            "Unknown Portfolio"
          })
        },
        character(1)
      )

      # Get Current Shares
      current_shares <- vapply(
        pf_list,
        function(portfolio) {
          tryCatch({
            portfolio$get_position(current_bbid)$get_qty()
          }, error = function(e) {
            warning(paste0(
              "Error getting current shares for portfolio: ", e$message
            ))
            0
          })
        },
        numeric(1)
      )

      # Get Current Weights
      current_weight <- vapply(
        pf_list,
        function(portfolio) {
          tryCatch({
            position <- portfolio$get_position(current_bbid)
            position$get_delta_pct_nav()
          }, error = function(e) {
            warning(paste0(
              "Error getting current weight for portfolio: ", e$message
            ))
            0
          })
        },
        numeric(1)
      )

      # Get Security Position Limits
      update_bloomberg_fields()
      limits <- vapply(
        pf_list,
        function(portfolio) {
          tryCatch({
            limits <- portfolio$get_security_position_limits(current_bbid, update_bbfields = FALSE)[[1]] #nolint
            if(
              is.list(limits) &&
              all(c("min", "max") %in% names(limits)) &&
              is.numeric(limits$min) && length(limits$min) == 1 &&
              is.numeric(limits$max) && length(limits$max) == 1
            ) {
              c("max" = limits$max, "min" = limits$min)
            }else {
              stop("Invalid limits structure")
            }
          }, error = function(e) {
            warning(paste0(
              "Error getting max shares for portfolio: ", e$message
            ))
            c("max" = NA_real_, "min" = NA_real_)
          })
        },
        numeric(2)
      )

      max_limit <- limits["max", ]
      min_limit <- limits["min", ]

      can_buy <- mapply(
        function(max_limit, current_shares) {
          if (!is.na(max_limit) && !is.na(current_shares)) {
            max(max_limit - current_shares, 0)
          } else {
            NA_real_
          }
        },
        max_limit = max_limit,
        current_shares = current_shares
      )

      can_sell <- mapply(
        function(min_limit, current_shares) {
          if (!is.na(min_limit) && !is.na(current_shares)) {
            min(-(current_shares - min_limit), 0)
          } else {
            NA_real_
          }
        },
        min_limit = min_limit,
        current_shares = current_shares
      )

      df <- data.frame(
        Portfolio = as.character(pname),
        CurrentShares = as.numeric(current_shares),
        Weight = as.numeric(current_weight),
        MaxBuy = as.numeric(can_buy),
        MaxSell = as.numeric(can_sell),
        stringsAsFactors = FALSE
      )

      if (nrow(df) == 0) {
        message(">>> [positionsModule] No valid data rows to bind after processing portfolios.")
        df <- data.frame(
          Portfolio = character(0),
          CurrentShares = numeric(0),
          Weight = numeric(0),
          MaxBuy = numeric(0),
          MaxSell = numeric(0),
          stringsAsFactors = FALSE
        )
      }

      message(paste0(">>> [positionsModule] allPositionsDF() processed. Rows: ", nrow(df)))
      print(df) # Uncomment for debugging the dataframe contents
      df
    })

    # Render the UI for the table
    output$positions_table_ui <- renderUI({
      message("--- POSITIONS MODULE: renderUI IS DEFINITELY EXECUTING NOW ---")
      if (input$refresh == 0) {
        message("--- POSITIONS MODULE: renderUI - input$refresh is 0 ---")
        return(tags$p("Please enter a BBID and click 'Refresh' to view positions."))
      }

      current_bbid_isolated <- trimws(isolate(input$bbid))
      if (current_bbid_isolated == "") {
        return(tags$p("BBID is empty. Please enter a BBID and click 'Refresh'."))
      }

      df_data <- allPositionsDF() # This will trigger the eventReactive

      if (is.null(df_data)) {
        # This might happen if eventReactive had an unhandled error that resulted in NULL
        message(">>> [positionsModule] df_data is NULL.")
        return(tags$p("An unexpected error occurred: Data frame is NULL."))
      }

      # Add explicit check for errors from eventReactive, though req should handle most
      if (inherits(df_data, "try-error")) {
        message(">>> [positionsModule] allPositionsDF() resulted in an error.")
        return(tags$p("An error occurred while fetching positions data."))
      }

      if (nrow(df_data) == 0) {
        message(">>> [positionsModule] df_data has 0 rows.")
        escaped_bbid <- htmltools::htmlEscape(current_bbid_isolated)
        return(tags$p(HTML(paste0(
          "No position data found for BBID: <strong>",
          escaped_bbid,
          "</strong> or no portfolios available with this instrument."
        ))))
      }

      message(paste0(">>> [positionsModule] Rendering semantic_DT with ", nrow(df_data), " rows."))
      message("test updating")
      semantic_DT(
        df_data,
        options = list(
          dom       = "t",
          paging    = FALSE,
          searching = FALSE,
          info      = FALSE
        )
      ) %>%
        formatPercentage(
          columns = "Weight",
          digits  = 2
        ) %>%
        formatRound(
          columns = c("CurrentShares", "MaxBuy", "MaxSell"),
          digits  = 0
        )
    })
  })
}