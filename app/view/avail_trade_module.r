box::use(
  DT[JS],
  htmltools[HTML],
  magrittr[`%>%`],
  shiny.semantic[semantic_DT],
  shiny[actionButton, eventReactive, icon, isolate, moduleServer, NS],
  shiny[renderUI, req, tagList, tags, textInput, uiOutput],
  SMAManager[update_security_data],
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
      #update_security_data()
      current_bbid <- trimws(input$bbid)
      req(current_bbid, message = "Bloomberg ID is required")
      message(paste(
        ">>> [positionsModule] Refresh triggered for BBID:",
        current_bbid
      ))

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
            Current = numeric(0),
            MaxBuy = numeric(0),
            MaxSell = numeric(0),
            stringsAsFactors = FALSE
          )
        )
      }

      rows <- lapply(pf_list, function(portfolio_obj) {
        portfolio_obj$update_enfusion()
        pname <- "Unknown Portfolio" # Default name
        current_shares <- NA_real_
        limits <- list(min = NA_real_, max = NA_real_)

        # Safely get portfolio name
        tryCatch({
          pname_val <- portfolio_obj$get_short_name()
          if (is.character(pname_val) && length(pname_val) == 1) {
            pname <- pname_val
          } else {
            warning(paste(
              "get_short_name for a portfolio did not return a single string. Using default name."
            ))
          }
        }, error = function(e) {
          warning(paste("Error getting short name for a portfolio:", e$message))
        })

        # Safely get current shares
        tryCatch({
          current_shares <- 0
          cs <- portfolio_obj$get_target_position(current_bbid)$get_qty()
          if (is.numeric(cs) && length(cs) == 1) {
            current_shares <- cs
          } else {
            current_shares <- 0
            warning(paste0(
              "get_position for '",
              pname,
              "' (BBID: '", current_bbid, "') returned invalid data. Value: ",
              paste(cs, collapse = ", ")
            ))
          }
        }, error = function(e) {
          current_shares <- 0
          warning(paste0(
            "Error calling get_position for '",
            pname,
            "' (BBID: '", current_bbid, "'): ",
            e$message
          ))
        })

        # Safely get security position limits
        tryCatch({
          lim <- portfolio_obj$get_security_position_limits(current_bbid)[[1]]
          if (
            is.list(lim) &&
              all(c("min", "max") %in% names(lim)) &&
              is.numeric(lim$min) && length(lim$min) == 1 &&
              is.numeric(lim$max) && length(lim$max) == 1
          ) {
            limits <- lim
          } else {
            warning(paste0(
              "get_security_position_limits for '",
              pname,
              "' (BBID: '", current_bbid, "') returned invalid data."
            ))
          }
        }, error = function(e) {
          warning(paste0(
            "Error calling get_security_position_limits for '",
            pname,
            "' (BBID: '", current_bbid, "'): ",
            e$message
          ))
        })

        max_limit <- limits$max
        min_limit <- limits$min

        # Calculations, ensuring NAs propagate correctly
        can_buy <- NA_real_
        can_sell <- NA_real_
        if (!is.na(max_limit) && !is.na(current_shares)) {
          can_buy <- max(max_limit - current_shares, 0)
        }
        if (!is.na(min_limit) && !is.na(current_shares)) {
          can_sell <- min(-(current_shares - min_limit), 0)
        }

        data.frame(
          Portfolio = pname,
          Current = current_shares,
          MaxBuy = can_buy,
          MaxSell = can_sell,
          stringsAsFactors = FALSE
        )
      })

      # Filter out any NULLs that might have resulted from invalid portfolio objects
      valid_rows <- Filter(Negate(is.null), rows)

      if (length(valid_rows) == 0) {
        message(">>> [positionsModule] No valid data rows to bind after processing portfolios.")
        df <- data.frame(
          Portfolio = character(0),
          Current = numeric(0),
          MaxBuy = numeric(0),
          MaxSell = numeric(0),
          stringsAsFactors = FALSE
        )
      } else {
        df <- do.call(rbind, valid_rows)
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

      semantic_DT(
        df_data,
        options = list(
          dom       = "t",
          paging    = FALSE,
          searching = FALSE,
          info      = FALSE,
          columnDefs = list(
            list(
              # Suppose "Portfolio" is column 0, then
              # "Current" is column 1, "MaxBuy" is 2, "MaxSell" is 3
              targets = c(1, 2, 3),
              render  = JS("
                function(data, type, row, meta) {
                  // If the cell is null/undefined, show blank
                  if (data === null || data === undefined) {
                    return '';
                  }
                  // If data is infinite, return the literal 'Inf' or '-Inf'
                  if (!isFinite(data)) {
                    return (data > 0 ? 'Inf' : '-Inf');
                  }
                  // Otherwise, format as a number with commas, 0 decimals
                  return $.fn.dataTable.render
                           .number(',', '', 0, '')
                           .display(data);
                }
              ")
            )
          )
        )
      )

    })
  })
}