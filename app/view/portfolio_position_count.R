box::use(
  shiny[div, h4, uiOutput, moduleServer, NS, renderText],
)

#' @export
ui <- function(id) {
  ns <- NS(id)
  div(class = "ui raised segment",
    h4("Position count:"),
    uiOutput(ns("count"))
  )
}

#' @export
server <- function(id, portfolio_name) {
  moduleServer(id, function(input, output, session) {
    output$count <- renderText({
      portfolio <- SMAManager::.portfolio(portfolio_name, create = FALSE)
      length(portfolio$get_position())
    })
  })
}