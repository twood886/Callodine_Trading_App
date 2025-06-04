# app/main.R

# ────────────────────────────────────────────────────────────────────────────────
# A minimal Shiny app that:
#  • Shows one button on the main window
#  • When that button is clicked, sends "electron-open-plot" to Electron
#  • Never calls tagList(), never calls any other modules that might call tagList()
# ────────────────────────────────────────────────────────────────────────────────

library(shiny)
library(shinyjs)
library(waiter)

# ────────────────────────────────────────────────────────────────────────────────
# 1) TOP‐LEVEL UI
# ────────────────────────────────────────────────────────────────────────────────
# We use fluidPage() (not tagList), and tags$head() for the small JS snippet.
ui <- function(request) {
  fluidPage(
    # Inject a tiny JavaScript handler into <head>
    tags$head(
      tags$script(HTML("
        Shiny.addCustomMessageHandler('electron-open-plot', function(message) {
          if (window.electronAPI && window.electronAPI.openPlotWindow) {
            window.electronAPI.openPlotWindow();
          }
        });
      "))
    ),
    # A single button which will trigger the "open-plot-window" IPC
    wellPanel(
      use_waiter(),                     # show a loading spinner until we call waiter_hide()
      waiter_on_load(html = NULL),      # you can customize this if you like
      useShinyjs(),
      br(),
      actionButton("openPlotBtn", "Open Delta Weights in New Window")
    ),
    # (You can add more UI elements here in the future,
    #  but keep them free of tagList() until you verify this minimal app works.)
    br(),
    h4("Main window loaded. Click the button above to open a second window.")
  )
}

# ────────────────────────────────────────────────────────────────────────────────
# 2) TOP‐LEVEL SERVER
# ────────────────────────────────────────────────────────────────────────────────
server <- function(input, output, session) {
  # If the user closes this window, stop the entire Shiny app:
  session$onSessionEnded(function() {
    stopApp()
  })

  # When the button is clicked, send a custom message that Electron's preload.js will pick up
  observeEvent(input$openPlotBtn, {
    session$sendCustomMessage(type = "electron-open-plot", message = list())
  })

  # Once the UI is fully rendered, hide the waiter spinner:
  waiter_hide()
}

# ────────────────────────────────────────────────────────────────────────────────
# 3) LAUNCH THE SHINY APP
# ────────────────────────────────────────────────────────────────────────────────
shinyApp(ui = ui, server = server)