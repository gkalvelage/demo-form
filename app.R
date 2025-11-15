library(shiny)
library(shinyjs)
library(httr)

ui <- fluidPage(
  useShinyjs(),
  tags$head(
    tags$style(HTML("
      @import url('https://fonts.googleapis.com/css2?family=Roboto:wght@300;400&display=swap');
      body {
        font-family: 'Roboto', sans-serif;
        background-color: #f8f9fa;
      }
      .main-title {
        text-align: center;
        margin-top: 20px;
        margin-bottom: 30px;
      }
      .card-custom {
        max-width: 600px;
        margin: 0 auto;
        padding: 20px;
        border-radius: 10px;
        box-shadow: 0 4px 8px rgba(0, 0, 0, 0.1);
        background: white;
      }
      .shiny-input-radio {
        text-align: left;
        margin-bottom: 15px;
      }
      #submit {
        background-color: #007bff;
        color: white;
        border: none;
        padding: 10px 20px;
        border-radius: 5px;
        font-weight: bold;
        margin-bottom: 20px;
      }
      #submit:hover:enabled {
        background-color: #0056b3;
      }
      #submit:disabled {
        background-color: #6c757d;
        cursor: not-allowed;
      }
      .footer-text {
        text-align: center;
        margin-top: 20px;
        font-size: 12px;
        color: #6c757d;
      }
      .error-overlay {
        position: fixed;
        top: 0;
        left: 0;
        width: 100%;
        height: 100%;
        background-color: rgba(0, 0, 0, 0.8);
        display: flex;
        align-items: center;
        justify-content: center;
        z-index: 9999;
      }
      .error-box {
        background: white;
        padding: 40px;
        border-radius: 10px;
        text-align: center;
        max-width: 500px;
      }
      .error-box h2 {
        color: #dc3545;
        margin-bottom: 20px;
      }
    ")),
    tags$script(HTML("
      Shiny.addCustomMessageHandler('redirect', function(url) {
        window.location.href = url;
      });
    "))
  ),
  uiOutput("main_content")
)

server <- function(input, output, session) {
  query <- reactive({
    parseQueryString(session$clientData$url_search)
  })
  
  output$main_content <- renderUI({
    if (is.null(query()$id) || query()$id == "") {
      div(class = "error-overlay",
          div(class = "error-box",
              h2("Fehler beim Scannen"),
              p(style = "font-size: 18px;", "Bitte QR-Code erneut scannen.")
          )
      )
    } else {
      tagList(
        div(class = "main-title",
            img(src = "https://meldeeimer.de/wp-content/uploads/2024/05/meldeeimer-150x150.png", 
                style = "max-width: 100%; height: auto;"),
            h2("Was möchten Sie melden?")
        ),
        div(class = "card-custom",
            div(style = "text-align: left;",
                radioButtons(
                  "answer",
                  NULL,
                  choiceNames = list(
                    HTML("Der Container ist <strong>voll</strong>."),
                    HTML("Hier wurde <strong>wilder Müll</strong> abgestellt."),
                    HTML("Das Umfeld ist <strong>stark verschmutzt</strong>."),
                    HTML("Der Container wurde <strong>stark beschädigt</strong>."),
                    HTML("Ein <strong>sonstiges</strong> Problem.")
                  ),
                  choiceValues = list(
                    "Der Container ist voll.",
                    "Hier wurde wilder Müll abgestellt.",
                    "Das Umfeld ist stark verschmutzt.",
                    "Der Container wurde stark beschädigt.",
                    "Ein sonstiges Problem."
                  ),
                  selected = character(0)
                )
            ),
            div(style = "text-align: center; margin-top: 20px;",
                actionButton("submit", "Absenden", class = "btn btn-primary")
            ),
            div(class = "footer-text",
                p("Ihre Angaben werden anonymisiert gespeichert."),
                p(
                  tags$a("Impressum", href = "https://meldeeimer.de/impressum/"),
                  " | ",
                  tags$a("Datenschutz", href = "https://meldeeimer.de/datenschutz/")
                )
            )
        )
      )
    }
  })
  
  observe({
    req(!is.null(query()$id), query()$id != "")
    if (is.null(input$answer) || input$answer == "") {
      disable("submit")
    } else {
      enable("submit")
    }
  })
  
  observeEvent(input$submit, {
    url <- paste0("https://", Sys.getenv("SUPABASE_PROJECT_ID"), 
                  ".supabase.co/rest/v1/form_submissions")
    
    POST(
      url,
      body = list(source_id = query()$id, answer = input$answer),
      encode = "json",
      add_headers(
        apikey = Sys.getenv("SUPABASE_ANON_KEY"),
        Authorization = paste0("Bearer ", Sys.getenv("SUPABASE_ANON_KEY")),
        `Content-Type` = "application/json"
      )
    )
    
    session$sendCustomMessage("redirect", "https://meldeeimer.de/danke-demo/")
  })
}

shinyApp(ui = ui, server = server)