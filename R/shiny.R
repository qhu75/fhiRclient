
#' Shiny Demo
#'
#' @importFrom dplyr %>%
#' @import shiny
#' @export
ShinyDemo <- function(){
    ui <- fluidPage(
        titlePanel("fhiRclient Demo"),
        sidebarLayout(
            sidebarPanel(
                selectInput(inputId = "server",
                            label = "Choose a sandbox server:",
                            choices = c("smarthealthit", "HAPI")),
                textInput("patientid", "Patient ID",
                          value = "0e61c3ad-d11e-4080-a6aa-cac89cae4e37")
            ),
            mainPanel(
                verbatimTextOutput("res"),
                plotOutput("plot1")
            )
        )
    )
    
    server <- function(input, output) {
        
        dataInput <- reactive({
            endpoint <- switch(
                input$server,
                "smarthealthit" = "https://r4.smarthealthit.org",
                "HAPI" = "http://hapi.fhir.org/baseR4")
            ct <- Client(app_id = "my_app",
                         api_base = endpoint)
            res <- Search(ct, "observation", "Observation",
                          list(patient = input$patientid,
                               code = "http://loinc.org|29463-7"),
                          page = "all")
            res %>% filterBundle("entry.resource") %>%
                group_index("entry.resource.meta.lastUpdated") %>%
                filterBundle("valueQuantity.value") %>%
                arrange_at("entry.resource.meta.lastUpdated") %>%
                mutate(value = as.numeric(.data[["value"]]))
        })
        
        output$res <- renderPrint(dataInput())
        output$plot1 <- renderPlot({
            res <- dataInput()
            if(nrow(res) > 0) {
                plot(res$value, type = "o", ylab = "Weight", xlab = "time")
            }
        })

    }

    shinyApp(ui, server)
}
