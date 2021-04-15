require(shiny)
require(ggplot2)
require(dplyr)
Loki <- read.csv("https://raw.githubusercontent.com/ManasaShankaraMurthy/ProjectLoki/main/Loki-data.csv")
Loki <- read.csv("C:/Users/manas/Documents/Check/Try/App/Loki-data.csv", stringsAsFactors = FALSE)

ui <- fluidPage(
  
  
  titlePanel("Loki Liquor Store prices"),
  sidebarLayout(
    sidebarPanel(
      h3(
        "This app will help you find the right drink for a right price from the country you prefer"
      ),
      br(),
      sliderInput("priceInput", "Price", 0, 100, c(25, 40), pre = "$"),
      sliderInput("SweetnessInput","Sweetness",0,10, c(0,10)),
      uiOutput("typeSelectOutput"),
      checkboxInput("filterCountry", "Filter by country", FALSE),
      conditionalPanel(
        condition = "input.filterCountry",
        uiOutput("countrySelectorOutput")
      
      
      )
    ),
    mainPanel(
      h3(textOutput("summaryText")),
      downloadButton("download", "Download results"),
      br(),
      plotOutput("plot"),
      br(), br(),
      #tableOutput("prices")
      DT::dataTableOutput("prices")
    )
  )
)

server <- function(input, output, session) {
  output$countrySelectorOutput <- renderUI({
    selectInput("countryInput", "Country",
                sort(unique(Loki$Country)),
                selected = "ITALY")
  })
  
  output$typeSelectOutput <- renderUI({
    radioButtons("typeInput", "Product type",
                sort(unique(Loki$Type)),
              
                selected = c( "WINE"))
  })
  
  output$summaryText <- renderText({
    numOptions <- nrow(prices())
    if (is.null(numOptions)) {
      numOptions <- 0
    }
    paste0("We found ", numOptions, " options for you")
  })
  
  prices <- reactive({
    prices <- Loki
    
    if (is.null(input$countryInput)) {
      return(NULL)
    }
    
    prices <- dplyr::filter(prices, Type %in% input$typeInput)
    if (input$filterCountry) {
      prices <- dplyr::filter(prices, Country == input$countryInput)
    }
    prices <- dplyr::filter(prices, Price >= input$priceInput[1],    
                            Price <= input$priceInput[2])
    
    if(nrow(prices) == 0) {
      return(NULL)
      
    }
    prices
  })
  
  output$plot <- renderPlot({
    if (is.null(prices())) {
      return(NULL)
    }
    
    ggplot(prices(), aes(Alcohol_Content, fill = Type)) +
      geom_histogram(colour = "black") +
      theme_classic(20)
  })
  
  output$prices <- DT::renderDataTable({
    prices()
  })
  
  output$download <- downloadHandler(
    filename = function() {
      "Loki-results.csv"
    },
    content = function(con) {
      write.csv(prices(), con)
    }
  )
}

shinyApp(ui = ui, server = server)
