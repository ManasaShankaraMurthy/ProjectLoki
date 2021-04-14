library(shiny)
library(ggplot2)
library(dplyr)

Loki <- read.csv("https://raw.githubusercontent.com/ManasaShankaraMurthy/ProjectLoki/main/Loki-data.csv")

ui <- fluidPage(
    titlePanel("MadhuLoka Liquor Store prices"),
    sidebarLayout(
        sidebarPanel(
            sliderInput("priceInput", "Price", 0, 100, c(20, 40), pre = "$"),
            sliderInput("SweetnessInput","Sweetness",0,10, c(0,10)),
            radioButtons("typeInput", "Product type",
                         choices =sort(unique(Loki$Type)),
                         selected = "WINE"),
            #checkboxInput("SweetnessInput","Sweetness"),
            
            
            uiOutput("countryOutput")
            
        ),
        mainPanel(
            downloadButton("downloadData", "Download"),
            br(), br(),
            plotOutput("plot"),
            br(), br(),
            DT::dataTableOutput("results")
        )
    )
)

server <- function(input, output) {
    output$countryOutput <- renderUI({
        selectInput("countryInput", "Country",
                    sort(unique(Loki$Country)),
                    selected = "ITALY")
        
        
        
    })  
    
    filtered <- reactive({
        if (is.null(input$countryInput)) {
            return(NULL)
        }    
        
        Loki %>%
            filter(Price >= input$priceInput[1],
                   Price <= input$priceInput[2],
                   Sweetness >= input$SweetnessInput[1],
                   Sweetness <= input$SweetnessInput[2],
                   Type == input$typeInput,
                   Country == input$countryInput
            )
    })
    
    output$plot <- renderPlot({
        if (is.null(filtered())) {
            return() }
            
        ggplot(filtered(), aes(Alcohol_Content)) +
            geom_histogram(colour="blue")+
            theme_classic(20)
    })
    
    output$results <- DT::renderDataTable({
        filtered()
        
    })
}
#Output$DownloadData <- downloadHandler(filename = function(){
    #paste("data_t","csv",sep = ',')
#content = function(file){
    #write.csv(output$results,file)
    

shinyApp(ui = ui, server = server)
