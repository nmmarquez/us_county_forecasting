library(shiny)
library(leaflet)
source("./utilities.R")

shinyServer(function(input,output){
    df <- reactive({sim_county_data(input$seed, input$rho, 
                                    input$sigma, input$method)})
    output$mapplot <- renderLeaflet({
        ar_counties_map(df())
    })
    output$hist <- renderPlot({
        hist_plot(df())
    })
})