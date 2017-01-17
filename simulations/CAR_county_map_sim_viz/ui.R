rm(list=ls())
library(shiny)
library(shinydashboard)
library(leaflet)

header <- dashboardHeader(
    title = 'AR Methods for US counties'
)

body <- dashboardBody(
    fluidRow(
        column(width=12,
               tabBox(id='tabvals', width=NULL,
                   tabPanel('Map', leafletOutput('mapplot'), value=1),
                   tabPanel('Histogram', plotOutput('hist'), value=2)
               )
        ) 
    ),
    tags$head(tags$style(HTML('
                              section.content {
                              height: 2500px;
                              }
                              ')))
    )



sidebar <- dashboardSidebar(
    textInput('seed', 'Seed', 1234),
    sliderInput('rho', 'Rho', min=0, max=.99, step=.01, value=.98),
    sliderInput('sigma', 'Sigma', min=0.1, max=10, step=.1, value=1),
    selectInput('method', 'Method', c('Q_pCAR', 'Q_lCAR'))
)

dashboardPage(
    header,
    sidebar,
    body
)