###################################################
##########           Libraries     ################
###################################################
#install.packages("shiny")
#install.packages("shinydashboard")
#install.packages("leaflet")
#install.packages("plyr")
#install.packages("dplyr")
#install.packages("tidyverse")
#install.packages("stringr")
#install.packages("ggplot2")
#install.packages("leaflet")
#install.packages("tidyverse")
#install.packages("ggmap")
#install.packages("htmltools")
#install.packages("openxlsx")
#install.packages("dashboardthemes")
#install.packages("treemap")
#install.packages("ECharts2Shiny")
#install.packages("d3treeR")
#install.packages("visNetwork")
#install.packages("plotly")
#install.packages("DT")
#install.packages("gdata")
#install.packages("shinyBS")

library(shiny)
library(shinydashboard)
library(leaflet)
library(plyr)
library(dplyr)
library(tidyverse)
library(stringr)
library(ggplot2)
library(leaflet)
library(tidyverse)
library(ggmap)
library(htmltools)
library(openxlsx)
library(dashboardthemes)
library(treemap)
library(ECharts2Shiny)
library(d3treeR)
library(visNetwork)
library(plotly)
library(DT)
library(gdata)
library(shinyBS)


###################################################
##########           Sources       ################
###################################################

source("global.R")
source("layout.R")
source("helpers.R")

###################################################
##########           UI            ################
###################################################

ui <- dashboardPage(
  header("Header"),
  dashboardSidebar(
    sidebar("Sidebar")
  ),
  dashboardBody(shinyDashboardThemes(
      theme = "blue gradient"
    ),
    tags$head(
      tags$link(rel = "stylesheet", type = "text/css")
    ),
    content("Content")
  )
)

###################################################
##########           Server        ################
###################################################

server <- function(input, output, session) {
    callModule(Int_server, "Integrity")
    callModule(KPI_server, "KPI")
    callModule(Map_server, "Map")
    callModule(Graph_server, "Graph")
    callModule(Alg_server, "Alg")
    callModule(Wel_server, "Welc")
}

shinyApp(ui, server)