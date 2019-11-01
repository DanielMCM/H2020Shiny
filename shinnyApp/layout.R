###################################################
##########           Layout        ################
###################################################

source("00 Welcome/module.R")
source("01 Integrity/module.R")
source("02 KPIs/module.R")
source("03 Map/module.R")
source("04 Graph/module.R")
source("05 Algorithm/module.R")

# Header

header <- function(id) {
    ns <- NS(id)
    dashboardHeader(title = "H2020 Projects",
    dropdownMenu(type = "notifications", icon = icon("warning"), badgeStatus = "warning",
                                 notificationItem("Some tabs take a while to load")),
        # Pending add UE web page
    tags$li(a(href = 'https://www.fi.upm.es/',
              img(src = 'Captura.png', title = "a", height = "31px", width = "96px"),
              style = "padding-top:10px; padding-bottom:10px;"
              ), class = "dropdown"))
       # )
    #)
}

# Menu

sidebar <- function(id) {
    ns <- NS(id)
    sidebarMenu(
        Wel_menuItem("Welc"),
        Int_menuItem("Integrity"),
        KPI_menuItem("KPI"),
        Map_menuItem("Map"),
        Graph_menuItem("Graph"),
        Alg_menuItem("Alg")
    )
}

# Content

content <- function(id) {
    ns <- NS(id)
    do.call(tabItems, c(Wel_ui("Welc"),
                        Int_ui("Integrity"),
                        KPI_ui("KPI"),
                        Map_ui("Map"),
                        Graph_ui("Graph"),
                        Alg_ui("Alg")))
}