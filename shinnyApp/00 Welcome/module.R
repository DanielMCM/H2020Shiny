###################################################
#########        Integrity Module       ###########
###################################################

Wel_menuItem <- function(id) {
    ns <- NS(id)
    menuItem("Welcome", tabName = str_c(id, "Wel1"), icon = icon("thumbs-up"))
}

# UI (tabs)

Wel_ui <- function(id) {
    ns <- NS(id)
    list(tabItem(tabName = str_c(id, "Wel1"),
        fluidPage(
            column(12, align = "Center",h2(HTML("<b> Welcome to Horizon 2020 dashboard!! </b>")),
            p(HTML("<b> Daniel Minguez Camacho </b>")),
            p(HTML("<b> Yoselin Garcia Salinas </b>"))),
            column(12, align="center",img(src = 'Captura1.png', align = "center")),
            column(12, align = "center", img(src = 'Captura2.png', align = "center"))
            )))
}

# Server

Wel_server <- function(input, output, session) {
}