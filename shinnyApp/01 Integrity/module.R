###################################################
#########        Integrity Module       ###########
###################################################

source(str_c("01 Integrity/", "00 Input Files.R"))
source(str_c("01 Integrity/", "01 Comparative.R"))

Int_menuItem <- function(id) {
    ns <- NS(id)
    menuItem("Integrity", icon = icon("pencil"),
            menuItem("Input Files", tabName = str_c(id, "Int"), icon = icon("pencil")),
            menuItem("Integrity", tabName = str_c(id, "Comp"), icon = icon("pencil"))
    )
}

# UI (tabs)

Int_ui <- function(id) {
    ns <- NS(id)
    list(In_ui(id), In2_ui(id))
}

# Server

Int_server <- function(input, output, session) {

    In_server(input, output, session)
    In2_server(input, output, session)

}