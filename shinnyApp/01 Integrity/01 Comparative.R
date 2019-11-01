In2_ui <- function(id) {
    ns <- NS(id)
    tabItem(tabName = str_c(id, "Comp"),
        fluidPage(
            h2(HTML("<b> Data Integrity </b>")),
            p("We have checked the integrity of the data by joining the different data sets, obtaining the following."),
            h2(HTML("<b> Main files </b>")),
            tags$ul(
                tags$li(HTML("<b> Regarding Organizations: </b>"),
                    tags$ul(
                        tags$li(HTML("There are 7 Organizations in the researchers files that does not appear in the Organization file.")),
                        tags$li(HTML("There are some organizations with different Organization ID but the same description.")))),
                tags$li(HTML("<b> Regarding Projects: </b>"), 
                    tags$ul(
                        tags$li(HTML("There are 59 projects in the researchers files that does not appear in the Projects file.")))),
                tags$li(HTML("<b> Regarding Researchers: </b>"),
                    tags$ul(
                        tags$li(HTML("There are 158 projects in the researchers files that does not have any researcher associated.")),
                        tags$li(HTML("There are duplicate researchers that vary only in the title field ('PROF' and 'DR' for example).")),
                        tags$li(HTML("There are 1.859 researchers without organization."))))
            ),
            p("We have removed incongruent records from our analysis."),
            h2(HTML("<b>  Other Files </b>")),
            p('We have also used other support files:'),
            tags$ul(
                tags$li("External data regarding geolocalization for each organization."),
                tags$li("We have included some preprocessed data in order to improve performance.")
                )
       ))
    
}

# Server

In2_server <- function(input, output, session) {

}