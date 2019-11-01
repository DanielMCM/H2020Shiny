###################################################
#########        Map Module       ###########
###################################################

Map_menuItem <- function(id) {
    ns <- NS(id)
    menuItem("Map", tabName = str_c(id, "Mp"), icon = icon("globe"))
}

# UI (tabs)
r_colors <- rgb(t(col2rgb(colors()) / 255))
names(r_colors) <- colors()

Map_ui <- function(id) {
    ns <- NS(id)
    list(tabItem(tabName = str_c(id, "Mp"), tags$style(HTML("#big-heading{color: #1e88e5; margin-top: 0%;}
            #controls {
            /* Appearance */
            background-color: #eceff1;
            padding: 0 20px 20px 20px;
            cursor: move;
            /* Fade out while not hovering */
            opacity: 0.65;
            zoom: 0.9;
            transition: opacity 500ms 1s;
            margin-top:23px;
        }

        #controls:hover {
            /* Fade in while hovering */
            opacity: 0.95;
            transition-delay: 0;
        }
    
    
    ")),
    fluidPage(
   
    # App title ----
         h3(id = "big-heading", "Number of projects per country"),
         p("It is recommended to use Internet explorer in order to use the date slider"),
         leafletOutput(ns("mymap"), width = "102%", height = "500px"),
         absolutePanel(id = "controls", class = "panel panel-default", fixed = TRUE,
            draggable = TRUE, top = 120, left = "auto", right = 20, bottom = "auto",
            width = 250, height = "auto",
            p(),
           #  h2("ZIP explorer"),
             selectizeInput(ns("country"), "Visualize by Country", choices = countries,
                   options = list(placeholder = 'select a country', maxItems = 5, 'plugins' = list('remove_button')), multiple = TRUE),
             sliderInput(ns("range"),
                  "Projects started in years ",
                  value = c(2014, 2020),
                  sep = "",
                  min = 2014,
                  max = 2020)
          ),
    
         br(),
         h4("Questions that can be answered using the interactive Map: "),
         tags$div(tags$ul(
                  tags$li(tags$span("How many projects are being develop in each country?")),
                  tags$li(tags$span("How are the projects distributed along a specific country?")),
                  tags$li(tags$span("Which are the countries with more projects?")),
                  tags$li(tags$span("How many projects started in a year in each country?")),
                  tags$li(tags$span("How many projects started in certain period in each country?"))
                )
              )
)





        ))

}

# Server

Map_server <- function(input, output, session) {
   
    # Load reactives
      
    points <- eventReactive(input$recalc, {
    cbind(rnorm(40) * 2 + 13, rnorm(40) + 48)
    }, ignoreNULL = FALSE)
    print(points)

    sliderValues <- reactive({
        projects <- values$ProjectsByCountry[!is.na(values$ProjectsByCountry$startDate),]
        projects$startYear <- as.numeric(projects$startYear)
        projects$endDate <- as.numeric(projects$endDate)
    
        start <- as.numeric(input$range[1])
        end <- as.numeric(input$range[2])
        a <- projects[(projects$startYear >= start & projects$startYear <= end),]
        cat(file = stderr(), "TYPE ->", typeof(input$country), "<-\n")
        if (!is.null(input$country)) {
                a <- projects[projects$countryName == input$country,]
            }
        
            cat(file = stderr(), "drawing a map with", start, " range ", end, "\n")
            num <- nrow(a)
        
              cat(file = stderr(), "AFTER", num, "\n")
         
                return(a)
    })

    # Call questions' servers
    output$mymap <- renderLeaflet({
      projects <- sliderValues() # Add this
        zoomSize <- 2
        if (!is.null(input$country)) {
            zoomSize <- 4
        }
        leaflet(data = projects) %>% addTiles() %>% setView(6.143158, 46.204391, zoom = zoomSize) %>% addMarkers(
          lng = ~uni_lng, lat = ~uni_lat,
          clusterOptions = markerClusterOptions(),
          popup = ~paste("<b style=\"color:#1e88e5\">", projects$coordinator, "</b>",
                         "<hr style=\"color:#b0bec5;  margin-top: 0em; margin-bottom: 0em;\">",
          "Country: ", projects$countryName
          )
         
        )
    })


}