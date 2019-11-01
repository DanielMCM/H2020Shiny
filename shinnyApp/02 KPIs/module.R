###################################################
#########        KPI Module       ###########
###################################################

# Data preprocessing


interactive_treemap <- readRDS(file = "../00 Files/data processed/interactive_tree_map.rds")

projects_geolocated_with_others$count <- rep(1, nrow(projects_geolocated_with_others)) # make new column 
projects_geolocated_with_others <- transform(projects_geolocated_with_others, cost = totalCost / 1000000)

# treemap




# Menu item

KPI_menuItem <- function(id) {
    ns <- NS(id)
    menuItem("Tree Map", tabName = str_c(id, "KP"), icon = icon("dashboard"))

}

# UI (tabs)

KPI_ui <- function(id) {
    ns <- NS(id)
    list(tabItem(tabName = str_c(id, "KP"), tags$style(HTML("#legenda.1{font-size:10px;} #legenda_title.1{font-size:10px;} .legend{font-size:10px;}")),
        navbarPage("KPIs!",
    #First Tab Panel
            tabPanel("TreeMap",
            #content inside Second Tab here
                h3("Area representing the number of projects and the investment size per country"),
                d3tree2Output(ns("treeMap")),
                img(src = 'bar.png', align = "center", width = 650, style = "margin-top: 0%;"),
            
                br(),
                h4("Some questions that can be answered using the Tree Map: "),
                tags$div(tags$ul(
                          tags$li(tags$span("Is it possible to compare the countries based on the number of projects and the capital invested in every country")),
                          tags$li(tags$span("What is the relation between the number of projects being developed in a country and the money invested?")),
                          tags$li(tags$span("Which are the countries with more projects?")),
                          tags$li(tags$span("Which are the countries with more investment??"))                  
                        )
                      )
            ),
            #Second Tab
            tabPanel("Bar Chart",
    #Layout with a side bar and a Main Panel
                sidebarLayout(
    #side bar
                    sidebarPanel(
                        radioButtons(ns("orderBy"), "Ordered by cost of the Project:",
                         c("Most expensive Projects" = "more", "Cheapest Projects" = "less")
                        ),
                    h4(textOutput(ns("topText"))),
                    sliderInput(ns("topN"), "Number of countries",
                         min = 1, max = 25,
                         value = 5, step = 1), "-----",br(),br(),
                    h5("Some questions that can be answered using the histogram: "),
                    tags$div(tags$ul(
                          tags$li(tags$span("What is the average cost of a project in each Country? ")),
                          tags$li(tags$span("In which countries is less expensive to develop a project?")),
                          tags$li(tags$span("what is the difference in the cost of a project among different countries?")),
                          tags$li(tags$span("Which are the Top N countries with more investment per project?"))
                        )
                      )
                    ),
                    
                    #Main Panel to the right
                    mainPanel(
                        plotOutput(ns("plot1"))
                    )
                )

            ),
            tabPanel("Static TreeMap",
                  #content inside thirds Tab here
                h2("Area representing the project investment size per country"),
                d3tree2Output(ns("treeMapStatic"))

            )
        )
   
       )#tab item
    ) #list
}

# Server

KPI_server <- function(input, output, session) {

    # Load reactives

    output$plot1 <- renderPlot({
        avgCost <- aggregate(values$projects[, "totalCost"], list(values$projects$coordinatorCountry), mean, na.rm = TRUE, na.action = NULL)
        topN <- as.numeric(input$topN)
        orderedAvgCost <- list()
        sign <- 1
        if (input$orderBy == "more") {
            orderedAvgCost <- avgCost[order(-avgCost$x),]
            sign <- -1
        }

        else {
           orderedAvgCost <- avgCost[order(avgCost$x),] 
        }

        barPlot <- ggplot(data = orderedAvgCost[1:topN,], aes(x = reorder(Group.1[1:topN], sign * x[1:topN]), y = x[1:topN])) +
        geom_bar(stat = "identity", fill = "steelblue") +
        theme_minimal() + labs(title = "Avg Project Cost per Country", x = "Country", y = "Avg Cost") + ylim(0, 3309162)
        
        if (topN <= 12) {
           barPlot<- barPlot + geom_text(aes(label = format(as.integer(x[1:topN]), nsmall = 1, big.mark = ",")), vjust = -.5, color = "#0d47a1", size = 3.5)
        }
        return (barPlot)
    })

    output$treeMap <- renderD3tree2({
                    interactive_treemap
    })

    output$treeMapStatic <- renderD3tree2({
        d3tree2(
        treemap(projects_geolocated_with_others,
        index = c("country"),
        vSize = "count",
        type = "value",
        vColor = "cost",
        palette = "YlOrRd",
        title = "Treemap with area representing the number of projects per country",
        fontsize.title = 14,
        title.legend = "Investment capital in million of euros",
        format.legend = list(scientific = FALSE, big.mark = " "),
        position.legend = "bottom"
     
        ))
    })
   
}