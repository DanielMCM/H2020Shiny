


In_ui <- function(id) {
    ns <- NS(id)
    tabItem(tabName = str_c(id, "Int"),
        navbarPage("Files Summary",
                   tabPanel("Organizations",
                            fluidRow(
			                    h2(HTML("<b>  Input Files summary </b>")),
                                p('We are using 4 main files extracted with date 2018-10-10 from', a("EU Data Portal", href = "http://data.europa.eu/euodp/en/data/dataset/cordisH2020projects"),
                                   '. Also availables in ', a("this link", href = "https://www.europeandataportal.eu/data/dataset/http-cordis-europa-eu-projects"), "."),
			                    h2(HTML("<b>  File 1: Organizations </b>")),
                                p('This file includes all organizations that have participated in any of the EU research projects under Horizon 2020.')
                                ), #FluidRow
                            fluidRow(valueBoxOutput(ns("org1"), width = 3),
                                        valueBoxOutput(ns("org2"), width = 3),
                                        valueBoxOutput(ns("org3"), width = 3),
                                        valueBoxOutput(ns("org4"), width = 3)
                                        ), #FluidRow
                            fluidRow(column(6,
                                        plotlyOutput(ns("orgpie1"))
                                        ),
                                     column(6,
                                        plotlyOutput(ns("orgpie2"))
                                        )
                                    ), #FluidRow
                            fluidRow(
                                wellPanel(DT::dataTableOutput(ns("table1")))
                                    ) #FluidRow
                           ), #TabPanel
                    tabPanel("Projects",
                            fluidRow(h2(HTML("<b>  File 2: Projects </b>")),
                            p('This file includes all projects that have been included under the EU research projects under Horizon 2020.')
                            ), #FluidRow
                            fluidRow(valueBoxOutput(ns("pro1"), width = 3),
                                    valueBoxOutput(ns("pro2"), width = 3),
                                    valueBoxOutput(ns("pro3"), width = 3),
                                    valueBoxOutput(ns("pro4"), width = 3)
                            ), #FluidRow
                            fluidRow(column(6,
                                        plotOutput(ns("plot_pro_1"))
                                        ),
                                        column(6,
                                        plotOutput(ns("plot_pro_2"))
                                        )
                                    ), #FluidRow
                            fluidRow(h2(HTML("<b>  Click a row to see the report </b>"))),
                            fluidRow(
                                    box(htmlOutput(ns("text2")), width = 12)
                                    ), #FluidRow
                            fluidRow(
                                wellPanel(DT::dataTableOutput(ns("table2")))
                                    ) #FluidRow
                            ), #TabPanel
                    tabPanel("Reports",
                            fluidRow(h2(HTML("<b>  File 3: Report summaries </b>")),
                            p('This file includes different reports regarding some projects from EU research projects under Horizon 2020.'),
                            p('We have detected a problem in line 4395, where there is a line break problem.')
                            ), #FluidRow
                            fluidRow(valueBoxOutput(ns("rep1"), width = 6),
                                    valueBoxOutput(ns("rep2"), width = 6)
                            ), #FluidRow
                            fluidRow(h2(HTML("<b>  Click a row to see the report </b>"))),
                            fluidRow(
                                    box(htmlOutput(ns("text1")), width=12)
                                    ), #FluidRow
                            fluidRow(
                                wellPanel(DT::dataTableOutput(ns("table3")))
                                    ) #FluidRow
                            ), #TabPanel
                    tabPanel("Researchers",
			                fluidRow(h2(HTML("<b>  File 4-5: Principal investigators </b>")),
                            p('This file includes all organizations that have participated in any of the EU research projects under Horizon 2020.')
                            ), #FluidRow
                            fluidRow(valueBoxOutput(ns("res1"), width = 3),
                                    valueBoxOutput(ns("res2"), width = 3),
                                    valueBoxOutput(ns("res3"), width = 3),
                                    valueBoxOutput(ns("res4"), width = 3)
                            ), #FluidRow
                            fluidRow(
                                wellPanel(DT::dataTableOutput(ns("table4")))
                                    ) #FluidRow
                            ) #TabPanel
                    ) #NavBar
    ) #TabItem
}

# Server

In_server <- function(input, output, session) {
    #### ORGANIZATIONS ####
    output$org1 <- renderValueBox({
        valueBox(
            formatC(length(values$Org[,1]), format = "d", big.mark = ',')
            , 'Nº of lines'
            , icon = icon("align-left", lib = 'glyphicon')
            , color = "green")
    })

    output$org2 <- renderValueBox({
    valueBox(
            formatC(length(unique(values$Org[, 5])), format = "d", big.mark = ',')
            , 'Nº Unique Organizations (Ids)'
            , icon = icon("align-left", lib = 'glyphicon')
            , color = "yellow")
    })
    output$org3 <- renderValueBox({
    valueBox(
            formatC(length(unique(values$Org[, 2])), format = "d", big.mark = ',')
            , 'Nº Unique Projects (Ids)'
            , icon = icon("align-left", lib = 'glyphicon')
            , color = "red")
    })
    output$org4 <- renderValueBox({
    values$Org$ecContribution[is.na(values$Org$ecContribution)] <- 0
    valueBox(
             
            formatC(sum((values$Org$ecContribution)) / 1000000, format = "d", big.mark = ',')
            , 'Total millions European Contribution'
            , icon = icon("align-left", lib = 'glyphicon')
            , color = "aqua")
    })

    output$orgpie1 <- renderPlotly({
          
          dataplot <- aggregate(values$Org$ecContribution, by = list(Country = values$Org$country), FUN = sum)
          s <- sum(dataplot[order(dataplot$x, decreasing = TRUE),][16:length(dataplot), 2])
          dataplot <- dataplot[order(dataplot$x, decreasing = TRUE),][1:15,]
          dataplot[16, 1] <- "Others"
          dataplot[16, 2] <- s
          plot_ly() %>%
          add_pie(data = dataplot,
                  labels = ~Country, values = ~x) %>%
                  layout(title = "Contribution per country (€)")
    })
    output$orgpie2 <- renderPlotly({

        dataplot1 <- unique(values$Org[, c(4, 5)])
        dataplot1 <- aggregate(dataplot1$role,
                              by = list(Roles = dataplot1$id),
                              FUN = toString)
        dataplot1 <- aggregate(dataplot1$Roles,
                              by = list(CC = dataplot1$x),
                              FUN = length)
        s <- sum(dataplot1[order(dataplot1$x, decreasing = TRUE),][4:length(dataplot1), 2])
        dataplot1 <- dataplot1[order(dataplot1$x, decreasing = TRUE),][1:3,]
        dataplot1[4, 1] <- "Combination"
        dataplot1[4, 2] <- s
        plot_ly() %>%
              add_pie(data = dataplot1,
                      labels = ~CC, values = ~x) %>%
                      layout(title = "Organizations roles")
    })
    output$table1 <- DT::renderDataTable({
        datatable(values$Org[, c(1:6, 9:13, 15)],
                  options = list(scrollX = TRUE))
    })
    #### Projects ####
    output$pro1 <- renderValueBox({
    valueBox(
            formatC(length(values$Proj[, 1]), format = "d", big.mark = ',')
            , 'Nº of lines'
            , icon = icon("align-left", lib = 'glyphicon')
            , color = "green")
    })
    output$pro2 <- renderValueBox({
    valueBox(
            formatC(length(unique(values$Proj[, 2])), format = "d", big.mark = ',')
            , 'Nº of projects (IDs)'
            , icon = icon("align-left", lib = 'glyphicon')
            , color = "yellow")

    })
    output$pro3 <- renderValueBox({
    valueBox(
            formatC(sum(values$Proj[, 14]) / 1000000, format = "d", big.mark = ',')
            , 'European contribution (Millions)'
            , icon = icon("align-left", lib = 'glyphicon')
            , color = "red")
    })
    output$pro4 <- renderValueBox({
    valueBox(
            formatC(sum(values$Proj[, 13], na.rm = TRUE) / 1000000, format = "d", big.mark = ',')
            , 'Total Cost (Millions)'
            , icon = icon("align-left", lib = 'glyphicon')
            , color = "aqua")
    })
    output$table2 <- DT::renderDataTable({
    datatable(values$Proj[, c(22, 2:6, 7:11,13:17)],
                  options = list(scrollX = TRUE), selection = "single")
    })
    output$plot_pro_1 <- renderPlot({
        values$Proj$totalCost[is.na(values$Proj$totalCost)] <- values$Proj$ecMaxContribution[is.na(values$Proj$totalCost)]
        values$Proj$ecMaxContribution[is.na(values$Proj$ecMaxContribution)] <- 1
        ggplot(values$Proj, aes(x = log(totalCost),
              y = log(ecMaxContribution),
              color = substr(startDate, 1, 4)),
        ) +
        geom_point() + labs(title = "Contribution vs total cost",
                           subtitle = "log cost in €", color = "Year") +
                           xlab(label = "Log of total cost") +
                           ylab(label = "Log of European contribution") +
                           scale_color_brewer(palette = "Accent") 
                           
    })
    output$plot_pro_2 <- renderPlot({
        dataplot2 <- aggregate(values$Proj$ecMaxContribution, by = list(Year = substr(values$Proj$startDate, 1, 4)), FUN = sum)
        ggplot(dataplot2, aes(x = Year,
              y = x / 1000000)) + geom_bar(stat = "identity", fill = "steelblue") +
              theme_minimal() + labs(title = "Contribution per year") +
                           xlab(label = "Year") +
                           ylab(label = "European contribution in Millions")

    })
    output$text2 <- eventReactive(input$table2_rows_selected, {
        HTML(paste("<b> Title </b>",
               p(HTML(values$Proj[input$table2_rows_selected, c(8)])),
               "<b> Objective </b>",
               p(HTML(values$Proj[input$table2_rows_selected, c(12)]))))
    })
    #### REPORTS ####

    output$rep1 <- renderValueBox({
    valueBox(
            formatC(length(values$Rep[, 1]), format = "d", big.mark = ',')
            , 'Nº of lines'
            , icon = icon("align-left", lib = 'glyphicon')
            , color = "green")
    })
    output$rep2 <- renderValueBox({
    valueBox(
            formatC(length(values$Rep[, 10]), format = "d", big.mark = ',')
            , 'Nº of projects (IDs)'
            , icon = icon("align-left", lib = 'glyphicon')
            , color = "yellow")
    })
    output$table3 <- DT::renderDataTable({
        
    datatable(values$Rep[, c(1, 10:15,17)],
                  options = list(scrollX = TRUE), selection = "single")
    })
    output$text1 <- eventReactive(input$table3_rows_selected, {
           HTML(paste("<b>",h2(values$Rep[input$table3_rows_selected, c(4)]),"</b>",
           p(HTML(paste("<b> Teaser: </b>",values$Rep[input$table3_rows_selected, c(5)]))),
           p(HTML(paste("<b> Summary: </b>", values$Rep[input$table3_rows_selected, c(6)]))),
           p(HTML(paste("<b> Work performed: </b>", values$Rep[input$table3_rows_selected, c(7)]))),
           p(HTML(paste("<b> Final results: </b>", values$Rep[input$table3_rows_selected, c(8)])))))
        })
    #### Researchers ####

    output$res1 <- renderValueBox({
    valueBox(
            formatC(length(values$Res[, 1]), format = "d", big.mark = ',')
            , 'Nº of lines'
            , icon = icon("align-left", lib = 'glyphicon')
            , color = "green")
    })
    output$res2 <- renderValueBox({
    valueBox(
            formatC(length(unique(values$Res[, 1])), format = "d", big.mark = ',')
            , 'Nº of projects (IDs)'
            , icon = icon("align-left", lib = 'glyphicon')
            , color = "yellow")

    })
    output$res3 <- renderValueBox({
    valueBox(
            formatC(length(unique(values$Res[, 7])), format = "d", big.mark = ',')
            , 'Nº of Organisations'
            , icon = icon("align-left", lib = 'glyphicon')
            , color = "red")
    })
    output$res4 <- renderValueBox({
    valueBox(
            formatC(length(unique(paste0(values$Res[, 5], " ", values$Res[, 6]))), format = "d", big.mark = ',')
            , 'Nº of researchers'
            , icon = icon("align-left", lib = 'glyphicon')
            , color = "aqua")
    })
    output$table4 <- DT::renderDataTable({
    datatable(values$Res[, c(1:3, 5:7)],
                  options = list(scrollX = TRUE))
    })
}