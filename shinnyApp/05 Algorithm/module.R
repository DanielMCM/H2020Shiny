###################################################
#########        Integrity Module       ###########
###################################################

# Menu item

Alg_menuItem <- function(id) {
    ns <- NS(id)
    menuItem("Algorithms", tabName = str_c(id, "Algr"), icon = icon("equalizer", lib = "glyphicon"))
}

# UI (tabs)

Alg_ui <- function(id) {
    ns <- NS(id)
    list(tabItem(tabName = str_c(id, "Algr"), fluidPage(
        h2(HTML("<b>  Term Frequency / Inverse Term Frequency analysis </b>")),
        tags$ol(
            tags$li("Select any word above in order to check in which topics it has been found relevant"),
            tags$li("Below you can check the relation between the average TF-IDF and the number of topics in which a word is relevant")
        ),
        selectizeInput(ns("selectWord"), "Look for a word!", choices = NULL,
                         options = NULL, multiple = FALSE),
        fluidRow(box(DT::dataTableOutput(ns("text1"))),
                 box(plotOutput(ns("WordPlot")))),
        fluidRow(column(6, align = "center", plotOutput(ns("pointplot"), click = ns("pointplot_click"))),
                box(p("Terms selected:"),
                    verbatimTextOutput(ns("text2")))
                )
    )))
}

# Server

Alg_server <- function(input, output, session) {
    updateSelectizeInput(session, "selectWord", choices = isolate(values$Words)[,2], server = TRUE)

    output$WordPlot <- renderPlot({
       prev <- data.frame(Topics = unique(values$Words[which(values$Words[, c("term")] %in% input$selectWord), c("Title")]))
       theTable <- values$Words[which(values$Words$Title %in% prev[input$text1_rows_selected,]),]
       p <- ggplot(data = theTable,
              aes(x = reorder(term, -count), y = count)) +
              geom_bar(stat = "identity", fill = "steelblue") +
              theme_minimal() + labs(title = "TF-IDF per Topic", x = "Terms", y = "TF-IDF")
       return(p)
    })

    output$text1 <- DT::renderDataTable({
        datatable(data.frame(Topics = unique(values$Words[which(values$Words[, c("term")] %in% input$selectWord), c("Title")])),
                options = list(scrollX = TRUE), selection = "single")
    })

    output$text2 <- renderText({
        p <- nearPoints(values$gplot, input$pointplot_click, xvar = "count", yvar = "TFIDFavg")
        paste0(p$term)
    })

    output$pointplot <- renderPlot({
        ggplot(values$gplot, aes(x = count, y = TFIDFavg)) +
            geom_point() +
            xlab(label = "Number of titles in which the term appears") +
            ylab(label = "Average TF-IDF")
    })
}