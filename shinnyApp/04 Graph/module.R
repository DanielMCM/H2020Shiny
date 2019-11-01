###################################################
#########        Integrity Module       ###########
###################################################

# Load questions

source("04 Graph/data.R")

# Menu item

Graph_menuItem <- function(id) {
    ns <- NS(id)
    menuItem("Graph", tabName = str_c(id, "Grph"), icon = icon("connectdevelop"))
}

# UI (tabs)

Graph_ui <- function(id) {
    ns <- NS(id)
    list(tabItem(tabName = str_c(id, "Grph"), fluidPage(
        h2(HTML("<b>  Graph </b>")),
        tags$ol(
            tags$li("Select any number of nodes"),
            tags$li("Click on 'Generate 1 leve connexion.If you want to see next level connexions, clikc again"),
            tags$li("In order to reset, click on 'Reset graph', afterwards go to 1.- and repeat the process")

        ),
        selectizeInput(ns("select"), "Select any node", choices = NULL,
                         options = NULL, multiple = TRUE),
        actionButton(ns("do"), "Generate 1 level conexions"),
        actionButton(ns("reset"), "Reset graph"),
        visNetworkOutput(ns("network"))


    )))
}

# Server

Graph_server <- function(input, output, session) {
    updateSelectizeInput(session, "select", choices = isolate(values$node)[, 4], server = TRUE)
    mark <- reactiveValues(counter = -1,
                           node_5 = data.frame(id = character(),
                                               label = character(),
                                               group = character(),
                                               label_long = character()))
    node_3 <- reactive({
        print(mark$counter)
        calculate_nodes(input$select)
    })
    
    observeEvent(input$do, {
        mark$counter <- mark$counter + 1
    })

    observeEvent(input$reset, {
        mark$counter <- -1
    })

    node_4 <- eventReactive(input$do, {
        if (mark$counter == 0) {
            mark$node_5 <- node_3()
            return(mark$node_5)
        }
        else {
            mark$node_5 <- calculate_nodes(mark$node_5$label_long)
            return(mark$node_5)
        }
    })

    output$network <- renderVisNetwork({
        ledges <- data.frame(color = c("#339933",
                                rgb(252, 0, 0, max = 255), "lightblue"),
                                label = c("European Contribution under 993K",
                                          "European Contribution above 993K",
                                          "Researcher"))
        # minimal example
        visNetwork(node_4(),
               values$edge[(values$edge[, "from"] %in% node_4()[, c("id")]) & (values$edge[, "to"] %in% node_4()[, c("id")]),],
               height = "1000px", width = "100%") %>%
               visGroups(groupname = "Projects", color = "#0f5ba4") %>%
               visGroups(groupname = "Organizations", color = "#fecc00") %>%
               visGroups(groupname = "Researchers", color = "#0099cc") %>%
               visEdges(color = list(color = "#339933", highlight = "red")) %>%
               visOptions(highlightNearest = list(enabled = T, degree = 1, hover = T),
                            nodesIdSelection = T) %>%
        visEvents(hoverNode = "function(e){
                var label_info = this.body.data.nodes.get({
                fields: ['label', 'label_long'],
                filter: function (item) {
                return item.id === e.node
                },
                returnType :'Array'
                });
                this.body.data.nodes.update({id: e.node, label : label_info[0].label_long, label_long : label_info[0].label});
                }") %>%
                visEvents(blurNode = "function(e){
                var label_info = this.body.data.nodes.get({
                fields: ['label', 'label_long'],
                filter: function (item) {
                return item.id === e.node
                },
                returnType :'Array'
                });
                this.body.data.nodes.update({id: e.node, label : label_info[0].label_long, label_long : label_info[0].label});
        }") %>%
        visIgraphLayout() %>%
        visPhysics(stabilization = FALSE) %>%
        visLegend(addEdges = ledges, useGroups = TRUE) %>%
        visInteraction(hideEdgesOnDrag = TRUE) %>%
        visEdges(smooth = FALSE)
        })

}