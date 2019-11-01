calculate_nodes <- function(selection) {
    node_1a <- unique(merge(values$node[values$node[, "label_long"] %in%
                        selection,],
                    values$edge[, c("from", "to")],
                    by.x = "id",
                    by.y = "from",
                    all.y = FALSE)[, c("id", "label", "group", "value", "label_long")])

    node_1b <- unique(merge(values$node[values$node[, "label_long"] %in%
                        selection,],
                    values$edge[, c("from", "to")],
                    by.x = "id",
                    by.y = "to",
                    all.y = FALSE)[, c("id", "label", "group", "value", "label_long")])

    node_1 <- unique(rbind(node_1a, node_1b))
    #Selecciono los nodos a los que les llegan entradas

    node_2a <- unique(merge(values$node,
                    values$edge[edge[, "from"] %in% node_1[, c("id")],][, c("from", "to")],
                    by.x = "id",
                    by.y = "to")[, c("id", "label", "group", "value", "label_long")])

    node_2b <- unique(merge(values$node,
                    values$edge[edge[, "to"] %in% node_1[, c("id")],][, c("from", "to")],
                    by.x = "id",
                    by.y = "from")[, c("id", "label", "group", "value", "label_long")])

    node_2 <- unique(rbind(node_2a, node_2b))
    node_3a <- unique(rbind(node_1, node_2))
    return(node_3a)
}