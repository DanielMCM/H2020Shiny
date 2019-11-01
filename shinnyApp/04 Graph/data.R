#Compruebo que todas las organizaciones en researchers estan en organizaciones
a2 <- merge(isolate(values$Res), data.frame(unique(isolate(values$Org)[, c("id", "name")])), by.x = c("organisationId"), by.y = c("id"))
a2 <- a2[complete.cases(a2),]
#Compruebo que todos los proyectos en researchers estan en organizaciones
a2 <- merge(a2, data.frame(unique(isolate(values$Org)[, c("projectID", "projectAcronym")])), by.x = c("projectId", "projectAcronym"), by.y = c("projectID", "projectAcronym"))
a2 <- a2[complete.cases(a2),]

#METO IDS A RESEARCHERS Y NOMBRES
a2$label_long <- paste0(a2$firstName, " ", a2$lastName)
#head(a)

#NODOS DE PROYECTOS
node1 <- data.frame(unique(isolate(values$Org)[, c("projectRcn", "projectAcronym")]))
node1$group <- "Projects"


colnames(node1)[1] <- "id2"
colnames(node1)[2] <- "label_long"

node1$label <- ""

#NODOS DE ORGANIZATIONS
node2 <- data.frame(unique(isolate(values$Org)[, c("id", "name")]))
node2$group <- "Organizations"

colnames(node2)[1] <- "id2"
colnames(node2)[2] <- "label_long"

node2$label <- ""

#NODOS DE RESEARCHERS
node3 <- data.frame("label_long" = unique(a2[, c("label_long")]), "id2" = 1)
node3$id2<- row.names(node3)
node3$group <- "Researchers"

node3$label <- ""

#UNIMOS NODOS Y RENOMBRAMOS IDs

node <- rbind(node1, node2, node3)
rownames(node) <- NULL
node$id <- row.names(node)


#Añado el ID de nodo a las organizaciones

b <- merge(isolate(values$Org), node[, c("id2", "label_long", "id")], by.x = c("id", "name"), by.y = c("id2", "label_long"))
colnames(b)[25] <- "from"

#Añado el ID de organizaciones a los researchers

a2 <- merge(a2, unique(b[, c("id", "from")]), by.x = c("organisationId"), by.y = c("id"))
colnames(a2)[10] <- "to2"

#Añado el id de proyecto a las organizaciones

b <- merge(b, node[, c("id2", "label_long", "id")], by.x = c("projectRcn", "projectAcronym"), by.y = c("id2", "label_long"))
colnames(b)[26] <- "to"
colnames(b)[10] <- "value"

#Añado el id de proyecto a los researchers

a2 <- merge(a2, unique(b[, c("projectID", "projectAcronym", "to")]), by.x = c("projectId", "projectAcronym"), by.y = c("projectID", "projectAcronym"))
colnames(a2)[11] <- "to"

#Añado el id de researcher a los researchers

a2 <- merge(a2, node[, c("label_long", "id")], by.x = c("label_long"), by.y = c("label_long"))

#Cojo los edges de las organizaciones->proyectos

edge1 <- b[, c("from", "to", "value")]

#Cojo los edges de Researchers -> Organizaciones
edge2 <- a2[, c("id", "to2")]
colnames(edge2)[1] <- "from"
colnames(edge2)[2] <- "to"
edge2$value <- -1

#Cojo los edges de Researchers -> Proyectos

edge3 <- a2[, c("id", "to")]
colnames(edge3)[1] <- "from"

edge3$value <- -1

edge <- rbind(edge1, edge2, edge3)


edge[is.na(edge)] <- 1
edge$arrows <- "from"

edge$color <- rgb(51, 153, 51, max = 255)
val <- edge$value[which(edge$value != -1)]
edge$color[which(edge$value > (summary(val)[5] + 1.5 * (summary(val)[5] - summary(val)[2])))] <- rgb(252, 0, 0, max = 255)
edge$color[which(edge$value == -1)] <- "lightblue"

values$edge <- edge

node <- node[, c("id", "label", "group", "label_long")]

counts <- data.frame(plyr::count(edge, "to"))
colnames(counts)[1] <- "from"

counts_2 <- rbind(data.frame(plyr::count(edge, "from")), counts)
counts_3 <- aggregate(counts_2$freq, by = list(from = counts_2$from), FUN = sum)

colnames(counts_3)[2] <- "value"

values$node <- merge(node, counts_3, by.x = "id", by.y = "from")

#rm(counts, node1, node2, node3, edge1, edge2, edge3, b, a2, val)