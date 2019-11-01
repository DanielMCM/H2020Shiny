values <- reactiveValues()

# ProjectsGeoDate
data <- read.csv("../00 Files/projectsGeoDate.csv")
data$totalCost <- gsub(",", ".", isolate(data$totalCost))
data$totalCost <- as.numeric(isolate(data$totalCost))
values$projects <- data
top <- read.xlsx("../00 Files/cordisref-H2020topics.xlsx", sheet = 1)
# Projects Geolocated
values$ProjectsByCountry <- data
countries <- unique(data$countryName)

#projects_geolocated_with_others for the treemap
projects_geolocated_with_others <- read.csv("../00 Files/data processed/projects_geolocated_with_others.csv")

# Organizations

values$Org <- readRDS("../00 Files/cordis-h2020organizations.rds")

# Projects

init <- readRDS("../00 Files/cordis-h2020projects.rds")

values$Proj <- merge(init, top[,c("Title","CODE")], by.x = c("topics"), by.y = c("CODE"))

# Projects

Rep1 <- readRDS("../00 Files/cordis-h2020reports.rds")

values$Rep <- merge(Rep1, top[, c("Title", "CODE")], by.x = c("topics"), by.y = c("CODE"))


# Researchers

values$Res <- readRDS("../00 Files/researchers.rds")

init2 <- read.csv("../00 Files/Words.csv", header = TRUE, sep = ";", row.names = NULL)

init2$count <- as.numeric(gsub(",", ".", gsub("\\.", "", init2$count)))

gplot <- aggregate(x = init2$count, by = list(init2$term), FUN = "mean")
colnames(gplot)[1] <- "term"
colnames(gplot)[2] <- "TFIDFavg"

gplot2 <- count(init2, init2$term)

colnames(gplot2)[1] <- "term"
colnames(gplot2)[2] <- "count"

values$gplot <- merge(gplot, gplot2, by = c("term"))

values$Words <- init2 