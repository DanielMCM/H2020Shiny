options(java.parameters = "- Xmx1024m")
require(xlsx)
require(ggmap)


countries <- read.xlsx("cordisref-countries.xls", sheetIndex = 1)
projects <- read.xlsx2("cordis-h2020projects.xlsx", sheetIndex = 1)

p2 <- projects

#get list of coordinatorCountry countries
uniqueCountries <- unique(projects[18])
length(uniqueCountries[, 1])
str(uniqueCountries)

#get list of countries (eucode/countryCoordinator) which language name is in english (all countries)
countriesNames <- countries[which(countries$language == 'en'), c(1, 3)]
countriesNames$euCode <- as.character(countriesNames$euCode)

write.csv(countriesNames, "countriesNames.csv", row.names = FALSE)

#Adding countries Names to uniqueCountries projects matrix 
names <- vector()
uniqueCountries[, 2] <- NA
for (row in 1:length(uniqueCountries[, 1])) {
    print(row)
    tmp <- countriesNames[which(countriesNames$euCode == uniqueCountries[row, 1]), c(2)]
    print(tmp)
    print(uniqueCountries[row, 1])
    uniqueCountries[row, 2] <- levels(countriesNames$name)[tmp]
    names[row] <- levels(countriesNames$name)[tmp]

}
write.csv(uniqueCountries, "uniqueCountries.csv", row.names = FALSE)
uniqueCountries

#ProjectsByCountry: number of projects by Coordinator Country  Country_CODE/FREQ
#Adding countries names to ProjectsByCountry
ProjectsByCountry <- as.data.frame(table(projects$coordinatorCountry))
ProjectsByCountry[, 3] <- NA
for (row in 1:nrow(ProjectsByCountry)) {
    countryCode = ProjectsByCountry[row, 1]
    index = match(countryCode, uniqueCountries$coordinatorCountry)

    countryName = uniqueCountries$V2[index] #uniqueCountries Name

    ProjectsByCountry[row, 3] <- countryName

    print(countryCode)
    print(countryName)

}

# Var:euCode Freq  V3:Countries' names
#Adding longitude and latitude to  ProjectsByCountry
ProjectsByCountry[, 4] <- as.numeric(0.0) # Latitude
ProjectsByCountry[, 5] <- as.numeric(0.0) # Longitude
for (row in 1:nrow(ProjectsByCountry)) {
    countryName = ProjectsByCountry$V3
    direction <- geocode(countryName, source = "dsk", force = TRUE)
    ProjectsByCountry[row, 4] <- as.numeric(direction[row, 2])
    ProjectsByCountry[row, 5] <- as.numeric(direction[row, 1])

    print(direction)
}


colnames(ProjectsByCountry) <- c("code", "freq", "name", "latitude", "longitude")

#ProjectsByCountry$longitude


write.csv(ProjectsByCountry, "ProjectsByCountry.csv", row.names = FALSE)

#####################################################################

ProjectsByCountry <- read.csv("ProjectsByCountry.csv")
#projects$latitude <- NULL
projects[, ncol(projects) + 1] <- as.numeric(0.0)
colnames(projects)[ncol(projects)] <- "latitude"

#projects$longitude <- NULL
projects[, ncol(projects) + 1] <- as.numeric(0.0)
colnames(projects)[ncol(projects)] <- "longitude"

#nrow(projects)
for (row in 1:nrow(projects)) {
    index = match(projects$coordinatorCountry[row], ProjectsByCountry$code)
    projects[row, "latitude"] <- as.numeric(ProjectsByCountry$latitude[index])
    projects[row, "longitude"] <- as.numeric(ProjectsByCountry$longitude[index])
    print(index)
    print(ProjectsByCountry$latitude[index])
    print(ProjectsByCountry$longitude[index])
    print("---------")
}

head(projects, 100)
head(ProjectsByCountry$longitude, 30)

table <- merge(x = projects, y = ProjectsByCountry, by.x = "coordinatorCountry", by.y = "code")
table$longitude.y <- NULL
table$latitude.y <- NULL

colnames(table)[which(names(table) == ("latitude.x"))] <- "latitude"
colnames(table)[which(names(table) == ("longitude.x"))] <- "longitude"

names(table)

nrow(projects)
nrow(table)
table

tail(table, 1)
colnames(table)[which(names(table) == ("name"))] <- "countryName"

reducedDataframe <- table[, c("coordinatorCountry", "countryName", "coordinator", "totalCost", "latitude", "longitude", "startDate", "endDate", "status")]


reducedDataframe
write.csv(reducedDataframe, "ProjectsCoordinatesReduced.csv")

head(reducedDataframe$longitude)


###################################################################3

universities_geolocated <- read.csv("universities_geolocated.csv")
head(universities_geolocated)

names(universities_geolocated)
colnames(universities_geolocated)[which(names(universities_geolocated) == ("x"))] <- "coordinator"
colnames(universities_geolocated)[which(names(universities_geolocated) == ("lat"))] <- "universityLat"
colnames(universities_geolocated)[which(names(universities_geolocated) == ("lng"))] <- "universityLng"

universities_geolocated$X <- NULL
universities_geolocated$Unnamed..0 <- NULL