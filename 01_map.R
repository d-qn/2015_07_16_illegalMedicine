library(countrycode)
library(tidyr)
library(dplyr)
library(swiTheme)
library(swiMap)
library(leaflet)
library(htmlwidgets)

nTopMedicine <- 5



read.data <- read.csv("Illegal-medicine-imports-Swismedic_cleaned.csv", check.names = F)
read.data <- rename(read.data, country = `Shipment origin`, medicine = `Type of medicine`) %>% select(one_of(c('country', 'medicine')))
read.data$count <- 1


data <- do.call(rbind, by(read.data, read.data[,1], function(dd) {
	result <- dd %>% group_by(medicine) %>% summarise(total = sum(count))
	cbind(country = dd[1,1], result)
}))

rownames(data) <- NULL
# add iso2
data$iso2 <- countrycode(data$country, "country.name", "iso2c")
data <- data %>% group_by(country) %>% mutate(tot = sum(total))

# collapse countries
data <- do.call(rbind, by(data, data$country, function(dd) {
	dd <- dd[order(dd$total, decreasing = T),]
	ddd <- head(dd, nTopMedicine)
	data.frame(country = ddd[1,'country'], iso2 = ddd[1,'iso2'], total = ddd[1,'tot'], details = paste("<li>", paste(ddd$medicine, ddd$total, sep=":  "), "</li>", collapse = ""), stringsAsFactors = F)
}))

rownames(data) <- NULL


### geocode
library(ggmap)
# Geocode every country (concatenate country name with iso2 code)

latlon <- geocode(paste(data$country, data$iso2, sep = ", country="), output = c("latlon"), messaging = F)
data <- cbind(data, latlon)

# remove parenthesis and rep. abbreviations
naCountry <- gsub("Rep\\.", "Republic", gsub(" \\(.*\\)$", "", data[is.na(data$lat),'country']))
latlon2 <- geocode(paste(naCountry,  data[is.na(data$lat),'iso2'], sep = ", country="),  output = c("latlon"), messaging = F)
data[is.na(data$lat), c('lon', 'lat')] <- latlon2

# remove lines with NA lat lon
data <- data[!is.na(data$lat),]




######## MAP

fontSize <- "0.9em"

mb_tiles <- 'http://server.arcgisonline.com/ArcGIS/rest/services/Canvas/World_Light_Gray_Base/MapServer/tile/{z}/{y}/{x}'
mb_attribution <- paste0(" swissinfo.ch | source: source", '| Tiles &copy; Esri &mdash; Esri, DeLorme, NAVTEQ')
source <-

popup <- paste0('<strong>', data$country, "</strong><br>",
	  '<p style=\"font-size:', "1.15em", '\">',
	  "Total number of illegal drug shipments seized in Switzerland in 2014: ", "<strong>",  data$tot, "</strong><br></p>",
	  paste("Top", nTopMedicine, "categories of illegal drugs seized"),
	  '<p style=\"font-size:', fontSize, '\">', '<ul style="list-style-type:square">', data$details, "</ul></p>")


map <- leaflet(data = data) %>% addTiles(urlTemplate = mb_tiles, attribution = mb_attribution) %>%
  		addCircleMarkers(lng = ~lon, lat = ~lat, stroke = FALSE, fillOpacity = 0.6, fillColor = swi_rpal[5],
  	    radius = ~ sqrt(tot * 4), popup = popup) %>%
  		setView(6, 23, zoom = 3) %>%
		addLegend(position = "topright", title = "Map title", opacity = 0, colors = NULL, labels = NULL)

saveWidget(map, file = "illegalDrugs_en.html", selfcontained = FALSE, libdir = "leafletjs")

