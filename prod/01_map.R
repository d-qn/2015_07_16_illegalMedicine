library(countrycode)
library(tidyr)
library(dplyr)
library(swiTheme)
library(swiMap)
library(leaflet)
library(htmlwidgets)


############################################################################################
###		SETTINGS
############################################################################################

nTopMedicine <- 5
trad.file <- "trad.csv"
trad_tmp.file <- "trad_tmp.csv"
data.file <- "Illegal-medicine-imports-Swismedic_cleaned.csv"


############################################################################################
###		LOAD DATA
############################################################################################

trad <- read.csv(trad.file, row.names = 1, stringsAsFactors = F)

read.data <- read.csv(data.file, check.names = F)
read.data <- rename(read.data, country = `Shipment origin`, medicine = `Type of medicine`) %>% select(one_of(c('country', 'medicine')))
read.data$count <- 1


# count by country
data <- do.call(rbind, by(read.data, read.data[,1], function(dd) {
	result <- dd %>% group_by(medicine) %>% summarise(total = sum(count))
	cbind(country = dd[1,1], result)
}))

rownames(data) <- NULL

# add iso2
data$iso2 <- countrycode(data$country, "country.name", "iso2c")
data <- data %>% group_by(country) %>% mutate(tot = sum(total))

# collapse countries in df
df <- do.call(rbind, by(data, data$country, function(dd) {
	dd <- dd[order(dd$total, decreasing = T),]
	data.frame(country = dd[1,'country'], iso2 = dd[1,'iso2'], total = dd[1,'tot'])
}))

# details of top categories
details <- do.call(rbind, by(data, data$country, function(dd) {
	dd <- dd[order(dd$total, decreasing = T),]
	ddd <- head(dd, nTopMedicine)
	data.frame(iso2 = ddd[1,'iso2'], medicine = ddd$medicine, count  = ddd$total)
}))
rownames(details) <- NULL

# discard NA iso2
df <- df[!is.na(df$iso2),]
details <- details[!is.na(details$iso2),]


############################################################################################
###		Build translation file
############################################################################################

medicine <- as.character(unique(details$medicine))
details$medicine <-  gsub(" ", "_", details$medicine)
tmp.trad <- data.frame( code = gsub(" ", "_", medicine), en = medicine)
write.csv(tmp.trad, row.names = F, file = trad_tmp.file)

### geocode
library(ggmap)
# Geocode every country (concatenate country name with iso2 code)

latlon <- geocode(paste(df$country, df$iso2, sep = ", country="), output = c("latlon"), messaging = F)
df <- cbind(df, latlon)

# remove parenthesis and rep. abbreviations
naCountry <- gsub("Rep\\.", "Republic", gsub(" \\(.*\\)$", "", df[is.na(df$lat),'country']))
if(length(naCountry) > 0) {
	latlon2 <- geocode(paste(naCountry,  df[is.na(df$lat),'iso2'], sep = ", country="),  output = c("latlon"), messaging = F)
	df[is.na(df$lat), c('lon', 'lat')] <- latlon2
}

# remove lines with NA lat lon
df <- df[!is.na(df$lat),]

############################################################################################
###		MAP
############################################################################################

fontSize <- "0.9em"
mb_tiles <- 'http://server.arcgisonline.com/ArcGIS/rest/services/Canvas/World_Light_Gray_Base/MapServer/tile/{z}/{y}/{x}'

i <- 1

for (i in 1:ncol(trad)) {

	df.l <- df

	lang <- colnames(trad)[i]
	output.html <- paste("illegalDrugs_", lang, ".html", sep ="")
	df.l$geo <- countryTranslation(df$iso2, toupper(lang))[,2]

	details$cat <- as.character(trad[match(details$medicine, rownames(trad)), lang])

	details.l <- by(details, details$iso2, function(dd) {
		paste("<li>", dd$cat, ":  ", dd$count, "</li>", collapse ="", sep = "")
	}, simplify = F)

	stopifnot(df.l$iso2 %in% names(details.l))
	# map details to df
	df.l$details <- unlist(details.l[match(df.l$iso2, names(details.l))])

	mb_attribution <- paste0(" swissinfo.ch | ", trad["source",lang] , ' | Tiles &copy; Esri &mdash; Esri, DeLorme, NAVTEQ')

	popup <- paste0('<h3>', df.l$geo, "</h3>",
		  '<p style=\"font-size:', "1.15em", '\">',
		  trad['popup_1', lang], " ", "<strong>",  df.l$tot, "</strong><br></p>",
		  trad['popup_2', lang],
		  '<p style=\"font-size:', fontSize, '\">', '<ul style="list-style-type:square">', df.l$details, "</ul></p>")

	map <- leaflet(data = df.l) %>% addTiles(urlTemplate = mb_tiles, attribution = mb_attribution) %>%
	  		addCircles(lng = ~lon, lat = ~lat, stroke = FALSE, fillOpacity = 0.6, fillColor = swi_rpal[5],
	  	    radius = ~ sqrt(tot) * 10^4 * 4, popup = popup) %>%
	  		setView(6, 23, zoom = 3) %>%
			addLegend(position = "topright", title = trad['map_title', lang], opacity = 0, colors = NULL, labels = NULL)

	saveWidget(map, file = output.html, selfcontained = FALSE, libdir = "leafletjs")

}






