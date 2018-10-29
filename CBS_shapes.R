#### inlezen shape file
library(sp)
library(maptools)
library(dplyr)
library(ggmap)
library(leaflet)

### shape file van CBS OP buurt nivo, is ontzettend dicht op een kaart
# https://www.cbs.nl/nl-nl/dossier/nederland-regionaal/geografische%20data/wijk-en-buurtkaart-2018
tmp <- readShapeSpatial("Uitvoer_shape/buurt2018.shp")

tmp <- readShapeSpatial("Uitvoer_shape/gem_2018.shp")

#### Zet coordinatensysteem
proj4string(tmp) <- CRS("+init=epsg:28992")

#### transformeer naar long /lat
tmp = spTransform(tmp, CRS("+proj=longlat +datum=WGS84"))


#### Het object tmp is een zgn spatialpolygons object, daar zit een data frame in
tmp@data


#### maak een hele simpele plot
# traditional plot
plot(tmp)

#ggplot variant
ggplot(tmp) +
  geom_polygon(
    aes(x = long, y = lat, group = group),
    color = "grey23", 
    fill = NA
  )

# met google maps 
NLMap = ggmap(get_googlemap(center = c(4.8952,52.3702), scale=2, zoom=7), extent="normal")
NLMap +  geom_polygon( 
  data = tmp,
  aes(x = long, y = lat, group = group),
  color = "grey", 
  fill = NA
)


### op leaflet maar dit is net te veel op buurt niveau
leaflet(tmp) %>%
  addTiles() %>%
  addPolygons(
    stroke = TRUE, weight = 1, fillOpacity = 0.15, smoothFactor = 0.15,
    popup = as.character(tmp$AANT_INW)
  )






