#### voorbeeld code voor inlezen shape files en parkeervakken data Amsterdam #################

library(sp)
library(maptools)
library(dplyr)
library(ggmap)
library(leaflet)
library(stringr)
library(dplyr)
library(tidyr)


### shape file van CBS OP buurt nivo, is ontzettend dicht op een kaart

# https://www.cbs.nl/nl-nl/dossier/nederland-regionaal/geografische%20data/wijk-en-buurtkaart-2018
CBSbuurten <- readShapeSpatial("Uitvoer_shape/buurt2018.shp")
CBSgem  <- readShapeSpatial("Uitvoer_shape/gem_2018.shp")

#vanuit opendata gemeente amsterdam
amsterdamparkeervakken <- read_csv("AmsterdamParkeerplekken.csv")

#### Zet coordinatensysteem
proj4string(CBSbuurten) <- CRS("+init=epsg:28992")

#### transformeer naar long /lat
CBSbuurten = spTransform(CBSbuurten, CRS("+proj=longlat +datum=WGS84"))


#### Het object is een zgn spatialpolygons object, daar zit een data frame in
CBSbuurten@data

amsterdamPC <- CBSbuurten[str_sub(CBSbuurten$POSTCODE,1,2) == "10" ,]
plot(amsterdamPC)

#ggplot variant
ggplot(amsterdamPC) +
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

# op een leaflet
pal <- colorQuantile(
  palette = "Reds",
  domain = amsterdamPC$P_GEHUWD, n=9)

### op leaflet maar dit is net te veel op buurt niveau
leaflet(amsterdamPC) %>%
  addTiles() %>%
  addPolygons(
    stroke = TRUE, weight = 1, fillOpacity = 0.35, smoothFactor = 0.15,
    popup = as.character(paste(amsterdamPC$BU_NAAM, amsterdamPC$P_GEHUWD)),
    color = ~pal(P_GEHUWD)
)

########## info over ingelezen parkeervakken #########
stadsdeel = amsterdamparkeervakken %>% group_by(stadsdeel) %>%  summarise(n=n())
buurt = amsterdamparkeervakken %>% group_by(buurtcode) %>%  summarise(n=n())

## in WKT staan polygonen met coordinaten die ik er uit wil halen 
y = tidyr::separate(
  amsterdamparkeervakken %>%
    mutate(WKT2 = str_remove(WKT, "POLYGON")) %>% 
    select(WKT2) , 
  WKT2, c("kol", "kol2"), sep = ",")

x = str_extract_all(y$kol2, "\\d+\\.\\d+", simplify = TRUE) %>% as_tibble() 
x$V1 = as.numeric(x$V1)
x$V2 = as.numeric(x$V2)

amsterdamparkeervakken = bind_cols(amsterdamparkeervakken, x)

# nu nog transformeren naar long lat
tmp = rd_to_wgs84(amsterdamparkeervakken$V1, amsterdamparkeervakken$V2)
amsterdamparkeervakken$lat = tmp$phi
amsterdamparkeervakken$long = tmp$lambda

#check met leaflet
amsterdamparkeervakken %>% sample_frac(0.0051) %>% 
leaflet() %>% addTiles() %>% addCircleMarkers(lng=~long, lat = ~lat, popup = ~straatnaam, radius = 1)


## nu kan je met 'over' bepalen in welke CBS buurt een parkeerplek ligt
adampvakken = amsterdamparkeervakken %>% filter(!is.na(long))
coordinates(adampvakken) <- ~long + lat
proj4string(adampvakken) = CRS("+proj=longlat +datum=WGS84")

tmp = sp::over(adampvakken, amsterdamPC)

#combineer nu 

adampvakken = bind_cols(
  adampvakken@data,
  adampvakken@coords %>% as.data.frame,
  tmp
)

############ nu kan je op CBS BU_NAAM aggregegeren
adampvakkenBU = adampvakken %>% group_by(BU_NAAM) %>%  summarise(nplekken = n())

### join met amsterdamPC zodat je het op een leaflet kan zetten

tmp = amsterdamPC@data
tmp2 = tmp %>% left_join(adampvakkenBU)
tmp2 = tmp2 %>%
  mutate(
    parkeerperc = nplekken / AANT_INW
  )

tmp2 = tmp2 %>% mutate(
  parkeerperc = ifelse(parkeerperc > 1, NA, parkeerperc ),
  parkeerperc = ifelse(AANT_INW < 600, NA, parkeerperc )
)
amsterdamPC@data = tmp2

# op een leaflet
pal <- colorQuantile(
  palette = "Greens",
  domain = amsterdamPC$parkeerperc, n=9)

### op leaflet maar dit is net te veel op buurt niveau
ptekst = paste(amsterdamPC$BU_NAAM, "<br>",
               "aantal plekken " , amsterdamPC$nplekken, "<br>",
               "aantal inwoners ", amsterdamPC$AANT_INW, "<br>",
               "percentage ", round(amsterdamPC$parkeerperc,2))
leaflet(amsterdamPC) %>%
  addTiles() %>%
  addPolygons(
    stroke = TRUE, weight = 1, fillOpacity = 0.35, smoothFactor = 0.15,
    popup = ptekst,
    color = ~pal(parkeerperc)
  )

tmp2 %>%  filter(AANT_INW > 500) %>% 
ggplot(aes(x=parkeerperc)) + geom_histogram(bins=30, col = "black")

tmp2 %>%  filter(P_GEHUWD > 0) %>% 
ggplot( aes(x=P_GEHUWD, y = nplekken)) + geom_point() + geom_smooth()

tmp2 %>%  filter(AANT_INW > 500) %>% 
  ggplot( aes(x=AANT_INW, y = nplekken)) + geom_point() + geom_smooth()
