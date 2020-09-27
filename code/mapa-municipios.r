# script invoked from within incumbent-reelection.r

## # OJO: when using spTranform in script, use line below for google earth, or next line for OSM/google maps
#x.map <- spTransform(x.map, CRS("+proj=longlat +datum=WGS84"))
#x.map <- spTransform(x.map, osm()) # project to osm native Mercator

# to use osm backgrounds
library(rJava)
library(OpenStreetMap)

# make discrete altitude variables and colors for maps
library(arules)
tmp <- censo$wmeanalt
tmp <- discretize(tmp, method = c("interval","frequency")[1], breaks = 9) # factors
censo$wmeanaltcat <- as.numeric(tmp)
tmp <- censo$wsdalt
tmp <- discretize(tmp, method = c("interval","frequency")[2], breaks = 9) # factors
censo$wsdaltcat <- as.numeric(tmp)
tmp <- censo$meanalt
tmp <- discretize(tmp, method = c("interval","frequency")[1], breaks = 9) # factors
censo$meanaltcat <- as.numeric(tmp)
tmp <- censo$sdalt
tmp <- discretize(tmp, method = c("interval","frequency")[2], breaks = 9) # factors
censo$sdaltcat <- as.numeric(tmp)
# make colors
library(RColorBrewer)
nclr <- 9                                    #CATEGORÍAS DE COLOR (MIN=3 MAX=9)
blues <- brewer.pal(nclr,"Blues")            #GENERA CODIGOS DE COLOR QUE CRECEN CON GRADO
reds <- brewer.pal(nclr,"Reds")              
yellows <- brewer.pal(nclr,"YlOrBr")         
library(plyr)
censo$wmeanaltcol <- mapvalues ( censo$wmeanaltcat, from = 1:9, to = yellows )
censo$wsdaltcol   <- mapvalues ( censo$wsdaltcat, from = 1:9, to = reds )
censo$meanaltcol <- mapvalues ( censo$meanaltcat, from = 1:9, to = yellows )
censo$sdaltcol   <- mapvalues ( censo$sdaltcat, from = 1:9, to = reds )
censo$wmeanaltcat <- censo$wsdaltcat <- censo$meanaltcat <- censo$sdaltcat <- NULL

# geospatial data 
library(spdep); # no longer available as of 22sep2020
library(maptools)
# used to determine what datum rojano data has
library(rgdal)
#gpclibPermit()

# state borders
md <- "/home/eric/Downloads/Desktop/data/mapas/entidad/estados2013/"
ed.map <- readOGR(dsn = md, layer = 'ESTADOS')
# projects to a different datum with long and lat
ed.map <- spTransform(ed.map, osm())

# read municipios
md <- "/home/eric/Downloads/Desktop/data/mapas/municipio/munInegi2013/"
mu.map <- readOGR(dsn = md, layer = 'MUNICIPIOS')
# projects to a different datum with long and lat
mu.map <- spTransform(mu.map, osm())
#
colnames(mu.map@data) <- c("edon","munn","mun")
mu.map$edon <- as.numeric(as.character(mu.map$edon))
mu.map$munn <- as.numeric(as.character(mu.map$munn))
mu.map$mun <-             as.character(mu.map$mun)
mu.map$ife <- mu.map$edon*1000 + mu.map$munn



## # read terrain ruggedness raster --- ATTEMPT TO READ DATA FROM RASTER USING MUN POLYGONS FAILED
## Useful resources:
## https://diegopuga.org/data/rugged/#grid
## https://luisdva.github.io/rstats/GIS-with-R/
## https://gis.stackexchange.com/questions/23212/how-to-calculate-the-average-value-of-a-raster-within-polygons
## https://gis.stackexchange.com/questions/92221/extract-raster-from-raster-using-polygon-shapefile-in-r/92227
## library(raster)
## library(rgeos)
## library(sp)
## #
## # get terrain ruggedness raster
## tmp.r <- raster("/home/eric/Dropbox/data/mapas/terrain-ruggedness/tri-clipped-mx.tif")
## proj4string(tmp.r) <- CRS("+init=epsg:3857")
## plot(tmp.r)
## plot(mu.map[mu.map$ife==1001,], add = TRUE)
## plot(mu.map[mu.map$ife==1001,])
## plot(tmp.r, add = TRUE)
## summary(mu.map)
## #
## summary(getValues(tmp.r, 50))/1000
## #
## # alternative approach
## library(stars)
## raster_extract = function(x, y, fun = NULL, na.rm = FALSE) {
##   x = as(x, "Raster")
##   y = as(y, "Spatial")
##   raster::extract(x = x, y = y, fun = fun, na.rm = na.rm)
## }
## md <- "/home/eric/Downloads/Desktop/data/mapas/municipio/munInegi2013/"
## tmp.m <- st_read(md, layer = 'MUNICIPIOS')
## plot(tmp.m)
## tmp <- read_stars("/home/eric/Dropbox/data/mapas/terrain-ruggedness/tri-clipped-mx.tif")
## #
## tmp2 <- raster_extract(tmp, tmp.m)
## tmp2[[2]]
## #
## tmp.m <-
##   tmp.m %>% mutate(
##     hfpMean = raster_extract(tmp, tmp.m), fun = mean, na.rm = TRUE),
##     hfpMax = raster_extract(tmp, tmp.m, fun = max, na.rm = TRUE),
##     hfpMin = raster_extract(tmp, tmp.m, fun = min, na.rm = TRUE)
##   )
##  tmp.m %>%
##      st_set_geometry(NULL)
## %>%
##      knitr::kable()
## #
## tmp.m <- within(tmp.m, hfpMax <- hfpMin <- hfpMean <- NULL)
## #
## raster_extract(tmp, tmp.m, fun = mean, na.rm = TRUE)
## plot(tmp)
## tmp


# merge census and altitude data
tmp1 <- mu.map@data
tmp1$ord <- 1:nrow(tmp1)
tmp2 <- censo; tmp2$inegi <- tmp2$edon <- NULL
tmp1 <- merge(x = tmp1, y = tmp2, all.x = TRUE, all.y = FALSE)
tmp1 <- tmp1[order(tmp1$ord),] # SORT AS IT WAS
mu.map@data <- tmp1 # return manipulated data

## # maybe useful
## wd2 <- c("~/Dropbox/data/elecs/MXelsCalendGovt/atlasDis/data/")
## setwd(wd2)
## load(file="elDatForMaps.RData")
## wd <- c("/home/eric/Downloads/Desktop/MXelsCalendGovt/reelec/data/")
## setwd(wd)


# shave map's bb to exclude pacific island 
shave <- function(m = NA, p = .5, s = 0, eastwest = TRUE){ # m is map to be shaved, p the rightmost part (share) to keep, -1<s<1 a shift rightward, eastwest=FALSE makes shift northsouth
    #m <- ed.map$col # duplicate
    m <- m; p <- p;
    dim <- ifelse(eastwest==TRUE, 1, 2) 
    b <- as.data.frame(m@bbox)
    b[dim,] <- b[dim,] - s*(b$max[dim] - b$min[dim])       # shift map rightward (bbox leftward)
    b$min[dim] <- b$max[dim] - p*(b$max[dim] - b$min[dim]) # keeps only 100*p% of horizontal length
    m@bbox <- as.matrix(b)
    #ed.map$col <- m
    return(m)
}
#
# (use 1984 long/lat for this map when mercator projection was chosen)
p84 <- function(x = NA){
    x <- x
    x <- spTransform(x, CRS("+proj=longlat +datum=WGS84"))
}
portray <- mu.map$wmeanaltcol  # elegir qué reportará el mapa
portray <- mu.map$wsdaltcol   # elegir qué reportará el mapa
portray <- mu.map$meanaltcol  # elegir qué reportará el mapa
portray <- mu.map$sdaltcol   # elegir qué reportará el mapa

setwd("/home/eric/Dropbox/data/elecs/MXelsCalendGovt/reelec/graph")
#pdf("map.pdf")
par(mar=c(0,0,2,0)) ## SETS B L U R MARGIN SIZES
#par(mar=c(2,2,2,1)) ## SETS B L U R MARGIN SIZES
plot(shave(p84(ed.map),  p = .93), col = "white", axes = TRUE, main = "sd(Altitud municipal)", bg = "lightblue")
#library(scales) # has function alpha()
#plot(p84(mu.map), add = TRUE, border = NULL, col = alpha(portray, .25)) # color nwin
plot(p84(mu.map), add = TRUE, border = portray, col = portray) # color nwin
# thick state border
plot(p84(ed.map), add = TRUE, lwd = 1)
#plot(p84(ed.map), add = TRUE, border = "red", lty = 3, lwd = 1.5)
#
# add neighboring states
## text( x = -117, y = 29.5, labels = "O C E A N O   P A C I F I C O", cex = .9, col = "deepskyblue", srt = -60 )
## text( x = -113.2, y = 30, labels = "M A R   D E   C O R T E Z", cex = .9, col = "deepskyblue", srt = -55 )
## text( x = -113.4, y = 27.9, labels = "B.C.S.", col = "darkgray", cex = .9 )
## text( x = -113.5, y = 31.8, labels = "SONORA", col = "darkgray", cex = .9, srt = -20 )
## text( x = -116, y = 32.75, labels = "EE.UU.", col = "darkgray", cex = .9, srt = 6 )
#dev.off()

