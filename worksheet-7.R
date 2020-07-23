
#https://www.youtube.com/embed/1ssfmeEMPgQ?vp=1080
## Vector Data

library(sf)
#sf package reads shapefiles
#if GEOS GDAL or PROJ were not already loaded you would need to load them

shp <- 'data/cb_2016_us_county_5m'
counties <- st_read(
    shp,
    stringsAsFactors = FALSE)
#multipolygon object with attr

#manually create spatial objects of the sfc class
#do this for a specific point showing sesync's lat/long
#create point with st_point and specify crs as the same one as counties
sesync <- st_sfc(
    st_point(c(-76.503394, 38.976546)),
    crs = st_crs(counties))

## Bounding box
#another way to get info about the min/max x/y coord that contain all features in df
library(dplyr)
#We can use dplyr with sf
#here we are filtering on the counties object by the condition that the statefp col = 24
#filter is basically select by attribute
counties_md <- filter(
    counties,
    STATEFP == '24'
)

## Grid
# the bounding box is just an object, not geom that can be plotted
#we can make a grid that can be plotted around sf project

grid_md <- st_make_grid(counties_md, n=4)
#error, reminder that we are working with unproj lat/lon coordinates
#given the small scale we can ignore this, but for larger scales we want
#to use projected coord. system


#https://www.youtube.com/embed/lNJQDqFw_Mw?vp=1080
## Plot Layers
plot(grid_md)
#now plot counties_md on top, but only the 'ALAND' col. and the fill color will correspond to aland
#use add=TRUE to plot on top of grid
plot(counties_md['ALAND'], add = TRUE)
#plot point
# color is green, pch is a filled circle
plot(sesync, col = "green", pch = 20, add=TRUE)


#str_within is basically select by location
#it filters by geometry
st_within(sesync, counties_md)
#returns 5, sesync is located in the polygon corresponding to the 5th row (Ann Arrundel county)

## Coordinate Transforms
shp <- 'data/huc250k'
huc <- st_read(
    shp,
    stringsAsFactors = FALSE)

#this file has a different projection, so we need to transform before plotting
# if we want to transform one spatial object (SO) from one coord system to another
#we use the function st_transform
#arguments can be either numeric or character
#numeric could be a epsg code, 4-5 digit code corresponding to common projs
#if character is must be in special proj 4 format (can find online)
prj <- '+proj=aea +lat_1=29.5 +lat_2=45.5 \
    +lat_0=23 +lon_0=-96 +x_0=0 +y_0=0    \
    +ellps=GRS80 +towgs84=0,0,0,0,0,0,0   \
    +units=m +no_defs'

#assign three layers to common projection string of prj
#do seperately for every layer, may take time
counties_md <- st_transform(
    counties_md,
    crs = prj)
huc <- st_transform(huc, crs = prj)
sesync <- st_transform(sesync, crs = prj)

#now plot
#plot only geomotry from counties, which means only border - not fill
plot(counties_md$geometry)
plot(huc$geometry,
     border = 'blue', add = TRUE)
plot(sesync, col = 'green',
     pch = 20, add = TRUE)


#https://www.youtube.com/embed/9Y0H1bv3vmQ?vp=1080
## Geometric Operations
#just include watershed boundaries within MD
#clip, 2 step process
state_md <- st_union(counties_md) #combine all counties to only get border of MD
plot(state_md)

huc_md <- st_intersection(huc, state_md)
#error, does not take into account recalculation of attr in polygon
#i.e. if you had area of huc polygons, the value would not be recalculated

plot(huc_md, border = 'blue',
     col = NA, add = TRUE)

#https://www.youtube.com/embed/jV4kCUVhKEM?vp=1080
## Raster Data
library(raster)
library(sp)
# grd formatted data
nlcd <- raster('data/nlcd_agg.grd')
nlcd #get metadata
plot(nlcd) #land cover

## Crop
#trim out piece of raster to get smaller raster
#extent object will be a matrix of xy coord based on bbox of huc_md
#2x2 matrix, coerice bbox into numeric matrix so {raster} can work with it
extent <- matrix(st_bbox(huc_md), nrow = 2)
#create the raster using crop function with the first argument being the raster
#file and the second being the new extent
nlcd <- crop(nlcd, extent)
plot(nlcd)
#plot vector over raster
plot(huc_md, col = NA, add = TRUE)

## Raster data attributes
#manually extractvalues from raster at particular pixels
#1,1 is lower left corner
nlcd[1,1] #the value is 41, but what LC is that?
head(nlcd@data@attributes[[1]]) #looking at first element in attributes list, which is attr table
#IF is LCC, 0 is unclassified
#pull out ID into an object in our workspace to make a vector of LCC that we can
#then use to index numerical codes and get names
nlcd_attr <- nlcd@data@attributes
lc_types <- nlcd_attr[[1]]$Land.Cover.Class
#later we would have to change numbers bc indexing starts at 1, but attr starts at 0
levels(lc_types)


## Raster math
# get only pasture pixels
pasture <- mask(nlcd, nlcd == 81,
    maskvalue = FALSE)
#mask value = false will set all other pixels to NA
plot(pasture)

#reduce resolution if file is too big
#fact is factor of 25, so 25x25 pixel blocks will be reduced to single pixel
#use the mode to aggregate, so new pixel will have mode value of 25x25 block
nlcd_agg <- aggregate(nlcd,
    fact = 25,
    fun = modal)
#supply legend from original raster
nlcd_agg@legend <- nlcd@legend
plot(nlcd_agg)


#https://www.youtube.com/embed/5_TgGJQQ8yc?vp=1080
## Mixing rasters and vectors
plot(nlcd)
plot(sesync, col = 'green',
     pch = 16, cex = 2, add = TRUE)

#what is the lc at the point?
#first argument is nldc - raster we are using
#second argument is the point, but we need to use a second argument, st_coordinates
#this coerices to a numeric matrix, for a single pt it needs to be numeric matrix
sesync_lc <- extract(nlcd, st_coordinates(sesync))
#this gives us 23, to find LC class of 23 use indexing
#use lc descriptive names we created earlier, but remember to add 1 bc indexing starts at 1
lc_types[sesync_lc + 1]

#extract from polygon instead of point, we will get vector of all pixel values
county_nlcd <- extract(nlcd_agg, counties_md[1,])
table(county_nlcd) #freq dist. of land cov. types
#summary of raster values from each polygon, what if we wanted modal value at each polygon
modal_lc <- extract(nlcd_agg,huc_md, fun = modal)
#we can use mutate to add a modal lc type as a col to df
#we index so we get actual descriptive name of lc type
huc_md <- huc_md %>%
    mutate(modal_lc = lc_types[modal_lc + 1])
#most common land cover type in each watershed as col modal_lc
huc_md


#https://www.youtube.com/embed/wW-tTtHhfZ0?vp=1080
## Leaflet
library(leaflet)
#interfaces with javascript library by the same name
#leave blank to use default
#center with lng lat
#typically you would imbed in webpage for viewers ot look at
leaflet() %>%
    addTiles() %>%
    setView(lng = -77, lat = 39, 
        zoom = 7)

#add polygons, use epsg code 4236 and transform huc_md so it has the same coord
#leaflet uses lat lon so you need to transform huc_md into lat lon
leaflet() %>%
    addTiles() %>%
    addPolygons(
        data = st_transform(huc_md, 4236)) %>%
    setView(lng = -77, lat = 39, 
        zoom = 7)

#add real time data
leaflet() %>%
    addTiles() %>%
    addWMSTiles(
        "http://mesonet.agron.iastate.edu/cgi-bin/wms/nexrad/n0r.cgi",
        layers = "nexrad-n0r-900913", group = "base_reflect",
        options = WMSTileOptions(format = "image/png", transparent = TRUE),
        attribution = "weather data © 2012 IEM Nexrad") %>%
    setView(lng = -77, lat = 39, 
        zoom = 7)
