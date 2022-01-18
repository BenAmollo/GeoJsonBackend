#
# This is a Plumber API. You can run the API by clicking
# the 'Run API' button above.
#
# Find out more about building APIs with Plumber here:
#
#    https://www.rplumber.io/
#

library(plumber)

library(sf)
library(rgdal)
library(geojsonio)
library(rspatial)
library(raster)
library(ggplot2)
library(leaflet)
library("rjson")
library(jsonlite)

#* @apiTitle Plumber Example API
#* 
#* 

Shapefile <- st_read("E:/Spatial Data/R Stuff/R API/rdata/Data/Shp/Sample_Shapefile_prj2.shp")
head(Shapefile)

#Convert Shapefile to GeoJson
convertShapeFile <- function(shapeFile){
  GeoJson <- geojson_json(shapeFile)
  newGeoJson <- fromJSON(GeoJson)
  return(newGeoJson)
}

#* Get Geojson for parcels
#* @param shapeType The type of search you want e.g [OWNER,PARCELNO,PRICE,STATUS,DESC,AREA ]
#* @param value The parameter in question e.g. ['David Njau','NBI BLK 126/544 (NGUNDU FARMERS)', '2500000', 'SOLD/RESERVED', 'Corner Plot', '0.2' ]
#* @param operation This is only required for PRICES e.g. ['LESS', 'EQUAL', 'GREATER']
#* @serializer unboxedJSON
#* @get /get-parcel-geojson/
function(shapeType, value, operation) {
  
  #NBI BLK 126/544 (NGUNDU FARMERS)
  
  x = searchShapeFile(shapeType, value, operation)
  return(x)
}

searchShapeFile = function(shapeType, value, operation){
  
  if(shapeType == "OWNER"){
    SpatialData_Subset <- Shapefile[Shapefile$Owner %in% value,]
  }else if(shapeType == "PARCELNO"){
    SpatialData_Subset <- Shapefile[Shapefile$PARCEL_NO %in% value,]
  }else if(shapeType == "PRICE"){
    
    price = as.numeric(value)
    SpatialData_Subset <- priceResults(price, operation)
    
  }else if(shapeType == "STATUS"){
    SpatialData_Subset = Shapefile[Shapefile$Status == value,]
  }else if(shapeType == "DESC"){
    SpatialData_Subset <- Shapefile[Shapefile$Descriptio %in% value,]
  }else if(shapeType == "AREA"){
    
    area = as.double(value)
    SpatialData_Subset <- Shapefile[Shapefile$AREA_HA %in% area,]
  }
  
  
  newShapeFile = convertShapeFile(SpatialData_Subset)
  return(newShapeFile)
  
}

priceResults = function(value, operation){
  
  if(operation == "EQUAL"){
    SpatialData_Subset <- subset(Shapefile, Price == value)
  }else if(operation == "LESS"){
    SpatialData_Subset <- subset(Shapefile, Price < value)
  }else if(operation == "GREATER"){
    SpatialData_Subset <- subset(Shapefile, Price > value)
  }
  
}









