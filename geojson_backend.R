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

#* @apiTitle Spatial Data Query and Display API
#* 
#* 

Parcels <- st_read("E:/Spatial Data/R Stuff/R API/rdata/Data/Shp/Sample_Shapefile_prj2.shp")
Schools <- st_read("E:/Spatial Data/Vector data/Kenya_Data/prj_Data/School2_prj.shp")
Health_Facilities <- st_read("E:/Spatial Data/Vector data/Kenya_Data/prj_Data/Health_Facilitie2_prj.shp")
East_Kwa_Mabeast <- st_read("E:/Spatial Data/Vector data/Kenya_Data/East_Kwa_Mabeast.shp")
Wards <- st_read("E:/Spatial Data/Vector data/Kenya_Data/prj_Data/Ward2_prj.shp")



#Transform function something
Transform = function(Layer2Transform, crs){
  Transformed <- st_transform(Layer2Transform, crs=(crs = 4326))
  return(Transformed)
}

ParcelsTransformed <- Transform(Parcels, crs)
SchoolsTransformed <- Transform(Schools, crs)
Health_FacilitiesTransformed <- Transform(Health_Facilities, crs)
East_Kwa_MabeastTransformed <- Transform(East_Kwa_Mabeast, crs)
WardsTransformed <- Transform(Wards, crs)


#Convert Shapefile to GeoJson
convertShapeFile <- function(shapeFile){
  GeoJson <- geojson_json(shapeFile)
  newGeoJson <- fromJSON(GeoJson)
  return(newGeoJson)
}

# Subset ward data to individual ward
wardSubset = function(Shapefile, wardName){
  Subset = subset(Shapefile,wardName == ward)
  return(Subset)
}


#* Get Geojson for parcels
#* @param shapeType The type of search you want e.g [OWNER,PARCELNO,PRICE,STATUS,DESC,AREA ]
#* @param value The parameter in question e.g. ['David Njau','NBI BLK 126/544 (NGUNDU FARMERS)', '2500000', 'SOLD/RESERVED', 'Corner Plot', '0.2' ]
#* @param operation This is only required for PRICES e.g. ['LESS', 'EQUAL', 'GREATER']
#* @serializer unboxedJSON
#* @get /get-parcel-geojson/
function(shapeType, value, operation) {
    Parcel = searchShapeFile(shapeType, value, operation)
  return(Parcel)
}

searchShapeFile = function(shapeType, value, operation){
  
  if(shapeType == "OWNER"){
    ParcelData_Subset <- Parcels[Parcels$Owner %in% value,]
  }else if(shapeType == "PARCELNO"){
    ParcelData_Subset <- Parcels[Parcels$PARCEL_NO %in% value,]
  }else if(shapeType == "PRICE"){
    price = as.numeric(value)
    ParcelData_Subset <- priceResults(price, operation)
  }else if(shapeType == "STATUS"){
    ParcelData_Subset = Parcels[Parcels$Status == value,]
  }else if(shapeType == "DESC"){
    ParcelData_Subset <- Parcels[Parcels$Descriptio %in% value,]
  }else if(shapeType == "AREA"){
    
    area = as.double(value)
    ParcelData_Subset <- Parcels[Parcels$AREA_HA %in% area,]
  }else{
    
    ParcelData_Subset = Parcels
  }
  
  
  Parcel_Subset = convertShapeFile(ParcelData_Subset)
  return(Parcel_Subset)
  
}

### Select by Price Operations
priceResults = function(value, operation){
  
  if(operation == "EQUAL"){
    SpatialData_Subset <- subset(ShapeFile, Price == value)
  }else if(operation == "LESS"){
    SpatialData_Subset <- subset(ShapeFile, Price < value)
  }else if(operation == "GREATER"){
    SpatialData_Subset <- subset(ShapeFile, Price > value)
  }else{
    
    SpatialData_Subset = ShapeFile
  }
  
}



#* Get Geojson for Health Facilities
#* @param valueType The type of search you want e.g [OWNER,NAME,COUNTY,TYPE]
#* @param uniqueValue The parameter in question e.g. ['MoH','Frontier Health Services Clinic', 'Busia', 'Dispensary']
#* @serializer unboxedJSON
#* @get /get-health-facilities-geojson/
function(valueType, uniqueValue) {
    Health_Facility = hospitalValues(valueType, uniqueValue)
  return(Health_Facility)
}
#Return all the specific values under a given Health facility
hospitalValues = function(valueType, uniqueValue){
  
  if(valueType == "OWNER"){
    HF_data = subset(Health_Facilities, Ownership==uniqueValue)
  }else if(valueType == "NAME"){
    HF_data = subset(Health_Facilities, Facility_n==uniqueValue)
  }else if(valueType == "COUNTY"){
    HF_data = subset(Health_Facilities, Admin1==uniqueValue)
  }else if(valueType == "TYPE"){
    HF_data = subset(Health_Facilities, Facility_t==uniqueValue)
  }else{
    HF_data = Health_Facilities
  } 
  
  HF_data_Subset = convertShapeFile(HF_data)
  return(HF_data_Subset)
}


#* Get Geojson for Schools
#* @param valueType The type of search you want e.g [NAME,LEVEL, STATUS,COUNTY,ZONE, SUB_COUNTY, WARD]
#* @param uniqueValue The parameter in question e.g. ['St Michaels','Primary', 'Busia', 'Makadara', 'Makadara', Hamza]
#* @serializer unboxedJSON
#* @get /get-schools-geojson/
function(valueType, uniqueValue) {
  School = schoolValues(valueType, uniqueValue)
  return(School)
}

schoolValues = function(valueType, uniqueValue){
  
  if(valueType == "NAME"){
    sch_data = subset(Schools, SCHOOL_NAM==uniqueValue)
  }else if(valueType == "LEVEL"){
    sch_data = subset(Schools, LEVEL==uniqueValue)
  }else if(valueType == "STATUS"){
    sch_data = subset(Schools, Status==uniqueValue)
  }else if(valueType == "COUNTY"){
    sch_data = subset(Schools, County==uniqueValue)
  }else if(valueType == "ZONE"){
    sch_data = subset(Schools, ZONE==uniqueValue)
  }else if(valueType == "SUB_COUNTY"){
    sch_data = subset(Schools, SUB_COUNTY==uniqueValue)
  }else if(valueType == "WARD"){
    sch_data = subset(Schools, Ward==uniqueValue)
  }else{
    sch_data = subset(Schools, Status=="Public")
    
  } 
  
  sch_data_Subset = convertShapeFile(sch_data)
  return(sch_data_Subset)
}



#* Get Geojson for Wards
#* @param wardName The Ward you want to search e.g [North Kamagambo Ward]
#* @serializer unboxedJSON
#* @get /get-wards-geojson/
function(wardName) {
  WardData = WardSearch(shapeFile, wardName)
  return(WardData)
}

WardSearch = function(shapeFile, wardName){
  WardData_Subset <- Wards[Wards$ward %in% wardName,]
  #data = subset(Wards, ward==wardName)
  transFormedData = Transform(WardData_Subset)
  WardDataJS = convertShapeFile(transFormedData)
  return(WardDataJS)
  
}


#Get a specific ward details by Name
WardData = function(shapeFile, wardName){
  
  data = subset(shapeFile, ward==wardName)
  transFormedData = Transform(data)
  WardDataJS = convertShapeFile(transFormedData)
  WardDataJSdf = data.frame(t(sapply(WardDataJS,c)))
  #data.frame(t(sapply(mylistlist,c)))
  return(WardDataJSdf)
  
}


#* Get Geojson for Wards Health Facilities
#* @param wardName The type of search you want e.g [North Kamagambo Ward]
#* @serializer unboxedJSON
#* @get /get-health-facilities-in-a-specific-ward-geojson/
WardHealthFac = function(wardName){
  ward = wardSubset(Wards, wardName)
  WarDTransformed = Transform(ward, crs)
  Ward_Facilty = st_intersection(HealthTransformed, st_union(WarDTransformed))
  Ward_Facilty_Subset = convertShapeFile(Ward_Facilty)
  return(Ward_Facilty_Subset)
}



#* Get Geojson for Wards Schools
#* @param wardName The type of search you want e.g [Makongeni Ward]
#* @serializer unboxedJSON
#* @get /get-schools-in-a-specific-ward-geojson/
wardSchool = function(wardName){
  ward = wardSubset(Wards, wardName)
  WarDTransformed = Transform(ward, crs)
  Ward_School <- st_intersection(SchoolsTransformed, st_union(WarDTransformed))
  Ward_School_Subset = convertShapeFile(Ward_School)
  
  return(Ward_School_Subset)
}

#-1.2895171362243985, 36.901310823514464


#* Get Geojson for Schools within a buffer
#* @param bufferDistance The distance which you want to buffer e.g [1000meters]
#* @serializer unboxedJSON
#* @get /get-schools-in-a-buffer-area/
Buffer = function(bufferDistance, latitude, longitude, bufferType){
  
  Point <- data.frame(
    lat = c(latitude),     
    lon = c(longitude))
  #Convert hysplit data frame to an sf object
  Pointsf <- st_as_sf(Point, coords = c("lon", "lat"), crs=4326)
  
  #Buffer the points
  bufferDistance = as.double(bufferDistance)
  buffer <- st_buffer(Pointsf, dist = bufferDistance)
  Bufferjs = convertShapeFile(buffer)
  Buffer_df = data.frame(t(sapply(Bufferjs,c)))
  
  if(bufferType == "SCHOOLS"){
    #Finding Schools within  buffer
    fac = st_intersection(SchoolsTransformed, st_union(buffer))
    fac_in_buffer = convertShapeFile(fac)
  }else if(bufferType == "HEALTH_FACILITY"){
    #Finding Schools within  buffer
    fac = st_intersection(Health_FacilitiesTransformed, st_union(buffer))
    fac_in_buffer = convertShapeFile(fac)
  }else{
    #Finding Schools within  buffer
    fac = st_intersection(SchoolsTransformed, st_union(buffer))
    fac_in_buffer = convertShapeFile(fac)
  }
  
 
  return(fac_in_buffer)
}





