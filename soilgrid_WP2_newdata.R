#Extracting soild data for new datasets 2025
#Use same path: soilgrid_WP2.R 

# 1. Open NEFINEO_MS project #####

# 1.1 Open datasets
new.diam= read.csv("Data/new.diam.csv")
new.dryf= read.csv("Data/new.dryf.csv")
new.mymo= read.csv("Data/new.mymo.csv") 
new.spun= read.csv("Data/new.spun.csv")
new.long= read.csv("Data/new.long.csv")

# 1.2 Extract coordinates for each dataset
#new.diam$latitude, new.diam$longitude #mal escritas
#xy_mymo= data.frame(lat= new.mymo$Latitude, lon=new.mymo$Longitude) #no ok
xy_long= data.frame(Latitude= new.long$Latitude, Longitude=new.long$Longitude) #ok
xy_long= data.frame(new.ID=new.long$new.ID, lapply(xy_long, function(x) round(x,3)))# round 3 digits
xy_spun= data.frame(Latitude= new.spun$decimalLatitude, Longitude=new.spun$decimalLongitude) #decimals
xy_spun= data.frame(new.ID=new.spun$new.ID, lapply(xy_spun, function(x) as.numeric(gsub(",", ".", x)))) #replace "," by "." 
xy_dryf= data.frame(new.ID=new.dryf$new.ID, Latitude= new.dryf$y, Longitude=new.dryf$x)
#xy_diam= data.frame(new.ID=new.dryf$new.ID, Latitude= new.diam$latitude, Longitude=new.diam$longitude)

sites= rbind(xy_long, xy_spun, xy_dryf) #all coords

# 2. Open and install packages ####   
# Packages we will need
packages <- c("geosphere", "raster", "sf", "stats", "sp")
library(geosphere)
library(stats)

# 3. Identify clustered sites #### 
# 3.1. Calculate distance matrix between sites
x= sites[,c(2,3)]
dist= distm(x, fun = distHaversine)

# 3.2. Calculate hierarchical clustering 
clustering= hclust(as.dist(dist), method = "complete")

# 3.3. Cut hierarchical tree at 150000m 
groups= cutree(clustering, h = 150000)

# 3.4. Add groups as a new column
sites$groups= groups

# 4. Downloading individual raster by group and soil property ####

# 4.1 Vector of group number to loop
n.group= 1:max(sites$groups)

# 4.2 Downloading raster using mean geographical coordinates into each group
# NITROGEN
for (i in n.group) {
  lon= mean(subset(sites,groups== i)$Longitude)
  lat= mean(subset(sites,groups== i)$Latitude)
  
  lon_min= lon -1
  lon_max= lon +1
  
  lat_min= lat -1
  lat_max= lat +1
  
  id= paste0("Group",sep="_",n.group[i])
  
  attribute= "nitrogen.map"
  
  layer= "nitrogen_0-5cm_mean"
  
  url= paste0("https://maps.isric.org/mapserv?map=/map/",attribute,
              "&SERVICE=WCS&VERSION=2.0.1&REQUEST=GetCoverage&COVERAGEID=",layer,
              "&FORMAT=image/tiff&SUBSET=long(",lon_min,",",lon_max,")&",
              "SUBSET=lat(",lat_min,",",lat_max,")&",
              "SUBSETTINGCRS=http://www.opengis.net/def/crs/EPSG/0/4326",
              "&OUTPUTCRS=http://www.opengis.net/def/crs/EPSG/0/4326")
  
  wd.file= paste0("C:/Users/Usuario/Documents/SoilGrids_/newdata/",
                  paste(layer,id, sep="_"),".tif")
  
  download.file(url, destfile= wd.file, mode = 'wb', timeout = 120)
}

# SAND
for (i in n.group) {
  lon= mean(subset(sites,groups== i)$Longitude)
  lat= mean(subset(sites,groups== i)$Latitude)
  
  lon_min= lon -1
  lon_max= lon +1
  
  lat_min= lat -1
  lat_max= lat +1
  
  id= paste0("Group",sep="_",n.group[i])
  
  attribute= "sand.map"
  
  layer= "sand_0-5cm_mean"
  
  url= paste0("https://maps.isric.org/mapserv?map=/map/",attribute,
              "&SERVICE=WCS&VERSION=2.0.1&REQUEST=GetCoverage&COVERAGEID=",layer,
              "&FORMAT=image/tiff&SUBSET=long(",lon_min,",",lon_max,")&",
              "SUBSET=lat(",lat_min,",",lat_max,")&",
              "SUBSETTINGCRS=http://www.opengis.net/def/crs/EPSG/0/4326",
              "&OUTPUTCRS=http://www.opengis.net/def/crs/EPSG/0/4326")
  
  wd.file= paste0("C:/Users/Usuario/Documents/SoilGrids_/newdata/",
                  paste(layer,id, sep="_"),".tif")
  
  download.file(url, destfile= wd.file, mode = 'wb', timeout = 120)
}

# CEC: Cation exchange capacity (at ph 7)
for (i in n.group) {
  lon= mean(subset(sites,groups== i)$Longitude)
  lat= mean(subset(sites,groups== i)$Latitude)
  
  lon_min= lon -1
  lon_max= lon +1
  
  lat_min= lat -1
  lat_max= lat +1
  
  id= paste0("Group",sep="_",n.group[i])
  
  attribute= "cec.map"
  
  layer= "cec_0-5cm_mean"
  
  url= paste0("https://maps.isric.org/mapserv?map=/map/",attribute,
              "&SERVICE=WCS&VERSION=2.0.1&REQUEST=GetCoverage&COVERAGEID=",layer,
              "&FORMAT=image/tiff&SUBSET=long(",lon_min,",",lon_max,")&",
              "SUBSET=lat(",lat_min,",",lat_max,")&",
              "SUBSETTINGCRS=http://www.opengis.net/def/crs/EPSG/0/4326",
              "&OUTPUTCRS=http://www.opengis.net/def/crs/EPSG/0/4326")
  
  wd.file= paste0("C:/Users/Usuario/Documents/SoilGrids_/newdata/",
                  paste(layer,id, sep="_"),".tif")
  
  download.file(url, destfile= wd.file, mode = 'wb', timeout = 120)
}

# SOC: Soil Organic Carbon
for (i in n.group) {
  lon= mean(subset(sites,groups== i)$Longitude)
  lat= mean(subset(sites,groups== i)$Latitude)
  
  lon_min= lon -1
  lon_max= lon +1
  
  lat_min= lat -1
  lat_max= lat +1
  
  id= paste0("Group",sep="_",n.group[i])
  
  attribute= "soc.map"
  
  layer= "soc_0-5cm_mean"
  
  url= paste0("https://maps.isric.org/mapserv?map=/map/",attribute,
              "&SERVICE=WCS&VERSION=2.0.1&REQUEST=GetCoverage&COVERAGEID=",layer,
              "&FORMAT=image/tiff&SUBSET=long(",lon_min,",",lon_max,")&",
              "SUBSET=lat(",lat_min,",",lat_max,")&",
              "SUBSETTINGCRS=http://www.opengis.net/def/crs/EPSG/0/4326",
              "&OUTPUTCRS=http://www.opengis.net/def/crs/EPSG/0/4326")
  
  wd.file= paste0("C:/Users/Usuario/Documents/SoilGrids_/newdata/",
                  paste(layer,id, sep="_"),".tif")
  
  download.file(url, destfile= wd.file, mode = 'wb', timeout = 120)
}

# PHH2O: pH WATER
for (i in n.group) {
  lon= mean(subset(sites,groups== i)$Longitude)
  lat= mean(subset(sites,groups== i)$Latitude)
  
  lon_min= lon -1
  lon_max= lon +1
  
  lat_min= lat -1
  lat_max= lat +1
  
  id= paste0("Group",sep="_",n.group[i])
  
  attribute= "phh2o.map"
  
  layer= "phh2o_0-5cm_mean"
  
  url= paste0("https://maps.isric.org/mapserv?map=/map/",attribute,
              "&SERVICE=WCS&VERSION=2.0.1&REQUEST=GetCoverage&COVERAGEID=",layer,
              "&FORMAT=image/tiff&SUBSET=long(",lon_min,",",lon_max,")&",
              "SUBSET=lat(",lat_min,",",lat_max,")&",
              "SUBSETTINGCRS=http://www.opengis.net/def/crs/EPSG/0/4326",
              "&OUTPUTCRS=http://www.opengis.net/def/crs/EPSG/0/4326")
  
  wd.file= paste0("C:/Users/Usuario/Documents/SoilGrids_/newdata/",
                  paste(layer,id, sep="_"),".tif")
  
  download.file(url, destfile= wd.file, mode = 'wb', timeout = 120)
}

# SILT
for (i in n.group) {
  lon= mean(subset(sites,groups== i)$Longitude)
  lat= mean(subset(sites,groups== i)$Latitude)
  
  lon_min= lon -1
  lon_max= lon +1
  
  lat_min= lat -1
  lat_max= lat +1
  
  id= paste0("Group",sep="_",n.group[i])
  
  attribute= "silt.map"
  
  layer= "silt_0-5cm_mean"
  
  url= paste0("https://maps.isric.org/mapserv?map=/map/",attribute,
              "&SERVICE=WCS&VERSION=2.0.1&REQUEST=GetCoverage&COVERAGEID=",layer,
              "&FORMAT=image/tiff&SUBSET=long(",lon_min,",",lon_max,")&",
              "SUBSET=lat(",lat_min,",",lat_max,")&",
              "SUBSETTINGCRS=http://www.opengis.net/def/crs/EPSG/0/4326",
              "&OUTPUTCRS=http://www.opengis.net/def/crs/EPSG/0/4326")
  
  wd.file= paste0("C:/Users/Usuario/Documents/SoilGrids_/newdata/",
                  paste(layer,id, sep="_"),".tif")
  
  download.file(url, destfile= wd.file, mode = 'wb', timeout = 120)
}


# 5. Extracting soil property from raster #####

# 5.1 Empty vectors to save soil property values 
nitrogen= vector()
sand= vector()
cec= vector()
soc= vector()
phh2o= vector()
silt=vector()

# 5.2 Open package
library(sp)
library(sf)
library(raster)

# 5.3 Extracting soil property value per each site

# NITROGEN
for (i in 1:nrow(sites)) {  
  
  id= paste0("Group",sep="_",sites$groups[i])
  
  layer= "nitrogen_0-5cm_mean"
  
  raster= raster(paste0("C:/Users/Usuario/Documents/SoilGrids_/newdata/",
                        paste(layer,id, sep="_"),".tif"))
  
  xy= c(sites[i,]$Longitude, sites[i,]$Latitude)
  
  xy_sp= SpatialPoints(coords=matrix(xy, ncol = 2), 
                       proj4string = CRS("+proj=longlat +datum=WGS84 +no_defs"))
  
  nitrogen[i]= extract(raster, xy_sp)
  
}

# SAND
for (i in 1:nrow(sites)) {  
  
  id= paste0("Group",sep="_",sites$groups[i])
  
  layer= "sand_0-5cm_mean"
  
  raster= raster(paste0("C:/Users/Usuario/Documents/SoilGrids_/newdata/",
                        paste(layer,id, sep="_"),".tif"))
  
  xy= c(sites[i,]$Longitude, sites[i,]$Latitude)
  
  xy_sp= SpatialPoints(coords=matrix(xy, ncol = 2), 
                       proj4string = CRS("+proj=longlat +datum=WGS84 +no_defs"))
  
  sand[i]= extract(raster, xy_sp)
  
}

# CEC
for (i in 1:nrow(sites)) {  
  
  id= paste0("Group",sep="_",sites$groups[i])
  
  layer= "cec_0-5cm_mean"
  
  raster= raster(paste0("C:/Users/Usuario/Documents/SoilGrids_/newdata/",
                        paste(layer,id, sep="_"),".tif"))
  
  xy= c(sites[i,]$Longitude, sites[i,]$Latitude)
  
  xy_sp= SpatialPoints(coords=matrix(xy, ncol = 2), 
                       proj4string = CRS("+proj=longlat +datum=WGS84 +no_defs"))
  
  cec[i]= extract(raster, xy_sp)
  
}

# SOC
for (i in 1:nrow(sites)) {  
  
  id= paste0("Group",sep="_",sites$groups[i])
  
  layer= "soc_0-5cm_mean"
  
  raster= raster(paste0("C:/Users/Usuario/Documents/SoilGrids_/newdata/",
                        paste(layer,id, sep="_"),".tif"))
  
  xy= c(sites[i,]$Longitude, sites[i,]$Latitude)
  
  xy_sp= SpatialPoints(coords=matrix(xy, ncol = 2), 
                       proj4string = CRS("+proj=longlat +datum=WGS84 +no_defs"))
  
  soc[i]= extract(raster, xy_sp)
  
}

# PHH2O
for (i in 1:nrow(sites)) {  
  
  id= paste0("Group",sep="_",sites$groups[i])
  
  layer= "phh2o_0-5cm_mean"
  
  raster= raster(paste0("C:/Users/Usuario/Documents/SoilGrids_/newdata/",
                        paste(layer,id, sep="_"),".tif"))
  
  xy= c(sites[i,]$Longitude, sites[i,]$Latitude)
  
  xy_sp= SpatialPoints(coords=matrix(xy, ncol = 2), 
                       proj4string = CRS("+proj=longlat +datum=WGS84 +no_defs"))
  
  phh2o[i]= extract(raster, xy_sp)
  
}

# SILT
for (i in 1:nrow(sites)) {  
  
  id= paste0("Group",sep="_",sites$groups[i])
  
  layer= "silt_0-5cm_mean"
  
  raster= raster(paste0("C:/Users/Usuario/Documents/SoilGrids_/newdata/",
                        paste(layer,id, sep="_"),".tif"))
  
  xy= c(sites[i,]$Longitude, sites[i,]$Latitude)
  
  xy_sp= SpatialPoints(coords=matrix(xy, ncol = 2), 
                       proj4string = CRS("+proj=longlat +datum=WGS84 +no_defs"))
  
  silt[i]= extract(raster, xy_sp)
  
}

# Add soil properties as new columns in ITS2 dataset ####
sites$nitrogen= nitrogen
sites$sand= sand
sites$cec= cec
sites$soc= soc
sites$phh2o= phh2o
sites$silt= silt

# Save data.frame as .csv ####
write.csv(sites, "C:/Users/Usuario/Desktop/WP2_newdata_soil.csv")


