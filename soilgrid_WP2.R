# Tutorial to get SoilGrid data to WP2 #####
# Created by Micaela Santos
# Date: 05/03/2024

# Description:
# It contains an automatic way to download soil maps from SoilGrids.org and...
# to extract soil property values using geographical coordinates from ITS2 dataset.
# Each map is a raster image which describes an individual soil property.
# However, there are some empty cells in maps which return “0” values.
# It is mandatory that raster has 2 degrees of extension.
# Due to many sites in the ITS2 data set are close to each other...
# I clustered sites and used the same raster to extract soil values (at 150k).


# 1. Open coordinates for ITS2 data #####
# 1.1 Downloaded NEFINEO>WP2>info_dataset>WP2_coordinates_ITS2.csv

# 1.2 Open geographical coordinates (all sites)
setwd("C:/Users/Usuario/Dropbox") #my directory
sites= read.csv("WP2_coordinates_ITS2.csv")

# 2. Open and install packages ####   

# Packages we will need
packages <- c("geosphere", "raster", "sf", "stats", "sp")

# Install missing packages
install<- packages[!packages%in% installed.packages()[, "Package"]]
if (length(install)) install.packages(install) ##


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
  
  wd.file= paste0("C:/Users/Usuario/Documents/SoilGrids_/",
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
  
  wd.file= paste0("C:/Users/Usuario/Documents/SoilGrids_/",
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
  
  wd.file= paste0("C:/Users/Usuario/Documents/SoilGrids_/",
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
  
  wd.file= paste0("C:/Users/Usuario/Documents/SoilGrids_/",
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
  
  wd.file= paste0("C:/Users/Usuario/Documents/SoilGrids_/",
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
  
  wd.file= paste0("C:/Users/Usuario/Documents/SoilGrids_/",
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
  
  raster= raster(paste0("C:/Users/Usuario/Documents/SoilGrids_/",
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
  
  raster= raster(paste0("C:/Users/Usuario/Documents/SoilGrids_/",
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
  
  raster= raster(paste0("C:/Users/Usuario/Documents/SoilGrids_/",
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
  
  raster= raster(paste0("C:/Users/Usuario/Documents/SoilGrids_/",
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
  
  raster= raster(paste0("C:/Users/Usuario/Documents/SoilGrids_/",
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
  
  raster= raster(paste0("C:/Users/Usuario/Documents/SoilGrids_/",
                        paste(layer,id, sep="_"),".tif"))
  
  xy= c(sites[i,]$Longitude, sites[i,]$Latitude)
  
  xy_sp= SpatialPoints(coords=matrix(xy, ncol = 2), 
                       proj4string = CRS("+proj=longlat +datum=WGS84 +no_defs"))
  
  silt[i]= extract(raster, xy_sp)
  
}

# Add soil properties as new columns in ITS2 dataset ####
sites$nitogen= nitrogen
sites$sand= sand
sites$cec= cec
sites$soc= soc
sites$phh2o= phh2o
sites$silt= silt

# Save data.frame as .csv ####
#write.csv(sites, "C:/Users/Usuario/Desktop/WP2_ITS2_soil.csv")


