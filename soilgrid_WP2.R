# Tutorial to get SoilGrid data to WP2 #####
# Created by Micaela Santos
# Date: 05/03/2024

# Description_:
# It contains an automatic way to download soil maps from SoilGrids.org and...
# to extract soil property values using geographical coordinates from ITS2 dataset.
# Each map is a raster image which describes an individual soil property.
# However, there are some empty cells in maps which return “0” values.
# It is mandatory that raster has 2 degrees of extension.
# Due to many sites in the ITS2 data set are close to each other...
# I clustered sites and used the same raster to extract soil values (at 150k).

# 0. Open NEFINEO_MS project #####
setwd("~/nefineo-cesab")

# 1. Open datasets its1/its2 #####
# 1.1 no grouped samples (site=sample)
its1= read.csv("Data/new.its1.csv", row.names=1)
its2= read.csv("Data/new.its2.csv", row.names= 1)
  
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
#first for its1, then replace by its2 and run again
sites= rbind(data.frame(new.ID=its1$new.ID, Latitude=its1$latitude, Longitude=its1$longitude),
             data.frame(new.ID=its2$new.ID, Latitude=its2$latitude, Longitude=its2$longitude))

x= sites[,c("Longitude","Latitude")]
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
sites$nitrogen= nitrogen
sites$sand= sand
sites$cec= cec
sites$soc= soc
sites$phh2o= phh2o
sites$silt= silt

# Save data.frame as .csv ###
#write.csv(sites, "Data/soilgrid_its1_its2.csv")

#Filtering rows by paper to keep and neotropics
its1= read.csv("Data/new.its1.csv", row.names=1)
#filtering by papers to keep and bioregion
its1.f= subset(its1, paper_to_keep=="yes" & 
                 morrone_biogeoregions_Region== "Neotropical") 

its2= read.csv("Data/new.its2.csv", row.names= 1)
#filtering by papers to keep and bioregion
its2.f= subset(its2, paper_to_keep=="yes" & 
                 morrone_biogeoregions_Region== "Neotropical") 

rows.to.keep= match(c(its1.f$new.ID, its2.f$new.ID),sites$new.ID)
sites.to.keep= sites[rows.to.keep,]

# Save data.frame as .csv ####
write.csv(sites.to.keep, "Data/soilgrid_its1_its2_filtered.csv")

# Missing data ####
#soil grid missing data for any property (e.g. nitrogen)
missing_soil= subset(sites.to.keep, nitrogen == 0 | is.na(nitrogen))
miss_soil_n_samples= unique(missing_soil$new.ID)
length(miss_soil_n_samples) #nº of missing grouped samples 98/1094 
