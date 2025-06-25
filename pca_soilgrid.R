# Retrive missing soilgrid data #####
# Created by Micaela Santos
# Date: 25/06/2025

# 1. Open NEFINEO_MS project #####
setwd("~/nefineo-cesab")

# 2. Open soildata ####
new.datasets= read.csv("Data/WP2_newdata_soil.csv")[,-1] #remove first col. 
its.dataset= read.csv("Data/soilgrid_its1_its2_filtered.csv")[,-1] #remove first col. 
mex.dataset= read.csv("Data/WP2_newdata_mex_soil.csv")[,-1] #remove first col. 
#Groups column corresponds of grouping by raster image  

# 3. Aggregate soildatasets ####
soilgrid= rbind(its.dataset, new.datasets, mex.dataset) 
length(unique(soilgrid$new.ID)) #nº grouped samples/sites
nrow(soilgrid) #nº samples/sites
nrow(subset(soilgrid, nitrogen == 0 | is.na(nitrogen))) #nº missing data=201
soilgrid[soilgrid == 0] = NA #convert zeros to NA 
#zeros are unmeasured or missing data

# 4. Impute NA values to complete missing data ####
install.packages("missMDA")
library(missMDA)

imputation= imputePCA(soilgrid[,5:10])
soilgrid_imp= imputation$completeObs

# 5. PCA of soildata variables #### 
cor(soilgrid_imp) #check correlations
pca_soil= prcomp(soilgrid_imp, scale.=T)
pca_soil$rotation[,c(1,2)]
summary(pca_soil) # PC1 and PC2 gather 74% of variability of all soil variables
biplot(pca_soil, cex= c(0.5, 1))

pca_soil$x
