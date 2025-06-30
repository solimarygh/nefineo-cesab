# Retrive missing soilgrid data #####
# Created by Micaela Santos
# Date: 25/06/2025

# 1. Open NEFINEO_MS project #####
setwd("~/nefineo-cesab")

# 2.1 Open soildata ####
new.datasets= read.csv("Data/WP2_newdata_soil.csv")[,-1] #remove first col. 
its.dataset= read.csv("Data/soilgrid_its1_its2_filtered.csv")[,-1] #remove first col. 
mex.dataset= read.csv("Data/WP2_newdata_mex_soil.csv")[,-1] #remove first col. 
#Groups column corresponds of grouping by raster image  

# 2.2 Open biogeographical data ####
its1= read.csv("Data/new.its1.csv")
its1= subset(its1, paper_to_keep=="yes" & 
         morrone_biogeoregions_Region=="Neotropical")[,"morrone_biogeoregions_Subregion"]
its2= read.csv("Data/new.its2.csv")
its2= subset(its2, paper_to_keep=="yes" & 
               morrone_biogeoregions_Region=="Neotropical")[,"morrone_biogeoregions_Subregion"]
new.long= rep("long",12)
new.spun= rep("spun",20)
new.dryf= rep("dryf",125)
new.diam= rep("diam",169)
new.mymo= rep("mymo",46)
atlas.a= rep("atlas.a",73)
atlas.b= rep("atlas.b",78)

subregion= c(its1,its2,new.long,new.spun,new.dryf,new.diam,new.mymo,atlas.a,atlas.b) #3419

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
cor(soilgrid_imp) #check correlations (nitrogen-cec-soc, silt-sand-phh2o)
pca_soil= prcomp(soilgrid_imp, scale.=T) #pca
pca_soil$rotation[,c(1,2)] #Correlation matrix
summary(pca_soil) # PC1 and PC2 gather 74% of variability of all soil variables
biplot(pca_soil) # pca plot
#points(x=pca_soil$x[,1], y=pca_soil$x[,2], col=as.numeric(as.factor(subregion)), pch=16)
pca_var= pca_soil$x[,1:2] #PC1 and PC2
