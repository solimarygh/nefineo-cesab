##### Script to identify replicated samples #####
# Created by Solimary García & Micaela
# Date: 26/05/2025

# Description:
# The goal is to identify samples that are likely pseudoreplicates because they are located 
# within 90 meters of each other.
# 
# The plan is to aggregate locations with samples (potential pseudoreplicates) that are less than 90 meters apart,
# considering the methodology used in field plots. 
# To help choose or be sure about the threshold, we inspect the full distance distribution and zoom into distances <200 meters.
# A visible gap around 90 meters supports this threshold for aggregation, for both ITS1 and ITS2.
# See the rest of the analysis in the 'Filtering_datasets.r' script.
# 
# 
# 
## LOAD REQUIRED LIBRARIES
# install.packages("readr")
library(readr)
library(dplyr)
library(geosphere)

# Load final ITS1 dataset
datosITS1 <- read.table("Data/globfungi_metadata_filtered_complemented_its1_soil.tsv", 
                        sep = "\t")
sites_ITS1 <- datosITS1 %>%
  select(PermanentID, latitude, longitude)
length(unique(sites_ITS1$PermanentID))

# Load final ITS2 dataset
datosITS2 <- read.table("Data/globfungi_metadata_filtered_complemented_its2_soil.tsv", 
                        sep = "\t")
sites_ITS2 <- datosITS2 %>%
  select(PermanentID, latitude, longitude)
length(unique(sites_ITS2$PermanentID))



##### ITS1 Analysis #####
coords1 <- sites_ITS1[, c("longitude", "latitude")]  # Select only coordinates
dist_matrix1 <- distm(coords1, fun = distHaversine)  # Distance matrix using Haversine (in meters)

# Convert full matrix to a vector to visualize all pairwise distances
dist_values1 <- dist_matrix1[lower.tri(dist_matrix1)]

# Full distance histogram - difficult to see the threshold
hist(dist_values1,
     breaks = 100,
     main = "ITS1 - All pairwise site distances",
     xlab = "Distance (m)",
     col = "lightblue", border = "white")
abline(v = 100, col = "red", lwd = 2, lty = 2)

# Histogram for distances under 200 meters - easy to see the threshold
dist_under_200_1 <- dist_values1[dist_values1 < 200]
hist(dist_under_200_1,
     breaks = 100,
     main = "ITS1 - Distances < 200 m",
     xlab = "Distance (m)",
     col = "lightblue", border = "white")
abline(v = 90, col = "red", lwd = 2, lty = 2)

# Identify pairs below and above the 90-meter threshold
distance_group1 <- ifelse(dist_values1 <= 90, "≤ 90 m", "> 90 m")
table(distance_group1)

length(dist_values1)  # Total ITS1 site pairs
sum(distance_group1 == "≤ 90 m") / length(distance_group1) * 100  # Percentage of close pairs



##### ITS2 Analysis #####
coords2 <- sites_ITS2[, c("longitude", "latitude")]  # Select only coordinates
dist_matrix2 <- distm(coords2, fun = distHaversine)  # Distance matrix using Haversine

# Convert matrix to vector for histogram analysis
dist_values2 <- dist_matrix2[lower.tri(dist_matrix2)]

# Full distance histogram
hist(dist_values2,
     breaks = 100,
     main = "ITS2 - All pairwise site distances",
     xlab = "Distance (m)",
     col = "lightblue", border = "white")
abline(v = 100, col = "red", lwd = 2, lty = 2)

# Histogram for distances under 200 meters
dist_under_200_2 <- dist_values2[dist_values2 < 200]
hist(dist_under_200_2,
     breaks = 100,
     main = "ITS2 - Distances < 200 m",
     xlab = "Distance (m)",
     col = "lightblue", border = "white")
abline(v = 90, col = "red", lwd = 2, lty = 2)

# Identify pairs below and above the 90-meter threshold
distance_group2 <- ifelse(dist_values2 <= 90, "≤ 90 m", "> 90 m")
table(distance_group2)

length(dist_values2)  # Total ITS2 site pairs
sum(distance_group2 == "≤ 90 m") / length(distance_group2) * 100  # Percentage of close pairs


# Now that we confirmed 90 m is a good cutoff, we proceed to clustering
# Perform hierarchical clustering using geographic distances
# Cut the dendrogram to group sites within 90 meters: See the rest in the 'Filtering_datasets.r' script

