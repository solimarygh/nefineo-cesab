# Script to identify replicated samples #####
# Created by Solimary García
# Date: 26/05/2025

# Description:
# Comenzaré con los datos de coordenadas ITS1, que contienen también los datos de los artículos. La idea es identificar las muestras que probablemente son pseudoréplicas, pues están a menos de 100m de distancia entre sí.

### revisar metodo de vizinhanza para agrupar 100m to 90m
### ver s'i entendemos el plan de muestreo de los articulos que terminan con pseudo replicas
### verificar overlap -its1 y 2
###

library (readr)
library (dplyr)

# 1. Leer el archivo con encabezado como normalmente
datosITS1 <- read_tsv("Data/globfungi_metadata_filtered_complemented_its1_soil.tsv")
#el nombre de las columnas esta corrida. Para arregalar esso:
nombres_actuales <- names(datosITS1)
datosITS1 <- datosITS1 %>% select(-dataset)
names(datosITS1) <- nombres_actuales
glimpse(datosITS1)


datosITS2 <- read_tsv("Data/globfungi_metadata_filtered_complemented_its2_soil.tsv")
nrow(datosITS2) #2380

#el nombre de las columnas esta corrida. Para arregalar esso:
nombres_actuales <- names(datosITS2)
datosITS2 <- datosITS2 %>% select(-dataset)
names(datosITS2) <- nombres_actuales
glimpse(datosITS2)




#Testando si tienen todas la columna completas
all(colnames(datosITS1) %in% colnames(datosITS2)) 

clases_df1 <- sapply(datosITS1, class)
clases_df2 <- sapply(datosITS2, class)
comparacion_clases <- data.frame(
  columna = union(names(clases_df1), names(clases_df2)),
  clase_df1 = clases_df1[match(union(names(clases_df1), names(clases_df2)), names(clases_df1))],
  clase_df2 = clases_df2[match(union(names(clases_df1), names(clases_df2)), names(clases_df2))]
)
diferentes <- comparacion_clases %>%
  filter(clase_df1 != clase_df2)
# columna clase_df1 clase_df2
# year_of_sampling year_of_sampling   numeric character
# 
# modificando la clase de its1
datosITS1 <- datosITS1 %>% mutate(year_of_sampling = as.character(year_of_sampling))


# Combinando ITS1 e ITS2
# conserva todas las filas de ambos
datosITS1_ITS2 <- bind_rows(datosITS1, datosITS2)
nrow (datosITS1_ITS2 ) # 5195
write_csv(datosITS1_ITS2, "datosITS1_ITS2.csv")  # también de `readr`


nrow(datosITS1_ITS2)
length(unique(datosITS1_ITS2$PermanentID))
length(datosITS1_ITS2$PermanentID)

conteo <- datosITS1_ITS2 %>%
  group_by(dataset) %>%
  summarise(PermanentID_unicos = n_distinct(PermanentID))
#   dataset PermanentID_unicos
#   <chr>                <int>
# 1 its1                  2815
# 2 its2                  1499

duplicados <- datosITS1_ITS2 %>%
  group_by(PermanentID) %>%
  filter(n() > 1) %>%
  arrange(PermanentID)


conteo <- datosITS1_ITS2 %>%
  count(PermanentID) %>%
  filter(n > 1) %>%
  arrange(desc(n))

#Identificar los PermanentID que aparecen en ambos datasets
#ninguno ID se repite entre datasets!
ids_ambos <- datosITS1_ITS2 %>%
  group_by(PermanentID) %>%
  summarise(n_datasets = n_distinct(dataset)) %>%
  filter(n_datasets > 1)




### De aqui en adelante puedo trabajar con solo una parte de los datos
datos_filtrados <- datosITS1_ITS2 %>%
  select(1:19)

sites <-datos_filtrados 

colnames (sites)
#install.packages(readr)
library(readr)
library(geosphere)

sites_ITS1 <- datosITS1_ITS2 %>%
  filter(dataset == "its1") %>%
  select(PermanentID, latitude, longitude)

sites_ITS2 <- datosITS1_ITS2 %>%
  filter(dataset == "its2") %>%
  select(PermanentID, latitude, longitude)


library(geosphere)

# ITS1
coords1 <- sites_ITS1[, c("longitude", "latitude")]# Select only the coordinates (longitude and latitude)
dist_matrix1 <- distm(coords1, fun = distHaversine)# Compute distance matrix (in meters) using Haversine formula
clustering1 <- hclust(as.dist(dist_matrix1), method = "complete")# Perform hierarchical clustering based on geographic distance
groups1 <- cutree(clustering1, h = 100) # Cut the dendrogram into clusters: group sites that are within 100 meters #cutree(..., h = 100) creates clusters where each site is within 100 meters of others in its group. You can adjust the h parameter to control the radius of clustering in meters.

sites_ITS1$grupo <- groups1

# ITS2
coords2 <- sites_ITS2[, c("longitude", "latitude")]# Select only the coordinates (longitude and latitude)
dist_matrix2 <- distm(coords2, fun = distHaversine)# Compute distance matrix (in meters) using Haversine formula
clustering2 <- hclust(as.dist(dist_matrix2), method = "complete")# Perform hierarchical clustering based on geographic distance
groups2 <- cutree(clustering2, h = 100) # Cut the dendrogram into clusters: group sites that are within 100 meters #cutree(..., h = 100) creates clusters where each site is within 100 meters of others in its group. You can adjust the h parameter to control the radius of clustering in meters.

sites_ITS2$grupo <- groups2





library(geosphere)

# Coordenadas
coords1 <- sites_ITS1[, c("longitude", "latitude")]

# Matriz de distancias
dist_matrix1 <- distm(coords1, fun = distHaversine)
dist_values1 <- dist_matrix1[lower.tri(dist_matrix1)]

# Histograma completo
hist(dist_values1,
     breaks = 100,
     main = "ITS1 - Todas las distancias entre sitios",
     xlab = "Distancia (m)",
     col = "lightblue", border = "white")
abline(v = 100, col = "red", lwd = 2, lty = 2)

# Histograma solo < 300 m
dist_under_1k_1 <- dist_values1[dist_values1 < 300]
hist(dist_under_1k_1,
     breaks = 100,
     main = "ITS1 - Distancias < 1 km",
     xlab = "Distancia (m)",
     col = "lightblue", border = "white")
abline(v = 100, col = "red", lwd = 2, lty = 2)

# Tabla de frecuencia
distance_group1 <- ifelse(dist_values1 <= 100, "≤ 100 m", "> 100 m")
table(distance_group1)
________________


coords2 <- sites_ITS2[, c("longitude", "latitude")]
dist_matrix2 <- distm(coords2, fun = distHaversine)
dist_values2 <- dist_matrix2[lower.tri(dist_matrix2)]

# Histograma completo
hist(dist_values2,
     breaks = 100,
     main = "ITS2 - Todas las distancias entre sitios",
     xlab = "Distancia (m)",
     col = "lightblue", border = "white")
abline(v = 100, col = "red", lwd = 2, lty = 2)

# Histograma solo < 1 km
dist_under_1k_2 <- dist_values2[dist_values2 < 300]
hist(dist_under_1k_2,
     breaks = 100,
     main = "ITS2 - Distancias < 1 km",
     xlab = "Distancia (m)",
     col = "lightblue", border = "white")
abline(v = 100, col = "red", lwd = 2, lty = 2)

# Tabla de frecuencia
distance_group2 <- ifelse(dist_values2 <= 100, "≤ 100 m", "> 100 m")
table(distance_group2)


_______________


# Check the first few rows to confirm everything worked
head(sites_ITS2)


# Compute distance matrix (in meters)
dist_matrix <- distm(coords, fun = distHaversine)

# Convert the full distance matrix to a vector (exclude redundant and diagonal elements)
dist_values <- dist_matrix[lower.tri(dist_matrix)]



# Plot a histogram of the pairwise distances
hist(dist_values,
     breaks = 100,  # Adjust number of bins as needed
     main = "Histogram of Pairwise Distances Between Sites",
     xlab = "Distance (meters)",
     col = "lightblue",
     border = "white")
abline(v = 100, col = "red", lwd = 2, lty = 2)


# Filter distances below 1000 meters (1 km) # si grafico todo es mucho
dist_under_100m <- dist_values[dist_values < 100]
hist(dist_under_100m,
     breaks = 100,  # Adjust number of bins as needed
     main = "Histogram of Pairwise Distances Between Sites",
     xlab = "Distance (meters)",
     col = "lightblue",
     border = "white")
abline(v = 90, col = "red", lwd = 2, lty = 2)


# Create logical groups based on the 100-meter threshold
distance_group <- ifelse(dist_values <= 100, "≤ 100 m", "> 100 m")

# Show the frequency of each group
table(distance_group)

# distance_group
# > 100 m   ≤ 100 m 
# 3919514    41191 
# 
#  3.960.705 é o 100% de pares de distancia,   os 41191 pares sao  1.05%
#  
#  
 distance_group <- ifelse(dist_values <= 90, "≤ 100 m", "> 100 m")
 table(distance_group)
 
#  usando o 90%
# distance_group
# > 100 m ≤ 100 m 
# 3919739   40966 
#  
#  -----
#  # 1. Compute the distance matrix
dist_matrix <- distm(coords, fun = distHaversine)

# 2. Get matrix of logical values: TRUE if distance ≤ 100m (excluding diagonal)
close_pairs <- dist_matrix <= 100 & dist_matrix != 0

# 3. For each row, check if the site is close to any other
sites_to_keep <- apply(close_pairs, 1, any)

# 4. Filter the original data frame to keep only those close to others
nearby_sites <- sites[sites_to_keep, ]

# 5. Optional: check how many were selected
nrow(nearby_sites)
#--------------------
  
  
  library(geosphere)
library(igraph)

# 1. Compute the distance matrix
dist_matrix <- distm(coords, fun = distHaversine)

# 2. Create adjacency matrix: 1 if distance ≤ 100m (excluding self-loops)
adjacency <- dist_matrix <= 100 & dist_matrix != 0

# 3. Create an undirected graph from the adjacency matrix
g <- graph_from_adjacency_matrix(adjacency, mode = "undirected")

# 4. Identify which sites are part of a "close" group
# If a site has at least one edge, it's close to someone
is_close <- degree(g) > 0

# 5. Assign group IDs to connected components (each group of close sites)
groups <- rep(NA, nrow(sites))  # Default NA for isolated points
components <- components(g)  # Get connected components
groups[as.integer(names(components$membership))] <- components$membership

# 6. Add both columns to the original data frame
sites$is_close <- is_close
sites$close_group <- groups

# 7. View result
head(sites)



library(ggplot2)


library(dplyr)

nearby_sites <- sites %>% 
  filter(is_close == TRUE)

write.csv(sites, "Data/sites_prox.csv", row.names = FALSE)
write.csv(nearby_sites, "Data/nearby_sites.csv", row.names = FALSE)
