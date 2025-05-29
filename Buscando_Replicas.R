# Script to identify replicated samples #####
# Created by Solimary García
# Date: 26/05/2025

# Description:
# Comenzaré con los datos de coordenadas ITS1, que contienen también los datos de los artículos. La idea es identificar las muestras que probablemente son pseudoréplicas, pues están a menos de 100m de distancia entre sí.

### revisar metodo de vizinhanza para agrupar 100m to 90m
### ver s'i entendemos el plan de muestreo de los articulos que terminan con pseudo replicas
### verificar overlap -its1 y 2
###
###
#install.packages(readr)
library (readr)
library (dplyr)
library (readr)
library (geosphere)

# 1. Leer el archivo con encabezado como normalmente
# #Base de datos ITS1
datosITS1 <- read_tsv("Data/globfungi_metadata_filtered_complemented_its1_soil.tsv")
nrow(datosITS1) #2815

#el nombre de las columnas esta corrida. Para arregalar esso:
nombres_actuales <- names(datosITS1)
datosITS1 <- datosITS1 %>% select(-dataset)
names(datosITS1) <- nombres_actuales
glimpse(datosITS1)
length (unique(datosITS1$PermanentID)) #2815 No hay IDs repetidas


# #Base de datos ITS2
datosITS2 <- read_tsv("Data/globfungi_metadata_filtered_complemented_its2_soil.tsv")
nrow(datosITS2) #2380

#el nombre de las columnas esta corrida. Para arregalar esso:
nombres_actuales <- names(datosITS2)
datosITS2 <- datosITS2 %>% select(-dataset)
names(datosITS2) <- nombres_actuales
glimpse(datosITS2)
length (unique(datosITS1$PermanentID)) #2815 # Si hay IDs repetidas


#Testando si tienen todas la columna completas
all(colnames(datosITS1) %in% colnames(datosITS2)) 

clases_df1 <- sapply(datosITS1, class)
clases_df2 <- sapply(datosITS2, class)
comparacion_clases <- data.frame(
  columna = union(names(clases_df1), names(clases_df2)),
  clase_df1 = clases_df1[match(union(names(clases_df1), names(clases_df2)), names(clases_df1))],
  clase_df2 = clases_df2[match(union(names(clases_df1), names(clases_df2)), names(clases_df2))]
)
comparacion_clases %>%
  filter(clase_df1 != clase_df2)
# columna clase_df1 clase_df2
# year_of_sampling year_of_sampling   numeric character
# 
# modificando la clase de its1
datosITS1 <- datosITS1 %>% mutate(year_of_sampling = as.character(year_of_sampling))


# Combinando ITS1 e ITS2
# conserva todas las filas de ambos
datosITS1_ITS2 <- bind_rows(datosITS1, datosITS2)
nrow (datosITS1_ITS2 ) # 5195 (suma 2380+2815)
#write_csv(datosITS1_ITS2, "Data/datosITS1_ITS2.csv")  # también de `readr`



length(unique(datosITS1_ITS2$PermanentID)) #4314 (suma 2815+1499)


#Cuantos PermanentID por cada dataset?
datosITS1_ITS2 %>%
  group_by(dataset) %>%
  summarise(PermanentID_unicos = n_distinct(PermanentID))
#   dataset PermanentID_unicos
#   <chr>                <int>
# 1 its1                  2815
# 2 its2                  1499


#Identificar los PermanentID que aparecen en ambos datasets
#ninguno ID se repite entre datasets!
ids_ambos <- datosITS1_ITS2 %>%
  group_by(PermanentID) %>%
  summarise(n_datasets = n_distinct(dataset)) %>%
  filter(n_datasets > 1)




### De aqui en adelante puedo trabajar con solo una parte de los datos

sites_ITS1 <- datosITS1_ITS2 %>%
  filter(dataset == "its1") %>%
  select(PermanentID, latitude, longitude)

sites_ITS2 <- datosITS1_ITS2 %>%
  filter(dataset == "its2") %>%
  select(PermanentID, latitude, longitude)


## The plan is to aggregate locations that have samples (pseudoreplicates) within 90 meters, 
# considering the methodology used in field plots. Compute the distance matrix (in meters) using the Haversine formula. Then, to identify the best threshold for sample aggregation, we converted the full distance matrix to a vector (excluding redundant and diagonal elements). We did a Histogram for all and also a zoom of distances < 200 meters. We identify that 90-meter threshold is a good number (there is a evident gap). 

#Then, we performed the hierarchical clustering based on geographic distance, we use the method "completes" which Only merges groups if all pairs within the group are within 90 meters of each other. This results in smaller and more compact clusters. There is no "chain" effect: if A is within 100 m of B, and B is within 100 m of C, A and C will not be in the same group unless A is also within 100 m of C. It is ideal because we want stricter and more homogeneous spatial clusters.

# ITS1
coords1 <- sites_ITS1[, c("longitude", "latitude")]# Select only the coordinates (longitude and latitude)
dist_matrix1 <- distm(coords1, fun = distHaversine)# Compute distance matrix (in meters) using Haversine formula

# To identify the best threshold for aggregation of samples
# Convert the full distance matrix to a vector (exclude redundant and diagonal elements)
dist_values1 <- dist_matrix1[lower.tri(dist_matrix1)]

# Histograma completo
hist(dist_values1,
     breaks = 100,
     main = "ITS1 - Todas las distancias entre sitios",
     xlab = "Distancia (m)",
     col = "lightblue", border = "white")
abline(v = 100, col = "red", lwd = 2, lty = 2)

# Histograma solo < 200 m
dist_under_1k_1 <- dist_values1[dist_values1 < 200]
hist(dist_under_1k_1,
     breaks = 100,
     main = "ITS1 - Distancias < 1 km",
     xlab = "Distancia (m)",
     col = "lightblue", border = "white")
abline(v = 90, col = "red", lwd = 2, lty = 2) #Filtramos abajo de 90m

# Create logical groups based on the 90-meter threshold
distance_group1 <- ifelse(dist_values1 <= 90, "≤ 90 m", "> 90 m")  #Filtramos abajo de 90m
table(distance_group1)
#distance_group1
# > 90 m  ≤ 90 m 
# 6775876   52484 

length(dist_values1)  #6828360 total de pares ITS1
sum(distance_group1 == "≤ 90 m") / length(distance_group1) * 100  # porcentaje 0.7686179%


# ok, now:
# Now that we know 90m is a good limit, let's create the clusters
# Perform hierarchical clustering based on geographic distance
# Cut the dendrogram into clusters: group sites that are within 90 meters
# Add group assignments to the original dataset — saving with a generic column name

clustering1 <- hclust(as.dist(dist_matrix1), method = "complete")# Perform hierarchical clustering based on geographic distance

groups1 <- cutree(clustering1, h = 90) # Cut the dendrogram into clusters: group sites that are within 100 meters #cutree(..., h = 100) creates clusters where each site is within 100 meters of others in its group. You can adjust the h parameter to control the radius of clustering in meters.

## Agregar a la base original -- guardando con un nombre genérico de columna
sites_ITS1$grupo_espacial <- groups1 

# Ver grupos creados
table(sites_ITS1$grupo_espacial)

sites_ITS1 <- sites_ITS1 %>%
  group_by(grupo_espacial) %>%
  mutate(Pseudoreplicas_site = ifelse(n() == 1, "no", "yes")) %>% #solamente una muestra a cada 90m
  ungroup()


# View result
head(sites_ITS1)

nearby_sites <- sites_ITS1%>% 
  filter(Pseudoreplicas_site == "no")

nrow(nearby_sites) #solo 37!!sin pseudoreplicas

max( sites_ITS1$grupo_espacial) #682 grupos espaciais! !! 

write.csv(sites_ITS1, "Data/sites_ITS1.csv", row.names = FALSE)

##### duda!! no vamos a mantener un representante de los sitios que tienen varias muestras
##### # A. For groups with more than one sample, keep only one (the first one)
representantes_de_grupos1 <- sites_ITS1 %>%
  group_by(grupo_espacial) %>%
  filter(n() > 1) %>%
  slice(1) %>%
  ungroup()

# B. Add the unique sites that were already alone (To_keep == "yes")
sitios_unicos1 <- sites_ITS1 %>%
  filter(Pseudoreplicas_site == "no")

# C. Combine both: unique sites + representatives from groups
final_sites_ITS1 <- bind_rows(sitios_unicos1, representantes_de_grupos1)

nrow(final_sites_ITS1) # 682 sitios totais! ok, é o mesmo numero que grupos creado por cluster de 90m

# D. Optional: save the result
write.csv(final_sites_ITS1, "Data/sites_ITS1_unicos_o_representantes.csv", row.names = FALSE)






#### ITS2 ------------------------

coords2 <- sites_ITS2[, c("longitude", "latitude")]# Select only the coordinates (longitude and latitude)
dist_matrix2 <- distm(coords2, fun = distHaversine)# Compute distance matrix (in meters) using Haversine formula

# To identify the best threshold for aggregation of samples
# Convert the full distance matrix to a vector (exclude redundant and diagonal elements)
dist_values2 <- dist_matrix2[lower.tri(dist_matrix2)]

# Histograma completo
hist(dist_values2,
     breaks = 100,
     main = "ITS1 - Todas las distancias entre sitios",
     xlab = "Distancia (m)",
     col = "lightblue", border = "white")
abline(v = 100, col = "red", lwd = 2, lty = 2)

# Histograma solo < 200 m
dist_under_1k_2 <- dist_values2[dist_values2 < 200]
hist(dist_under_1k_2,
     breaks = 100,
     main = "ITS1 - Distancias < 1 km",
     xlab = "Distancia (m)",
     col = "lightblue", border = "white")
abline(v = 90, col = "red", lwd = 2, lty = 2) #Filtramos abajo de 90m

# Create logical groups based on the 90-meter threshold
distance_group2 <- ifelse(dist_values2 <= 90, "≤ 90 m", "> 90 m")  #Filtramos abajo de 90m
table(distance_group2)
# distance_group2
# > 90 m  ≤ 90 m 
# 1100115   22636 

length(dist_values2)  #1122751 total de pares ITS1
sum(distance_group2 == "≤ 90 m") / length(distance_group2) * 100  # porcentaje  2.016119%


# ok, now:
# Now that we know 90m is a good limit, let's create the clusters
# Perform hierarchical clustering based on geographic distance
# Cut the dendrogram into clusters: group sites that are within 90 meters
# Add group assignments to the original dataset — saving with a generic column name

clustering2 <- hclust(as.dist(dist_matrix2), method = "complete")# Perform hierarchical clustering based on geographic distance

groups2 <- cutree(clustering2, h = 90) # Cut the dendrogram into clusters: group sites that are within 100 meters #cutree(..., h = 100) creates clusters where each site is within 100 meters of others in its group. You can adjust the h parameter to control the radius of clustering in meters.

## Agregar a la base original -- guardando con un nombre genérico de columna
sites_ITS2$grupo_espacial <- groups2

# Ver grupos creados
table(sites_ITS2$grupo_espacial)

sites_ITS2 <- sites_ITS2 %>%
  group_by(grupo_espacial) %>%
  mutate(Pseudoreplicas_site = ifelse(n() == 1, "no", "yes")) %>% #solamente una muestra a cada 90m
  ungroup()

# View result
head(sites_ITS2)

nearby_sites2 <- sites_ITS2%>% 
  filter(Pseudoreplicas_site == "no")

nrow(nearby_sites2) #solo 227!! sin pseudoreplicas
max( sites_ITS2$grupo_espacial) #367 grupos espaciais! !! 

write.csv(sites_ITS2, "Data/sites_ITS2.csv", row.names = FALSE)


##### duda!! no vamos a mantener un representante de los sitios que tienen varias muestras
##### # A. For groups with more than one sample, keep only one (the first one)
representantes_de_grupos2 <- sites_ITS2 %>%
  group_by(grupo_espacial) %>%
  filter(n() > 1) %>%
  slice(1) %>%
  ungroup()

# B. Add the unique sites that were already alone (To_keep == "yes")
sitios_unicos2 <- sites_ITS2 %>%
  filter(Pseudoreplicas_site == "no")

# C. Combine both: unique sites + representatives from groups
final_sites_ITS2 <- bind_rows(sitios_unicos2, representantes_de_grupos2)

nrow(final_sites_ITS2) #367 sitios totais!

# D. Optional: save the result
write.csv(final_sites_ITS2, "Data/sites_ITS2_unicos_o_representantes.csv", row.names = FALSE)
