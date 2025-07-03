# Aplicar este flujo a cada archivo:
# Leer el .csv
# Filtrar según criterios comunes
# Convertir a sf
# Agrupar por new.ID, unir geometrías y calcular el centroide
# Volver a unir con datos originales
# Guardar solo una fila por grupo con coordenadas del centroide



#install.packages("leaflet")
library(leaflet)
library(readr)
library(dplyr)
library(sf)
library(purrr)
library(stringr)






###Bases privadas----
####new_atlasmxb----
new_atlasmxb <- read_csv("Data/new.atlasmxb.csv", 
                         locale = locale(encoding = "Latin1"))  # o prueba "UTF-8"!
colnames (new_atlasmxb) #"Latitud"                "Longitud" 
# Convertir a objeto sf
sf_its1 <- st_as_sf(new_atlasmxb, coords = c("Longitud", "Latitud"), crs = 4326) #(x = longitude, y = latitude)
colnames (sf_its1)


# Transformar a proyección métrica para trabajar en metros (recomendado)
sf_its1_proj <- st_transform(sf_its1, crs = 3857)  # Web Mercator
unique (sf_its1_proj$new.ID)

# Agrupar por grupo y unir geometrías
grupos_its1 <- sf_its1_proj %>%
  group_by(new.ID) %>%
  summarise(geometry = st_union(geometry)) %>%  # Une todos los puntos del grupo
  st_centroid()  # Calcula el centroide del grupo
grupos_its1_latlon <- st_transform(grupos_its1, crs = 4326)

# Extraer coordenadas del centroide
coords_centroides1 <- st_coordinates(grupos_its1_latlon)

grupos_its1_df <- grupos_its1_latlon %>%
  st_drop_geometry() %>% #para quitar el agrupamiento...
  mutate(
    centroid_long = coords_centroides1[, "X"],
    centroid_lat = coords_centroides1[, "Y"]
  )

# Unir con dataset original
new_atlasmxb_centroid<- new_atlasmxb %>%
  left_join(grupos_its1_df, by = "new.ID") %>%
  arrange(new.ID)%>%
  group_by(new.ID)%>%
  slice_head(n=1)





####new_diam----
new_diam     <- read_csv("Data/new.diam.csv")
colnames (new_diam) #latitude"                     "longitude" 
            

# Convertir a objeto sf
sf_its1 <- st_as_sf(new_diam , coords = c("longitude", "latitude"), crs = 4326) #(x = longitude, y = latitude)
colnames (sf_its1)


# Transformar a proyección métrica para trabajar en metros (recomendado)
sf_its1_proj <- st_transform(sf_its1, crs = 3857)  # Web Mercator
unique (sf_its1_proj$new.ID)

# Agrupar por grupo y unir geometrías
grupos_its1 <- sf_its1_proj %>%
  group_by(new.ID) %>%
  summarise(geometry = st_union(geometry)) %>%  # Une todos los puntos del grupo
  st_centroid()  # Calcula el centroide del grupo
grupos_its1_latlon <- st_transform(grupos_its1, crs = 4326)

# Extraer coordenadas del centroide
coords_centroides1 <- st_coordinates(grupos_its1_latlon)

grupos_its1_df <- grupos_its1_latlon %>%
  st_drop_geometry() %>% #para quitar el agrupamiento...
  mutate(
    centroid_long = coords_centroides1[, "X"],
    centroid_lat = coords_centroides1[, "Y"]
  )

# Unir con dataset original
new_diam_centroid<- new_diam %>%
  left_join(grupos_its1_df, by = "new.ID") %>%
  arrange(new.ID)%>%
  group_by(new.ID)%>%
  slice_head(n=1)




####new_long----
new_long     <- read_csv("Data/new.long.csv")
colnames (new_long ) #"Latitude"            "Longitude"  

# Convertir a objeto sf
sf_its1 <- st_as_sf(new_long , coords = c("Longitude", "Latitude"), crs = 4326) #(x = longitude, y = latitude)
colnames (sf_its1)


# Transformar a proyección métrica para trabajar en metros (recomendado)
sf_its1_proj <- st_transform(sf_its1, crs = 3857)  # Web Mercator
unique (sf_its1_proj$new.ID)

# Agrupar por grupo y unir geometrías
grupos_its1 <- sf_its1_proj %>%
  group_by(new.ID) %>%
  summarise(geometry = st_union(geometry)) %>%  # Une todos los puntos del grupo
  st_centroid()  # Calcula el centroide del grupo
grupos_its1_latlon <- st_transform(grupos_its1, crs = 4326)

# Extraer coordenadas del centroide
coords_centroides1 <- st_coordinates(grupos_its1_latlon)

grupos_its1_df <- grupos_its1_latlon %>%
  st_drop_geometry() %>% #para quitar el agrupamiento...
  mutate(
    centroid_long = coords_centroides1[, "X"],
    centroid_lat = coords_centroides1[, "Y"]
  )

# Unir con dataset original
new_long_centroid<- new_long %>%
  left_join(grupos_its1_df, by = "new.ID") %>%
  arrange(new.ID)%>%
  group_by(new.ID)%>%
  slice_head(n=1)





#### new_dryf ----
### 
new_dryf     <- read_csv("Data/new.dryf.csv")
colnames(new_dryf) #"x"  "y"                

# Convertir a objeto sf
sf_its1 <- st_as_sf(new_dryf, coords = c("x", "y"), crs = 4326) #(x = longitude, y = latitude)
colnames (sf_its1)


# Transformar a proyección métrica para trabajar en metros (recomendado)
sf_its1_proj <- st_transform(sf_its1, crs = 3857)  # Web Mercator
unique (sf_its1_proj$new.ID)

# Agrupar por grupo y unir geometrías
grupos_its1 <- sf_its1_proj %>%
  group_by(new.ID) %>%
  summarise(geometry = st_union(geometry)) %>%  # Une todos los puntos del grupo
  st_centroid()  # Calcula el centroide del grupo
grupos_its1_latlon <- st_transform(grupos_its1, crs = 4326)

# Extraer coordenadas del centroide
coords_centroides1 <- st_coordinates(grupos_its1_latlon)

grupos_its1_df <- grupos_its1_latlon %>%
  st_drop_geometry() %>% #para quitar el agrupamiento...
  mutate(
    centroid_long = coords_centroides1[, "X"],
    centroid_lat = coords_centroides1[, "Y"]
  )

# Unir con dataset original
new_dryf_centroid <- new_dryf%>%
  left_join(grupos_its1_df, by = "new.ID") %>%
  arrange(new.ID)%>%
  group_by(new.ID)%>%
  slice_head(n=1)







####new_mymo ----
new_mymo     <- read_csv("Data/new.mymo.csv")
colnames (new_mymo) # "Latitude"            "Longitude"

# Convertir a objeto sf
sf_its1 <- st_as_sf(new_mymo, coords = c("Longitude", "Latitude"), crs = 4326) #(x = longitude, y = latitude)
colnames (sf_its1)


# Transformar a proyección métrica para trabajar en metros (recomendado)
sf_its1_proj <- st_transform(sf_its1, crs = 3857)  # Web Mercator
unique (sf_its1_proj$new.ID)

# Agrupar por grupo y unir geometrías
grupos_its1 <- sf_its1_proj %>%
  group_by(new.ID) %>%
  summarise(geometry = st_union(geometry)) %>%  # Une todos los puntos del grupo
  st_centroid()  # Calcula el centroide del grupo
grupos_its1_latlon <- st_transform(grupos_its1, crs = 4326)

# Extraer coordenadas del centroide
coords_centroides1 <- st_coordinates(grupos_its1_latlon)

grupos_its1_df <- grupos_its1_latlon %>%
  st_drop_geometry() %>% #para quitar el agrupamiento...
  mutate(
    centroid_long = coords_centroides1[, "X"],
    centroid_lat = coords_centroides1[, "Y"]
  )

# Unir con dataset original
new_mymo_centroid<- new_mymo %>%
  left_join(grupos_its1_df, by = "new.ID") %>%
  arrange(new.ID)%>%
  group_by(new.ID)%>%
  slice_head(n=1)

#decimalLatitude

####new_spun ----
new_spun     <- read_csv("Data/new.spun.csv")
colnames (new_spun ) # "decimalLatitude"               "decimalLongitude" 
class (new_spun$decimalLongitude)
class(new_spun$decimalLatitude)
new_spun$decimalLatitude  <- as.numeric(new_spun$decimalLatitude)
#solucionado problema en coordenadas rapidamente
#Divide los valores de longitud por 1000:
new_spun$decimalLongitude <- new_spun$decimalLongitude / 1000
#Y  que las latitudes estén correctamente en formato numérico (con puntos y no comas):
new_spun$decimalLatitude <- as.numeric(gsub(",", ".", new_spun$decimalLatitude))

new_spun<-new_spun %>%
  filter(is.na(decimalLongitude) | is.na(decimalLatitude))

sum(is.na(new_spun$decimalLongitude))  # ¿Cuántos NAs en longitud?
sum(is.na(new_spun$decimalLatitude))   # ¿Cuántos NAs en latitud?

# Convertir a objeto sf
sf_its1 <- st_as_sf(new_spun, coords = c("decimalLongitude", "decimalLatitude"), crs = 4326) #(x = longitude, y = latitude)


# Transformar a proyección métrica para trabajar en metros (recomendado)
sf_its1_proj <- st_transform(sf_its1, crs = 3857)  # Web Mercator
unique (sf_its1_proj$new.ID)

# Agrupar por grupo y unir geometrías
grupos_its1 <- sf_its1_proj %>%
  group_by(new.ID) %>%
  summarise(geometry = st_union(geometry)) %>%  # Une todos los puntos del grupo
  st_centroid()  # Calcula el centroide del grupo
grupos_its1_latlon <- st_transform(grupos_its1, crs = 4326)

# Extraer coordenadas del centroide
coords_centroides1 <- st_coordinates(grupos_its1_latlon)

grupos_its1_df <- grupos_its1_latlon %>%
  st_drop_geometry() %>% #para quitar el agrupamiento...
  mutate(
    centroid_long = coords_centroides1[, "X"],
    centroid_lat = coords_centroides1[, "Y"]
  )

# Unir con dataset original
new_spun_centroid<- new_spun %>%
  left_join(grupos_its1_df, by = "new.ID") %>%
  arrange(new.ID)%>%
  group_by(new.ID)%>%
  slice_head(n=1)



###Bases publicas----

#### ITS1 only ----
##
new_its1 <- read_csv("Data/new2.its1.csv") #teste com dados new.its1
colnames (new_its1 )
new_its1_filtro <- new_its1 %>%
  filter(
    paper_to_keep == "yes",
    morrone_biogeoregions_Region == "Neotropical",
    sequencing_platform != "PacBio",
    #!(sample_type %in% c("litter")),
    paper_id != "Donald_2021_SQ"
  )


nrow (new_its1_filtro )# 965   
length (unique (new_its1_filtro$new.ID))#77


# Convertir a objeto sf
sf_its1 <- st_as_sf(new_its1_filtro, coords = c("longitude", "latitude"), crs = 4326)

# Transformar a proyección métrica para trabajar en metros (recomendado)
sf_its1_proj <- st_transform(sf_its1, crs = 3857)  # Web Mercator

unique (sf_its1_proj$new.ID)

# Agrupar por grupo y unir geometrías
grupos_its1 <- sf_its1_proj %>%
  group_by(new.ID) %>%
  summarise(geometry = st_union(geometry)) %>%  # Une todos los puntos del grupo
  st_centroid()  # Calcula el centroide del grupo

grupos_its1_latlon <- st_transform(grupos_its1, crs = 4326)


# Extraer coordenadas del centroide
coords_centroides1 <- st_coordinates(grupos_its1_latlon)

grupos_its1_df <- grupos_its1_latlon %>%
  st_drop_geometry() %>% #para quitar el agrupamiento...
  mutate(
    centroid_long = coords_centroides1[, "X"],
    centroid_lat = coords_centroides1[, "Y"]
  )

# Unir con dataset original

new.its1_sites_centroid <- new_its1_filtro %>%
  left_join(grupos_its1_df, by = "new.ID") %>%
  arrange(new.ID)%>%
  group_by(new.ID)%>%
  slice_head(n=1)

nrow (new.its1_sites_centroid) #77



####### ITS2 only-----
new_its2 <- read_csv("Data/new2.its2.csv") #teste com dados new.its2


new_its2_filtro <- new_its2 %>%
  filter(
    paper_to_keep == "yes",
    morrone_biogeoregions_Region == "Neotropical",
    sequencing_platform != "PacBio",
    !(sample_type %in% c("litter")))



nrow (new_its2_filtro )# 741
length (unique (new_its2_filtro$new.ID)) # 239


# Convertir a objeto sf
sf_its2 <- st_as_sf(new_its2_filtro, coords = c("longitude", "latitude"), crs = 4326)

# Transformar a proyección métrica para trabajar en metros (recomendado)
sf_its2_proj <- st_transform(sf_its2, crs = 3857)  # Web Mercator

unique (sf_its2_proj$new.ID)

# Agrupar por grupo y unir geometrías
grupos_its2 <- sf_its2_proj %>%
  group_by(new.ID) %>%
  summarise(geometry = st_union(geometry)) %>%  # Une todos los puntos del grupo
  st_centroid()  # Calcula el centroide del grupo

grupos_its2_latlon <- st_transform(grupos_its2, crs = 4326)


# Extraer coordenadas del centroide
coords_centroides2 <- st_coordinates(grupos_its2_latlon)

grupos_its2_df <- grupos_its2_latlon %>%
  st_drop_geometry() %>% #para quitar el agrupamiento...
  mutate(
    centroid_long = coords_centroides2[, "X"],
    centroid_lat = coords_centroides2[, "Y"]
  )

# Unir con dataset original

new.its2_sites_centroid <- new_its2_filtro %>%
  left_join(grupos_its2_df, by = "new.ID") %>%
  arrange(new.ID)%>%
  group_by(new.ID)%>%
  slice_head(n=1)

nrow (new.its2_sites_centroid) #239 # solo its2


#### ITSboth ----

new_itsboth <- read_csv("Data/new2.its1.csv") #teste com dados new.its1

new_itsboth_filtro <- new_itsboth %>%
  filter(
    paper_to_keep == "yes",
    morrone_biogeoregions_Region == "Neotropical",
    sequencing_platform == "PacBio",
    !(sample_type %in% c("litter"))
  )

colnames(new_itsboth)


levels(as.factor(new_itsboth_filtro$target_gene))
nrow (new_itsboth_filtro )# 429
length (unique (new_itsboth_filtro$new.ID)) # 384


# Convertir a objeto sf
sf_itsboth <- st_as_sf(new_itsboth_filtro, coords = c("longitude", "latitude"), crs = 4326)

# Transformar a proyección métrica para trabajar en metros (recomendado)
sf_itsboth_proj <- st_transform(sf_itsboth, crs = 3857)  # Web Mercator

unique (sf_itsboth_proj$new.ID)

# Agrupar por grupo y unir geometrías
grupos_itsboth <- sf_itsboth_proj %>%
  group_by(new.ID) %>%
  summarise(geometry = st_union(geometry)) %>%  # Une todos los puntos del grupo
  st_centroid()  # Calcula el centroide del grupo

grupos_itsboth_latlon <- st_transform(grupos_itsboth, crs = 4326)


# Extraer coordenadas del centroide
coords_centroidesboth <- st_coordinates(grupos_itsboth_latlon)

grupos_itsboth_df <- grupos_itsboth_latlon %>%
  st_drop_geometry() %>% #para quitar el agrupamiento...
  mutate(
    centroid_long = coords_centroidesboth[, "X"],
    centroid_lat = coords_centroidesboth[, "Y"]
  )

# Unir con dataset original

new.itsboth_sites_centroid <- new_itsboth_filtro %>%
  left_join(grupos_itsboth_df, by = "new.ID") %>%
  arrange(new.ID)%>%
  group_by(new.ID)%>%
  slice_head(n=1)

nrow (new.itsboth_sites_centroid) #384






### Mapa Global FUNGi ------
Neotropic_Map <- st_read("geodata_neotropic/NeotropicMap_Geo.shp")

### MAP Private and Public DB ----
leaflet() %>%
 addPolygons(data = Neotropic_Map ,  weight = 1, fillOpacity = 0.0)  %>%

  addTiles() %>%
  #setView(lng = -3.7, lat = 40.4, zoom = 13) %>%
  #addCirclesMarkers(data = new.its1_sites_centroid , color = "red") #%>%
  addCircles(data = new_diam_centroid, 
             lng = ~centroid_long, lat = ~centroid_lat,
             radius = 10, color = "red", #85pontos
             popup = ~paste0("<b>ID:</b> ", new.ID),
             label = ~new.ID) %>%
   addCircles(data = new_long_centroid, 
              lng = ~centroid_long, lat = ~centroid_lat,
              radius = 10, color = "red", #85pontos
              popup = ~paste0("<b>ID:</b> ", new.ID),
             label = ~new.ID)%>%
  addCircles (data = new_dryf_centroid , 
              lng = ~centroid_long, lat = ~centroid_lat,
             radius = 10, color = "red", #85pontos
             popup = ~paste0("<b>ID:</b> ", new.ID),
             label = ~new.ID)%>%
  addCircles (data = new_mymo_centroid, 
              lng = ~centroid_long, lat = ~centroid_lat,
            radius = 10, color = "red", #85pontos
            popup = ~paste0("<b>ID:</b> ", new.ID),
            label = ~new.ID)%>%
  addCircles (data = new_atlasmxb_centroid, 
              lng = ~centroid_long, lat = ~centroid_lat,
              radius = 10, color = "red", #85pontos
              popup = ~paste0("<b>ID:</b> ", new.ID),
              label = ~new.ID)%>%
  # ITS1 - círculos azules
addCircles(data = new.its1_sites_centroid, 
           lng = ~centroid_long, lat = ~centroid_lat,
           radius = ~n_aggregated_samples * 3,  # Ajusta el factor para un buen tamaño visual
           color = "blue",
           popup = ~paste0(
             "<b>ID:</b> ", new.ID, "<br>",
             "<b>n_aggregated_samples:</b> ", n_aggregated_samples
           ),
           label = ~new.ID) %>%
  
  # ITS2 - círculos verdes
  addCircles(data = new.its2_sites_centroid,
             lng = ~centroid_long, lat = ~centroid_lat,
             radius = ~n_aggregated_samples * 3,
             color = "green",
             popup = ~paste0(
               "<b>ID:</b> ", new.ID, "<br>",
               "<b>n_aggregated_samples:</b> ", n_aggregated_samples
             ),
             label = ~new.ID) %>%
  
  # ITSboth - círculos violetas
  addCircles(data = new.itsboth_sites_centroid,
             lng = ~centroid_long, lat = ~centroid_lat,
             radius = ~n_aggregated_samples * 3,
             color = "violet",
             popup = ~paste0(
               "<b>ID:</b> ", new.ID, "<br>",
               "<b>n_aggregated_samples:</b> ", n_aggregated_samples
             ),
             label = ~new.ID)



