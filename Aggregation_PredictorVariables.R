# Objetivo crear. un data.frame llamado env_data_summary con una fila por new.ID y el promedio de cada variable ambiental y climática, excluyendo los NA y posibles ceros no confiables.


library(readr)
new_its2 <- read_csv("Data/new.its2.csv")
new_its1 <- read_csv("Data/new.its1.csv")

colnames(new_its1 )

#Paso 1: Define las columnas de interés

# Variables ambientales y climáticas
# Seleccionar todas las columnas relevantes (incluye mensuales)
vars_env_clima <- c(
  "new.ID",
  
  # Clima anual (anuales del dataset climenv)
  "MAT", "MAP", "MAT_study", "MAP_study",
  "globfungi_elevation", "elevatr_elevation_4.5km2",
  "climenv_annavg_tavg_m", "climenv_annavg_abmt", 
  "climenv_annavg_tmin_m", "climenv_annavg_tmax_m", 
  "climenv_annavg_prec_m",
  
  # Clima mensual: temperatura promedio
  "climenv_monthavg_tavg_m_Jan", "climenv_monthavg_tavg_m_Feb", "climenv_monthavg_tavg_m_Mar",
  "climenv_monthavg_tavg_m_Apr", "climenv_monthavg_tavg_m_May", "climenv_monthavg_tavg_m_Jun",
  "climenv_monthavg_tavg_m_Jul", "climenv_monthavg_tavg_m_Aug", "climenv_monthavg_tavg_m_Sep",
  "climenv_monthavg_tavg_m_Oct", "climenv_monthavg_tavg_m_Nov", "climenv_monthavg_tavg_m_Dec",
  
  # Clima mensual: anomalías de temperatura base (ABMT)
  "climenv_monthavg_abmt_Jan", "climenv_monthavg_abmt_Feb", "climenv_monthavg_abmt_Mar",
  "climenv_monthavg_abmt_Apr", "climenv_monthavg_abmt_May", "climenv_monthavg_abmt_Jun",
  "climenv_monthavg_abmt_Jul", "climenv_monthavg_abmt_Aug", "climenv_monthavg_abmt_Sep",
  "climenv_monthavg_abmt_Oct", "climenv_monthavg_abmt_Nov", "climenv_monthavg_abmt_Dec",
  
  # Clima mensual: temperatura mínima
  "climenv_monthavg_tmin_m_Jan", "climenv_monthavg_tmin_m_Feb", "climenv_monthavg_tmin_m_Mar",
  "climenv_monthavg_tmin_m_Apr", "climenv_monthavg_tmin_m_May", "climenv_monthavg_tmin_m_Jun",
  "climenv_monthavg_tmin_m_Jul", "climenv_monthavg_tmin_m_Aug", "climenv_monthavg_tmin_m_Sep",
  "climenv_monthavg_tmin_m_Oct", "climenv_monthavg_tmin_m_Nov", "climenv_monthavg_tmin_m_Dec",
  
  # Clima mensual: temperatura máxima
  "climenv_monthavg_tmax_m_Jan", "climenv_monthavg_tmax_m_Feb", "climenv_monthavg_tmax_m_Mar",
  "climenv_monthavg_tmax_m_Apr", "climenv_monthavg_tmax_m_May", "climenv_monthavg_tmax_m_Jun",
  "climenv_monthavg_tmax_m_Jul", "climenv_monthavg_tmax_m_Aug", "climenv_monthavg_tmax_m_Sep",
  "climenv_monthavg_tmax_m_Oct", "climenv_monthavg_tmax_m_Nov", "climenv_monthavg_tmax_m_Dec",
  
  # Clima mensual: precipitación
  "climenv_monthavg_prec_m_Jan", "climenv_monthavg_prec_m_Feb", "climenv_monthavg_prec_m_Mar",
  "climenv_monthavg_prec_m_Apr", "climenv_monthavg_prec_m_May", "climenv_monthavg_prec_m_Jun",
  "climenv_monthavg_prec_m_Jul", "climenv_monthavg_prec_m_Aug", "climenv_monthavg_prec_m_Sep",
  "climenv_monthavg_prec_m_Oct", "climenv_monthavg_prec_m_Nov", "climenv_monthavg_prec_m_Dec",
  
  # Otras variables climáticas
  "climenv_elev", "climenv_abt", "climenv_tap", "climenv_per", "climenv_holdridge_class",
  
  # Propiedades del suelo
  "total_C_content", "total_N_content", "organic_matter_content", 
  "pH", "total_Ca", "total_P", "total_K", 
  "soc", "cec", "sand", "silt", "nitogen", "phh2o"
)

# Agregar columna de ID
vars_env_clima <- c("new.ID", vars_env_clima)

# Filtrar solo las columnas seleccionadas
env_data <- new_its1 %>%
  select(all_of(vars_env_clima))

# Verificación de ceros (opcional: revisar manualmente antes de imputar como NA)
summary(env_data == 0)

# Convertir ceros a NA si lo consideras apropiado
env_data_clean <- env_data %>%
  mutate(across(where(is.numeric) & !matches("new.ID"), ~ ifelse(. == 0, NA, .)))

# Agrupar por new.ID y calcular la media ignorando NA
env_data_summary <- env_data_clean %>%
  group_by(new.ID) %>%
  summarise(across(where(is.numeric), ~ mean(.x, na.rm = TRUE)), .groups = "drop")

