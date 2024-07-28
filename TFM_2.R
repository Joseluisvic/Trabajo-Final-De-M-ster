rm(list = ls())

library(readr)
library(openxlsx)
library(dplyr)
library(visdat)
library(lubridate)
library (ggplot2)
library(factoextra)
library(kableExtra)
library(NbClust)
library(ClusterR)
library(gridExtra)
library(patchwork)
library(PerformanceAnalytics)
library(pgirmess)
library(tree)
library(ISLR2)
library(broom)

datos <- read_csv("listings_Mal.csv")
datos$barrio <- datos$neighbourhood_cleansed
#Limpio las columnas de baños
datos$bathrooms <- as.numeric(gsub("[^0-9.]", "", datos$bathrooms_text))
datos$bathrooms

#Limpio la columna de precio
datos$price <- as.numeric(gsub("[^0-9.]", "", datos$price))
datos$price
# Limpio la columna 'host_response_time' 
datos$host_response_rate <- as.numeric(gsub("%", "", datos$host_response_rate)) / 100
datos$host_response_rate
# Limpio la columna 'host_acceptance_rate' 
datos$host_acceptance_rate <- as.numeric(gsub("%", "", datos$host_acceptance_rate)) / 100
datos$host_acceptance_rate
#Se dan valores a las columnas de textos
#Empiezo con host_response_time
datos <- datos %>%
  mutate(host_response_time = case_when(
    host_response_time == "within an hour" ~ 1,
    host_response_time == "within a few hours" ~ 2,
    host_response_time == "within a day" ~ 3,
    host_response_time == "a few days or more" ~ 4,
    TRUE ~ NA_real_  # Para manejar cualquier otro valor inesperado
  ))
#Columna de superhost => Se darán valores binarios, 0 si False y 1 si True 
datos <- datos %>%
  mutate(host_is_superhost = case_when(
    host_is_superhost  == "FALSE" ~ 0,
    host_is_superhost  == "TRUE" ~ 1,
    TRUE ~ NA_real_  # Para manejar cualquier otro valor inesperado
  ))
#Columna foto de perfil
datos <- datos %>%
  mutate(host_has_profile_pic = case_when(
    host_has_profile_pic  == "FALSE" ~ 0,
    host_has_profile_pic  == "TRUE" ~ 1,
    TRUE ~ NA_real_  # Para manejar cualquier otro valor inesperado
  ))
#Columna verificación
datos <- datos %>%
  mutate(host_identity_verified = case_when(
    host_identity_verified == "FALSE" ~ 0,
    host_identity_verified  == "TRUE" ~ 1,
    TRUE ~ NA_real_  # Para manejar cualquier otro valor inesperado
  ))
#columna tipo de cosa a alquilar
datos <- datos %>%
  mutate(room_type = case_when(
    room_type == "Entire home/apt" ~ 1,
    room_type == "Private room" ~ 2,
    room_type == "Shared room" ~ 3,
    TRUE ~ NA_real_  # Para manejar cualquier otro valor inesperado
  ))
#Columna tipo de propiedad

# Crear un vector de equivalencias
equivalencias <- c(
  "Entire rental unit" = 1,
  "Private room in rental unit" = 2,
  "Shared room in rental unit" = 3,
  "Private room in townhouse" = 4,
  "Private room in home" = 5,
  "Entire loft" = 6,
  "Entire condo" = 7,
  "Entire home" = 8,
  "Entire serviced apartment" = 9,
  "Entire townhouse" = 10,
  "Entire cottage" = 11,
  "Entire guest suite" = 12,
  "Entire chalet" = 13,
  "Private room in condo" = 14,
  "Room in boutique hotel" = 15,
  "Entire villa" = 16,
  "Private room in guesthouse" = 17,
  "Room in serviced apartment" = 18,
  "Entire guesthouse" = 19,
  "Floor" = 20,
  "Camper/RV" = 21,
  "Tiny home" = 22,
  "Entire vacation home" = 23,
  "Private room in guest suite" = 24,
  "Room in aparthotel" = 25,
  "Private room" = 26,
  "Shared room in earthen home" = 27,
  "Private room in loft" = 28,
  "Private room in chalet" = 29,
  "Private room in hostel" = 30,
  "Private room in casa particular" = 31,
  "Entire cabin" = 32,
  "Private room in serviced apartment" = 33,
  "Entire place" = 34,
  "Shared room in chalet" = 35,
  "Casa particular" = 36,
  "Private room in bed and breakfast" = 37,
  "Dome" = 38,
  "Private room in villa" = 39,
  "Room in hotel" = 40,
  "Private room in vacation home" = 41,
  "Private room in farm stay" = 42,
  "Private room in yurt" = 43,
  "Shared room in casa particular" = 44,
  "Cave" = 45,
  "Shared room in hotel" = 46
)
# Realizar la conversión usando mutate() y case_when()
datos <- datos %>%
  mutate(property_type = case_when(
    property_type %in% names(equivalencias) ~ equivalencias[property_type],
    TRUE ~ NA_integer_  # En caso de que haya algún valor que no esté en las equivalencias
  ))

#Columna del barrio 
# Crear un vector de equivalencias para neighbourhood_cleansed
neighbourhood_equivalencias <- c(
  "Este" = 1,
  "Centro" = 2,
  "Churriana" = 3,
  "Carretera de Cadiz" = 4,
  "Bailen-Miraflores" = 5,
  "Cruz De Humilladero" = 6,
  "Teatinos-Universidad" = 7,
  "Puerto de la Torre" = 8,
  "Ciudad Jardin" = 9,
  "Campanillas" = 10,
  "Palma-Palmilla" = 11
)

# Realizar la conversión usando mutate() y case_when()
datos <- datos %>%
  mutate(neighbourhood_cleansed = case_when(
    neighbourhood_cleansed %in% names(neighbourhood_equivalencias) ~ neighbourhood_equivalencias[neighbourhood_cleansed],
    TRUE ~ NA_integer_  # En caso de que haya algún valor que no esté en las equivalencias
  ))

#Creo una nueva columna 'neighborhood_overview_flag'; Devuelve 1 si tiene descripción y 0 si no
datos$neighborhood_overview_flag <- ifelse(!is.na(datos$neighborhood_overview), 1, 0)
#Genero la columna de antigüedad en la plataforma, basándome en el tiempo que lleva
#el host en la plataforma y le doy un valor numérico para poder trabajar con ellas.
datos$antiguedad <- as.numeric(datos$host_since)

#Extraigo las columnas de datos que interesan

data <- datos[, c("id", "accommodates", "antiguedad", "host_response_rate", "host_response_time", 
                           "neighborhood_overview_flag", "bathrooms", "beds", "price",
                           "host_is_superhost", "host_listings_count", "host_has_profile_pic", 
                           "host_identity_verified", "neighbourhood_cleansed", "property_type", "room_type",
                           "minimum_nights", "maximum_nights")]

#Identifico y elimino los missing values
vis_miss(data)
data %>% filter(
  !is.na(id),
  !is.na(accommodates),
  !is.na(antiguedad),
  !is.na(host_response_rate),
  !is.na(host_response_time),
  !is.na(neighborhood_overview_flag),
  !is.na(bathrooms),
  !is.na(beds),
  !is.na(price),
  !is.na(host_is_superhost),
  !is.na(host_listings_count),
  !is.na(host_has_profile_pic),
  !is.na(host_identity_verified),
  !is.na(neighbourhood_cleansed),
  !is.na(property_type),
  !is.na(room_type),
  !is.na(minimum_nights),
  !is.na(maximum_nights)
) %>%  select(id, accommodates, antiguedad, host_response_rate, host_response_time, neighborhood_overview_flag,
       bathrooms, beds, price, host_is_superhost, host_listings_count, host_has_profile_pic, 
       host_identity_verified, neighbourhood_cleansed, property_type, room_type, minimum_nights, maximum_nights)

data <- data %>% filter(
  !is.na(id)&
  !is.na(accommodates)&
  !is.na(antiguedad)&
  !is.na(host_response_rate)&
  !is.na(host_response_time)&
  !is.na(neighborhood_overview_flag)&
  !is.na(bathrooms)&
  !is.na(beds)&
  !is.na(price)&
  !is.na(host_is_superhost)&
  !is.na(host_listings_count)&
  !is.na(host_has_profile_pic)&
  !is.na(host_identity_verified)&
  !is.na(neighbourhood_cleansed)&
  !is.na(property_type)&
  !is.na(room_type)&
  !is.na(minimum_nights)&
  !is.na(maximum_nights)
)
#Estudio la frecuencia de aparición de los barrios. Al ser datos individuales, con ello se puede hacer un mapa de calor
#para detectar en que barrios hay mayor presencia de anuncios.

# Calcular la frecuencia de los barrios
frecuencias <- table(datos$barrio)
df <- as.data.frame(frecuencias)
colnames(df) <- c("barrio", "frecuencia")

# Ordenar por frecuencia
df <- df[order(-df$frecuencia), ]

# Crear el gráfico de burbujas
plot_burbujas <- ggplot(df, aes(x = reorder(barrio, frecuencia), y = frecuencia, size = frecuencia, fill = frecuencia)) +
  geom_point(shape = 21, color = "black", alpha = 0.7) +
  scale_size(range = c(3, 15)) +
  scale_fill_gradient(low = "blue", high = "red") +
  theme_minimal() +
  coord_flip() +
  labs(title = "Frecuencia de los Barrios",
       x = "Barrios",
       y = "Frecuencia",
       size = "Frecuencia",
       fill = "Frecuencia")
print(plot_burbujas)

kable(df, format = "html", table.attr = "class='table table-bordered'") %>%
  kable_styling(full_width = FALSE, position = "left")


#Ahora paso al análisis Clúster. Para hallar el número óptimo de Clúster utilizo el índice de Silhouette
#Primero, tipifico las variables:
zdata <- data.frame(scale(data))
summary(zdata)
#Índice de Silhouette
fviz_nbclust(x = zdata, FUNcluster = kmeans, method = "silhouette", k.max = 15) +
  labs(title = "Número óptimo de clusters")
#Aunque el índice de Silhouette dice que el número óptimo de clusters es 2, hay otros 2 picos
#que pueden considerarse, 5 cluster y 11. En este caso se van a construir 5.
set.seed(123)
k <- 5

cluster_k <-KMeans_rcpp (zdata,
                         clusters = k,
                         num_init = 10,
                         max_iters = 100,
                         initializer = "kmeans++")

whatcluster_k <-as.factor(cluster_k$clusters)
levels(whatcluster_k)
data <-cbind(data, whatcluster_k)
summary(data)

tablamedias <- data %>%
  group_by(whatcluster_k) %>%
  summarise( obs = length(whatcluster_k),
    accommodates = round(mean(accommodates, na.rm = TRUE), 2),
    antiguedad = round(mean(antiguedad, na.rm = TRUE), 2),
    host_response_rate = round(mean(host_response_rate, na.rm = TRUE), 2),
    host_response_time = round(mean(host_response_time, na.rm = TRUE), 2),
    neighborhood_overview_flag = round(mean(neighborhood_overview_flag, na.rm = TRUE), 2),
    bathrooms = round(mean(bathrooms, na.rm = TRUE), 2),
    beds = round(mean(beds, na.rm = TRUE), 2),
    price = round(mean(price, na.rm = TRUE), 2),
    host_is_superhost = round(mean(host_is_superhost, na.rm = TRUE), 2),
    host_listings_count = round(mean(host_listings_count, na.rm = TRUE), 2),
    host_has_profile_pic = round(mean(host_has_profile_pic, na.rm = TRUE), 2),
    host_identity_verified = round(mean(host_identity_verified, na.rm = TRUE), 2),
    neighbourhood_cleansed = round(mean(neighbourhood_cleansed, na.rm = TRUE), 2),
    property_type = round(mean(property_type, na.rm = TRUE), 2),
    room_type = round(mean(room_type, na.rm = TRUE), 2),
    minimum_nights = round(mean(minimum_nights, na.rm = TRUE), 2),
    maximum_nights = round(mean(maximum_nights, na.rm = TRUE), 2)
  )
knitr.table.format = "html"

tablamedias %>%
  kable(caption = "Método de k-medias. Medias de variables",
        col.names = c("Clúster", "Observaciones", "Accommodates", "Antiguedad", "Host Response Rate", "Host Response Time", "Neighborhood Overview Flag", "Bathrooms", "Beds", "Price", "Host is Superhost", "Host Listings Count", "Host has Profile Pic", "Host Identity Verified", "Neighbourhood Cleansed", "Property Type", "Room Type", "Minimum Nights", "Maximum Nights")) %>%
  kable_styling(full_width = F, bootstrap_options = c("striped", "bordered", "condensed"), position = "center", font_size = 12) %>%
  row_spec(0, bold = TRUE, align = "c") %>%
  row_spec(1:nrow(tablamedias), bold = FALSE, align = "c")

#Análisis Kruskal-Wallis

KMC_acc <- kruskalmc(data$accommodates ~ data$whatcluster_k)
print(KMC_acc)

KMC_ant <- kruskalmc(data$antiguedad ~ data$whatcluster_k)
print(KMC_ant)

KMC_host_res_rate <- kruskalmc(data$host_response_rate ~ data$whatcluster_k)
print(KMC_host_res_rate)

KMC_host_res_time <- kruskalmc(data$host_response_time ~ data$whatcluster_k)
print(KMC_host_res_time)

KMC_bath <- kruskalmc(data$bathrooms ~ data$whatcluster_k)
print(KMC_bath)

KMC_beds <- kruskalmc(data$beds ~ data$whatcluster_k)
print(KMC_beds)

KMC_price <- kruskalmc(data$price ~ data$whatcluster_k)
print(KMC_price)

KMC_superhost <- kruskalmc(data$host_is_superhost ~ data$whatcluster_k)
print(KMC_superhost)

KMC_listings <- kruskalmc(data$host_listings_count ~ data$whatcluster_k)
print(KMC_listings)

KMC_profile_pic <- kruskalmc(data$host_has_profile_pic ~ data$whatcluster_k)
print(KMC_profile_pic)

KMC_identity_verified <- kruskalmc(data$host_identity_verified ~ data$whatcluster_k)
print(KMC_identity_verified)

KMC_neigh_cleansed <- kruskalmc(data$neighbourhood_cleansed ~ data$whatcluster_k)
print(KMC_neigh_cleansed)

KMC_property_type <- kruskalmc(data$property_type ~ data$whatcluster_k)
print(KMC_property_type)

KMC_room_type <- kruskalmc(data$room_type ~ data$whatcluster_k)
print(KMC_room_type)

KMC_minimum_nights <- kruskalmc(data$minimum_nights ~ data$whatcluster_k)
print(KMC_minimum_nights)

KMC_maximum_nights <- kruskalmc(data$maximum_nights ~ data$whatcluster_k)
print(KMC_maximum_nights)

KMC_neigh_overview_flag <- kruskalmc(data$neighborhood_overview_flag ~ data$whatcluster_k)
print(KMC_neigh_overview_flag)

#ANÁLISIS DE LA OFERTA: OFERTA POR LOS DIFERENTES BARRIOS
# Agrupar datos por barrio y calcular estadísticas descriptivas
# Vector de equivalencias de los barrios
neighbourhood_equivalencias <- c(
  "1" = "Este",
  "2" = "Centro",
  "3" = "Churriana",
  "4" = "Carretera de Cadiz",
  "5" = "Bailen-Miraflores",
  "6" = "Cruz De Humilladero",
  "7" = "Teatinos-Universidad",
  "8" = "Puerto de la Torre",
  "9" = "Ciudad Jardin",
  "10" = "Campanillas",
  "11" = "Palma-Palmilla"
)

# Convertir códigos a nombres de barrios
data <- data %>%
  mutate(neighbourhood_cleansed = recode(neighbourhood_cleansed, !!!neighbourhood_equivalencias))

#Análisis de precios por barrios
# Crear boxplot de precios por barrios
# Filtrar valores no finitos y valores extremadamente altos
data_filtered <- data %>%
  filter(is.finite(price) & price < 1000) # Ajustar el umbral según sea necesario

# Crear histograma
ggplot(data_filtered, aes(x = price)) +
  geom_histogram(binwidth = 10, fill = "blue", color = "black") +
  labs(title = "Distribución de Precios de Airbnb", x = "Precio", y = "Frecuencia") +
  theme_minimal()

# Crear boxplot
ggplot(data_filtered, aes(y = price)) +
  geom_boxplot(fill = "blue") +
  labs(title = "Boxplot de Precios de Airbnb", y = "Precio") +
  theme_minimal()
# Crear boxplot de precios por barrios
ggplot(data_filtered, aes(x = neighbourhood_cleansed, y = price)) +
  geom_boxplot(fill = "blue") +
  labs(title = "Distribución de Precios por Barrios", x = "Barrio", y = "Precio") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

# Agrupar datos por barrio y calcular estadísticas descriptivas
tabla_barrios <- data %>%
  group_by(neighbourhood_cleansed) %>%
  summarise(
    num_properties = n(),
    accommodates = round(mean(accommodates, na.rm = TRUE), 2),
    antiguedad = round(mean(antiguedad, na.rm = TRUE), 2),
    host_response_rate = round(mean(host_response_rate, na.rm = TRUE), 2),
    host_response_time = round(mean(host_response_time, na.rm = TRUE), 2),
    neighborhood_overview_flag = round(mean(neighborhood_overview_flag, na.rm = TRUE), 2),
    bathrooms = round(mean(bathrooms, na.rm = TRUE), 2),
    beds = round(mean(beds, na.rm = TRUE), 2),
    price = round(mean(price, na.rm = TRUE), 2),
    host_is_superhost = round(mean(host_is_superhost, na.rm = TRUE), 2),
    host_listings_count = round(mean(host_listings_count, na.rm = TRUE), 2),
    host_has_profile_pic = round(mean(host_has_profile_pic, na.rm = TRUE), 2),
    host_identity_verified = round(mean(host_identity_verified, na.rm = TRUE), 2),
    property_type = round(mean(property_type, na.rm = TRUE), 2),
    room_type = round(mean(room_type, na.rm = TRUE), 2),
    minimum_nights = round(mean(minimum_nights, na.rm = TRUE), 2),
    maximum_nights = round(mean(maximum_nights, na.rm = TRUE), 2)
  )

# Mostrar tabla con formato
tabla_barrios %>%
  kable(caption = "Estadísticas descriptivas por barrio",
        col.names = c("Barrio", "Num. Propiedades", "Accommodates", "Antigüedad", "Host Response Rate", "Host Response Time", "Neighborhood Overview Flag", "Bathrooms", "Beds", "Price", "Host is Superhost", "Host Listings Count", "Host has Profile Pic", "Host Identity Verified", "Property Type", "Room Type", "Minimum Nights", "Maximum Nights")) %>%
  kable_styling(full_width = F, bootstrap_options = c("striped", "bordered", "condensed"), position = "center", font_size = 12) %>%
  row_spec(0, bold = TRUE, align = "c") %>%
  row_spec(1:nrow(tabla_barrios), bold = FALSE, align = "c")

#Histograma por barrios
ggplot(data_filtered, aes(x = price, fill = factor(neighbourhood_cleansed))) +
  geom_histogram(binwidth = 10, position = "dodge") +
  labs(title = "Distribución del Precio por Barrio",
       x = "Precio",
       y = "Número de Propiedades",
       fill = "Barrio") +
  theme_minimal() +
  scale_fill_discrete(name = "Barrio") + 
  ylim(0, 200)

#BoxPlot precio por barrios
ggplot(data_filtered, aes(x = factor(neighbourhood_cleansed), y = price)) +
  geom_boxplot( fill = "red") +
  labs(title = "Boxplot del Precio por Barrio",
       x = "Barrio",
       y = "Precio") +
  theme_minimal() +
  scale_x_discrete(name = "Barrio")

# Modelo de regresión para entender los factores que afectan al precio
modelo_precio <- lm(price ~ accommodates + antiguedad + host_response_rate + host_response_time + bathrooms + beds + host_is_superhost + host_listings_count + host_has_profile_pic + host_identity_verified + property_type + room_type + minimum_nights + maximum_nights + factor(neighbourhood_cleansed), data = data_filtered)

# Resumen del modelo
summary(modelo_precio)

# Extraer los coeficientes del modelo con sus intervalos de confianza
coef_modelo <- tidy(modelo_precio, conf.int = TRUE)

# Visualizar los coeficientes del modelo
ggplot(coef_modelo, aes(x = term, y = estimate)) +
  geom_point() +
  geom_errorbar(aes(ymin = conf.low, ymax = conf.high), width = 0.2) +
  coord_flip() +  # Girar el gráfico para una mejor visualización
  labs(title = "Coeficientes del Modelo de Regresión",
       x = "Variables",
       y = "Estimaciones de los Coeficientes") +
  theme_minimal()
# Predicciones del modelo
data_filtered$predicted <- predict(modelo_precio, newdata = data_filtered)
data_filtered$residuals <- residuals(modelo_precio)

# Gráfico de residuos vs valores ajustados
ggplot(data_filtered, aes(x = predicted, y = residuals)) +
  geom_point(alpha = 0.5) +
  geom_hline(yintercept = 0, color = "red", linetype = "dashed") +
  labs(title = "Residuos vs Valores Ajustados",
       x = "Valores Ajustados",
       y = "Residuos") +
  theme_minimal()
# Gráfico de predicciones vs valores reales
ggplot(data_filtered, aes(x = price, y = predicted)) +
  geom_point(alpha = 0.5) +
  geom_abline(intercept = 0, slope = 1, color = "red", linetype = "dashed") +
  labs(title = "Predicciones vs Valores Reales",
       x = "Valores Reales",
       y = "Predicciones") +
  theme_minimal()
# Filtrar solo las variables de los barrios
coef_barrios <- coef_modelo %>% filter(grepl("neighbourhood_cleansed", term))

# Visualizar los coeficientes para cada barrio
ggplot(coef_barrios, aes(x = term, y = estimate)) +
  geom_point() +
  geom_errorbar(aes(ymin = conf.low, ymax = conf.high), width = 0.2) +
  coord_flip() +  # Girar el gráfico para una mejor visualización
  labs(title = "Efecto de Cada Barrio en el Precio",
       x = "Barrio",
       y = "Estimaciones de los Coeficientes") +
  theme_minimal()
