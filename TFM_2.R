rm(list = ls())

library(readr)
library(openxlsx)
library(dplyr)
library(MASS)
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
library(viridis)
library(pROC)
library(tidyverse)
library(caret)
library(htmlTable)
library(caret)

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
data <- data %>%
  filter(
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
  ) %>%
  dplyr::select(
    id, accommodates, antiguedad, host_response_rate, host_response_time, neighborhood_overview_flag,
    bathrooms, beds, price, host_is_superhost, host_listings_count, host_has_profile_pic,
    host_identity_verified, neighbourhood_cleansed, property_type, room_type, minimum_nights, maximum_nights
  )
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
write.csv(data, "data.csv", row.names = FALSE)
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


# ANÁLISIS DE LA DEMANDA
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
data$neighbourhood_cleansed <- as.character(neighbourhood_equivalencias[as.character(data$neighbourhood_cleansed)])

# Análisis de precios por barrios
# Filtrar valores no finitos y valores extremadamente altos
data$price <- as.numeric(data$price)
data_filtered <- data[!is.na(data$price) & data$price < 1000, ]

# Crear histograma de precios

ggplot(data_filtered, aes(x = price)) +
  geom_histogram(breaks = seq(0, 1000, by = 10), fill = "blue", color = "black") +
  ggtitle('Distribución de Precios de Airbnb') +
  xlab('Precio') +
  ylab('Frecuencia') +
  theme_minimal() +
  theme(plot.title = element_text(hjust = 0.5)) +

# Crear boxplot de precios
ggplot(data_filtered, aes(y = price)) +
  geom_boxplot(fill = "blue", color = "black") +
  ggtitle('Boxplot de Precios de Airbnb') +
  ylab('Precio') +
  theme_minimal() +
  theme(plot.title = element_text(hjust = 0.5))

# Crear boxplot de precios por barrios
ggplot(data_filtered, aes(x = neighbourhood_cleansed, y = price)) +
  geom_boxplot(fill = "blue", color = "black") +
  ggtitle('Distribución de Precios por Barrios') +
  xlab('Barrio') +
  ylab('Precio') +
  theme_minimal() +
  theme(
    plot.title = element_text(hjust = 0.5),
    axis.text.x = element_text(angle = 45, hjust = 1) # Rotar etiquetas del eje x
  )

# Crear el histograma de precios
ggplot(data_filtered, aes(x = price)) +
  geom_histogram(bins = 300, fill = "blue", color = "black") +
  ggtitle('Distribución de Precios de Airbnb') +
  xlab('Precio') +
  ylab('Frecuencia') +
  theme_minimal() +
  theme(
    plot.title = element_text(hjust = 0.5),
    axis.text.x = element_text(angle = 45, hjust = 1) # Rotar etiquetas del eje x
  )

# Crear boxplot de precios por barrios
ggplot(data_filtered, aes(x = neighbourhood_cleansed, y = price)) +
  geom_boxplot(fill = "blue", color = "black") +
  ggtitle('Distribución de Precios por Barrios') +
  xlab('Barrio') +
  ylab('Precio') +
  theme_minimal() +
  theme(
    plot.title = element_text(hjust = 0.5),
    axis.text.x = element_text(angle = 45, hjust = 1) # Rotar etiquetas del eje x
  )

# Agrupar datos por barrio y calcular estadísticas descriptivas
tabla_barrios <- data_filtered %>%
  group_by(neighbourhood_cleansed) %>%
  summarise(
    num_properties = n(),
    accommodates = mean(accommodates, na.rm = TRUE),
    antiguedad = mean(antiguedad, na.rm = TRUE),
    host_response_rate = mean(host_response_rate, na.rm = TRUE),
    host_response_time = mean(as.numeric(host_response_time), na.rm = TRUE),
    neighborhood_overview_flag = mean(neighborhood_overview_flag, na.rm = TRUE),
    bathrooms = mean(bathrooms, na.rm = TRUE),
    beds = mean(beds, na.rm = TRUE),
    price = mean(price, na.rm = TRUE),
    host_is_superhost = mean(host_is_superhost, na.rm = TRUE),
    host_listings_count = mean(host_listings_count, na.rm = TRUE),
    host_has_profile_pic = mean(host_has_profile_pic, na.rm = TRUE),
    host_identity_verified = mean(host_identity_verified, na.rm = TRUE),
    property_type = mean(as.numeric(property_type), na.rm = TRUE),
    room_type = mean(as.numeric(room_type), na.rm = TRUE),
    minimum_nights = mean(minimum_nights, na.rm = TRUE),
    maximum_nights = mean(maximum_nights, na.rm = TRUE)
  )

# Redondear las columnas a 4 decimales
tabla_barrios <- tabla_barrios %>%
  mutate_if(is.numeric, round, digits = 4)

# Crear una fila de encabezado personalizada
header <- c("Neighbourhood", "Number of Properties", "Accommodates (Media)", 
            "Antigüedad (Media)", "Host Response Rate (Media)", "Host Response Time (Media)", 
            "Neighborhood Overview Flag (Media)", "Bathrooms (Media)", "Beds (Media)", 
            "Price (Media)", "Host is Superhost (Media)", "Host Listings Count (Media)", 
            "Host Has Profile Pic (Media)", "Host Identity Verified (Media)", 
            "Property Type (Media)", "Room Type (Media)", "Minimum Nights (Media)", 
            "Maximum Nights (Media)")

# Convertir la tabla a HTML con estilos personalizados
tabla_html <- htmlTable(tabla_barrios,
                        header = header,
                        rnames = FALSE,
                        css.cell = "padding-right: 30px; padding-bottom: 5px;",
                        col.rgroup = c("none", "#F7F7F7"),
                        align.header = "c",
                        align = "c")

# Añadir estilo al encabezado para que esté más separado del contenido
tabla_html <- addHtmlTableStyle(tabla_html, css.table = "border-collapse: collapse; width: 100%;", css.header = "border-bottom: 2px solid #ddd; padding: 10px;")

# Mostrar la tabla HTML
print(tabla_html)
# Exportar la tabla HTML a un archivo
writeLines(tabla_html, "tabla_barrios.html")

#TABLA BARRIOS MEDIANA
# Agrupar datos por barrio y calcular estadísticas descriptivas (medianas)
tabla_barrios_mediana <- data_filtered %>%
  group_by(neighbourhood_cleansed) %>%
  summarise(
    num_properties = n(),
    accommodates = median(accommodates, na.rm = TRUE),
    antiguedad = median(antiguedad, na.rm = TRUE),
    host_response_rate = median(host_response_rate, na.rm = TRUE),
    host_response_time = median(as.numeric(host_response_time), na.rm = TRUE),
    neighborhood_overview_flag = median(neighborhood_overview_flag, na.rm = TRUE),
    bathrooms = median(bathrooms, na.rm = TRUE),
    beds = median(beds, na.rm = TRUE),
    price = median(price, na.rm = TRUE),
    host_is_superhost = median(host_is_superhost, na.rm = TRUE),
    host_listings_count = median(host_listings_count, na.rm = TRUE),
    host_has_profile_pic = median(host_has_profile_pic, na.rm = TRUE),
    host_identity_verified = median(host_identity_verified, na.rm = TRUE),
    property_type = median(as.numeric(property_type), na.rm = TRUE),
    room_type = median(as.numeric(room_type), na.rm = TRUE),
    minimum_nights = median(minimum_nights, na.rm = TRUE),
    maximum_nights = median(maximum_nights, na.rm = TRUE)
  )

# Redondear las columnas a 4 decimales
tabla_barrios_mediana <- tabla_barrios_mediana %>%
  mutate_if(is.numeric, round, digits = 4)

# Crear una fila de encabezado personalizada
header <- c("Neighbourhood", "Number of Properties", "Accommodates (median)", 
            "Antigüedad (median)", "Host Response Rate (median)", "Host Response Time (median)", 
            "Neighborhood Overview Flag (median)", "Bathrooms (median)", "Beds (median)", 
            "Price (median)", "Host is Superhost (median)", "Host Listings Count (median)", 
            "Host Has Profile Pic (median)", "Host Identity Verified (median)", 
            "Property Type (median)", "Room Type (median)", "Minimum Nights (median)", 
            "Maximum Nights (median)")

# Convertir la tabla a HTML con estilos personalizados
tabla_html_mediana <- htmlTable(tabla_barrios_mediana,
                        header = header,
                        rnames = FALSE,
                        css.cell = "padding-right: 30px; padding-bottom: 5px;",
                        col.rgroup = c("none", "#F7F7F7"),
                        align.header = "c",
                        align = "c")

# Añadir estilo al encabezado para que esté más separado del contenido
tabla_html_mediana <- addHtmlTableStyle(tabla_html_mediana, css.table = "border-collapse: collapse; width: 100%;", css.header = "border-bottom: 2px solid #ddd; padding: 10px;")

# Exportar la tabla HTML a un archivo
writeLines(tabla_html_mediana, "tabla_barrios_median.html")

# Mostrar la tabla HTML en la consola (opcional)
print(tabla_html_mediana)

#VARIANZAS POR BARRIOS
# Agrupar datos por barrio y calcular estadísticas descriptivas (varianza)
tabla_barrios_var <- data_filtered %>%
  group_by(neighbourhood_cleansed) %>%
  summarise(
    num_properties = n(),
    accommodates = var(accommodates, na.rm = TRUE),
    antiguedad = var(antiguedad, na.rm = TRUE),
    host_response_rate = var(host_response_rate, na.rm = TRUE),
    host_response_time = var(as.numeric(host_response_time), na.rm = TRUE),
    neighborhood_overview_flag = var(neighborhood_overview_flag, na.rm = TRUE),
    bathrooms = var(bathrooms, na.rm = TRUE),
    beds = var(beds, na.rm = TRUE),
    price = var(price, na.rm = TRUE),
    host_is_superhost = var(host_is_superhost, na.rm = TRUE),
    host_listings_count = var(host_listings_count, na.rm = TRUE),
    host_has_profile_pic = var(host_has_profile_pic, na.rm = TRUE),
    host_identity_verified = var(host_identity_verified, na.rm = TRUE),
    property_type = var(as.numeric(property_type), na.rm = TRUE),
    room_type = var(as.numeric(room_type), na.rm = TRUE),
    minimum_nights = var(minimum_nights, na.rm = TRUE),
    maximum_nights = var(maximum_nights, na.rm = TRUE)
  )

# Redondear las columnas a 4 decimales
tabla_barrios_var <- tabla_barrios_var %>%
  mutate_if(is.numeric, round, digits = 4)

# Crear una fila de encabezado personalizada
header <- c("Neighbourhood", "Number of Properties", "Accommodates (variance)", 
            "Antigüedad (variance)", "Host Response Rate (variance)", "Host Response Time (variance)", 
            "Neighborhood Overview Flag (variance)", "Bathrooms (variance)", "Beds (variance)", 
            "Price (variance)", "Host is Superhost (variance)", "Host Listings Count (variance)", 
            "Host Has Profile Pic (variance)", "Host Identity Verified (variance)", 
            "Property Type (variance)", "Room Type (variance)", "Minimum Nights (variance)", 
            "Maximum Nights (variance)")

# Convertir la tabla a HTML con estilos personalizados
tabla_html <- htmlTable(tabla_barrios_var,
                        header = header,
                        rnames = FALSE,
                        css.cell = "padding-right: 30px; padding-bottom: 5px;",
                        col.rgroup = c("none", "#F7F7F7"),
                        align.header = "c",
                        align = "c")

# Añadir estilo al encabezado para que esté más separado del contenido
tabla_html <- addHtmlTableStyle(tabla_html, css.table = "border-collapse: collapse; width: 100%;", css.header = "border-bottom: 2px solid #ddd; padding: 10px;")

# Exportar la tabla HTML a un archivo
writeLines(tabla_html, "tabla_barrios_var.html")

# Mostrar la tabla HTML en la consola (opcional)
print(tabla_html)

# Definir el número de intervalos
num_bins <- 10

# Calcular los bordes de los intervalos de precios
min_price <- min(data_filtered$price, na.rm = TRUE)
max_price <- max(data_filtered$price, na.rm = TRUE)
bins <- seq(min_price, max_price, length.out = num_bins + 1)  # Crear los bordes de los bins
labels <- paste0(floor(bins[-length(bins)]), "-", floor(bins[-1]))

#Histograma de los precios por barrio
# Crear el histograma sin filtrar por número de propiedades
ggplot(data, aes(x = price, fill = neighbourhood_cleansed)) +
  geom_histogram(bins = 10, position = "stack", color = "black") +
  scale_fill_viridis_d() +
  ggtitle('Distribución del Precio por Barrio (Sin Filtrar)') +
  xlab('Precio') +
  ylab('Número de Alojamientos') +
  theme_minimal() +
  theme(
    plot.title = element_text(hjust = 0.5),
    axis.text.x = element_text(angle = 45, hjust = 1),  # Rotar las etiquetas del eje x
    legend.title = element_text(size = 13),            # Ajustar tamaño del título de la leyenda
    legend.text = element_text(size = 11)               # Ajustar tamaño de la leyenda
  ) +
  ylim(0, max(table(data$neighbourhood_cleansed)) * 1.1)  # Ajustar el límite del eje y

# Colores generados automáticamente
colors <- scales::viridis_pal()(length(labels))

# Calcular el número de propiedades por barrio
property_counts <- table(data$neighbourhood_cleansed)

# Filtrar barrios que tienen hasta 200 propiedades
filtered_data <- data[data$neighbourhood_cleansed %in% names(property_counts[property_counts <= 200]), ]

# Identificar barrios que exceden el límite de 200 propiedades
exceeding_boroughs <- property_counts[property_counts > 200]

# Para cada barrio que excede las 200 propiedades, tomar una muestra aleatoria de 200
sampled_data <- data %>%
  group_by(neighbourhood_cleansed) %>%
  filter(neighbourhood_cleansed %in% names(exceeding_boroughs)) %>%
  slice_sample(n = 200)

# Combinar los barrios filtrados y las muestras aleatorias
combined_data <- bind_rows(filtered_data, sampled_data)

# Calcular el número de propiedades para cada barrio en el conjunto combinado
combined_property_counts <- table(combined_data$neighbourhood_cleansed)

# Mostrar los barrios que exceden el límite de 200 propiedades
exceeding_data <- data.frame(
  Barrio = names(exceeding_boroughs),
  `Número de Propiedades` = as.integer(exceeding_boroughs),
  `Exceso sobre 200` = as.integer(exceeding_boroughs) - 200
)

cat("Barrios que exceden el límite de 200 Alojamientos:\n")
print(exceeding_data)

# Crear el histograma con los datos combinados
ggplot(combined_data, aes(x = price, fill = neighbourhood_cleansed)) +
  geom_histogram(bins = 10, position = "stack", color = "black") +
  scale_fill_viridis_d() +
  ggtitle('Distribución del Precio por Barrio (Muestra y Filtrado)') +
  xlab('Precio') +
  ylab('Número de Alojamientos') +
  theme_minimal() +
  theme(
    plot.title = element_text(hjust = 0.5),
    axis.text.x = element_text(angle = 45, hjust = 1),  # Rotar las etiquetas del eje x
    legend.title = element_text(size = 13),             # Ajustar tamaño del título de la leyenda
    legend.text = element_text(size = 11)               # Ajustar tamaño de la leyenda
  ) +
  ylim(0, max(table(combined_data$neighbourhood_cleansed)) * 1.1)  # Ajustar el límite del eje y

# Crear el histograma 
ggplot(filtered_data, aes(x = price, fill = neighbourhood_cleansed)) +
  geom_histogram(bins = 10, position = "stack", color = "black") +
  scale_fill_viridis_d() +
  ggtitle('Distribución del Precio por Barrio') +
  xlab('Precio') +
  ylab('Número de Alojamientos') +
  theme_minimal() +
  theme(
    plot.title = element_text(hjust = 0.5),
    axis.text.x = element_text(angle = 45, hjust = 1),  # Rotar las etiquetas del eje x
    legend.title = element_text(size = 13),             # Ajustar tamaño del título de la leyenda
    legend.text = element_text(size = 11)               # Ajustar tamaño de la leyenda
  ) +
  ylim(0, max(table(filtered_data$neighbourhood_cleansed)) * 1.1)  # Ajustar el límite del eje y

# Crear el boxplot del precio por barrio
ggplot(data_filtered, aes(x = neighbourhood_cleansed, y = price)) +
  geom_boxplot(fill = "aquamarine2", color = "black") +
  ggtitle('Boxplot del Precio por Barrio') +
  xlab('Barrio') +
  ylab('Precio') +
  theme_minimal() +
  theme(
    plot.title = element_text(hjust = 0.5),
    axis.text.x = element_text(angle = 45, hjust = 1)  # Rotar las etiquetas del eje x
  )

#Elimino los outliers; Considero solo las variables que puede ver un cliente en el anuncio
remove_outliers <- function(data, variables) {
  data_clean <- data
  
  for (var in variables) {
    if (is.numeric(data[[var]])) {
      Q1 <- quantile(data[[var]], 0.25, na.rm = TRUE)
      Q3 <- quantile(data[[var]], 0.75, na.rm = TRUE)
      IQR <- Q3 - Q1
      upper_threshold <- Q3 + 1.5 * IQR
      
      # Filtrar los datos para eliminar outliers
      data_clean <- data_clean %>% filter(data_clean[[var]] <= upper_threshold)
    }
  }
  
  return(data_clean)
}

# Especificar las variables a considerar
variables <- c("price", "accommodates", "host_response_rate", 
               "bathrooms", "beds", "minimum_nights", 
               "maximum_nights", "neighbourhood_cleansed")

# Aplicar la función al conjunto de datos
data_limpio <- remove_outliers(data, variables)

# Verificar el tamaño del conjunto de datos original y el limpiado
cat("Tamaño original:", nrow(data), "filas\n")
cat("Tamaño después de eliminar outliers:", nrow(data_limpio), "filas\n")

# Ahora pasaré a hacer un análisis de segmentación del mercado. 
#Calculo un modelo de regresión en base al precio y lo hago con las variables significativas
#Se seleccionan las variables que ven los clientes a la hora de buscar alojamiento
#accomodates, bathrooms, beds, host_response_rate, host_is_superhost, host_has_profile_pic, host_identity_verified
#property_type, room_type, minimum_nights, neighbourhood_cleansed
#ESTO SE HACE CON LOS DATOS SIN OUTLIERS
formula <- price ~ accommodates + host_response_rate + 
  bathrooms + beds + host_is_superhost + 
  host_has_profile_pic + host_identity_verified + 
  property_type + room_type + minimum_nights + maximum_nights + 
  neighbourhood_cleansed

modelo_reg <- lm(formula, data = data_limpio)
sink('Regresion_model_summary.txt')
print(summary(modelo_reg))
sink()

#SE HACE EL CLUSTERING CON LAS VARIABLES del modelo de regresión hecho

# Seleccionar las variables significativas
significant_vars <- c('accommodates', 'host_response_time', 'bathrooms', 'beds', 'host_is_superhost',
                      'property_type', 'room_type', 'maximum_nights', 'minimum_nights', "neighbourhood_cleansed")

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
data_limpio <- data_limpio %>%
  mutate(neighbourhood_cleansed = case_when(
    neighbourhood_cleansed %in% names(neighbourhood_equivalencias) ~ neighbourhood_equivalencias[neighbourhood_cleansed],
    TRUE ~ NA_integer_  # En caso de que haya algún valor que no esté en las equivalencias
  ))

# Crear variables dummy para 'property_type' y 'room_type'
data_filtered_dummies <- data_limpio %>%
  mutate(across(c(property_type, room_type), as.factor)) %>%
  model.matrix(~property_type + room_type - 1, data = .) %>%
  as.data.frame()

# Añadir la columna 'price' a la lista de variables significativas
significant_vars <- c(names(data_filtered_dummies)[grep("property_type_|room_type_", names(data_filtered_dummies))], "price")

# Añadir las dummies significativas a la lista de variables
dummy_vars <- grep("property_type_|room_type_", names(data_filtered), value = TRUE)
significant_vars <- c(significant_vars, dummy_vars)

# Selección de columnas con subset y eliminación de filas con NA
datos_segmentacion <- subset(data_filtered, select = c("price", significant_vars))
datos_segmentacion <- na.omit(datos_segmentacion)

# Normalización de las variables
scaler <- preProcess(datos_segmentacion, method = c("center", "scale"))
datos_normalizados <- predict(scaler, datos_segmentacion)


# Gráfica del Método del Codo
# Esta gráfica ayuda a determinar el número óptimo de clusters (grupos) para un algoritmo de clustering (K-means en este caso).
# En el eje X se representan los diferentes números de clusters, y en el eje Y, la suma de las distancias cuadradas dentro de los clusters (Within-Cluster Sum of Squares, WSS).
# Decreciente: La línea decreciente muestra que a medida que se aumenta el número de clusters, la WSS disminuye.
# El "codo" de la gráfica es el punto donde la tasa de disminución se hace menos pronunciada.
# Este punto indica el número óptimo de clusters, ya que más clusters no aportan una mejora significativa en la compactación de los grupos.

# Determinar el número óptimo de clusters utilizando el método del codo
wss <- numeric(15)
for (i in 1:15) {
  kmeans_result <- kmeans(datos_normalizados, centers = i, nstart = 25)
  wss[i] <- kmeans_result$tot.withinss
}

# Graficar el Método del Codo
plot(1:15, wss, type = "b", pch = 19, frame = FALSE,
     xlab = "Número de Clusters", ylab = "Within groups sum of squares",
     main = "Método del Codo")
grid()

# Realizar K-means clustering con el número óptimo de clusters (e.g., 4 clusters)
optimal_clusters <- 4
set.seed(123)
kmeans_resultado <- kmeans(datos_normalizados, centers = optimal_clusters, nstart = 25)

clusters <- kmeans_resultado$cluster

# Añadir los clusters a los datos originales
data_filtered$cluster <- as.character(kmeans_resultado$cluster)

# Mostrar las primeras filas del DataFrame con el nuevo cluster
head(data_filtered)

# Realizar PCA
pca_resultado <- prcomp(datos_normalizados, center = TRUE, scale. = TRUE)

# Seleccionar las dos primeras componentes principales
pca_df <- data.frame(PCA1 = pca_resultado$x[, 1], PCA2 = pca_resultado$x[, 2])

# Asegurarse de que clusters sea un factor y añadirlo al DataFrame de PCA
pca_df$cluster <- factor(clusters)

# Gráfico de dispersión de los datos según las componentes principales
library(ggplot2)
ggplot(pca_df, aes(x = PCA1, y = PCA2, color = cluster)) +
  geom_point(alpha = 0.6, size = 3) +
  labs(title = "Datos según Componentes Principales",
       x = "Componente Principal 1", y = "Componente Principal 2", color = "Cluster") +
  theme_minimal() +
  theme(plot.title = element_text(hjust = 0.5)) +
  scale_color_viridis_d()

ggplot(pca_df, aes(x = PCA1, y = PCA2, color = PCA1)) +
  geom_point() +
  labs(title = "Gráfico de las dos primeras componentes principales",
       x = "Componente Principal 1",
       y = "Componente Principal 2") +
  theme_minimal() +
  scale_color_gradient(low = "blue", high = "red")  

# Gráfica de Varianza Explicada Acumulada (Scree Plot)
# Esta gráfica muestra la proporción de la varianza total explicada por cada componente principal. 
# En el eje X se representan los componentes principales, y en el eje Y, la varianza explicada acumulada.
varianza_explicada <- pca_resultado$sdev^2 / sum(pca_resultado$sdev^2)
varianza_explicada_acumulada <- cumsum(varianza_explicada)

# Scree Plot para mostrar la proporción de varianza explicada por cada componente
screeplot(pca_resultado, type = "lines", main = "Scree Plot")

# Calcular la varianza explicada acumulada
varianza_explicada_acumulada <- cumsum(pca_resultado$sdev^2 / sum(pca_resultado$sdev^2))

# Visualizar la varianza explicada acumulada con puntos rojos y línea verde
plot(varianza_explicada_acumulada, type = "b", pch = 19, xlab = "Número de Componentes", 
     ylab = "Varianza Explicada Acumulada", main = "Scree Plot", 
     col = "red")  # Color de los puntos
lines(varianza_explicada_acumulada, col = "green")  # Color de la línea
grid()

#MODELO LOGIT
#COMO LOS DATOS YA SE HAN LIMPIADO Y CONVERTIDO LAS VARIABLES A CATEGÓRICAS, UTILIZO ESOS DATOS, QUE SON 
#datos_segmentación

# Crear variable binaria de precio alto
data_filtered <- data_limpio %>%
  mutate(precio_alto = as.integer(price > 1.5*median(price, na.rm = TRUE)))

formula_l <- precio_alto ~ accommodates + host_response_rate + 
  bathrooms + beds + host_is_superhost + 
  host_has_profile_pic + host_identity_verified + 
  property_type + room_type + minimum_nights + maximum_nights + 
  neighbourhood_cleansed

# Modelo Logit
modelo_logit <- glm(formula_l, data = data_filtered, family = binomial(link = "logit"))

# Mostrar el resumen del modelo logit
summary(modelo_logit)

# Guardar el resumen del modelo en un archivo de texto
sink('logit_model_summary.txt')
print(summary(modelo_logit))
sink()

# Calcular las probabilidades predichas
data_filtered$pred_prob <- predict(modelo_logit, type = "response")

# Obtener las verdaderas etiquetas y las probabilidades predichas
y_true <- data_filtered$precio_alto
y_pred_prob <- data_filtered$pred_prob

# Calcular la curva ROC y el área bajo la curva ROC
roc_obj <- roc(y_true, y_pred_prob)
roc_auc <- auc(roc_obj)

# Graficar la curva ROC
ggplot(data = data.frame(fpr = roc_obj$specificities, tpr = roc_obj$sensitivities), aes(x = 1 - fpr, y = tpr)) +
  geom_line(color = "blue", size = 1.2) +
  geom_abline(linetype = "dashed", color = "gray") +
  ggtitle(paste("Receiver Operating Characteristic (ROC)", "\nArea under curve (AUC) =", round(roc_auc, 2))) +
  xlab("False Positive Rate") +
  ylab("True Positive Rate") +
  theme_minimal() +
  theme(plot.title = element_text(hjust = 0.5))


# Añadir predicción al DataFrame
data_filtered$pred_prob <- predict(modelo_logit, type = "response")




















