#Carga de librerías
import pandas as pd
import matplotlib.pyplot as plt
import seaborn as sns
from tabulate import tabulate

#Cargo el archivo
data = pd.read_csv('data.csv')
print(data.head())

# Calcular la frecuencia de los barrios
frecuencias = data['neighbourhood_cleansed'].value_counts().reset_index()
frecuencias.columns = ['barrio', 'frecuencia']

# Ordenar por frecuencia
frecuencias = frecuencias.sort_values(by='frecuencia', ascending=False)

# Crear el gráfico de burbujas
plt.figure(figsize=(10, 8))
bubble_plot = sns.scatterplot(
    data=frecuencias,
    x='barrio',
    y='frecuencia',
    size='frecuencia',
    legend=False,
    sizes=(100, 1000),
    alpha=0.7,
    palette=sns.color_palette("coolwarm", as_cmap=True)
)

bubble_plot.set_title("Frecuencia de los Barrios")
bubble_plot.set_xlabel("Barrios")
bubble_plot.set_ylabel("Frecuencia")
bubble_plot.tick_params(axis='x', rotation=90)
plt.gca().invert_yaxis()

plt.show()

# Mostrar la tabla en formato HTML
html_table = tabulate(frecuencias, headers='keys', tablefmt='html')
print(html_table)

#ANÁLISIS DE LA DEMANDA
# Vector de equivalencias de los barrios
neighbourhood_equivalencias = {
    "1": "Este",
    "2": "Centro",
    "3": "Churriana",
    "4": "Carretera de Cadiz",
    "5": "Bailen-Miraflores",
    "6": "Cruz De Humilladero",
    "7": "Teatinos-Universidad",
    "8": "Puerto de la Torre",
    "9": "Ciudad Jardin",
    "10": "Campanillas",
    "11": "Palma-Palmilla"
}
# Convertir códigos a nombres de barrios
data['neighbourhood_cleansed'] = data['neighbourhood_cleansed'].replace(neighbourhood_equivalencias)

# Análisis de precios por barrios
# Filtrar valores no finitos y valores extremadamente altos
data_filtered = data[(data['price'].apply(pd.to_numeric, errors='coerce').notnull()) & (data['price'] < 1000)]

# Crear histograma de precios
plt.figure(figsize=(10, 6))
sns.histplot(data_filtered['price'], bins=range(0, 1000, 10), kde=False, color='blue')
plt.title('Distribución de Precios de Airbnb')
plt.xlabel('Precio')
plt.ylabel('Frecuencia')
plt.grid(True)
plt.show()

# Crear boxplot de precios por barrios
plt.figure(figsize=(10, 6))
sns.boxplot(y=data_filtered['price'], color='blue')
plt.title('Boxplot de Precios de Airbnb')
plt.ylabel('Precio')
plt.grid(True)
plt.show()

# Crear boxplot de precios por barrios
plt.figure(figsize=(12, 8))
sns.boxplot(x='neighbourhood_cleansed', y='price', data=data_filtered, color='blue')
plt.title('Distribución de Precios por Barrios')
plt.xlabel('Barrio')
plt.ylabel('Precio')
plt.xticks(rotation=45)
plt.grid(True)
plt.show()

# Crear histograma
sns.histplot(data_filtered, x='price', bins=10, color='blue')
plt.title('Distribución de Precios de Airbnb')
plt.xlabel('Precio')
plt.ylabel('Frecuencia')
plt.show()

# Crear boxplot de precios por barrios
sns.boxplot(data=data_filtered, x='neighbourhood_cleansed', y='price', color='blue')
plt.title('Distribución de Precios por Barrios')
plt.xlabel('Barrio')
plt.ylabel('Precio')
plt.xticks(rotation=45)
plt.show()

# Agrupar datos por barrio y calcular estadísticas descriptivas
tabla_barrios = data_filtered.groupby('neighbourhood_cleansed').agg(
    num_properties=('id', 'count'),
    accommodates=('accommodates', 'mean'),
    antiguedad=('antiguedad', 'mean'),
    host_response_rate=('host_response_rate', 'mean'),
    host_response_time=('host_response_time', 'mean'),
    neighborhood_overview_flag=('neighborhood_overview_flag', 'mean'),
    bathrooms=('bathrooms', 'mean'),
    beds=('beds', 'mean'),
    price=('price', 'mean'),
    host_is_superhost=('host_is_superhost', 'mean'),
    host_listings_count=('host_listings_count', 'mean'),
    host_has_profile_pic=('host_has_profile_pic', 'mean'),
    host_identity_verified=('host_identity_verified', 'mean'),
    property_type=('property_type', 'mean'),
    room_type=('room_type', 'mean'),
    minimum_nights=('minimum_nights', 'mean'),
    maximum_nights=('maximum_nights', 'mean')
).reset_index()

# Mostrar tabla con formato
print(tabla_barrios)

# Histograma por barrios
sns.histplot(data_filtered, x='price', hue='neighbourhood_cleansed', bins=10, multiple='dodge')
plt.title('Distribución del Precio por Barrio')
plt.xlabel('Precio')
plt.ylabel('Número de Propiedades')
plt.legend(title='Barrio')
plt.ylim(0, 200)
plt.show()

# BoxPlot precio por barrios
sns.boxplot(data=data_filtered, x='neighbourhood_cleansed', y='price', color='red')
plt.title('Boxplot del Precio por Barrio')
plt.xlabel('Barrio')
plt.ylabel('Precio')
plt.show()