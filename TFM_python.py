#Carga de librerías
import pandas as pd
import matplotlib.pyplot as plt
import seaborn as sns
from tabulate import tabulate
import numpy as np

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
    x='frecuencia',
    y='barrio',
    size='frecuencia',
    legend=False,
    sizes=(100, 1000),
    alpha=0.7,
    palette=sns.color_palette("coolwarm", as_cmap=True)
)

# Ajustar título y etiquetas
bubble_plot.set_title("Frecuencia de los Barrios")
bubble_plot.set_xlabel("Frecuencia")
bubble_plot.set_ylabel("Barrios")
# Ajustar las etiquetas del eje y para que sean verticales
plt.yticks(rotation=0)
# Invertir el eje x para que las frecuencias más altas estén a la izquierda
plt.gca().invert_xaxis()
# Ajustar el espaciado del gráfico para aumentar el margen izquierdo
plt.subplots_adjust(left=0.2)  # Aumentar el margen izquierdo
# Guardar el gráfico como archivo PNG
plt.savefig('burbuja.png')
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
# Guardar el gráfico como archivo PNG
plt.savefig('histograma_precios.png')
plt.show()

# Crear boxplot de precios por barrios
plt.figure(figsize=(10, 6))
sns.boxplot(y=data_filtered['price'], color='blue')
plt.title('Boxplot de Precios de Airbnb')
plt.ylabel('Precio')
plt.grid(True)

# Guardar el gráfico como archivo PNG
plt.savefig('bp_precio_barrio.png')
plt.show()

# Crear boxplot de precios por barrios
plt.figure(figsize=(12, 8))
sns.boxplot(x='neighbourhood_cleansed', y='price', data=data_filtered, color='blue')
plt.title('Distribución de Precios por Barrios')
plt.xlabel('Barrio')
plt.ylabel('Precio')
plt.xticks(rotation=45)
plt.grid(True)
plt.savefig('bp_precio_barrio_2.png')
plt.show()

# Crear el histograma
plt.figure(figsize=(12, 6))  # Asegúrate de que el tamaño sea suficiente
sns.histplot(data_filtered, x='price', bins=10, color='blue')
# Ajustar título y etiquetas
plt.title('Distribución de Precios de Airbnb')
plt.xlabel('Precio')
plt.ylabel('Frecuencia')
# Ajustar el espaciado de las etiquetas del eje x
plt.xticks(rotation=45)  # Rotar las etiquetas del eje x para evitar superposición
# Ajustar el diseño para evitar que las etiquetas se sobrepongan
plt.tight_layout()
# Guardar y mostrar el gráfico
plt.savefig('hist.png')
plt.show()

# Crear boxplot de precios por barrios
sns.boxplot(data=data_filtered, x='neighbourhood_cleansed', y='price', color='blue')
plt.title('Distribución de Precios por Barrios')
plt.xlabel('Barrio')
plt.ylabel('Precio')
plt.xticks(rotation=45)
plt.savefig('bp_precio_barrio_3.png')
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
# Definir el número de intervalos
num_bins = 10

# Calcular los bordes de los intervalos de precios
min_price = data_filtered['price'].min()
max_price = data_filtered['price'].max()
bins = np.linspace(min_price, max_price, num_bins + 1)  # Crear los bordes de los bins
labels = [f'{int(bins[i])}-{int(bins[i+1])}' for i in range(len(bins) - 1)]
colors = plt.cm.viridis(np.linspace(0, 1, len(labels)))  # Colores generados automáticamente
# Calcular el número de propiedades por barrio
property_counts = data['neighbourhood_cleansed'].value_counts()
# Filtrar barrios que tienen hasta 200 propiedades
filtered_data = data[data['neighbourhood_cleansed'].isin(property_counts[property_counts <= 200].index)]
# Calcular el número de propiedades para cada barrio filtrado
filtered_property_counts = filtered_data['neighbourhood_cleansed'].value_counts()
# Identificar y mostrar barrios que exceden el límite de 200 propiedades
exceeding_boroughs = property_counts[property_counts > 200]
exceeding_data = pd.DataFrame({
    'Barrio': exceeding_boroughs.index,
    'Número de Propiedades': exceeding_boroughs.values,
    'Exceso sobre 200': exceeding_boroughs.values - 200
})

print("Barrios que exceden el límite de 200 propiedades:")
print(exceeding_data)
# Crear el histograma
plt.figure(figsize=(14, 10))
sns.histplot(filtered_data, x='price', hue='neighbourhood_cleansed', bins=10, multiple='stack', palette='viridis')
# Ajustar título y etiquetas
plt.title('Distribución del Precio por Barrio')
plt.xlabel('Precio')
plt.ylabel('Número de Propiedades')
# Ajustar la leyenda
plt.legend(title='Barrio', title_fontsize='13', fontsize='11')
# Ajustar el límite del eje y automáticamente
plt.ylim(0, filtered_data.groupby('neighbourhood_cleansed')['price'].count().max() * 1.1)  # Ajustar según sea necesario (agregamos un 10% de margen)
# Ajustar el espaciado de las etiquetas del eje x
plt.xticks(rotation=45)
# Ajustar el diseño para evitar que las etiquetas se sobrepongan
plt.tight_layout()
# Guardar y mostrar el gráfico
plt.savefig('histogram_by_neighbourhood_limited.png')
plt.show()

# BoxPlot precio por barrios
sns.boxplot(data=data_filtered, x='neighbourhood_cleansed', y='price', color='red')
plt.title('Boxplot del Precio por Barrio')
plt.xlabel('Barrio')
plt.ylabel('Precio')
plt.savefig('bp_precio_barrio_4.png')
plt.show()