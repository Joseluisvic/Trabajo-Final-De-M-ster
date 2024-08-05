#Carga de librerías
import pandas as pd
import matplotlib.pyplot as plt
import seaborn as sns
from tabulate import tabulate
import numpy as np
import statsmodels.api as sm
import statsmodels.formula.api as smf
import sklearn 
from sklearn.preprocessing import StandardScaler
from sklearn.metrics import roc_curve, roc_auc_score
from sklearn.decomposition import PCA
from sklearn.cluster import KMeans

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

#AJUSTE MODELO LOGIT
# Crear variable binaria de precio alto
data_filtered['precio_alto'] = (data_filtered['price'] > data_filtered['price'].median()).astype(int)

# Convertir variables categóricas a tipo 'category'
data_filtered['property_type'] = data_filtered['property_type'].astype('category')
data_filtered['room_type'] = data_filtered['room_type'].astype('category')
data_filtered['neighbourhood_cleansed'] = data_filtered['neighbourhood_cleansed'].astype('category')

# Fórmula del modelo
formula = 'precio_alto ~ accommodates + antiguedad + host_response_rate + host_response_time + bathrooms + beds + host_is_superhost + host_listings_count + host_has_profile_pic + host_identity_verified + C(property_type) + C(room_type) + minimum_nights + maximum_nights + C(neighbourhood_cleansed)'

# Modelo Logit
modelo_logit = smf.glm(formula=formula, data=data_filtered, family=sm.families.Binomial(link=sm.families.links.logit())).fit()
print(modelo_logit)
# Guardar el resumen del modelo en un archivo de texto
with open('logit_model_summary.txt', 'w') as f:
    f.write(modelo_logit.summary().as_text())
# Mostrar el resumen del modelo logit
print(modelo_logit.summary())

# Calcular las probabilidades predichas
data_filtered['pred_prob'] = modelo_logit.predict()

# Obtener las verdaderas etiquetas y las probabilidades predichas
y_true = data_filtered['precio_alto']
y_pred_prob = data_filtered['pred_prob']

# Calcular la curva ROC
fpr, tpr, thresholds = roc_curve(y_true, y_pred_prob)

# Calcular el área bajo la curva ROC
roc_auc = roc_auc_score(y_true, y_pred_prob)

# Graficar la curva ROC
plt.figure(figsize=(10, 6))
plt.plot(fpr, tpr, color='blue', lw=2, label=f'ROC curve (area = {roc_auc:.2f})')
plt.plot([0, 1], [0, 1], color='gray', linestyle='--')
plt.xlim([0.0, 1.0])
plt.ylim([0.0, 1.05])
plt.xlabel('False Positive Rate')
plt.ylabel('True Positive Rate')
plt.title('Receiver Operating Characteristic (ROC)')
plt.legend(loc='lower right')
plt.grid(True)
plt.savefig('curva_ROC.png')
plt.show()

#Ahora pasaré a hacer un análisis de segmentación del mercado. Para ello empezaré cogiendo las variables significatiavs del modelo logit 
#calculado anteriormente y haciendo un análisis clúster con ellas.

# Añadir predicción al DataFrame
data_filtered.loc[:, 'pred_prob'] = modelo_logit.predict()

# Seleccionar las variables significativas y normalizarlas
significant_vars = ['accommodates', 'host_response_time', 'bathrooms', 'beds', 'host_listings_count',
                    'host_has_profile_pic', 'minimum_nights', 'maximum_nights']

# Crear variables dummy para 'property_type' y 'neighbourhood_cleansed'
data_filtered = pd.get_dummies(data_filtered, columns=['property_type', 'neighbourhood_cleansed'], drop_first=True)

# Añadir las dummies significativas a la lista de variables
dummy_vars = [col for col in data_filtered.columns if 'property_type_' in col or 'neighbourhood_cleansed_' in col]
significant_vars.extend(dummy_vars)

# Filtrar el DataFrame con las variables significativas
datos_segmentacion = data_filtered[['price'] + significant_vars].dropna()

# Normalización de las variables
scaler = StandardScaler()
datos_normalizados = scaler.fit_transform(datos_segmentacion)

# Realizar PCA
pca = PCA()
pca_resultado = pca.fit_transform(datos_normalizados)
# Gráfica de Varianza Explicada Acumulada (Scree Plot)
# Esta gráfica muestra la proporción de la varianza total explicada por cada componente principal. En el eje X se representan los componentes principales, y en el eje Y, la varianza explicada acumulada.
# Creciente: La línea creciente indica que a medida que se agregan más componentes, se explica más varianza de los datos.
# Por ejemplo, si la línea se estabiliza después de 4 componentes, significa que los primeros 4 componentes explican la mayor parte de la varianza, y añadir más componentes no incrementará significativamente la varianza explicada.

# Visualizar la varianza explicada por cada componente
plt.figure()
plt.plot(np.cumsum(pca.explained_variance_ratio_))
plt.xlabel('Número de Componentes')
plt.ylabel('Varianza Explicada Acumulada')
plt.title('Scree Plot')
plt.grid()
plt.savefig("Var_exp.png")
plt.show()

# Gráfica del Método del Codo
# Esta gráfica ayuda a determinar el número óptimo de clusters (grupos) para un algoritmo de clustering (K-means en este caso).
# En el eje X se representan los diferentes números de clusters, y en el eje Y, la suma de las distancias cuadradas dentro de los clusters (Within-Cluster Sum of Squares, WSS).
# Decreciente: La línea decreciente muestra que a medida que se aumenta el número de clusters, la WSS disminuye.
# El "codo" de la gráfica es el punto donde la tasa de disminución se hace menos pronunciada.
# Este punto indica el número óptimo de clusters, ya que más clusters no aportan una mejora significativa en la compactación de los grupos.

# Determinar el número óptimo de clusters utilizando el método del codo
wss = []
for i in range(1, 16):
    kmeans = KMeans(n_clusters=i, random_state=123)
    kmeans.fit(datos_normalizados)
    wss.append(kmeans.inertia_)

plt.figure()
plt.plot(range(1, 16), wss, marker='o')
plt.xlabel('Número de Clusters')
plt.ylabel('Within groups sum of squares')
plt.title('Método del Codo')
plt.grid()
plt.savefig("codo.png")
plt.show()

# Realizar K-means clustering con el número óptimo de clusters (e.g., 4 clusters)
optimal_clusters = 4
kmeans_resultado = KMeans(n_clusters=optimal_clusters, random_state=123, n_init=25).fit(datos_normalizados)

# Añadir los clusters a los datos originales
data_filtered.loc[:, 'cluster'] = kmeans_resultado.labels_.astype(str)

# Realizar K-means clustering con el número óptimo de clusters (e.g., 4 clusters)
kmeans = KMeans(n_clusters=4, n_init=25, random_state=123)
kmeans.fit(datos_normalizados)
clusters = kmeans.labels_
# Mostrar las primeras filas del DataFrame con el nuevo cluster
print(data_filtered.head())

# Añadir los clusters a los datos originales
data_filtered['cluster'] = clusters

#PCA CON DOS COMPONENTES PRINCIPALES
# Realizar PCA
pca = PCA(n_components=2)
datos_pca = pca.fit_transform(datos_normalizados)

# Crear un DataFrame con los resultados de PCA y los clusters
pca_df = pd.DataFrame(data=datos_pca, columns=['PCA1', 'PCA2'])
pca_df['cluster'] = clusters

# Gráfico de dispersión de los datos según las componentes principales
plt.figure(figsize=(10, 7))
scatter = plt.scatter(pca_df['PCA1'], pca_df['PCA2'], c=pca_df['cluster'], cmap='viridis', alpha=0.6)
plt.xlabel('Componente Principal 1')
plt.ylabel('Componente Principal 2')
plt.title('Datos según Componentes Principales')
plt.colorbar(scatter, label='Cluster')
plt.grid()
plt.savefig("Componentes principales.png")
plt.show()