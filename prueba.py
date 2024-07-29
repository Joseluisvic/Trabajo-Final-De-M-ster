import matplotlib.pyplot as plt
import seaborn as sns
import pandas as pd

# Datos de ejemplo
data = pd.DataFrame({
    'barrio': ['Centro', 'Este', 'Carretera de Cadiz', 'Cruz De Humilladero', 'Bailen-Miraflores'],
    'frecuencia': [4806, 690, 652, 293, 206]
})

# Crear el gráfico de burbujas
plt.figure(figsize=(10, 8))
bubble_plot = sns.scatterplot(
    data=data,
    x='barrio',
    y='frecuencia',
    size='frecuencia',
    legend=True,  # Ajusta el parámetro para incluir leyenda si es necesario
    sizes=(100, 1000),
    alpha=0.7
)

bubble_plot.set_title("Frecuencia de los Barrios")
bubble_plot.set_xlabel("Barrios")
bubble_plot.set_ylabel("Frecuencia")
bubble_plot.tick_params(axis='x', rotation=90)

plt.show()
