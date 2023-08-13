# Visualizacion Censo 2017 Chile
 Visualizaciones de las variables demorgraficas del censo 2017 de Chile, por unidad censal
# Aplicación Shiny para Visualización de Mapas de Calor Interactivos

## Descripción

Esta aplicación Shiny genera un mapa de calor interactivo utilizando datos de un archivo shapefile (shp). Shiny es una herramienta de R que permite crear aplicaciones web interactivas y dinámicas, y es especialmente útil para presentar y visualizar datos de manera interactiva.

## Componentes Principales

1. **Bibliotecas Cargadas**: Se cargan varias bibliotecas de R necesarias para la funcionalidad de la aplicación, como sf para manipulación de datos espaciales, leaflet para visualización de mapas interactivos, shiny para crear la aplicación web, y otras bibliotecas como ggplot2, dplyr, etc.

2. **Lectura de Datos**: Se lee el archivo shapefile de la comuna de Chillán y se realiza una transformación en la proyección de coordenadas para que coincida con UTM (Universal Transverse Mercator) con zona 19 y datum WGS84.

3. **Interfaz de Usuario (UI)**: La interfaz de usuario se define utilizando la función dashboardPage de Shinydashboard. Incluye un encabezado con título, una barra lateral con elementos interactivos (menús desplegables y botones) y un cuerpo que contiene el gráfico interactivo y la información sobre la variable seleccionada.

4. **Servidor (Server)**: La parte del servidor de la aplicación se define utilizando la función shiny::server. Contiene funciones reactivas que responden a las interacciones del usuario y generan resultados dinámicos en función de las selecciones realizadas en la interfaz.

5. **Funciones Reactivas**: Entre las funciones reactivas se incluye el uso de observeEvent para actualizar las opciones de los menús desplegables, reactiveVal para cambiar el tipo de gráfico, renderUI para mostrar información sobre la variable seleccionada, y renderPlot para generar el mapa de calor.

6. **Descarga de Archivos**: Se agregan botones de descarga para el gráfico generado y el archivo shapefile correspondiente a los datos filtrados en formato ESRI Shapefile.

## Funcionalidades del Software

1. Visualización de mapas de calor interactivos.

2. Selección de ubicación geográfica.

3. Selección de variables socio-demográficas.

4. Alternar entre gráficos de nivel de comuna y distrito: El usuario puede cambiar entre visualizar el mapa de calor a nivel de comuna o distrito para obtener diferentes perspectivas de los datos.

5. Información sobre la variable seleccionada: La aplicación muestra información sobre la variable seleccionada en la interfaz de usuario.

6. Descarga de gráfico generado: Un botón de descarga permite al usuario descargar el gráfico de mapa de calor generado en formato PNG.

7. Descarga de archivo shapefile filtrado: La aplicación ofrece un botón de descarga para obtener el archivo shapefile filtrado correspondiente a los datos utilizados.

8. Interactividad y respuesta en tiempo real: La aplicación es interactiva y responde en tiempo real a las selecciones del usuario en la interfaz.

![Interfaz de Usuario](https://raw.githubusercontent.com/Pgadatos/Visualizacion-Censo-2017-Chile/main/app.png)


