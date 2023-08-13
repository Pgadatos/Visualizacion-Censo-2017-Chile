library(sf)
library(rgdal)
library(raster)
library(leaflet)
library(shiny)
library(shinydashboard)
library(dplyr)
library(ggplot2)
library(viridis)

## leer shp de la comuna de chillan
rutashp <- "C:/Users/Alumno(a) DEGI/Desktop/Doctorado/programacion-Rstudio/TareaF/TF2/a2/microdatourbano/Microdatos_Manzana.shp"  # Reemplaza "ruta_de_la_imagen.ecw" con la ubicación real de tu imagen ECW
chileshp<-st_read(rutashp)# leer el shp
chileshp <- st_transform(chileshp, crs = "+proj=utm +zone=19 +datum=WGS84 +units=m +no_defs")


# Definir la interfaz de usuario (UI)
ui <- dashboardPage(
  dashboardHeader(title = "Mapa de Calor"),
  dashboardSidebar(
    selectInput("region", "Seleccionar Región", choices = unique(chileshp$REGION)),
    selectInput("provincia", "Seleccionar Provincia", NULL),
    selectInput("comuna", "Seleccionar Comuna", NULL),
    actionButton("toggle_grafico", "Cambiar a Distrito"),
    selectInput("nombre_dis", "Seleccionar Nombre de Distrito", NULL),
    selectInput("variable", "Seleccionar Variable",
                choices = c(
                            "Número total de personas" = "TOTAL_PERS",
                            "Total de Hombres" = "TOTAL_HOMB",
                            "Total de Mujeres" = "TOTAL_MUJE",
                            "Total de personas de 0 a 5 años" = "PERSONAS_0",
                            "Total de personas de 6 a 14 años" = "PERSONAS_6",
                            "Total de personas de 15 a 64 años" = "PERSONAS_1",
                            "Total de personas de 65 y más años" = "PERSONAS_M",
                            "Total personas migrantes que residen habitualmente en el territorio nacional" = "PERSONAS_E",
                            "Total personas que se consideran pertenecientes a algún pueblo indígena u originario" = "PUEBLOS_IN",
                            "Total de viviendas particulares" = "TOTAL_VIV_",
                            "Total viviendas colectivas" = "TOTAL_VIV1",
                            "Total viviendas ocupadas con moradores presentes" = "VIV_OCUPA_",
                            "Total viviendas" = "TOTAL_VIVI",
                            "Cantidad de hogares" = "CANTIDAD_H",
                            "Cantidad de viviendas tipo casa" = "VIV_TIPO_C",
                            "Cantidad de viviendas tipo departamento en edificio" = "VIV_TIPO_D",
                            "Cantidad de viviendas tipo vivienda tradicional indígena (ruka-pae pae u otras)" = "VIV_TIPO_T",
                            "Cantidad de viviendas tipo pieza en casa antigua o en conventillo" = "VIV_TIPO_P",
                            "Cantidad de viviendas tipo mediagua-mejora-rancho o choza" = "VIV_TIPO_M",
                            "Cantidad de viviendas tipo móvil (carpa-casa rodante o similar)" = "VIV_TIPO_1",
                            "Cantidad de viviendas tipo otro tipo de vivienda particular" = "VIV_TIPO_O",
                            "Cantidad de viviendas con materialidad en paredes de Hormigón armado" = "VIV_PARED_",
                            "Cantidad de viviendas con materialidad en paredes de albañilería: bloque de cemento-piedra o ladrillo" = "VIV_PARED1",
                            "Cantidad de viviendas con materialidad en paredes de tabique forrado por ambas caras (madera o acero)" = "VIV_PARE_1",
                            "Cantidad de viviendas con materialidad en paredes de tabique sin forro interior (madera u otro)" = "VIV_PARE_2",
                            "Cantidad de viviendas con materialidad en paredes de adobe-barro-quincha-pirca u otro artesanal tradicional" = "VIV_PARE_3",
                            "Cantidad de viviendas con materialidad en paredes de materiales precarios (lata-cartón-plástico-etc.)" = "VIV_PARE_4",
                            "Cantidad de viviendas con materialidad de techo tejas o tejuelas de arcilla-metálicas de cemento de madera-asfálticas o plásticas" = "VIV_TECHO_",
                            "Cantidad de viviendas con materialidad de techo losa hormigón" = "VIV_TECHO1",
                            "Cantidad de viviendas con materialidad de techo planchas metálicas de zinc-cobre-etc. O fibrocemento (tipo pizarreño)" = "VIV_TECH_1",
                            "Cantidad de viviendas con materialidad de techo fonolita o plancha de fieltro embreado" = "VIV_TECH_2",
                            "Cantidad de viviendas con materialidad de techo paja-coirón-totora o caña" = "VIV_TECH_3",
                            "Cantidad de viviendas con materialidad de techo materiales precarios (lata-cartón-plástico-etc.)" = "VIV_TECH_4",
                            "Cantidad de viviendas con materialidad de techo sin cubierta sólida de techo" = "VIV_TECH_5",
                            "Cantidad de viviendas con materialidad de piso parquet-piso flotante-cerámico-madera-alfombra-flexit-cubrepiso u otro similar-sobre radier o vigas de madera" = "VIV_PISO_P",
                            "Viviendas con materialidad de piso radier sin revestimiento" = "VIV_PISO_R",
                            "Viviendas con materialidad de piso baldosa de cemento" = "VIV_PISO_B",
                            "Viviendas con materialidad de piso capa de cemento sobre tierra" = "VIV_PISO_C",
                            "Viviendas con materialidad de piso tierra" = "VIV_PISO_T",
                            "Total viviendas con materialidad aceptable" = "VIV_MATERI",
                            "Total viviendas con materialidad recuperable" = "VIV_MATE_1",
                            "Total viviendas con materialidad irrecuperable" = "VIV_MATE_2",
                            "Cantidad de viviendas con origen del agua por red pública" = "VIV_AGUA_R",
                            "Cantidad de viviendas con origen del agua por pozo o noria" = "VIV_AGUA_P",
                            "Cantidad de viviendas con origen del agua por camión aljibe" = "VIV_AGUA_C",
                            "Cantidad de viviendas con origen del agua por río, vertiente, estero, canal, lago, etc." = "VIV_AGUA_1"
                  )),
    downloadButton("download_grafico", "Descargar Gráfico"),
    downloadButton("download_shp", "Descargar Shapefile")
  ),
  dashboardBody( 
                 fluidRow( plotOutput("mapa")
               #    box(title = "Gráfico 1", plotOutput("mapa"))
              #     box(title = "Gráfico 2", plotOutput("grafico2"))
                 )
  )
)


# Definir el servidor
server <- function(input, output, session) {
  tipo_grafico <- reactiveVal("comuna")
  variable_matrix <- data.frame(
    nombre_extendido = c(
      "Número total de personas",
      "Total de Hombres",
      "Total de Mujeres",
      "Total de personas de 0 a 5 años",
      "Total de personas de 6 a 14 años",
      "Total de personas de 15 a 64 años",
      "Total de personas de 65 y más años",
      "Total personas migrantes que residen habitualmente en el territorio nacional",
      "Total personas que se consideran pertenecientes a algún pueblo indígena u originario",
      "Total de viviendas particulares",
      "Total viviendas colectivas",
      "Total viviendas ocupadas con moradores presentes",
      "Total viviendas",
      "Cantidad de hogares",
      "Cantidad de viviendas tipo casa",
      "Cantidad de viviendas tipo departamento en edificio",
      "Cantidad de viviendas tipo vivienda tradicional indígena (ruka-pae pae u otras)",
      "Cantidad de viviendas tipo pieza en casa antigua o en conventillo",
      "Cantidad de viviendas tipo mediagua-mejora-rancho o choza",
      "Cantidad de viviendas tipo móvil (carpa-casa rodante o similar)",
      "Cantidad de viviendas tipo otro tipo de vivienda particular",
      "Cantidad de viviendas con materialidad en paredes de Hormigón armado",
      "Cantidad de viviendas con materialidad en paredes de albañilería: bloque de cemento-piedra o ladrillo",
      "Cantidad de viviendas con materialidad en paredes de tabique forrado por ambas caras (madera o acero)",
      "Cantidad de viviendas con materialidad en paredes de tabique sin forro interior (madera u otro)",
      "Cantidad de viviendas con materialidad en paredes de adobe-barro-quincha-pirca u otro artesanal tradicional",
      "Cantidad de viviendas con materialidad en paredes de materiales precarios (lata-cartón-plástico-etc.)",
      "Cantidad de viviendas con materialidad de techo tejas o tejuelas de arcilla-metálicas de cemento de madera-asfálticas o plásticas",
      "Cantidad de viviendas con materialidad de techo losa hormigón",
      "Cantidad de viviendas con materialidad de techo planchas metálicas de zinc-cobre-etc. O fibrocemento (tipo pizarreño)",
      "Cantidad de viviendas con materialidad de techo fonolita o plancha de fieltro embreado",
      "Cantidad de viviendas con materialidad de techo paja-coirón-totora o caña",
      "Cantidad de viviendas con materialidad de techo materiales precarios (lata-cartón-plástico-etc.)",
      "Cantidad de viviendas con materialidad de techo sin cubierta sólida de techo",
      "Cantidad de viviendas con materialidad de piso parquet-piso flotante-cerámico-madera-alfombra-flexit-cubrepiso u otro similar-sobre radier o vigas de madera",
      "Viviendas con materialidad de piso radier sin revestimiento",
      "Viviendas con materialidad de piso baldosa de cemento",
      "Viviendas con materialidad de piso capa de cemento sobre tierra",
      "Viviendas con materialidad de piso tierra",
      "Total viviendas con materialidad aceptable",
      "Total viviendas con materialidad recuperable",
      "Total viviendas con materialidad irrecuperable",
      "Cantidad de viviendas con origen del agua por red pública",
      "Cantidad de viviendas con origen del agua por pozo o noria",
      "Cantidad de viviendas con origen del agua por camión aljibe",
      "Cantidad de viviendas con origen del agua por río, vertiente, estero, canal, lago, etc."
    ),
    variable_corta = c(
      "TOTAL_PERS",
      "TOTAL_HOMB",
      "TOTAL_MUJE",
      "PERSONAS_0",
      "PERSONAS_6",
      "PERSONAS_1",
      "PERSONAS_M",
      "PERSONAS_E",
      "PUEBLOS_IN",
      "TOTAL_VIV_",
      "TOTAL_VIV1",
      "VIV_OCUPA_",
      "TOTAL_VIVI",
      "CANTIDAD_H",
      "VIV_TIPO_C",
      "VIV_TIPO_D",
      "VIV_TIPO_T",
      "VIV_TIPO_P",
      "VIV_TIPO_M",
      "VIV_TIPO_1",
      "VIV_TIPO_O",
      "VIV_PARED_",
      "VIV_PARED1",
      "VIV_PARE_1",
      "VIV_PARE_2",
      "VIV_PARE_3",
      "VIV_PARE_4",
      "VIV_TECHO_",
      "VIV_TECHO1",
      "VIV_TECH_1",
      "VIV_TECH_2",
      "VIV_TECH_3",
      "VIV_TECH_4",
      "VIV_TECH_5",
      "VIV_PISO_P",
      "VIV_PISO_R",
      "VIV_PISO_B",
      "VIV_PISO_C",
      "VIV_PISO_T",
      "VIV_MATERI",
      "VIV_MATE_1",
      "VIV_MATE_2",
      "VIV_AGUA_R",
      "VIV_AGUA_P",
      "VIV_AGUA_C",
      "VIV_AGUA_1"
    )
  )
  
  observeEvent(input$region, {
    provincias <- unique(chileshp$PROVINCIA[chileshp$REGION == input$region])
    updateSelectInput(session, "provincia", choices = provincias)
  })
  
  observeEvent(input$provincia, {
    comunas <- unique(chileshp$COMUNA[chileshp$PROVINCIA == input$provincia])
    updateSelectInput(session, "comuna", choices = comunas)
    updateSelectInput(session, "nombre_dis", NULL)
  })
  
  observeEvent(input$comuna, {
    distritos <- unique(chileshp$NOMBRE_DIS[chileshp$COMUNA == input$comuna])
    updateSelectInput(session, "nombre_dis", choices = distritos)
  })
  
  observeEvent(input$toggle_grafico, {
    if (tipo_grafico() == "comuna") {
      tipo_grafico("distrito")
    } else {
      tipo_grafico("comuna")
    }
  })
  
  # Generar información sobre la variable seleccionada

  output$mapa <- renderPlot({
    req(input$region, input$provincia, input$comuna, input$nombre_dis, input$variable)
    
    if (tipo_grafico() == "comuna") {
      # Gráfico por comuna
      filtered_data <- chileshp %>%
        filter(REGION == input$region,
               PROVINCIA == input$provincia,
               COMUNA == input$comuna) %>%
        filter(!is.na(.[[input$variable]]))
      
      # Generar el título del gráfico
      titulo_grafico <- paste("Mapa de calor de la comuna de" ,input$comuna)
      
    } else {
      # Gráfico por distrito
      filtered_data <- chileshp %>%
        filter(REGION == input$region,
               PROVINCIA == input$provincia,
               COMUNA == input$comuna,
               NOMBRE_DIS == input$nombre_dis) %>%
        filter(!is.na(.[[input$variable]]))
      
      # Generar el título del gráfico
      titulo_grafico <- paste("Mapa de calor del distrito",input$nombre_dis)
    }
    
    # Convertir la columna input$variable a numérica
    filtered_data$input_variable <- as.numeric(as.character(filtered_data[[input$variable]]), na.rm = TRUE)
    
    # Buscar el nombre extendido de la variable seleccionada
    titulo_variable <- variable_matrix$nombre_extendido[variable_matrix$variable_corta == input$variable]
    
    
    # Crear el mapa de calor con ggplot2
    # Para mantener la relación de aspecto de las celdas cuadradas
    mapa<-ggplot(filtered_data) +
      geom_sf(aes(fill = get("input_variable")), size = 0.1) +
      scale_fill_viridis() +  # Utilizar escala de color continua
      theme_minimal() +
      theme(axis.text.x = element_blank(), axis.text.y = element_blank()) + # Agregar el título al gráfico
      labs(title = titulo_grafico,
         fill = titulo_variable )
    
    mapa + theme(legend.position = "bottom", legend.margin = margin(t = 10))
    })
  # Función para guardar el gráfico en un archivo PNG
  guardar_grafico <- function() {

    carpeta_descargas <- normalizePath("~/Downloads")
    nombre_archivo <- "mapa_calor.png"
    ruta_guardado <- file.path(carpeta_descargas, nombre_archivo)
    ggsave(filename = ruta_guardado, plot =  output$mapa(), width = 8, height = 6, units = "in")
    ruta_guardado # Devuelve la ruta del archivo guardado
  }
  
  # Función para descargar el gráfico
  output$download_grafico <- downloadHandler(
      filename = function() {
        paste("mapa_calor.png")
      },
      content = function(file) {
        # Guardar el gráfico en la carpeta de descargas y obtener la ruta del archivo guardado
        ruta_guardado <- guardar_grafico()
        # Mover el archivo desde la ruta de guardado a la ruta de descargas del usuario
        file.rename(ruta_guardado, file)
      }
    )
  
  # Función para descargar el archivo shapefile
  output$download_shp <- downloadHandler(
    filename = function() {
      paste("mapa_calor_", input$variable, ".shp", sep = "")
    },
    content = function(file) {
      # Guardar el archivo shapefile en el directorio de la aplicación
      writeOGR(obj = filtered_data, dsn = file, layer = "mapa_calor", driver = "ESRI Shapefile")
    }
  )

  }
  
  
  


# Ejecutar la aplicación Shiny
shinyApp(ui, server)
    

