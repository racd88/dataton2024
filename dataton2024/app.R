library(shiny)
library(dplyr)
library(DT)

# Antes de iniciar la aplicación, cargamos el script con los datos y funciones
source("banderas rojas.R")  # Ajusta el nombre y la ruta según corresponda

# Suponemos que tras ejecutar "banderas rojas.R" tenemos 'data_final_banderas' y variables derivadas.

# Interfaz de Usuario
ui <- fluidPage(
  titlePanel("Índice de Riesgo de Corrupción (CRI)"),
  
  sidebarLayout(
    sidebarPanel(
      h3("Descripción"),
      p("Esta aplicación muestra el Índice de Riesgo de Corrupción (CRI) calculado a partir de banderas rojas en los procesos de licitación.")
    ),
    
    mainPanel(
      tabsetPanel(
        tabPanel("Global",
                 h4("Promedio Global del CRI"),
                 textOutput("cri_global")
        ),
        tabPanel("Por Ramo",
                 h4("Promedio del CRI por Ramo"),
                 dataTableOutput("tabla_ramo")
        ),
        tabPanel("Detalle por Proceso",
                 h4("Detalle del CRI por ocid"),
                 dataTableOutput("tabla_ocid")
        )
      )
    )
  )
)

# Servidor
server <- function(input, output, session) {
  
  # Promedio global
  output$cri_global <- renderText({
    paste("El promedio global del CRI es:", round(global_cri, 3))
  })
  
  # Tabla por ramo
  output$tabla_ramo <- renderDataTable({
    promedio_cri_por_ramo %>%
      mutate(promedio_CRI = round(promedio_CRI, 3))
  })
  
  # Tabla con detalle por ocid
  output$tabla_ocid <- renderDataTable({
    banderas_rojas %>%
      select(ocid2, CRI) %>%
      mutate(CRI = round(CRI, 3))
  })
}

# Ejecutar la app
shinyApp(ui, server)
