library(shiny)
library(dplyr)
library(DT)
library(shinycssloaders)


datos <- readRDS("datos_app.RDS")
banderas_rojas <- datos$banderas_rojas

##Borrar nombre de proveedores
banderas_rojas$proveedor <- "proveedor"

ui <- fluidPage(
  titlePanel("Índice de Riesgo de Corrupción (CRI)"),
  sidebarLayout(
    sidebarPanel(
      h3("Descripción"),
      p("Esta aplicación muestra el Índice de Riesgo de Corrupción (CRI) calculado a partir de banderas rojas en los procesos de licitación."),
      selectInput(
        inputId = "filtro_ramo",
        label = "Selecciona un ramo para filtrar:",
        choices = c("Todos", sort(unique(banderas_rojas$descripcion_ramo))),
        selected = "Todos"
      ),
      # Agregamos el input para el ocid específico
      textInput(
        inputId = "filtro_ocid",
        label = "Ingrese el OCID específico:",
        value = ""
      )
    ),
    mainPanel(
      tabsetPanel(
        tabPanel("Global",
                 h4("Promedio Global del CRI"),
                 textOutput("cri_global")
        ),
        tabPanel("Por Ramo",
                 h4("Promedio del CRI por Ramo"),
                 withSpinner(dataTableOutput("tabla_ramo"))
        ),
        tabPanel("Detalle por Proceso",
                 h4("Detalle del CRI por ocid"),
                 withSpinner(dataTableOutput("tabla_ocid"))
        ),
        tabPanel("Detalle por OCID",
                 h4("Banderas Rojas activadas para el OCID seleccionado"),
                 p("Escriba el OCID en la barra lateral izquierda"),
                 withSpinner(dataTableOutput("tabla_ocid_detalle"))
        )
      )
    )
  )
)

server <- function(input, output, session) {
  
  datos_filtrados <- reactive({
    if (input$filtro_ramo == "Todos") {
      banderas_rojas
    } else {
      banderas_rojas %>% filter(descripcion_ramo == input$filtro_ramo)
    }
  })
  
  global_cri_filtrado <- reactive({
    mean(datos_filtrados()$CRI, na.rm = TRUE)
  })
  
  promedio_cri_por_ramo_filtrado <- reactive({
    datos_filtrados() %>%
      group_by(descripcion_ramo) %>%
      summarise(promedio_CRI = mean(CRI, na.rm = TRUE)) %>%
      mutate(
        promedio_CRI_rango = case_when(
          promedio_CRI <= 0.2 ~ "Bajo",
          promedio_CRI > 0.2 & promedio_CRI <= 0.4 ~ "Medio",
          promedio_CRI > 0.4 & promedio_CRI <= 0.6 ~ "Alto",
          promedio_CRI > 0.6 ~ "Muy Alto"
        )
      ) %>% 
      arrange(desc(promedio_CRI))
  })
  
  # Reactivo para filtrar por OCID específico
  datos_ocid <- reactive({
    req(input$filtro_ocid) 
    ocid_buscar <- input$filtro_ocid
    banderas_rojas %>%
      filter(ocid2 == ocid_buscar)
  })
  
  output$cri_global <- renderText({
    paste("El promedio global del CRI es:", round(global_cri_filtrado(), 3))
  })
  
  output$tabla_ramo <- renderDataTable({
    promedio_cri_por_ramo_filtrado() %>%
      mutate(promedio_CRI = round(promedio_CRI, 3))
  }, options = list(pageLength = 10, server = TRUE))
  
  output$tabla_ocid <- renderDataTable({
    datos_filtrados() %>%
      select(ocid2, CRI, CRI_rango, institucion,proveedor) %>%
      mutate(CRI = round(CRI, 3))
  }, options = list(pageLength = 10, server = TRUE))
  
  output$tabla_ocid_detalle <- renderDataTable({
    df <- datos_ocid()
    if(nrow(df) == 0) {
      return(data.frame(Mensaje = "No se encontraron datos para el OCID ingresado"))
    }
    df %>%
      select(ocid2, br1, br2, br3, br4, br5, br6, br7, CRI) %>%
      mutate(CRI = round(CRI, 3))
  }, options = list(pageLength = 10, server = TRUE))
  
}

shinyApp(ui, server)