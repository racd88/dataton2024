library(shiny)
library(dplyr)
library(DT)

datos <- readRDS("datos_app.RDS")
banderas_rojas <- datos$banderas_rojas
promedio_cri_por_ramo <- datos$promedio_cri_por_ramo
global_cri <- datos$global_cri


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
        tabPanel("Por Licitación",
                 h4("Detalle del CRI por ocid"),
                 dataTableOutput("tabla_ocid")
        )
      )
    )
  )
)

server <- function(input, output, session) {
  
  output$cri_global <- renderText({
    paste("El promedio global del CRI es:", round(global_cri, 3))
  })
  
  output$tabla_ramo <- renderDataTable({
    promedio_cri_por_ramo %>%
      mutate(promedio_CRI = round(promedio_CRI, 3))
  })
  
  output$tabla_ocid <- renderDataTable({
    banderas_rojas %>%
      select(ocid2, CRI) %>%
      mutate(CRI = round(CRI, 3))
  })
}

shinyApp(ui, server)
