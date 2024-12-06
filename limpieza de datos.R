######################################
#                                    #
# Limpieza de datos y procesamiuento #
#                                    #
######################################      

######## Cargar librerias necesarias ########
library(tidyverse)
library(janitor)
library(DataExplorer)
library(jsonlite)

######## Cargar datos ########

#contrataciones <- fromJSON("datos/apf_releases.json",flatten = TRUE) # Datos originales
#save(contrataciones, file = "contrataciones.RData") # El JSON original es muy pesado, por lo que se guarda en un archivo RData para mayor eficiencia
load("contrataciones.RData")

