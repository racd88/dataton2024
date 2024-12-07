##### PREPARACIÓN Y LIMPIEZA DE DATOS #####

#### Cargar librerías ####
library(tidyverse)
library(janitor)
library(DataExplorer)
library(jsonlite)

#### Cargar y preparar datos ####
# contrataciones <- fromJSON("datos/apf_releases.json",flatten = TRUE) # Datos originales
# El JSON original es muy pesado, por lo que se guarda en un archivo RData para mayor eficiencia
# save(contrataciones, file = "contrataciones.RData") 
load("contrataciones.RData")

### Limpieza inicial de nombres ###
contrataciones2 <- contrataciones %>% 
  clean_names()

### Exploración preliminar ###
glimpse(contrataciones)

### Eliminación de variables no necesarias para el análisis ###
contrataciones3 <- contrataciones2 %>% 
  select(-c(id,initiation_type,language,tag,id_oid,publisher_uid,publisher_uri,tender_has_enquiries,tender_title,tender_items, tender_number_of_tenderers,tender_tenderers,tender_framework_agreement, tender_framework_agreement_platform,tender_framework_agreement_title))

sort(unique(contrataciones2$initiation_type))
sort(unique(contrataciones2$tender_has_enquiries)) # NOTA: Es raro que ningún contrato tenga consultas?

#### Procesamiento de variables anidadas ####
# Hay varias variables de interés que están anidadas en listas, por lo que es necesario extraerlas.

### Awards ###
# Adentro de "awards" habia ids tipo character e integer, por lo que no se podian aplanar los datos con unnest.
# Se convitieron todos los ids a tipo characters
contrataciones3$awards <- lapply(contrataciones3$awards, function(x) {
  if ("id" %in% colnames(x)) {  
    x$id <- as.character(x$id) 
  }
  x
})

awards <- contrataciones3 %>% 
  select(ocid, awards) %>% 
  unnest(cols = awards)

### Contracts ###
contrataciones3$contracts <- lapply(contrataciones3$contracts, function(x) {
  if ("awardID" %in% colnames(x)) {  
    x$awardID <- as.character(x$awardID) 
  }
  x
})

contracts <- contrataciones3 %>% 
  select(ocid,contracts) %>% 
  unnest(cols = contracts)

### Parties ### 
parties <- contrataciones3 %>% 
  select(ocid,parties) %>% 
  unnest(cols = parties)

### Tender Documents ###
tender_documents <- contrataciones3 %>% 
  select(ocid,tender_documents) %>% 
  unnest(cols = tender_documents)

### Award Documents ###
award_documents <- awards %>% 
  select(ocid,documents) %>% 
  unnest(cols = documents)

### Suppliers ###
suppliers <- awards %>% 
  select(ocid,suppliers) %>% 
  unnest(cols = suppliers)

### Limpieza de contrataciones3, post extracción de variables anidadas ###
contrataciones4 <- contrataciones3 %>% 
  select(-c(awards,contracts,parties,tender_documents))



