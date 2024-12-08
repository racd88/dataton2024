##### PREPARACIÓN Y LIMPIEZA DE DATOS #####

#### Cargar librerías ####
library(tidyverse)
library(janitor)
library(DataExplorer)
library(jsonlite)
library(readxl)

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
  select(-c(id,initiation_type,language,tag,id_oid,publisher_uid,publisher_uri,tender_has_enquiries,tender_title,tender_items,tender_tenderers,tender_framework_agreement, tender_framework_agreement_platform,tender_framework_agreement_title))

sort(unique(contrataciones2$initiation_type))
sort(unique(contrataciones2$tender_has_enquiries)) # NOTA: Es raro que ningún contrato tenga consultas?

### Estandarizar OCID y crear ocid2 para identificar diferentes momentos ###
contrataciones3 <- contrataciones3 %>% 
  mutate(ocid = if_else(
    !str_detect(ocid, "^ocds-07smqs-"),  # si no empieza con ocds-07smqs-
    paste0("ocds-07smqs-", str_remove(ocid, "^-")),  # agregarlo y remover guión inicial si existe
    ocid
  )) %>%
  # Luego procedemos con la secuencia temporal
  arrange(ocid, date) %>% 
  group_by(ocid) %>%
  mutate(numero_secuencial = row_number()) %>%
  ungroup() %>%
  mutate(ocid2 = paste(ocid, numero_secuencial, sep = "_"))

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
  select(ocid2, awards) %>% 
  unnest(cols = awards)

### Contracts ###
contrataciones3$contracts <- lapply(contrataciones3$contracts, function(x) {
  if ("awardID" %in% colnames(x)) {  
    x$awardID <- as.character(x$awardID) 
  }
  x
})

contracts <- contrataciones3 %>% 
  select(ocid2,contracts) %>% 
  unnest(cols = contracts)

### Parties ### 
parties <- contrataciones3 %>% 
  select(ocid2,parties) %>% 
  unnest(cols = parties)

### Tender Documents ###
tender_documents <- contrataciones3 %>% 
  select(ocid2,tender_documents) %>% 
  unnest(cols = tender_documents)

### Award Documents ###
award_documents <- awards %>% 
  select(ocid2,documents) %>% 
  unnest(cols = documents)

### Suppliers ###
suppliers <- awards %>% 
  select(ocid2,suppliers) %>% 
  unnest(cols = suppliers)

### Limpieza de contrataciones3, post extracción de variables anidadas ###
contrataciones4 <- contrataciones3 %>% 
  select(-c(awards,contracts,parties,tender_documents,ocid,numero_secuencial))

#### Creación y selección de variables para construir las banderas rojas ####

### Número de licitante por OCID ###
parties$roles <- as.character(parties$roles)

licitantes_por_ocid <- parties %>% 
  filter(str_detect(roles, "tenderer|supplier")) %>%  # Usamos | como OR
  group_by(ocid2) %>%
  summarise(n_participantes = n_distinct(id), .groups = "drop")

### Fecha en la que terminó el periodo de recibir propuestas y fecha de adjudicación ###
fecha_termino_propuestas <- contrataciones4 %>% 
  select(ocid2, tender_tender_period_end_date) %>% 
  distinct()

fecha_adjudicacion <-award_documents %>% 
  select(ocid2, datePublished) %>% 
  distinct()
  
### Fecha inicio para recibir propuestas ###
fecha_inicio_propuestas <- contrataciones4 %>% 
  select(ocid2, tender_tender_period_start_date) %>% 
  distinct()

### Método de adjudicación ###
metodo_adjudicacion <- contrataciones4 %>% 
  select(ocid2, tender_procurement_method) %>% 
  distinct()

### Método de presentación de propuestas ###
metodo_presentacion <- contrataciones4 %>% 
  select(ocid2, tender_submission_method) %>% 
  distinct()

### Proveedores ganadores ###
# objeto suppliers ya fue creado

### Criterios de evaluación ###
criterios_evaluacion <- contrataciones4 %>% 
  select(ocid2, tender_award_criteria) %>% 
  distinct()

### Ciclo ###
año_proceso <- contrataciones4 %>% 
  select(ocid2, date, tender_tender_period_start_date) %>% 
  distinct() %>% 
  mutate(
    date = ymd_hms(date),
    tender_start = ymd_hms(tender_tender_period_start_date),
    fecha_final = if_else(is.na(date), tender_start, date),
    año_proceso = year(fecha_final)
  ) %>%
  select(ocid2, año_proceso)

### Monto por adjudicación ###
monto <- awards %>% 
  select(ocid2, value.amount, value.currency) %>% 
  group_by(ocid2) %>%
  slice_head(n = 1) %>%  # toma solo el primer registro de cada ocid2
  ungroup()

### Proveedores unicos que ganaron ###
proveedores_unicos <- suppliers %>% 
  select(ocid2, id, name) %>% 
  distinct()


### Compradores e identificación del ramo de cada comprador ###
ramos <- read_xlsx("PIC_UnidadesCompradoras_20241207.xlsx")
ramos <- ramos %>% clean_names()
ramos <- ramos %>% 
  filter (orden_de_gobierno == "APF") %>%
  select (clave_ramo,descripcion_ramo,tipo_ramo,institucion,siglas_institucion) %>% 
  distinct()

compradores <- contrataciones4 %>% 
  select(ocid2, buyer_name) %>% 
  distinct()

# Limpieza de los nombres de los compradores del S6
compradores_limpios <- compradores %>%
  mutate(
    buyer_name = str_to_upper(buyer_name),  # Convertir a mayúsculas
    buyer_name = str_trim(buyer_name),      # Eliminar espacios extra
    # Limpiar signos de puntuación y abreviaciones
    buyer_name = str_replace_all(buyer_name, "[,\\.']", ""), # quita comas y puntos
    buyer_name = str_replace_all(buyer_name, "S A DE C V", "SA DE CV"),
    buyer_name = str_replace_all(buyer_name, "\\s+", " "),   # elimina espacios múltiples
    buyer_name = iconv(buyer_name, to = "ASCII//TRANSLIT")  # Eliminar acentos
  )

# Limpieza de los nombres de los compradores en la base de Compranet
ramos_limpios <- ramos %>%
  mutate(
    institucion = str_to_upper(institucion),
    institucion = str_trim(institucion),
    institucion = str_replace_all(institucion, "[,\\.']", ""),
    institucion = str_replace_all(institucion, "S A DE C V", "SA DE CV"),
    institucion = str_replace_all(institucion, "\\s+", " "),
    institucion = iconv(institucion, to = "ASCII//TRANSLIT")
  )

# Comparacion de nombres de compradores y ramos
comparacion <- compradores_limpios %>%
  distinct(buyer_name) %>%
  mutate(institucion_cercana = map_chr(buyer_name,
                                       ~agrep(.x, unique(ramos_limpios$institucion),
                                              max.distance = 0.1,
                                              value = TRUE)[1]))

# Unir compradores con ramos
compradores_ramos <- compradores_limpios %>%
  left_join(ramos_limpios, by = c("buyer_name" = "institucion"))

data_final <- licitantes_por_ocid %>%
  left_join(fecha_termino_propuestas, by = "ocid2") %>%
  left_join(fecha_adjudicacion, by = "ocid2") %>%
  left_join(fecha_inicio_propuestas, by = "ocid2") %>%
  left_join(metodo_adjudicacion, by = "ocid2") %>%
  left_join(metodo_presentacion, by = "ocid2") %>%
  left_join(criterios_evaluacion, by = "ocid2") %>%
  left_join(año_proceso, by = "ocid2") %>%  # -date si no la necesitas
  left_join(monto, by = "ocid2") %>%
  left_join(proveedores_unicos, by = "ocid2") %>%
  left_join(compradores_ramos, by = "ocid2")

#### Creación de DB final para análisis ####

# Guardar datos finales
save(data_final, file = "data_final.RData")

