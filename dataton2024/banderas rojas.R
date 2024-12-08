##### CREACIÓN DE BANDERAS ROJAS #####

#### Cargar librerías ####
library(tidyverse)
library(DataExplorer)
library(janitor)

#### Cargar y preparar datos ####
load("data_final.RData")

### Formato de  fechas ###
data_final_banderas <- data_final %>%
  mutate(
    tender_tender_period_end_date = ymd_hms(tender_tender_period_end_date),
    datePublished = ymd_hms(datePublished),
    tender_tender_period_start_date = ymd_hms(tender_tender_period_start_date)
  )

### Diferencia entre fecha de adjudicación y fecha de cierre de propuestas ###
data_final_banderas <- data_final_banderas %>%
  mutate(
    diff_decision_dias = as.numeric(difftime(datePublished, tender_tender_period_end_date, units = "days")),
    diff_entrega_dias = as.numeric(difftime(tender_tender_period_end_date, tender_tender_period_start_date, units = "days"))
  )

### Estadisticos de las diferencias ###
mean_decision <- mean(data_final_banderas$diff_decision_dias, na.rm = TRUE)
sd_decision <- sd(data_final_banderas$diff_decision_dias, na.rm = TRUE)
mean_submission <- mean(data_final_banderas$diff_entrega_dias, na.rm = TRUE)
sd_submission <- sd(data_final_banderas$diff_entrega_dias, na.rm = TRUE)

#### Creación de banderas rojas#### 

### BR1: Procedimiento cerrado ###
data_final_banderas <- data_final_banderas %>%
  mutate(br1 = case_when(
    tender_procurement_method %in% c("direct","selective") ~ 1,
    TRUE ~ 0
  ))

### BR2: Licitante unico en procedimiento abierto ###
data_final_banderas <- data_final_banderas %>%
  mutate(br2 = case_when(
    tender_procurement_method == "open" & n_participantes == 1 ~ 1,
    TRUE ~ 0
  ))

### BR3: Periodo de decision extremo ###
# 1 si el periodo entre el fin de recepción de propuestas y la adjudicación es ≤5 días 
# o superior a (media + 2*sd). 0 en caso contrario.

data_final_banderas <- data_final_banderas %>%
  mutate(br3 = case_when(
    !is.na(diff_decision_dias) & diff_decision_dias <= 5 ~ 1,
    !is.na(diff_decision_dias) & diff_decision_dias > (mean_decision + 2*sd_decision) ~ 1,
    TRUE ~ 0
  ))

  
### BR4: Periodo de presentacion extremo ###
# 1 si el periodo de presentación es <14 días o > (media + 2*sd), 0 en caso contrario.
data_final_banderas <- data_final_banderas %>%
  mutate(br4 = case_when(
    !is.na(diff_entrega_dias) & diff_entrega_dias < 14 ~ 1,
    !is.na(diff_entrega_dias) & diff_entrega_dias > (mean_submission + 2*sd_submission) ~ 1,
    TRUE ~ 0
  ))

### BR5: Presentación de propuestas presencial ###
data_final_banderas <- data_final_banderas %>%
  mutate(br5 = case_when(
    tender_submission_method == "inPerson" ~ 1,
    TRUE ~ 0
  ))

### BR6: Criterios de evaluación no objetivos ###
data_final_banderas <- data_final_banderas %>%
  mutate(br6 = case_when(
    tender_award_criteria != "priceOnly" ~ 1,
    TRUE ~ 0
  ))

### BR7: Adjudicaciones a Proveedores Recién Establecidos ###
# Crear una nueva variable para identificar el año de creación de la empresa a partir del RFC
data_final_banderas <- data_final_banderas %>% 
  mutate(
    rfc_year = ifelse(
      grepl("^[A-Z]{3}[0-9]{6}[A-Z0-9]{3}$", id),  # Ajustar si `id` es el campo donde se encuentra el RFC
      substr(id, 4, 5),
      NA_character_
    )
  )

# Corregir el año de creación de la empresa agregando el siglo adecuado
data_final_banderas <- data_final_banderas %>% 
  mutate(
    año_creacion_empresa = case_when(
      !is.na(rfc_year) & as.numeric(rfc_year) >= 0 & as.numeric(rfc_year) <= 23 ~ paste0("20", rfc_year),
      !is.na(rfc_year) & as.numeric(rfc_year) >= 24 & as.numeric(rfc_year) <= 99 ~ paste0("19", rfc_year),
      TRUE ~ NA_character_
    )
  )

data_final_banderas$año_creacion_empresa <- as.numeric(data_final_banderas$año_creacion_empresa)

# Identificar contratos con empresas establecidas el año del contrato o el año anterior
data_final_banderas <- data_final_banderas %>% 
  mutate(
    br7 = case_when(
      año_proceso == año_creacion_empresa ~ 1,
      año_proceso == (año_creacion_empresa + 1) ~ 1,
      TRUE ~ 0
    )
  )

### Cambio nombnre de varibles ###
data_final_banderas <- data_final_banderas %>%
  rename(
    institucion = buyer_name,
    proveedor = name,
  )

### Base con solo banderas rojas ###
banderas_rojas <- data_final_banderas %>%
  select(año_proceso,ocid2, br1, br2, br3, br4, br5, br6, br7, institucion, proveedor,descripcion_ramo)

#### Análisis de banderas rojas#### 
# CRI = Corruption Risk Index
banderas_rojas <- banderas_rojas %>%
  mutate(CRI = (br1 + br2 + br3 + br4 + br5 + br6 + br7)/7)

banderas_rojas <- banderas_rojas %>%
  mutate(
    CRI_rango = case_when(
      CRI <= 0.2 ~ "Bajo",
      CRI > 0.2 & CRI <= 0.4 ~ "Medio",
      CRI > 0.4 & CRI <= 0.6 ~ "Alto",
      CRI > 0.6 ~ "Muy Alto"
    )
  )

promedio_CRI_por_ramo <- banderas_rojas %>%
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

global_cri <- mean(banderas_rojas$CRI, na.rm = TRUE)


