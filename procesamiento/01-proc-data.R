# - Procesamiento de base ICILS 2023 con QoG -

# ---- Preparación de datos individuales ----

rm(list=ls())     
options(scipen=999)

# 1. Cargar librerías y bbdd 

pacman::p_load(lme4, dplyr, haven, sjlabelled, psych, purrr, tidyr, reghelper,
               haven, stargazer, ggplot2, texreg, dplyr, car, parameter)

archivos_bsg <- list.files(
  path = "data/raw_data", # Directorio actual (cambia si es necesario)
  pattern = "^BSG.*\\.Rdata$",
  full.names = TRUE
)

# Ver qué archivos encontramos
print(archivos_bsg)

# Cargar todos los archivos encontrados
for (archivo in archivos_bsg) {
  load(archivo)
  cat("Cargado:", archivo, "\n")
}

# así hasta cargar todas

# Unir bases de datos

icils_2023 <- rbind(BSGAUTI3, BSGAZEI3, BSGBFLI3, BSGBIHI3, BSGCHLI3, BSGCYPI3, BSGCZEI3, BSGDEUI3, 
                    BSGDNKI3, BSGDNWI3, BSGESPI3, BSGFINI3, BSGFRAI3, BSGGRCI3, BSGHRVI3, BSGHUNI3,
                    BSGITAI3, BSGKAZI3, BSGKORI3, BSGLUXI3, BSGLVAI3, BSGMLTI3, BSGNLDI3, BSGNORI3, 
                    BSGOMNI3, BSGPRTI3, BSGROUI3, BSGSRBI3, BSGSVKI3, BSGSVNI3, BSGSWEI3, BSGTWNI3,
                    BSGURYI3, BSGUSAI3, BSGXKXI3)


# 2. Seleccionar variables de cada base y unirlas

icils_23_proc <- icils_2023 %>%
  select( CNTRY, S_SEX, S_EXCOMP, S_LRNINTS, S_GENCLASS, S_SPECLASS, PV1CIL)

# 3. Procesamiento de variables 

# 3.1. Recodificación de casos perdidos

icils_23_proc$S_SEX <- recode(icils_23_proc$S_SEX, "c(8, 9)=NA")
icils_23_proc$S_EXCOMP <- recode(icils_23_proc$S_EXCOMP, "c(8, 9)=NA")
icils_23_proc$S_GENCLASS <- recode(icils_23_proc$S_GENCLASS, "c(998, 999)=NA")
icils_23_proc$S_SPECLASS <- recode(icils_23_proc$S_SPECLASS, "c(998, 999)=NA")
icils_23_proc$PV1CIL <- recode(icils_23_proc$PV1CIL, "c(998, 999)=NA")
icils_23_proc$S_LRNINTS <- recode(icils_23_proc$S_LRNINTS, "c(998, 999)=NA")

# 3.2. Renombramiento de variables 

icils_23_proc <- icils_23_proc %>% rename("sexo"=S_SEX,
                                    "expcompu"=S_EXCOMP,
                                    "aprendizaje_escuela"=S_LRNINTS,
                                    "autoeffesp"=S_SPECLASS,
                                    "alf_digital"=PV1CIL,
                                    "autoeffgen"=S_GENCLASS,
                                    )


# ---- Agregado de variables nivel 2 de QoG ----

qog_23 <- read.csv("data/raw_data/qog_23.csv")

qog_23_proc <- qog_23 %>%
  select(egov_egov, wdi_gini, ccodealp)

qog_23_proc <- qog_23_proc %>% rename("CNTRY"=ccodealp)

icils23_completa <- merge(icils_23_proc, qog_23_proc, by = "CNTRY", all.x = TRUE)

# Revisar si hay países con NA en los índices de nivel 2

resumen_desigualdad <- icils23_completa %>%
  group_by(CNTRY) %>%
  summarise(
    n_estudiantes = n(), 
    n_na_desigualdad = sum(is.na(wdi_gini)), 
    pct_na_desigualdad = mean(is.na(wdi_gini)) * 100, 
    tiene_datos_desigualdad = any(!is.na(wdi_gini)) 
  )

# Eliminar del análisis los países que contienen NA en alguno de los índices

eliminados <- c("AZE", "BFL", "BIH", "DNW", "OMN", "TWN", "XKX")

icils23_completa <- icils23_completa %>%
  filter(!CNTRY %in% eliminados)

icils23_completa <- na.omit(icils23_completa)

saveRDS(icils23_completa, "data/proc_data/icils23_completa.rds")
