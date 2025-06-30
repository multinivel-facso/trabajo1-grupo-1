# - Procesamiento de base ICILS 2023 con QoG -

# ---- Preparación de datos individuales ----

rm(list=ls())     
options(scipen=999)

# 1. Cargar librerías y bbdd 

pacman::p_load(lme4, dplyr, readr, haven, sjlabelled, psych, purrr, tidyr,
               haven, stargazer, texreg, dplyr, car, tibble)

archivos_bsg <- list.files(
  path = "input/data/raw_data", # Directorio actual (cambia si es necesario)
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
                    BSGDNKI3, BSGESPI3, BSGFINI3, BSGFRAI3, BSGGRCI3, BSGHRVI3, BSGHUNI3,
                    BSGITAI3, BSGKAZI3, BSGKORI3, BSGLUXI3, BSGLVAI3, BSGMLTI3, BSGNLDI3, BSGNORI3, 
                    BSGOMNI3, BSGPRTI3, BSGROUI3, BSGSRBI3, BSGSVKI3, BSGSVNI3, BSGSWEI3, BSGTWNI3,
                    BSGURYI3, BSGUSAI3, BSGXKXI3)

rm(BSGAUTI3, BSGAZEI3, BSGBFLI3, BSGBIHI3, BSGCHLI3, BSGCYPI3, BSGCZEI3, BSGDEUI3, 
   BSGDNKI3, BSGESPI3, BSGFINI3, BSGFRAI3, BSGGRCI3, BSGHRVI3, BSGHUNI3,
   BSGITAI3, BSGKAZI3, BSGKORI3, BSGLUXI3, BSGLVAI3, BSGMLTI3, BSGNLDI3, BSGNORI3, 
   BSGOMNI3, BSGPRTI3, BSGROUI3, BSGSRBI3, BSGSVKI3, BSGSVNI3, BSGSWEI3, BSGTWNI3,
   BSGURYI3, BSGUSAI3, BSGXKXI3)

# 2. Seleccionar variables de cada base y unirlas

icils_23_proc <- icils_2023 %>%
  select( CNTRY, IISB, S_SEX, S_EXCOMP, S_LRNINTS, S_GENCLASS, PV1CIL)

# 3. Procesamiento de variables 

# 3.1. Recodificación de casos perdidos

icils_23_proc$S_SEX <- recode(icils_23_proc$S_SEX, "c(8, 9)=NA")
icils_23_proc$S_EXCOMP <- recode(icils_23_proc$S_EXCOMP, "c(8, 9)=NA")
icils_23_proc$S_GENCLASS <- recode(icils_23_proc$S_GENCLASS, "c(998, 999)=NA")
icils_23_proc$PV1CIL <- recode(icils_23_proc$PV1CIL, "c(998, 999)=NA")
icils_23_proc$S_LRNINTS <- recode(icils_23_proc$S_LRNINTS, "c(998, 999)=NA")
icils_23_proc$IISB <- recode(icils_23_proc$IISB, "c(998, 999)=NA")

# 3.2. Renombramiento de variables 

icils_23_proc <- icils_23_proc %>% rename("sexo"=S_SEX,
                                    "expcompu"=S_EXCOMP,
                                    "aprendizaje_escuela"=S_LRNINTS,
                                    "alf_digital"=PV1CIL,
                                    "autoeffgen"=S_GENCLASS,
                                    "iiseb"=IISB
                                    )


# ---- Agregado de variable nivel 2 de QoG ----

qog_23 <- read.csv("input/data/raw_data/qog_23.csv")

qog_23_proc <- qog_23 %>%
  select(egov_egov, ccodealp)

qog_23_proc <- qog_23_proc %>% rename("CNTRY"=ccodealp,
                                      "adm_digital" = egov_egov)

# Creamos una lista para poder filtrar por los países que necesitamos más adelante

qog_countries <- c("AUT", "AZE", "BFL", "BIH", "CHL", "TWN", "HRV", "CYP", "CZE",
                   "DNK", "FIN", "FRA", "DEU", "GRC", "HUN", "ITA", "KAZ", "KOR",
                   "XKX", "LVA", "LUX", "MLT", "NOR", "OMN", "PRT", "ROU", "SRB",
                   "SVK", "SVN", "ESP", "SWE", "NLD", "URY", "USA")

qog_23_proc <- filter(qog_23_proc, CNTRY %in% qog_countries)

# ---- Agregado de variable nivel 2 de Banco Mundial ----

pib_df <- read_csv(
  file = "input/data/raw_data/pib.csv",
  skip = 4,               # Saltar metadatos
  col_names = TRUE,       # Usar encabezados
  quote = "\"",
  na = c("", "NA")        # Valores faltantes
)

# Creamos lista para aplicar el mismo filtrado que con la base anterior

pib_countries <- c("Austria", "Azerbaiyán", "Bélgica", "Bosnia y Herzegovina", 
                   "Chile", "Croacia", "Chipre", "República Checa", "Dinamarca", 
                   "Finlandia", "Francia", "Alemania", "Grecia", "Hungría", 
                   "Italia", "Kazajstán", "Corea, República de", "Kosovo", 
                   "Letonia", "Luxemburgo", "Malta", "Noruega", "Omán", "Portugal",
                   "Rumania", "Serbia", "República Eslovaca", "Eslovenia", 
                   "España", "Suecia", "Países Bajos", "Uruguay", "Estados Unidos")

pib_df <- filter(pib_df, `Country Name` %in% pib_countries)

# Procesar la base para que solamente queden las columnas del año 2023 y el ID

pib_df <- pib_df %>%
  select(`Country Code`, "2023")

# Renombramos las variables; aquí es fundamental renombrar el ID de país para
# que calce con los de las demás bases y así el merge se aplique correctamente

pib_df <- rename(pib_df, "CNTRY" = `Country Code`,
                   "pib" = "2023")

pib_df$CNTRY[pib_df$CNTRY == "BEL"] <- "BFL"

# Reescalamos la variable para que esté en la misma medida que la otra
# variable nivel 2

pib_df$pib <- pib_df$pib * 0.001

# Redondeamos en dos decimales para facilitar la interpretación

pib_df$pib <- round(pib_df$pib, 2)

# Añadimos los datos de Taiwán

pib_df <- pib_df %>%
  add_row(
    CNTRY = "TWN",
    pib = 32.33
  )

# Unimos las bases 

icils23_completa <- merge(icils_23_proc, qog_23_proc, by = "CNTRY", all.x = TRUE)
icils23_completa <- merge(icils23_completa, pib_df, by = "CNTRY", all.x = TRUE)

# Creamos base agregada por país para ver qué paises tienen NA en variables nivel 2

icils_ver_na <- icils23_completa %>%
  group_by(CNTRY) %>%
  summarise_all(mean)

saveRDS(icils23_completa, "input/data/proc_data/icils_final.rds")
