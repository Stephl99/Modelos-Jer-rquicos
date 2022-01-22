# Data Preprocessing
# ----------------------------------------------------------------------------------------------------------------------------------------
# Seminario de Aplicación - Modelos Jerárquicos
# ----------------------------------------------------------------------------------------------------------------------------------------

# El siguiente script contiene comando de R que sirvieron para crear una base de datos estructurara, con el fin de
# ser analizada y procesada para la estimación del número de hios de un hogar colombiano por medio de modelos multinivel

# The following script contains R commands that were used to create a structured database, in order to
# be analyzed and processed to estimate the number of children in a Colombian household through multilevel models

# ----------------------------------------------------------------------------------------------------------------------------------------


# Estructuración de información de las viviendas -----------------
# lectura de la base de datos con información de las viviendas
vivienda <- read.csv("Datos de la vivienda.csv", sep = ";")

# la variable región puede servir como variable de agrupación par los hogares colombianos
table(vivienda$REGION) # se encuentra codificada numéricamente

# Se reemplazan los valores numéricos por los nombre reales de cada región

library(dplyr)
vivienda <- vivienda %>% mutate(region = case_when(vivienda$REGION == 1 ~ "Caribe",
                                                   vivienda$REGION == 2 ~ "Oriental",
                                                   vivienda$REGION == 3 ~ "Central",
                                                   vivienda$REGION == 4 ~ "Pacífica",
                                                   vivienda$REGION == 5 ~ "Bogotá",
                                                   vivienda$REGION == 6 ~ "Antioquia",
                                                   vivienda$REGION == 7 ~ "Valle del Cauca",
                                                   vivienda$REGION == 8 ~ "San Andrés",
                                                   vivienda$REGION == 9 ~ "Orinoquía - Amazonía"))
table(vivienda$region)

# También se crea un variable que indica la superficie en km2 de cada región

# Según estudios del Dane, se toma como región oriental los departamentos de Boyacá (23189km2),
# Norte de Santander (21648km2) y Santander (30537km2).
# También se toma como región central los departamentos de # Cundinamarca (24210km2), Meta ( km2)
# y Tolima (23562km2)

superficie_oriental <- 23189  +  + 21648 + 30537
superficie_central <- 24210 + 85635 + 23562
superficie_valle <- 22195
superficie_pacifica <- 83170 - superficie_valle
superficie_NDS <- 21648
superficie_cundinamarca <- 24210
superficie_orinoquia <- 254335 - superficie_NDS - superficie_cundinamarca
superficie_amazonia <- 483119 - 85635 # toca quitarle meta
superficie_orinoquia_amazonia <- superficie_orinoquia + superficie_amazonia

# superficie en km2 de cada región: puede estar sesgada ya que el DANE no presenta esta información con detalle
vivienda <- vivienda %>% mutate(superficie_region = case_when(vivienda$REGION == 1 ~ 132288,
                                                              vivienda$REGION == 2 ~ superficie_oriental,
                                                              vivienda$REGION == 3 ~ superficie_central,
                                                              vivienda$REGION == 4 ~ superficie_pacifica,
                                                              vivienda$REGION == 5 ~ 1775,
                                                              vivienda$REGION == 6 ~ 63612,
                                                              vivienda$REGION == 7 ~ superficie_valle,
                                                              vivienda$REGION == 8 ~ 300,
                                                              vivienda$REGION == 9 ~ superficie_orinoquia_amazonia))

# Ahora, se procede a filtar los campos de información creados, junto con la variable llave que más adelante servirá
# para unir esta información

# De acuerdo con la documentación, la variable llave entre información de viviendas y hogares es DIRECTORIO

vivienda %>% rename(DIRECTORIO = ï..DIRECTORIO) -> vivienda
vivienda1 <- vivienda %>% select(DIRECTORIO, region, superficie_region, P1070, P8520S1A1)

# Creación de la variable respuesta -------------------------------------

serv_hogar <- read.csv("./Servicios del hogar.csv", header = T, sep = ";")
composicion <- read.csv("./Caracteristicas y composicion del hogar.csv", header = T, sep = ";")

serv_hogar %>% rename(DIRECTORIO = ï..DIRECTORIO) -> serv_hogar
composicion %>% rename(DIRECTORIO = ï..DIRECTORIO) -> composicion

# Union de servicios del hogar con composicion del hogar
union_servhogar_composicion <- merge(serv_hogar, composicion,
                                     by.x =  c("DIRECTORIO", "SECUENCIA_ENCUESTA"),
                                     by.y = c("DIRECTORIO", "SECUENCIA_P"))



# creacion de id unico por hogar
for (i in 93993) {
        indice <- paste(union_servhogar_composicion$DIRECTORIO,
                        union_servhogar_composicion$SECUENCIA_ENCUESTA,
                        sep = "")
}


union_servhogar_composicion %>%
        mutate(indice = indice) -> union_servhogar_composicion

union_servhogar_composicion %>%
        relocate(indice) -> union_servhogar_composicion

union_servhogar_composicion %>%
        relocate(P6051) -> union_servhogar_composicion

# conteo de hijos por hogar
union_servhogar_composicion %>%
        filter(P6051 == 3) %>%
        count(indice) -> hijos_hogar
hijos_hogar <- hijos_hogar %>% rename(hijos = n)
nrow(hijos_hogar)

for (i in 93993) {
        indice <- paste(serv_hogar$DIRECTORIO,
                        serv_hogar$SECUENCIA_ENCUESTA,
                        sep = "")
}

serv_hogar <- serv_hogar %>% mutate(indice = indice) %>%
        relocate(indice)

#' unir número de hijos con la base de datos serv_hogar, que esta en
#'  términos del hogar
serv_hogar <- merge(serv_hogar, hijos_hogar,
                    by =  c("indice"), all = TRUE)

serv_hogar <- serv_hogar %>% relocate(hijos)
serv_hogar$hijos[is.na(serv_hogar$hijos)] <- 0

# Filtro de los datos de interés -------------------
colnames(serv_hogar)
datos <- serv_hogar %>%  select(DIRECTORIO, P5000, P5010, P5024, P8534, I_HOGAR, PERCAPITA, CANT_PERSONAS_HOGAR)

# Recodificación del tipo de vivienda
datos <- datos %>%  mutate(Vivienda =  case_when(datos$P1070 == 1 ~ "Casa",
                                                 datos$P1070 == 2 ~ "Apartamento",
                                                 datos$P1070 == 3 ~ "Cuarto(s)",
                                                 datos$P1070 == 4 ~ "Vivienda Indígena",
                                                 datos$P1070 == 5 ~ "Otro"))



# Unión de datos de los hogares con datos de la vivienda -----------------------------
datos <- merge(datos, vivienda1,
                  by.x =  c("DIRECTORIO"),
                  by.y = c("DIRECTORIO"))

# cambio de nombres para las columnas

datos <- datos %>%  rename(Cuartos = P5000, Dormitorios = P5010, Inodoros = P5024, Cocina =P8534,
                           Ingreso = I_HOGAR, Ingreso_percapita = PERCAPITA, Personas = CANT_PERSONAS_HOGAR,
                           Region = region, Superficie = superficie_region, Estrato = P8520S1A1)

colnames(datos)

# Se exportan los datos para comenzar con el análisis --------------------------------
save(datos, file = "./datos.RData")
# str(datos)
