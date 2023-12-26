library("dplyr")
library("tidyr")
library("purrr")
library("readr")
library("stringr")
library("survey")


# Datos ----------------------------------------------------------------------------------------------------------

files <- list.files("data/", full.names = T)
ene <- map(files, ~ read.csv2(.x))
nombres <- str_extract_all(files, "(2020|2021|2022|2023)-(\\d{2})-[a-zA-Z]{3}") %>% str_replace_all("-", "_")
names(ene) <- paste0("a", nombres)
agno <- str_extract_all(nombres, "2020|2021|2022|2023") %>% unlist()
trimestre <- str_extract_all(nombres, "(2020|2021|2022|2023)_(\\d{2})_[a-zA-Z]{3}") %>%
  str_replace_all("-", "_") %>%
  str_remove_all("2020_|2021_|2022_|2023_")
ene <- map2(ene, agno, ~ mutate(.x, agno = .y))
ene <- map2(ene, trimestre, ~ mutate(.x, trimestre = .y))

f_variables_interes <- function(datos) {
  datos %>%
    mutate(
      PET = if_else(edad >= 15, 1, 0),
      FDT = if_else(cae_especifico >= 1 & cae_especifico <= 9, 1, 0), 
      Ocupados = if_else(cae_especifico >= 1 & cae_especifico <= 7, 1, 0), 
      Desocupados = if_else(cae_especifico >= 8 & cae_especifico <= 9, 1, 0), 
      OI = if_else(ocup_form == 2, 1, 0),
      FFT = ifelse(cae_especifico >= 10 & cae_especifico <= 28, 1, 0)
    ) %>%
    select("region", "agno", "conglomerado", "estrato", "fact_cal", "trimestre", "PET", "FDT", "Ocupados", "Desocupados", "OI", "FFT", "sexo")
}
ene <- map(ene, f_variables_interes)
ene <- bind_rows(ene)

ene_1 <- ene %>% filter(sexo == 1)

ene_2 <- ene %>% filter(sexo == 2)

dc <- svydesign(ids = ~conglomerado, strata = ~estrato, weights = ~fact_cal, data = ene)
options(survey.lonely.psu = "certainty")

dc_1 <- svydesign(ids = ~conglomerado, strata = ~estrato, weights = ~fact_cal, data = ene_1)

dc_2 <- svydesign(ids = ~conglomerado, strata = ~estrato, weights = ~fact_cal, data = ene_2)

rm(ene, ene_1, ene_2, agno, files, nombres, trimestre, f_variables_interes)

crear_tabla <- function(disegno) {
  tabla <- data.frame()
  for (i in sort(unique(dc$variables$region))) {
    toi <- svyby(~OI, by = ~ trimestre + agno, denominator = ~Ocupados, na.rm = TRUE, design = subset(dc, region == i), svyratio, covmat = TRUE)
    toi$region <- i
    to <- svyby(~Ocupados, by = ~ trimestre + agno, denominator = ~PET, na.rm = TRUE, design = subset(dc, region == i), svyratio, covmat = TRUE)
    to$region <- i
    tp <- svyby(~FDT, by = ~ trimestre + agno, denominator = ~PET, na.rm = TRUE, design = subset(dc, region == i), svyratio, covmat = TRUE)
    tp$region <- i
    td <- svyby(~Desocupados, by = ~ trimestre + agno, denominator = ~FDT, na.rm = TRUE, design = subset(dc, region == i), svyratio, covmat = TRUE)
    td$region <- i
    OCUP <- svyby(~Ocupados, by = ~ trimestre + agno, subset(dc, region == i), svytotal, na.rm = TRUE, covmat = TRUE)[1:3]
    OCUP$region <- i
    DESOC <- svyby(~Desocupados, by = ~ trimestre + agno, subset(dc, region == i), svytotal, na.rm = TRUE, covmat = TRUE)[1:3]
    DESOC$region <- i
    FT <- svyby(~FDT, by = ~ trimestre + agno, subset(dc, region == i), svytotal, na.rm = TRUE, covmat = TRUE)[1:3]
    FT$region <- i
    MAS15 <- svyby(~PET, by = ~ trimestre + agno, subset(dc, region == i), svytotal, na.rm = TRUE, covmat = TRUE)[1:3]
    MAS15$region <- i
    FFT <- svyby(~FFT, by = ~ trimestre + agno, subset(dc, region == i), svytotal, na.rm = TRUE, covmat = TRUE)[1:3]
    FFT$region <- i
    OI <- svyby(~OI, by = ~ trimestre + agno, subset(dc, region == i), svytotal, na.rm = TRUE, covmat = TRUE)[1:3]
    OI$region <- i
    tabla_unir <- list(toi, to, tp, td, OCUP, DESOC, FT, MAS15, FFT, OI)
    tabla_unir <- Reduce(function(x, y) merge(x, y, by = c("agno", "trimestre", "region"), all = TRUE), tabla_unir)
    tabla <- rbind(tabla, tabla_unir)
  }
  toi <- svyby(~OI, by = ~ trimestre + agno, denominator = ~Ocupados, na.rm = TRUE, design = dc, svyratio, covmat = TRUE)
  toi$region <- 17
  to <- svyby(~Ocupados, by = ~ trimestre + agno, denominator = ~PET, na.rm = TRUE, design = dc, svyratio, covmat = TRUE)
  to$region <- 17
  tp <- svyby(~FDT, by = ~ trimestre + agno, denominator = ~PET, na.rm = TRUE, design = dc, svyratio, covmat = TRUE)
  tp$region <- 17
  td <- svyby(~Desocupados, by = ~ trimestre + agno, denominator = ~FDT, na.rm = TRUE, design = dc, svyratio, covmat = TRUE)
  td$region <- 17
  OCUP <- svyby(~Ocupados, by = ~ trimestre + agno, design = dc, svytotal, na.rm = TRUE, covmat = TRUE)[1:3]
  OCUP$region <- 17
  DESOC <- svyby(~Desocupados, design = dc, by = ~ trimestre + agno, svytotal, na.rm = TRUE, covmat = TRUE)[1:3]
  DESOC$region <- 17
  FT <- svyby(~FDT, by = ~ trimestre + agno, design = dc, svytotal, na.rm = TRUE, covmat = TRUE)[1:3]
  FT$region <- 17
  MAS15 <- svyby(~PET, by = ~ trimestre + agno, design = dc, svytotal, na.rm = TRUE, covmat = TRUE)[1:3]
  MAS15$region <- 17
  FFT <- svyby(~FFT, by = ~ trimestre + agno, design = dc, svytotal, na.rm = TRUE, covmat = TRUE)[1:3]
  FFT$region <- 17
  OI <- svyby(~OI, by = ~ trimestre + agno, design = dc, svytotal, na.rm = TRUE, covmat = TRUE)[1:3]
  OI$region <- 17
  tabla_unir <- list(toi, to, tp, td, OCUP, DESOC, FT, MAS15, FFT, OI)
  tabla_unir <- Reduce(function(x, y) merge(x, y, by = c("agno", "trimestre", "region"), all = TRUE), tabla_unir)
  tabla <- rbind(tabla, tabla_unir)

  rownames(tabla) <- NULL
  tabla$trimestre <- toupper(substr(tabla$trimestre, nchar(tabla$trimestre) - 2, nchar(tabla$trimestre)))
  tabla$periodo <- paste(tabla$agno, tabla$trimestre, sep = ".")
  names(tabla)[names(tabla) %in% c("OI/Ocupados", "Ocupados/PET", "FDT/PET", "Desocupados/FDT")] <- c("toi", "to", "tp", "td")
  tabla$periodo <- factor(tabla$periodo, levels = unique(tabla$periodo))
  correspondencia <- data.frame(
    numero_region = 1:17,
    nombre_region = c("Tarapacá", "Antofagasta", "Atacama", "Coquimbo", "Valparaíso", "O'Higgins", "Maule", "Biobío", "Araucanía", "Los Lagos", "Aysén", "Magallanes", "Metropolitana", "Los Ríos", "Arica y Parinacota", "Ñuble", "Total país")
  )
  tabla <- merge(tabla, correspondencia, by.x = "region", by.y = "numero_region", all.x = TRUE)
  tabla$nombre_region <- factor(tabla$nombre_region, levels = unique(tabla$nombre_region))

  return(tabla)
}
tabla <- crear_tabla(dc)

tabla_1 <- crear_tabla(dc_1)

tabla_2 <- crear_tabla(dc_2)

rm(dc, dc_1, dc_2, crear_tabla)

pais <- tabla %>% mutate(sexo = "Ambos sexos")

tabla_1 <- tabla_1 %>% mutate(sexo = "Hombre")

tabla_2 <- tabla_2 %>% mutate(sexo = "Mujer")

tabla_sexos <- rbind(pais, tabla_1, tabla_2)

rm(pais, tabla_1, tabla_2)

tabla <- tabla %>% 
select(c("periodo", "nombre_region", "toi", "to", "tp", "td", "Ocupados", "Desocupados", "FDT", "PET", "FFT", "OI")) %>%
  mutate(
    toi = round(toi, 4) * 100,
    to = round(to, 4) * 100,
    tp = round(tp, 4) * 100,
    td = round(td, 4) * 100,
    Ocupados = round(Ocupados),
    Desocupados = round(Desocupados),
    FDT = round(FDT),
    PET = round(PET),
    FFT = round(FFT),
    OI = round(OI)
  )

tabla_sexos <- tabla_sexos %>%
  select(c("periodo", "nombre_region", "toi", "to", "tp", "td", "Ocupados", "Desocupados", "FDT", "PET", "FFT", "OI", "sexo"))  %>%
  mutate(
    toi = round(toi, 4) * 100,
    to = round(to, 4) * 100,
    tp = round(tp, 4) * 100,
    td = round(td, 4) * 100,
    Ocupados = round(Ocupados),
    Desocupados = round(Desocupados),
    FDT = round(FDT),
    PET = round(PET),
    FFT = round(FFT),
    OI = round(OI)
  )

write.csv(tabla, "tabla.csv")
write.csv(tabla_sexos, "tabla_sexos.csv")
