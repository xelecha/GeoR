# Load readr, readxl, sf, tidyverse
library(readr)
library(readxl)
library(sf)
library(tidyverse)

# Check loaded packages
sessionInfo()

#First Exercise

#Load source data path to var
archivo_padron <- "data/nacional_2019.txt"

# Set var names to match INE definitions
nombres_padron <- c("CPRO",
                    "CMUN",
                    "SEXO",
                    "CPRON",
                    "CMUNN",
                    "NACI",
                    "EDAD",
                    "TAMU",
                    "TAMUN")

anchos_padron <- c(2, 3, 1, 2, 3, 3, 3, 2, 2)

variables_padron <- fwf_widths(anchos_padron,
                               col_names = nombres_padron)

padron <- read_fwf(archivo_padron,
                   col_positions = variables_padron,
                   col_types = "ccicccicc")

archivo_meta <- "data/Padron_2019.xlsx"

cpro <- read_xlsx(archivo_meta,
                  sheet = "CPRO",
                  skip = 3,
                  col_names = c("CPRO", "prov_residencia"))

cpron <- read_xlsx(archivo_meta,
                   sheet = "CPRON",
                   skip = 3,
                   col_names = c("CPRON", "prov_nacimiento"))

naci <- read_xlsx(archivo_meta,
                  sheet = "NACI",
                  skip = 3,
                  col_names = c("NACI", "nacionalidad"))

excel_sheets(archivo_meta)

#Load maps
prov <- read_sf("data/provincias/Provincias_ETRS89_30N.shp")

cmun <- read_xlsx(archivo_meta,
                  sheet = "CMUN",
                  skip = 5,
                  col_names = c("CPRO", "CMUN", "muni_residencia"))

head(cmun)

cmun %>%
  count(CMUN) 

cmun %>%
  count(CPRO,CMUN) 

cmun %>%
  count(CPRO,CMUN, sort=TRUE)

cmun <- filter(cmun, !is.na(CMUN))

nrow(cmun)

pob_provincia <- padron %>%
                group_by(CPRO) %>%
                count(name = "pob_total")

pob_capital_provincia <- padron %>%
                        filter(TAMU == "00")%>%
                        group_by(CPRO,CMUN) %>%
                        count(name = "pob_capital")

baseline <- (sum(pob_capital_provincia$pob_capital)/sum(pob_provincia$pob_total))*100

pob_capital_provincia %>%
  arrange(pob_capital) %>%
  left_join(cmun, by=c('CPRO','CMUN'))

por_capital <- pob_provincia %>%
              left_join(pob_capital_provincia, by="CPRO") %>%
              mutate(por_capital = (pob_capital/pob_total) * 100)

por_capital <- por_capital %>%
              arrange(por_capital) %>%
              left_join(cmun, by=c('CPRO','CMUN'))

por_capital %>% 
  arrange(desc(por_capital))

#Load viridis for extended color ramps
library(viridisLite)

prov %>%
  left_join(por_capital, by = c("Codigo" = "CPRO")) %>%
  select(por_capital) %>%
  plot(nbreaks = 10,
       pal = viridis,
       main = "Fracción de personas que viven en la capital")

SbRPal <- colorRampPalette(c('#2178c8','red'))

prov %>%
  left_join(por_capital, by = c("Codigo" = "CPRO")) %>%
  select(por_capital) %>%
  plot(nbreaks = 2,
       breaks = c(0, baseline, 100),
       pal = SbRPal,
       main = "Residentes en la capital respecto a la media")


#Second Exercise

naci_esp <- padron %>%
            filter(CPRON != "53",
                  CPRON != "66") %>%
            count()

naciesp_rescap <- padron %>%
                  filter(CPRON != "53",
                         CPRON != "66",
                         TAMU == "00") %>%
                  count()

naciesp_norescap <- padron %>%
                  filter(CPRON != "53",
                         CPRON != "66",
                         TAMU != "00") %>%
                  count()

naci_esp_cap <- padron %>%
                filter(CPRON != "53",
                       CPRON != "66",
                       TAMUN == "00") %>%
                count()

naci_esp_nocap <- padron %>%
                  filter(CPRON != "53",
                         CPRON != "66",
                         TAMUN != "00") %>%
                  count()

rescap_nacicap <- padron %>%
                  filter(TAMU == "00",
                         TAMUN == "00") %>%
                  count()

rescap_nonacicap <- padron %>%
                    filter(CPRON != "53",
                           CPRON != "66",
                           TAMU == "00",
                           TAMUN != "00") %>%
                    count()

norescap_nacicap <- padron %>%
                    filter(CPRON != "53",
                           CPRON != "66",
                           TAMU != "00",
                           TAMUN == "00") %>%
                    count()

norescap_nonacicap <- padron %>%
                      filter(CPRON != "53",
                             CPRON != "66",
                             TAMU != "00",
                             TAMUN != "00") %>%
                      count()

prov_total <- padron %>%
              filter(CPRON == CPRO)%>%
              group_by(CPRO) %>%
              count(name = "residentes_total")

movimiento_hacia_capital <- padron %>%
                            filter(CPRON == CPRO,
                                   TAMU == "00",
                                   TAMUN != "00")%>%
                            group_by(CPRO) %>%
                            count(name = "desplazados_hacia_capital")

movimiento_desde_capital <- padron %>%
                            filter(CPRON == CPRO,
                                   TAMU != "00",
                                   TAMUN == "00")%>%
                            group_by(CPRO) %>%
                            count(name = "desplazados_desde_capital")

movimientos <- prov %>%
              left_join(prov_total, by = c("Codigo" = "CPRO")) %>%
              left_join(movimiento_hacia_capital, by = c("Codigo" = "CPRO")) %>%
              left_join(movimiento_desde_capital, by = c("Codigo" = "CPRO"))

movimientos <- movimientos %>%
              mutate(por_hacia_capital = (desplazados_hacia_capital/residentes_total) * 100) %>%
              mutate(por_desde_capital = (desplazados_desde_capital/residentes_total) * 100) %>%
              mutate(balance_hacia_capital = por_hacia_capital - por_desde_capital)

movimientos %>%
  select(por_desde_capital) %>%
  plot(nbreaks = 8,
       pal = viridis,
       main = "Desplazados desde la capital (% prov)")

movimientos %>%
  select(por_hacia_capital) %>%
  plot(nbreaks = 8,
       breaks = c(0, 5, 10, 15, 20, 25, 30, 35, 40),
       pal = viridis,
       main = "Desplazados hacia la capital (% prov)")

library(RColorBrewer)

rdbuscale <- colorRampPalette(brewer.pal(8, "RdBu"))(16)

movimientos %>%
  select(balance_hacia_capital) %>%
  plot(breaks = c(-40, -35, -30, -25, -20, -15, -10, -5, 0, 5, 10, 15, 20, 25, 30, 35,40),
       pal = rdbuscale,
       main = "Balance movimientos hacia la capital (% prov)")
