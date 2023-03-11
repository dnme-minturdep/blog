library(gt)
library(arrow)
library(tidyverse)
library(scales)
library(glue)
library(sf)


serie_puna <- read_parquet("/srv/DataDNMYE/puna/serie_puna21.parquet") %>% 
  mutate(
    provincia = case_when(provincia == "Ciudad Autonoma de Buenos Aires" ~ "CABA",
                          TRUE ~ provincia),
    provincia_join = tolower(provincia),
    depto_join = herramientas::limpiar_texto(departamento_partido))

serie_puna <- serie_puna %>% 
  filter(anio == 2021) %>% 
  mutate(clasificacion_mintur_clean  = case_when(
    clasificacion_mintur == "Albergue / b&b / hostel"  ~       clasificacion_mintur,        
    clasificacion_mintur ==  "Cabañas / bungalows"   ~ clasificacion_mintur,
    clasificacion_mintur ==  "Establecimiento rural" ~        clasificacion_mintur,          
    clasificacion_mintur ==  "Hospedaje"             ~        clasificacion_mintur,          
    clasificacion_mintur ==  "Hosteria"              ~        clasificacion_mintur,          
    clasificacion_mintur ==  "Hotel 1 estrella"      ~        clasificacion_mintur,          
    clasificacion_mintur ==  "Hotel boutique"        ~        clasificacion_mintur,          
    clasificacion_mintur ==  "Hotel sin categorizar" ~        clasificacion_mintur,          
    clasificacion_mintur ==  "Hotel 2 estrellas"     ~        clasificacion_mintur,          
    clasificacion_mintur ==  "Hotel 3 estrellas"     ~        clasificacion_mintur,          
    clasificacion_mintur ==  "Sin clasificar"        ~        clasificacion_mintur,          
    clasificacion_mintur ==  "Conjunto de unidades turisticas"  ~ clasificacion_mintur,
    clasificacion_mintur ==  "Posada"                ~         clasificacion_mintur,        
    clasificacion_mintur ==  "Residencial"           ~         clasificacion_mintur,        
    clasificacion_mintur ==  "Apart hotel"           ~         clasificacion_mintur,        
    clasificacion_mintur ==  "Hotel 4 estrellas"     ~         clasificacion_mintur,        
    clasificacion_mintur ==  "Hotel 5 estrellas"     ~         clasificacion_mintur,        
    clasificacion_mintur ==  "Camping"               ~         clasificacion_mintur,        
    clasificacion_mintur ==  "Dormis"                ~         clasificacion_mintur,        
    clasificacion_mintur ==  "Motel"                 ~         clasificacion_mintur,        
    clasificacion_mintur ==  "Hotel sindical / mutual" ~       clasificacion_mintur,         
    clasificacion_mintur ==  "Albergue municipal / complejo deportivo" ~ "excluir",
    clasificacion_mintur == "Complejo turistico"    ~   clasificacion_mintur,                
    clasificacion_mintur ==   "Refugio"               ~ clasificacion_mintur,                  
    clasificacion_mintur ==   "Pension"               ~   "excluir",
    clasificacion_mintur == "Lodge de pesca / caza" ~ clasificacion_mintur,                  
    clasificacion_mintur == "Residencia universitaria" ~ "excluir"
  ))


resumen <- serie_puna %>% 
  filter(clasificacion_mintur_clean != "excluir") %>% 
  group_by(provincia_join, departamento_partido, depto_join, localidad ,
           camping = clasificacion_mintur_clean == "Camping"
  ) %>% 
  summarise(plazas = sum(plazas[camping == F]),
            unidades = sum(unidades[camping == T]))

resumen <- resumen %>%  ungroup() %>% pivot_wider(names_from = camping, values_from = c(plazas, unidades))

resumen <- resumen %>% 
  select(-c(plazas_TRUE, unidades_FALSE)) %>% 
  rename(plazas_alojamientos = plazas_FALSE, unidades_camping = unidades_TRUE ) 

resumen <- resumen %>% mutate(plazas_alojamientos = replace_na(plazas_alojamientos, 0),
                              unidades_camping = replace_na(unidades_camping, 0))



resumen <- resumen %>% 
  arrange(plazas_alojamientos) %>% 
  mutate(plazas_acum_prop = cumsum(plazas_alojamientos)/sum(plazas_alojamientos),
         localidades_acum = 1:n(),
         localidades_acum_prop = 1:n()/nrow(resumen)) 




ht_breaks <- classInt::classIntervals(resumen$plazas_alojamientos, style = "headtails", thr = .2)


# 
pal1 <- c("blue", "green", "yellow")


resumen <- resumen %>% 
  mutate(ht_clase = as.character(cut(plazas_alojamientos, 
                            breaks =  ht_breaks$brks, labels = F,
         include.lowest = T)),
         manual_clase = cut(plazas_alojamientos,
                            breaks = c(0,600,2000,80000), include.lowest = T)
         )

options(scipen = 999)
# 
# 

# 
fmt_int <- function(x) format(x, digits = 0, big.mark = ".", decimal.mark = ",")

resumen <- resumen %>% 
  select(-c(unidades_camping)) %>% 
  mutate(`limites clase algoritmo` = case_when(
    ht_clase == "1" ~ glue("Hasta {fmt_int(ht_breaks$brks[2])} plazas"),
    ht_clase == "2" ~ glue("De {fmt_int(ht_breaks$brks[2]+1)} a {fmt_int(ht_breaks$brks[3])} plazas"),
    ht_clase == "3" ~ glue("{fmt_int(ht_breaks$brks[3]+1)} plazas y más"))) 

x <- resumen %>% count(`limites clase algoritmo`)


resumen <- resumen %>% 
  mutate(`clase algoritmo` = case_when(
    ht_clase == "1" ~ "Emergente",
    ht_clase == "2" ~ "En desarrollo",
    ht_clase == "3" ~ "Consolidado"
  )) %>% 
  select(-c(ht_clase)) %>% 
  relocate(`limites clase algoritmo`,
           .after = everything()) 


resumen <- resumen %>% 
  mutate(
    "Clasificación" = case_when(
      as.character(manual_clase) == "[0,600]" ~ "Emergente",
      as.character(manual_clase) == "(600,2e+03]" ~ "En desarrollo",
      as.character(manual_clase) == "(2e+03,8e+04]" ~ "Consolidado"
      ), 
    "Límites" = case_when(
      as.character(manual_clase) == "[0,600]" ~ "Hasta 600 plazas",
      as.character(manual_clase) == "(600,2e+03]" ~ "601 a 2000 plazas",
      as.character(manual_clase) == "(2e+03,8e+04]" ~ "2001 plazas y más"))



serie_puna_clasificacion <- left_join(serie_puna,resumen %>% select(provincia_join,
                                                                    departamento_partido,
                                                                    depto_join, localidad,
                                                                    Clasificación,
                                                                    manual_clase))


serie_puna_clasificacion <- serie_puna_clasificacion %>% 
  select(-c(provincia_join, depto_join, clasificacion_mintur_clean))

serie_puna_clasificacion %>% 
  group_by(Clasificación) %>% 
  summarise(n_distinct(provincia, departamento_partido, localidad))


emergentes <- serie_puna_clasificacion %>% 
  filter(Clasificación == "Emergente")

# emergentes %>% 
#   writexl::write_xlsx(here::here("puna", "emergentes.xlsx"))
# 
# serie_puna_clasificacion %>% 
#   writexl::write_xlsx(here::here("puna","serie_puna_clasificacion.xlsx"))


puna_geom <- read_sf("/srv/DataDNMYE/capas_sig/puna_localidades_bahra.gpkg")

puna_geom <- puna_geom %>% 
  mutate(Clasificación = case_when(
    plazas <= 600 ~ "Emergente",
    plazas > 600 & plazas <= 2000 ~ "En desarrollo",
    plazas > 2000 ~ "Consolidado", 
    )) %>% 
  mutate(Clasificación =  factor(Clasificación, levels = c("Emergente",
                                                           "En desarrollo",
                                                           "Consolidado"))
         )

