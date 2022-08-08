library(tidyverse)
library(janitor)
library(readxl)
library(gt)
library(herramientas)
library(comunicacion)
library(sf)
library(geoAr)
library(biscale)
library(cowplot)

base_agencias <- readRDS("/srv/DataDNMYE/agencias/rlm/base_agencias.rds")

provincias <- get_geo("ARGENTINA", "provincia") %>% 
  add_geo_codes() %>% 
  mutate(name_iso = herramientas::limpiar_texto(tolower(name_iso), enie = F)) %>% 
  rename(provincia = name_iso)

# MAPA TOTAL
datos_mapa <- base_agencias %>% 
  filter(!provincia %in% c("sin informacion","otro lugar"))

sin_geo <- nrow(base_agencias %>% 
                  filter(provincia %in% c("sin informacion","otro lugar")))

provincias <- get_geo("ARGENTINA", "provincia") %>% 
  add_geo_codes() %>%
  mutate(name_iso = herramientas::limpiar_texto(tolower(name_iso), enie = F)) %>% 
  rename(provincia = name_iso)

datos_mapa <- datos_mapa %>% 
  group_by(provincia) %>% 
  count() %>% 
  ungroup() %>% 
  left_join(provincias) %>% 
  st_as_sf()

agencias_pais <- ggplot() +
  geom_sf(data= provincias, size = .1, fill = "white") +
  geom_sf(data = datos_mapa, aes(fill = n)) +
  scale_fill_distiller(type = "seq", direction = 1, palette = "Blues") +
  theme_void() +
  theme(legend.position = "none") +
  labs(fill = "Cantidad de agencias") 

agencias_caba <- ggplot() +
  geom_sf(data = datos_mapa %>% filter(provincia !="buenos aires"), 
          aes(fill=n)) +
  scale_fill_distiller(type = "seq", direction = 1, palette = "Blues",) +
  coord_sf(xlim = c(-58.91218, -57.91353), ylim = c(-35.10866, -34.32291), expand = FALSE) +
  theme_void() +
  theme(legend.position = "none")

ggdraw() + 
  draw_plot(agencias_pais, 0, 0, 1, 1) + 
  draw_plot(agencias_caba, 0.6, 0.3, 0.5, 0.5)



# MAPAS TIPO TURISMO
datos_map <- base_agencias %>% 
  mutate(provincia_local_fisico = herramientas::limpiar_texto(tolower(provincia_local_fisico), enie = F),
         provincia_local_virtual = herramientas::limpiar_texto(tolower(provincia_local_virtual), enie = F)) %>% 
  mutate(provincia = case_when(local_fisico_virtual=="Local físico" ~ provincia_local_fisico,
                               local_fisico_virtual=="Local virtual" ~ provincia_local_virtual,
                               TRUE ~ local_fisico_virtual)) %>% 
  mutate(provincia = case_when(provincia %in% c("ciudad de buenos aires",
                                                "ciudad autonoma de bs as",
                                                "ciudad autonoma buenos aires",
                                                "ciudad autonoma de buenos aires caba",
                                                "cdad aut buenos aires",
                                                "caba","capital federal","ciudadbsas") ~ "ciudad autonoma de buenos aires",
                               TRUE ~ provincia)) %>% 
  mutate(provincia = case_when(!provincia %in% c(unique(provincias$provincia), "sin informacion") ~ "otro lugar",
                               TRUE ~ provincia))


datos_map <- datos_map %>%
  select(provincia, emisivo, receptivo) %>%
  filter(!provincia %in% c("sin informacion","otro lugar"))

# EMISIVO
emisivo <- datos_map %>% 
  group_by(provincia) %>% 
  count(emisivo) %>% 
  mutate(agencias_prov = sum(n)) %>% 
  ungroup() %>% 
  filter(emisivo == "Si") %>% 
  mutate(proporcion = n/agencias_prov) %>% 
  left_join(provincias) %>% 
  st_as_sf()

data_biscale_e <- bi_class(emisivo, x=n, y=proporcion, style="quantile",dim=4)

pais_emisivo <- ggplot() +
  #geom_sf(data=provincias) +
  geom_sf(data = data_biscale_e, aes(fill=bi_class), color ="white", size = 0.1) +
  bi_scale_fill(pal = "DkBlue2", dim = 4) +
  #scale_fill_distiller(direction = 1, palette = "PuRd") +
  theme_void() +
  #labs(fill = "Proporción de \nagencias emisivas") +
  theme(legend.position = "none",
        text = element_text(size = 16)) +
  labs(title = "Agencias emisivas")

legend_e <- bi_legend(pal="DkBlue2", dim=4, xlab="Cantidad", ylab="% emisivo", size=12)

caba_emisivo <- ggplot() +
  geom_sf(data = data_biscale_e %>% filter(provincia =="ciudad autonoma de buenos aires"), 
          aes(fill=bi_class)) +
  bi_scale_fill(pal = "DkBlue2", dim = 4) +
  #scale_fill_distiller(direction = 1, palette = "PuRd") +
  theme_void() +
  theme(legend.position = "none")

#Mapa completo
mapa_emisivo <- ggdraw() + 
  draw_plot(pais_emisivo, 0, 0, 1, 1) + 
  draw_plot(caba_emisivo, 0.58, 0.56, 0.09, 0.09) +
  draw_plot(legend_e, 0.5, 0.1, 0.3, 0.3)


# RECEPTIVO
receptivo <- datos_map %>% 
  group_by(provincia) %>% 
  count(receptivo) %>% 
  mutate(agencias_prov = sum(n)) %>% 
  ungroup() %>% 
  filter(receptivo == "Si") %>% 
  mutate(proporcion = n/agencias_prov) %>% 
  left_join(provincias) %>% 
  st_as_sf()

data_biscale_r <- bi_class(receptivo, x=n, y=proporcion, style="quantile",dim=4)

pais_receptivo <- ggplot() +
  #geom_sf(data=provincias, color ="white", size = 0.1) +
  geom_sf(data = data_biscale_r, aes(fill=bi_class), color ="white", size = 0.1) +
  bi_scale_fill(pal = "DkBlue2", dim = 4) +
  #scale_fill_distiller(direction = 1, palette = "PuRd") +
  theme_void() +
  #labs(fill = "Proporción de \nagencias emisivas") +
  theme(legend.position = "none",
        text = element_text(size = 16)) +
  labs(title = "Agencias receptivas")

legend_r <- bi_legend(pal="DkBlue2", dim=4, xlab="Cantidad", ylab="% receptivo", size=12)

caba_receptivo <- ggplot() +
  geom_sf(data = data_biscale_r %>% filter(provincia =="ciudad autonoma de buenos aires"), 
          aes(fill=bi_class)) +
  bi_scale_fill(pal = "DkBlue2", dim = 4) +
  #scale_fill_distiller(direction = 1, palette = "PuRd") +
  theme_void() +
  theme(legend.position = "none")

#Mapa completo
mapa_receptivo <- ggdraw() + 
  draw_plot(pais_receptivo, 0, 0, 1, 1) + 
  draw_plot(caba_receptivo, 0.58, 0.56, 0.09, 0.09) +
  draw_plot(legend_r, 0.5, 0.1, 0.3, 0.3)

ggdraw() + 
  draw_plot(mapa_emisivo, -0.3, 0, 1, 1) + 
  draw_plot(mapa_receptivo, 0.2, 0, 1, 1)
