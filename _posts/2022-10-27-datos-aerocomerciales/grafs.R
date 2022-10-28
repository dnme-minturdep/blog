library(tidyverse)
library(lubridate)
library(cowplot)
library(glue)

# Cargamos los datos de ANAC con los que vamos a trabajar.
anac <- readRDS("/srv/DataDNMYE/aerocomercial/anac/base_anac_agrupada.rds") %>% 
  rename(Año = Año_Local,
         month = Mes_Local,
         ClasificacionVuelo = ClasificaciónVuelo) %>% 
  mutate(Fecha = as.Date(paste0(Año,"-",month,"-","01"), "%Y-%m-%d"),
         Mes = str_to_title(month(month, label = T, abbr = F))) %>% 
  filter(!ClasificacionVuelo=="N/A") 

# % vuelos y pasajeros x semana de 2019 y 2022.
anac %>% 
  filter(ClasedeVuelo == "Regular") %>% 
  group_by(ClasificacionVuelo, Año) %>% 
  summarise(vuelos = sum(vuelos, na.rm = T),
            pasajeros = sum(pasajeros, na.rm = T)) %>% 
  mutate(vuelos_x_semana = if_else(Año == "2022", vuelos/28, vuelos/56),
         pasajeros_x_semana = if_else(Año == "2022", pasajeros/28, pasajeros/56))


# Establecemos los parámetros para analizar vuelos internacionales.
i = 2022
tipo = "Internacional"


# Agrupamos los valores anuales x aerolínea.
data <- anac %>% 
  filter(ClasificacionVuelo == tipo & ClasedeVuelo == "Regular" & Año == i) %>% 
  group_by(empresa_agrup_def) %>% 
  summarise(pasajeros = sum(pax), 
            asientos = sum(asientos_pax),
            vuelos = n()) %>% 
  mutate(pasajeros_participacion = pasajeros/sum(pasajeros),
         vuelos_participacion = vuelos/sum(vuelos),
         asientos_participacion = asientos /sum(asientos),
         empresa_agrup_def = ifelse(empresa_agrup_def == "Gol Transportes Aéreos", "Gol", empresa_agrup_def)) %>% 
  mutate(across(where(is.numeric), ~ round(.x, digits = 4)))

data %>% 
  group_by(Empresa = fct_lump_n(as_factor(empresa_agrup_def), 
                                n = 5, w = pasajeros,other_level =  "Otras empresas")) %>% 
  summarise(pasajeros_participacion = sum(pasajeros_participacion)) %>% 
  mutate(Empresa = fct_reorder(Empresa, pasajeros_participacion, .desc = T)) %>% 
  mutate(Empresa = fct_relevel(Empresa, "Otras empresas", after = Inf)) %>% 
  arrange(Empresa) %>% 
  ggplot() +
  geom_col(aes(x = Empresa, y = pasajeros_participacion, fill = Empresa)) +
  scale_fill_brewer(palette = "Dark2", direction = -1) +
  theme_minimal() +
  scale_y_continuous(labels = scales::label_percent(decimal.mark = ","), name = "% Pasajeros",
                     breaks = seq(0, .45, by = .1), limits = c(0,.6)) +
  geom_label(aes(x = Empresa, y = pasajeros_participacion,
                 label = scales::label_percent(decimal.mark = ",")(round(pasajeros_participacion, 3))),
             fill = NA, fontface = "bold", hjust = -0.1, size = 7)+
  xlab("") +
  guides(fill = "none")  +
  # labs(title = glue("Participación por empresa en la cantidad de pax en vuelos {ifelse(tipo == 'Internacional', 'internacionales', 'de cabotaje')}"),
  #      subtitle = "Año 2022",
  #      caption = "Fuente: Elaboración propia sobre la base de la Admnistración Nacional de Aviación Civil (ANAC).") +
  theme(text = element_text(size = 15)) +
  coord_flip()

ggsave(glue("salidas/aercomercial_participacion_{tipo}.png"), width = 6)


# Cambiamos los parámetros para analizar los vuelos de cabotaje.
tipo = "Cabotaje"

data <- anac %>% 
  filter(ClasificacionVuelo == tipo & ClasedeVuelo == "Regular" & Año == i) %>% 
  group_by(empresa_agrup_def) %>% 
  summarise(pasajeros = sum(pax), 
            asientos = sum(asientos_pax),
            vuelos = n()) %>% 
  mutate(pasajeros_participacion = pasajeros/sum(pasajeros),
         vuelos_participacion = vuelos/sum(vuelos),
         asientos_participacion = asientos /sum(asientos),
         empresa_agrup_def = ifelse(empresa_agrup_def == "Gol Transportes Aéreos", "Gol", empresa_agrup_def)) %>% 
  mutate(across(where(is.numeric), ~ round(.x, digits = 4)))

data %>% 
  group_by(Empresa = fct_lump_n(as_factor(empresa_agrup_def), 
                                n = 3, w = pasajeros,other_level =  "Otras empresas")) %>% 
  summarise(pasajeros_participacion = sum(pasajeros_participacion)) %>% 
  mutate(Empresa = fct_reorder(Empresa, pasajeros_participacion, .desc = T)) %>% 
  mutate(Empresa = fct_relevel(Empresa, "Otras empresas", after = Inf)) %>% 
  arrange(Empresa) %>% 
  ggplot() +
  geom_col(aes(x = Empresa, y = pasajeros_participacion, fill = Empresa)) +
  scale_fill_brewer(palette = "Dark2", direction = -1) +
  theme_minimal() +
  scale_y_continuous(labels = scales::label_percent(decimal.mark = ","), name = "% Pasajeros",
                     breaks = seq(0, 1, by = .2), limits = c(0,1.2)) +
  geom_label(aes(x = Empresa, y = pasajeros_participacion,
                 label = scales::label_percent(decimal.mark = ",")(round(pasajeros_participacion, 3))),
             fill = NA, fontface = "bold", hjust = -0.1, size = 7)+
  xlab("") +
  guides(fill = "none")  +
  labs(
    #title = glue("Participación por empresa en la cantidad de pax en vuelos {ifelse(tipo == 'Internacional', 'internacionales', 'de cabotaje')}"),
    # subtitle = "Año 2022",
    # caption = "Fuente: Elaboración propia sobre la base de la Admnistración Nacional de Aviación Civil (ANAC)."
  ) +
  theme(text = element_text(size = 15))  +
  coord_flip()

ggsave(glue("salidas/aercomercial_participacion_{tipo}.png"), width = 6)