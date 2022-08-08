library(tidyverse)
library(herramientas)

base_agencias <- readRDS("/srv/DataDNMYE/agencias/rlm/base_agencias.rds") %>% 
  select(-contains("marzo"))


turismo_estudiantil <- nrow(base_agencias %>% 
                              select(realiza_turismo_estudiantil:externo) %>% 
                              filter(realiza_turismo_estudiantil=="Si"))


# BASE COMPLETA

destinos <- base_agencias %>%
  filter(realiza_turismo_estudiantil=="Si") %>%
  select(san_carlos_de_bariloche:especifique_destino)

destinos_nac <- destinos %>%
  select(destinos = especifique) %>%
  filter(destinos != "Sin información") %>%
  mutate(destinos = str_replace_all(destinos, "-", ","),
         destinos = str_replace_all(destinos, "/", ","),
         destinos = str_replace_all(destinos, "\\.", ","),
         destinos = tolower(destinos),
         destinos = remover_tildes(destinos),
         destinos = str_replace_all(destinos, " y ", ","))

n <- max(stringr::str_count(destinos_nac$destinos, ","))

destinos_test <- base_agencias %>%
  mutate(especifique = str_replace_all(especifique, "-", ","),
         especifique = str_replace_all(especifique, "/", ","),
         especifique = str_replace_all(especifique, "\\.", ","),
         especifique = tolower(especifique),
         especifique = remover_tildes(especifique),
         especifique = str_replace_all(especifique, " y ", ",")) %>% 
  separate(especifique, into = as.character(paste0("especifique",c(1:n))), sep = ",") %>% 
  mutate(across(c(especifique1:especifique19), str_trim)) %>% 
  pivot_longer(san_carlos_de_bariloche:otro_destino,
               names_to = "destinos", values_to = "Aplica") %>%
  mutate(destinos = case_when(str_detect(destinos, "especifique") ~ Aplica,
                              TRUE ~ destinos)) %>% 
  mutate(Aplica = case_when(!Aplica %in% c("Si", "No", "Sin información","sin informacion", NA) ~ "Si",
                            is.na(destinos) ~ `is.na<-`(Aplica),
                            TRUE ~ Aplica),
         destinos = case_when(destinos == "" ~ "Sin información",
                              TRUE ~ destinos)) %>% 
  filter(!is.na(destinos)) %>% 
  pivot_wider(names_from = destinos, values_from = Aplica, values_fn = function(x) paste(x, collapse="_"))


destinos_te_agregado <- destinos_test %>% 
  pivot_longer(c(san_carlos_de_bariloche:`mina clavero`), 
               names_to = "destinos", values_to = "Aplica") %>% 
  count(destinos, Aplica) %>% 
  filter(Aplica == "Si") %>% 
  select(-Aplica) %>% 
  mutate(destinos = case_when(destinos == "brasil_estudiantil" ~ "Brasil",
                              destinos == "mexico" ~ "México",
                              destinos %in% c("otro","otro_destino","otros") ~ "Otros",
                              destinos == "republica_dominicana" ~ "Rep. Dominicana",
                              destinos == "san_carlos_de_bariloche" ~ "Bariloche",
                              destinos == "villa_carlos_paz" ~ "Carlos Paz",
                              destinos %in% c("caba","buenos  aires","bs as") ~ "buenos aires",
                              destinos %in% c("cataratas","cataratas del iguazu","iguazu") ~ "puerto iguazu",
                              destinos %in% c("puero madryn","pto madryn","madryn") ~ "puerto madryn",
                              destinos == "villa gessell" ~ "villa gesell",
                              TRUE ~ destinos)) %>%
  # mutate(destinos = case_when(n >= 9 ~ destinos,
  #                             TRUE ~ "Otros")) %>%
  group_by(destinos) %>% 
  summarise(n = sum(n),
            porcentaje = n/turismo_estudiantil
  ) %>%
  ungroup() %>% 
  arrange(desc(n)) %>% 
  filter(!destinos  %in% c("Otros","Sin información"))

#saveRDS(destinos_te_agregado, "/srv/DataDNMYE/agencias/rlm/destinos_te.rds")

# destinos_nac_vec <- str_trim(unlist(strsplit(destinos_nac$destinos, ",")), side = "both")
# 
# destinos_nac <- data.frame(destinos = destinos_nac_vec)
# 
# 
# destinos_nac <- destinos_nac %>% 
#   count(destinos)
# 
# 
# destinos_ext <- destinos %>% 
#   select(destinos = especifique_destino) %>% 
#   filter(destinos != "Sin información") %>% 
#   mutate(destinos = str_replace_all(destinos, "-", ","),
#          destinos = str_replace_all(destinos, "/", ","),
#          destinos = str_replace_all(destinos, "\\.", ","),
#          destinos = tolower(destinos),
#          destinos = remover_tildes(destinos),
#          destinos = str_replace_all(destinos, " y ", ","))
# 
# destinos_ext_vec <- str_trim(unlist(strsplit(destinos_ext$destinos, ",")), side = "both")
# 
# destinos_ext <- data.frame(destinos = destinos_ext_vec)
# 
# destinos_ext <- destinos_ext %>% 
#   count(destinos)

