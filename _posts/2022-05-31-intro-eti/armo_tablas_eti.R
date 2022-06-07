### LEVANTO CODIGOS

# codigos	

codigos <- read_xlsx("/srv/DataDNMYE/eti/codigos_pais_prov_ciudad.xlsx") %>% 									
  janitor::clean_names(.) %>% 									
  mutate (cod_loc = paste (tipo, id_provincia_pais, id_ciudad, sep = "_"),
          cod_prov = paste (tipo, id_provincia_pais, sep = "_"))

# codigos para matchear provincia.
codigos_prov <-  codigos %>% 
  select (cod_prov, nombre_provincia_pais) %>% 
  group_by (cod_prov) %>% 
  summarise (nombre_provincia_pais = first (nombre_provincia_pais))

#codigos para matchear localidad.
codigos_loc <-  codigos %>% 
  select (cod_loc, nombre_ciudad) %>% 
  group_by (cod_loc) %>% 
  summarise (nombre_ciudad = first (nombre_ciudad))

#codigos pais de origen
cod_pais_origen <- data.frame(
  "cod_pais_origen" = 1:9 , 
  "pais_origen" = as.character (c("Bolivia", "Brasil", "Chile", "Paraguay", "Uruguay", 
                                  "EE.UU. y Canadá" , "Resto América", "Europa",
                                  "Resto del mundo"))
)

# selecciono turistas.

eti_tur <- base_eti_2019 %>% 
  filter (WPF > 0, vis == 2) 							
  

#aspectos <- eti_tur %>% 
#  select(373:382, WPF) %>% 
#pivot_longer(cols =1:11, names_to = ".value" )


colnames(eti_tur)  


#ETI POR PROVINCIA.

#creo id por fila.
eti_tur$id = seq(1:nrow(eti_tur))									

prueba <- eti_tur %>% 									
  mutate ( P6A_2= tolower(P6A_2),
           P21_1_9 = case_when (
             P21_1_9 != ""  ~ P21_1_9, # si no es misisng, dejo valor.
             P21_1_9 == "" & !(P6A_1 %in% c("88", "99", "--", "03" ) ) ~ "05",  #si es missing y tiene codigo aerolinea, es aéreo
             P6A_1 %in% c("88", "99", "03") ~ P6A_1,  # si en P6A1 tiene cod. de transporte, dejo codigo.
             str_detect(P6A_2, "bici|pie") ~ "00", # sino, codifico según p6a_2
             str_detect(P6A_2, "taxi") ~ "02",
             str_detect(P6A_2, "alquil") ~ "03",
             str_detect(P6A_2, "bqb|buque|buqe|ferry|barco|colonia|sea|sycat|cacciola")  ~ "04",
             str_detect(P6A_2, "jet|avion|airlines|copa|klm|emiratos") ~ "05",
             str_detect(P6A_2, "crucer|cruecero") ~ "08",
             str_detect(P6A_2, "micro|bus") &  !str_detect (P6A_2, "buque")~ "88", # excluyo buquebus
             str_detect(P6A_2, "auto|vehiculo|moto|camioneta|coche|vehículo") 
             &  !str_detect (P6A_2, "bus")   ~ "99", # excluyo autobus
             TRUE ~ as.character(P21_1_9)
           )) 


#creo variables para pivotear.

prueba <- eti_tur %>% 									
  mutate (cod_ciudad1 = paste (P21_1_1A, P21_1_1B, P21_1_2A, sep = "_"), #CONCAT DE CATEGORÍA PROVINCIA/PAÍS, CÓDIGO PROV								
          cod_ciudad2 = paste (P21_2_1A, P21_2_1B, P21_2_2A, sep = "_"),									
          cod_ciudad3 = paste (P21_3_1A, P21_3_1B, P21_3_2A, sep = "_"),									
          cod_ciudad4 = paste (P21_4_1A, P21_4_1B, P21_4_2A, sep = "_"),									
          cod_ciudad5 = paste (P21_5_1A, P21_5_1B, P21_5_2A, sep = "_"),									
          cod_ciudad6 = paste (P21_6_1A, P21_6_1B, P21_6_2A, sep = "_"),									
          cod_ciudad7 = paste (P21_7_1A, P21_7_1B, P21_7_2A, sep = "_"),									
          cod_ciudad8 = paste (P21_8_1A, P21_8_1B, P21_8_2A, sep = "_"),									
          cod_ciudad9 = paste (P21_9_1A, P21_9_1B, P21_9_2A, sep = "_"),									
          cod_ciudad9 = paste (P21_9_1A, P21_9_1B, P21_9_2A, sep = "_"),									
          cod_ciudad10 = paste (P21_10_1A, P21_10_1B, P21_10_2A, sep = "_"),
          #          crucero_ciudad1 = case_when paste (P21_1_1A, P21_1_1B, P21_1_2A, sep = "_"), #CONCAT DE CATEGORÍA PROVINCIA/PAÍS, CÓDIGO PROV								
          #          crucero_ciudad2 = paste (P21_2_1A, P21_2_1B, P21_2_2A, sep = "_"),									
          #          crucero_ciudad3 = paste (P21_3_1A, P21_3_1B, P21_3_2A, sep = "_"),									
          #          crucero_ciudad4 = paste (P21_4_1A, P21_4_1B, P21_4_2A, sep = "_"),									
          #          crucero_ciudad5 = paste (P21_5_1A, P21_5_1B, P21_5_2A, sep = "_"),									
          #          crucero_ciudad6 = paste (P21_6_1A, P21_6_1B, P21_6_2A, sep = "_"),									
          #          crucero_ciudad7 = paste (P21_7_1A, P21_7_1B, P21_7_2A, sep = "_"),									
          #          crucero_ciudad8 = paste (P21_8_1A, P21_8_1B, P21_8_2A, sep = "_"),									
          #          crucero_ciudad9 = paste (P21_9_1A, P21_9_1B, P21_9_2A, sep = "_"),									
          #          crucero_ciudad9 = paste (P21_9_1A, P21_9_1B, P21_9_2A, sep = "_"),									
          #          crucero_ciudad10 = paste (P21_10_1A, P21_10_1B, P21_10_2A, sep = "_"),
          noches_ciudad1 = P21_1_3 + P21_1_5 + P21_1_7,# SUMA DE NOCHES 1, NOCHES 2, NOCHES PAQUETE									
          noches_ciudad2 = P21_2_3 + P21_2_5 + P21_2_7,									
          noches_ciudad3 = P21_3_3 + P21_3_5 + P21_3_7,									
          noches_ciudad4 = P21_4_3 + P21_4_5 + P21_4_7,									
          noches_ciudad5 = P21_5_3 + P21_5_5 + P21_5_7,									
          noches_ciudad6 = P21_6_3 + P21_6_5 + P21_6_7,									
          noches_ciudad7 = P21_7_3 + P21_7_5 + P21_7_7,									
          noches_ciudad8 = P21_8_3 + P21_8_5 + P21_8_7,									
          noches_ciudad9 = P21_9_3 + P21_9_5 + P21_9_7,									
          noches_ciudad10 = P21_10_3 + P21_10_5 + P21_10_7,
          naturaleza = ifelse((P39_1 == 1 & (!is.na(P39_1))) | # Turismo aventura
                                (P39_17 == 1 & (!is.na(P39_17))) | # Actividades de baja dificultad en medios naturales
                                (P39_18 == 1 & (!is.na(P39_18))) | # Visita a parques nacionales
                                (P39_19 == 1 & (!is.na(P39_19))), # A ctividades en la nieve
                              1,0)) %>% # INDICA SI REALIZÓ ACTIV. DE NATURALEZA
  rename(transporte_ciudad1	=	P21_1_9	, #MEDIO DE TRANSPORTE DE INGRESO A LA CIUDAD
         transporte_ciudad2	=	P21_2_9	,
         transporte_ciudad3	=	P21_3_9	,
         transporte_ciudad4	=	P21_4_9	,
         transporte_ciudad5	=	P21_5_9	,
         transporte_ciudad6	=	P21_6_9	,
         transporte_ciudad7	=	P21_7_9	,
         transporte_ciudad8	=	P21_8_9	,
         transporte_ciudad9	=	P21_9_9	,
         transporte_ciudad10	=	P21_10_9 ) %>% 
  select(id, WPF, P18_1,P19_2, gastoest2, origen_cv, p17nuev, trimnue, 						
         cod_ciudad1,	cod_ciudad2,	cod_ciudad3,	cod_ciudad4,	cod_ciudad5,
         cod_ciudad6,	cod_ciudad7,	cod_ciudad8,	cod_ciudad9,	cod_ciudad10,
         noches_ciudad1,	noches_ciudad2,	noches_ciudad3,	noches_ciudad4,	noches_ciudad5,
         noches_ciudad6,	noches_ciudad7,	noches_ciudad8,	noches_ciudad9,	noches_ciudad10,
         transporte_ciudad1	,
         transporte_ciudad2	,
         transporte_ciudad3	,
         transporte_ciudad4	,
         transporte_ciudad5	,
         transporte_ciudad6	,
         transporte_ciudad7	,
         transporte_ciudad8	,
         transporte_ciudad9	,
         transporte_ciudad10,
         naturaleza
  )

#pivoteo tabla

pivot <- prueba %>% 									
  pivot_longer(!c("id","WPF","P18_1", "P19_2", "gastoest2", "origen_cv", "p17nuev", "trimnue","naturaleza" ),									
               names_to = c(".value", "ciudad"), 									
               names_sep = "_", 									
               values_drop_na = TRUE) %>% 
  mutate (cod_prov = str_sub(cod, 1, 5)) %>% 
  left_join ( codigos_prov, by = "cod_prov" )%>% 
  left_join ( codigos_loc, by = c("cod" = "cod_loc")) 


#Dejo solo destinos de Arg y con noches, y saco NA en prov y loc (dejo solo los de prov ok y loc NA). 

pivot_limpia <- pivot %>% 									
  filter( !is.na(nombre_provincia_pais),
          (str_detect(cod, "PR") | nombre_provincia_pais == "Antártida"),
          noches >0 ) %>% 
  mutate (motivo = as_factor(p17nuev),
          transporte = as_factor(transporte),
          trimestre = as_factor (trimnue),
          pais_origen = as_factor (origen_cv)) %>% 
  rename (provincia = nombre_provincia_pais) 		


# Tabla 2: Cuento visitantes, en lugar de visitas. (No repito prov visitadas dos veces (le sumo las noches) 
# y pondero noches gasto y estadia)               

pivot_pond <- pivot_limpia %>%									
  group_by(id, provincia, P19_2, WPF, P18_1, gastoest2, pais_origen, trimestre, motivo) %>%  									
  summarise (noches = sum(noches)) %>% #sumo noches en la prov. 									
  mutate (noches_tot = noches * WPF,
          gasto_tot = gastoest2 * WPF,
          estadia_tot = P19_2 * WPF) %>% 
  ungroup()



##### TABLAS VARIAS POR PROV.


turistas_prov <- pivot_pond %>%									
  group_by(provincia) %>%									
  summarise(turistas = sum(WPF), noches_prov = sum(noches_tot), 
            casos_muestra = sum(P18_1)) %>% 	
  mutate (estadia_prov = noches_prov / turistas,
          turistas_porc = round( turistas /sum(turistas),3))

#
##colnames(eti.e)
#
##seleccionar columnas:
##P5, P19_2: P21_12_3, P39_1.....WPF,
#
##probando mapa
### Cargo base con geometrias de localidades indec
#geo_localidades_indec <- sf::st_read("/srv/DataDNMYE/evyth/nomenclatura_geo/Codgeo_Pais_x_loc_con_datos/indec_cods/pxlocdatos.shp") 
#
#
##mapea unidad menor? (localidades)
#ggplot() + geom_sf(data = geo_localidades_indec)
#
##ggplot() + geom_sf(data = geo_localidades_indec, aes(fill = provincia))

library(geoAr)
mapa_arg <- get_geo("ARGENTINA", level = "provincia")

#mapea unidad menor? (localidades)
ggplot() + geom_sf(data = mapa_arg)
