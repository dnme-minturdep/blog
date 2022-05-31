# Limpieza de base de turismo internacional y armado de tablas. 

base_tur_internac <- base_tur_internac %>%
  rename(year = 'anio')  %>% 
  mutate(casos = str_replace_all(string = casos_ponderados, 
                                 pattern = ",", replacement = "." ), 
         casos = as.numeric(casos),
         paso_publ = str_replace_all(paso_publ, "Aero ", "Aeropuerto ")
  )

#base_receptivo_total años

base_tur_internac_rec <- base_tur_internac [turismo_internac == "Receptivo", .(turistas = sum(casos)), 
                                            by = .(year, mes, pais_agrupado,  
                                                   via)] 

#unique (base_tur_internac$pais_agrupado)

#completo meses faltantes.

base_tur_internac_rec <- data.table(complete (base_tur_internac_rec, 
                                              expand(base_tur_internac_rec, year, mes, 
                                                     nesting(via, 
                                                             pais_agrupado)),
                                              fill = list(turistas = 0)))

# defino ultimo mes y año.

mes_ult_nro <- as_tibble(base_tur_internac[nrow(base_tur_internac),2])
year_ult <- as_tibble(base_tur_internac[nrow(base_tur_internac),1])

#elimino meses posteriores al ultimo, que se completaron por nesting.

base_tur_internac_rec <- base_tur_internac_rec %>%
  filter ((year < as.numeric(year_ult)) | (year == as.numeric(year_ult) 
                                           & mes <= as.numeric(mes_ult_nro))) %>% 
  mutate (periodo = dmy(as.character(glue::glue("01/{mes}/{year}"))))

#defino mes ultimo en texto

base_tur_internac_rec_mes <- base_tur_internac_rec[, mes := .(fcase(mes == 1 ,"Enero", mes == 2 ,"Febrero", 
                                                                    mes == 3 ,"Marzo", mes == 4 ,"Abril", 
                                                                    mes == 5 ,"Mayo",    mes == 6 ,"Junio",
                                                                    mes == 7 ,"Julio", mes == 8 ,"Agosto",  
                                                                    mes == 9 ,"Septiembre", mes == 10 ,"Octubre",
                                                                    mes == 11 ,"Noviembre", mes == 12 ,"Diciembre"))] 						

Mes_ult <- as_tibble(base_tur_internac_rec_mes[nrow(base_tur_internac_rec_mes),2])

#borro base, solo la uso para pasar mes a texto.
rm(base_tur_internac_rec_mes)


### TABLAS

#tabla serie por via_total años. 

tabla_serie_via_ti <- base_tur_internac_rec %>% 
  group_by(periodo, via) %>%
  summarise(turistas = sum(turistas)) %>%
  mutate (turistas= (round(turistas))) 

tabla_pais_ti <- base_tur_internac_rec %>%
  filter (year ==2019) %>% 
  group_by(pais_agrupado, via) %>%
  summarise(turistas = sum(turistas)) %>%
  mutate (turistas= (round(turistas))) %>%
  group_by(pais_agrupado) %>% #calculo turistas por pais para reordenar factor
  mutate (turistas_pais = sum(turistas)) %>% 
  ungroup () %>% 
  mutate(pais_agrupado = as_factor(pais_agrupado),
         pais_agrupado = fct_rev(fct_reorder(pais_agrupado,turistas_pais)),
         turistas_porc = turistas/sum(turistas)
         )

#levels (tabla_pais_ti$pais_agrupado)

#levels no usados:
# mutate (turistas= (round(turistas)),
# mutate(pais_agrupado = fct_relevel(pais_agrupado, c("Bolivia","Brasil","Chile","Paraguay",
#                                                   "Uruguay","EE.UU. y Canadá","Resto de América", 
#                                                   "Europa", "Resto del mundo")))
# ) 


