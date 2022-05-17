
b_evyth <- b_evyth %>% 
  mutate(periodo = paste0(anio, trimestre),
         periodo_etiq = glue::glue("{anio} \n T{trimestre}"),
         tipo_visitante = factor(
           x = tipo_visitante, 
           levels = c(1,2), 
           labels = c("Turistas", "Excursionista")))
