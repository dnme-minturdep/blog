
library(tidyverse)
library(projmgr)
library(glue)
library(here)
library(lubridate)
library(ggplot2)
library(plotly)
library(comunicacion)
library(herramientas)
library(ggtext)
library(ggrepel)
library(gt)
library(data.table)
library(RColorBrewer)
library(evyth)
library(ggfittext)

options(encoding='UTF-8')

b_evyth_ct <- herramientas::read_file_srv("/srv/DataDNMYE/evyth/comportamiento_turistico/evyth_comp_turistico_acumulada22.csv") %>%
  crear_etiqueta( variables = 'region_origen')%>%
  mutate(region_origen=case_when(region_origen=='Provincia de Buenos Aires - Partidos del GBA'~ 'Pcia Buenos Aires - GBA',
                                 region_origen=='Provincia de Buenos Aires - Resto'~ 'Pcia Buenos Aires - Interior',#se renombró para que en informe diga interior y sea más entendible
                                 region_origen=='Centro'~'Córdoba',
                                 TRUE ~ region_origen)#,p005=fct_relevel(p005, c('Mujer', 'Varon'))
  )

#actualizar año de salida de informe


anio_actual<-b_evyth_ct %>% 
  filter(anio==2022) %>% 
  summarise(anio= unique(anio)) %>% 
  pull(anio)

anio_anterior<-b_evyth_ct %>% 
  filter(anio==2021) %>% 
  summarise(anio= unique(anio)) %>% 
  pull(anio)

#Años 2012-2022

t1<-b_evyth_ct %>% #tabla general
  group_by(anio, al_menos_un_viaje) %>%
  summarise(participacion = sum(pondera, na.rm = T)) %>% 
  ungroup() %>%
  group_by(anio) %>%
  mutate(part_relat = round(participacion/sum(participacion)*100,1),
         al_menos_un_viaje = case_when(al_menos_un_viaje == 1  ~ "Si",
                                       TRUE ~ "No"))

#plotly

rm(g1)
g1<- t1%>%
  filter(al_menos_un_viaje ==  "Si") %>% 
  plot_ly(
    x = ~anio,y = ~part_relat, 
    type='bar', #opacity=0,7,
    marker=list(color=~ifelse(anio==anio_actual,'#9283BE','#37BBED')),
    text=~paste0(formatC(part_relat,
                         big.mark = ".",
                         decimal.mark = ','),'%'),
    hovertext=~formatC(part_relat,
                       big.mark = ".",
                       decimal.mark = ','),
    textangle = 270,
    hovertemplate = '%{x}; %{hovertext}%<extra></extra>'
    ) %>% 
  layout(#title=list(text='.', font=list(size=12)), 
    xaxis= list(title=list(text= 'Año', standoff=3, font=list(size=12)),tickvals=~anio,tickangle = -70,
                tickfont=list(family='Helvetica', size=12, color='#6F6F6E'), showline=T, linecolor='#B8B8B8'),
    yaxis=list(title=list(text='Porcentaje de personas con viajes', font=list(size=12)), tickfont=list(family='Helvetica', size=12, color='#6F6F6E'),tickvals=seq(0,60,10), categoryorder='trace'),  standoff=4)

#análisis por quintil


t3<-b_evyth_ct %>% 
  group_by(anio,quintil_pcf_visitante,al_menos_un_viaje) %>%
  summarise(participacion = sum(pondera, na.rm = T)) %>% 
  mutate(part_relat=round(participacion/sum(participacion)*100,1)) %>% 
  filter(al_menos_un_viaje==1) 

rm(g3b)
g3b<-t3%>%
  ggplot(aes(x= quintil_pcf_visitante, y= part_relat, fill=as_factor(anio),
             text= paste('quintil:',quintil_pcf_visitante,
                              '<br>%:', part_relat,
                         '<br>año:',anio))) +
  geom_col(position = position_dodge()) +
  geom_label(aes(label = paste0(format(part_relat, decimal.mark = ",")#, "%"
  )), label.size = 0.10,label.r=unit(0.05, "lines"),
  label.padding =  unit(0.10, "lines"),
  position = position_dodge(width = 0.8), fill='white',vjust = -0.2, size =2.8) +
  facet_wrap(~anio, ncol=2, dir='h')+
  scale_fill_dnmye(palette = 'cualitativa')+
  #scale_color_dnmye(palette = 'cualitativa')+
  ylim(0,100)+
  theme_minimal(base_size = 10) +
  theme(legend.position = 'none',
        plot.caption  = element_markdown(hjust = 0,size=9),
        axis.text.x=element_text(hjust=1)
  ) +
  labs(x = "Quintil", y = "Porcentaje", fill = ""#, caption = ("**Fuente**: DNMyE en base a información de la EVyTH")
  )

g3bb<-ggplotly(g3b,tooltip = "text")


#Cuadro 2----

#preparo tabla
t18b<-b_evyth_ct %>% 
  filter(anio!= 2006) %>% #no es una variable relevada en 2006
  group_by(anio,al_menos_un_viaje, razon_no_viaje) %>% 
  summarise(participacion = sum(pondera, na.rm = T)) %>% 
  mutate(part_relat = round(participacion/sum(participacion)*100,1)) %>% #sobre los 20M q no viaj
  ungroup() %>% 
  mutate(part_sum = round(participacion/sum(participacion)*100,1)) %>% #sobre el total de población 28M
  filter(al_menos_un_viaje!= 1)  %>% select(-2,-4,-6) %>% 
  mutate(razon_no_viaje=case_when(razon_no_viaje == 0~'Menor de 14 años',
                                  razon_no_viaje == 1~'Falta de tiempo',
                                  razon_no_viaje == 2~'Falta de dinero',
                                  razon_no_viaje == 3~'Problemas de salud',
                                  razon_no_viaje == 4~'Problemas familiares',
                                  razon_no_viaje == 5~'No lo tenia planificado',
                                  razon_no_viaje == 6~'No le gusta/no queria viajar',
                                  razon_no_viaje == 7~'Muy joven para viajar solo',
                                  razon_no_viaje == 8~'Otros',
                                  razon_no_viaje == 9 ~'NS/NR'),
         razon_no_viaje=fct_relevel(razon_no_viaje,c('Otros','NS/NR'),after = Inf ),#mando las cat a lo último 
         #anio=paste('Año ',anio)
  )%>%
  arrange(anio,razon_no_viaje,desc(part_relat)) %>% 
  pivot_wider(names_from = anio, values_from = part_relat)  


#tabla gt. Distribución según motivo principal por el que no viajaron. Años 2010-anio_actual.

gt2<-t18b %>% 
  gt() %>% 
  gt_theme_dnmye() %>% 
  tab_header(title = md('**Razón por la que no realizó ningún viaje (%)(Base: Población no viajera)**')) %>% 
  cols_label(razon_no_viaje= '') %>%
  tab_spanner(label=md('**Año**'), columns=c(2:14) ) %>% #ACTUALIZAR
  tab_style(style = list(cell_text(weight =  "bold")),
            locations = cells_body(columns= 1,rows =c(1:10) )) %>%
  fmt_number(columns = c(2: length(t18b)), rows=c(1:10), decimals = 1, dec_mark = ",", sep_mark = ".") %>%
  tab_options(table.font.size = 12,
              data_row.padding = px(3),
              column_labels.font.size = 9,
              column_labels.font.weight = "bold") %>%
  cols_align(align = 'left',columns = c(1)) %>%
  tab_source_note(source_note = md("**Fuente: DNMyE según base de datos EVYTH acumulada**"))




