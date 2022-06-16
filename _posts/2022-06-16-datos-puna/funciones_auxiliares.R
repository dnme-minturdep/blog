# funciones
paletas2 <- list(
  
  `c2_continuo`  = c("#50B8B1", "#9283BE"),
  `c2_contraste` = c("#50B8B1", "#EE3D8F"),
  `c9`     = c("#EE3D8F", "#F7941E", "#FFD100", "#D7DF23", "#50B8B1",
               "#9283BE", "#37BBED", "#50535C"))

dnmye_paletas2 <- function(palette = "c9", reverse = FALSE, ...) {
  
  pal <- paletas2[[palette]]
  
  if (reverse) pal <- rev(pal)
  
  grDevices::colorRampPalette(pal, ...)
}


scale_fill_dnmye2 <- function(palette = "c9", discrete = TRUE, reverse = FALSE, ...) {
  
  pal <- dnmye_paletas2(palette = palette, reverse = reverse)
  
  if (discrete) {
    ggplot2::discrete_scale("fill", paste0("dnmye_", palette), palette = pal, ...)
  } else {
    ggplot2::scale_fill_gradientn(colors = pal(256), ...)
  }
}


scale_color_dnmye2 <- function(palette = "c9", discrete = TRUE, reverse = FALSE, ...) {
  
  pal <- dnmye_paletas2(palette = palette, reverse = reverse)
  
  if (discrete) {
    ggplot2::discrete_scale("color", paste0("dnmye_", palette), palette = pal, ...)
  } else {
    ggplot2::scale_color_gradientn(colors = pal(256), ...)
  }
}

# adaptar data para mapa arg

datos_grafico_pablo <- function (datos, variable) {
  datos %>% 
    mutate(
      col_y = rank(-!!enquo(variable)) %>% 
        data_rescale(to=c(-15, -65)),
      sigmoid_end = 22,
      col_x_start = 25,
      col_x_end = !!enquo(variable) %>%  
        data_rescale(to=c(25, 110), range = c(0, max(!!enquo(variable)))),
      area_label = format(!!enquo(variable), big.mark = ".", decimal.mark = ",")
    )
}
