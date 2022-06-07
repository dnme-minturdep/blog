### Defino funciones. Adaptación del origina: https://github.com/jkeirstead/r-slopegraph

tufte_sort <- function(df, x="anio", y="porc", group="transporte",
                       method="tufte", min.space=0.05) {
  ids <- match(c(x, y, group), names(df))
  df <- df[,ids]
  names(df) <- c("x", "y", "group")
  
  ## Expand grid to ensure every combination has a defined value
  tmp <- expand.grid(x=unique(df$x), group=unique(df$group))
  tmp <- merge(df, tmp, all.y=TRUE)
  df <- mutate(tmp, y=ifelse(is.na(y), 0, y))
  
  ## Cast into a matrix shape and arrange by first column
  require(reshape2)
  tmp <- dcast(df, group ~ x, value.var="y")
  ord <- order(tmp[,2])
  tmp <- tmp[ord,]
  
  min.space <- min.space*diff(range(tmp[,-1]))
  yshift <- numeric(nrow(tmp))
  
  ## Start at "bottom" row
  ## Repeat for rest of the rows until you hit the top
  for (i in 2:nrow(tmp)) {
    ## Shift subsequent row up by equal space so gap between two entries is >= minimum
    mat <- as.matrix(tmp[(i-1):i, -1])
    d.min <- min(diff(mat))
    yshift[i] <- ifelse(d.min < min.space, min.space - d.min, 0)
  }
  
  tmp <- cbind(tmp, yshift=cumsum(yshift))
  
  scale <- 1
  tmp <- melt(tmp, id=c("group", "yshift"), variable.name="x", value.name="y")
  
  ## Store these gaps in a separate variable so that they can be scaled ypos = a*yshift + y
  tmp <- transform(tmp, ypos=y + scale*yshift)
  return(tmp)
  
}


plot_slopegraph <- function(df, fontsize) {
  ylabs <- subset(df, x==head(x,1))$group
  yvals <- subset(df, x==head(x,1))$ypos
  fontSize <- fontsize
  
  gg <- ggplot(df,aes(x=x,y=ypos)) +
    geom_line(aes(group=group),colour="grey80") +
    geom_point(colour="white",size=8) +
    geom_text(aes(label=y), size=fontSize, family="Roboto") +
    scale_y_continuous(name="", breaks=yvals, labels=str_wrap(ylabs,60))
  return(gg)
}  


theme_slope <- function(){
  theme_classic() +
    theme(axis.title = element_blank(),
          axis.ticks = element_blank(),
          plot.title = element_text(
            size = 10,
            hjust = 1,
            vjust = 10,
            family = "Roboto",
            face = "bold",
            lineheight = 1),
          plot.subtitle = element_text(
            size   = 8,
            hjust  = 1,
            vjust  = 5,
            family = "Roboto",
            face   = "italic"),
          plot.caption = element_text(
            size   = 7, 
            hjust  = 1,
            vjust  = -6,
            family = "Roboto",
            face   ="plain"),
          axis.text.x = element_text(
            size   = 8, 
            family = "Roboto",
            face   = "bold",
            vjust  = 1),
          axis.text.y = element_text(
            size   = 6.5, 
            family = "Roboto",
            face   = "bold"),
          plot.margin = unit(c(1.5, 0.5, 0.5, 0.5), "cm"))
}

