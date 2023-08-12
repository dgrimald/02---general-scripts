cores <- list(
  azulclaro="#00B0F0",
  azulescuro = "#161D3A",
  azulaberto = "#077FB3",
  azulfechado = "#0F4E77",
  verdeclaro = "#c7e9c0",
  verde = "#74c476",
  verdeescuro = "#006d2c",
  cinzafechado="#838383",
  cinzaaberto = "#B9B9B9",
  cinzaclaro="#F2F2F2",
  cinzaescuro="#424242",
  cinza="#C8C8C8",
  preto="#000000",
  laranjaavermelhado = "#F04000",
  amarelo = "#F0B800",
  verdemarinho = "#50B3A0",
  azulopala = "#077FB3",
  laranja = "#F09000",
  vermelhoescuro = "#AB2D0C"
)

# windowsFonts(Helvetica = windowsFont("Helvetica"))

tema_base <-  function(base_size = 12){theme(
    line =               element_line(colour = cores$azulescuro, size = .5, linetype = 1,lineend = "butt"),
    rect =               element_rect(size = 0.5, fill = cores$cinzaclaro, colour = cores$cinzaclaro, linetype = 1),
    text =               element_text(family = "Helvetica", size=10, margin=margin(), face = "plain", colour = cores$cinzaescuro, hjust = 0.5, vjust = 0.5, angle = 0, debug=F, lineheight = 1),
    
    axis.line =          element_line(colour=NA),
    axis.line.x =        element_line(colour=cores$cinzaescuro, size=0.5),
    axis.line.y =        element_line(colour=NA),    
    axis.text =          element_text(size = 8, colour= cores$cinzaescuro),
    axis.text.x =        element_text(size = 10, vjust=0.4, colour=cores$cinzaescuro, face="plain", angle=90, margin = margin(.5,0,0,0)),
    axis.text.y =        element_text(size = 10, colour=cores$cinzaescuro, face="plain", hjust=1),
    axis.ticks =         element_line(colour=NA, size=0.2),
    axis.ticks.y =       element_line(colour=NA, size=0.2),
    axis.title =         element_text(colour = cores$cinzaescuro, margin=margin(t = 10, r = 10, b = 10, l = 10)),
    axis.title.x =       element_text(margin=margin(t = 10, r = 10, b = 10, l = 10)),
    axis.title.y =       element_text(angle = 90, margin=margin(t = 10, r = 10, b = 10, l = 10)),
    axis.ticks.length =  unit(0.3, "lines"),
    
    legend.background =  element_blank(),
    legend.key =         element_blank(),
    legend.key.size =    unit(0.5, "lines"),
    legend.text =        element_text(size=10, colour=cores$cinzaescuro),
    legend.direction =   "horizontal",
    legend.justification = "center",
    legend.position="bottom",
    
    panel.background =   element_rect(fill = cores$cinzaclaro, colour = NA),
    panel.border =       element_rect(fill = NA, colour = cores$cinza,  size=0.2),
    panel.grid.major =   element_line(colour = cores$cinza, size = 0.2),
    panel.grid.minor =   element_line(colour = NA, size = 0.2),
    panel.grid.major.x = element_line(colour=cores$cinza, size=0.2),
    panel.grid.minor.x = element_line(colour=NA),
    panel.grid.major.y = element_line(colour=cores$cinza, size=0.2),
    panel.grid.minor.y = element_line(colour=NA),

    strip.background =   element_blank(),
    strip.text.x =       element_text(),
    panel.spacing = unit(1, "lines"),
    strip.switch.pad.wrap = unit(3, "lines"),
    strip.switch.pad.grid = unit(3, "lines"),
    
    plot.background =    element_rect(fill = cores$cinzaclaro),
    plot.title =         element_text(hjust=0, face="bold.italic", size=14, colour=cores$cinzaescuro, margin=margin(0,0,10,0)),
    plot.subtitle =      element_text(hjust=0, face="bold.italic", size=10, colour=cores$cinzaescuro, margin=margin(0,0,10,0)),
    plot.caption = element_text(size = 10, hjust=0),
    plot.margin =  unit(c(2, 2, 2, 2), "lines"),
    complete = TRUE
  )}


tema_binario <- list(tema_base, scale_colour_manual(values = binario), scale_fill_manual(values=binario))
tema_diverso <-  list(tema_base, scale_color_manual(values = diverso), scale_fill_manual(values=diverso))

tema_base_fundobranco <- function(base_size = 12, base_family = "Helvetica"){
  theme(
  line =               element_line(colour = cores$azulescuro, size = .5, linetype = 1,lineend = "butt"),
  rect =               element_rect(size = 0.5, fill = "white", colour = "white", linetype = 1),
  text =               element_text(family = "Helvetica", size=10, margin=margin(), face = "plain", colour = cores$cinzaescuro, hjust = 0.5, vjust = 0.5, angle = 0, debug=F, lineheight = 1),
  
  axis.line =          element_line(colour=NA),
  axis.line.x =        element_line(colour=cores$cinzaescuro, size=0.5),
  axis.line.y =        element_line(colour=NA),    
  axis.text =          element_text(size = 8, colour= cores$cinzaescuro),
  axis.text.x =        element_text(size = 10, vjust=0.4, colour=cores$cinzaescuro, face="plain", angle=90, margin = margin(.5,0,0,0)),
  axis.text.y =        element_text(size = 10, colour=cores$cinzaescuro, face="plain", hjust=1),
  axis.ticks =         element_line(colour=NA, size=0.2),
  axis.ticks.y =       element_line(colour=NA, size=0.2),
  axis.title =         element_text(colour = cores$cinzaescuro, margin=margin(t = 10, r = 10, b = 10, l = 10)),
  axis.title.x =       element_text(margin=margin(t = 10, r = 10, b = 10, l = 10)),
  axis.title.y =       element_text(angle = 90, margin=margin(t = 10, r = 10, b = 10, l = 10)),
  axis.ticks.length =  unit(0.3, "lines"),
  
  legend.background =  element_blank(),
  legend.key =         element_blank(),
  legend.key.size =    unit(1, "lines"),
  legend.text =        element_text(size=10, colour=cores$cinzaescuro),
  legend.direction =   "horizontal",
  legend.justification = "center", 
  legend.position="bottom",
  
  panel.background =   element_rect(fill = "white", colour = NA),
  panel.border =       element_rect(fill = NA, colour = cores$cinza,  size=0.2),
  panel.grid.major =   element_line(colour = cores$cinza, size = 0.2),
  panel.grid.minor =   element_line(colour = NA, size = 0.2),
  panel.grid.major.x = element_line(colour=cores$cinza, size=0.2),
  panel.grid.minor.x = element_line(colour=NA),
  panel.grid.major.y = element_line(colour=cores$cinza, size=0.2),
  panel.grid.minor.y = element_line(colour=NA),
  
  strip.background =   element_blank(),
  strip.text.x =       element_text(),
  panel.spacing = unit(1, "lines"),
  strip.switch.pad.wrap = unit(3, "lines"),
  strip.switch.pad.grid = unit(3, "lines"),
  
  plot.background =    element_rect(fill = "white"),
  plot.title =         element_text(hjust=0, face="bold.italic", size=14, colour=cores$cinzaescuro, margin=margin(0,0,10,0)),
  plot.subtitle =      element_text(hjust=0, face="bold.italic", size=10, colour=cores$cinzaescuro, margin=margin(0,0,10,0)),
  plot.caption =       element_text(size = 10, hjust=0),
  plot.margin =        unit(c(2, 2, 2, 2), "lines"),
  complete =           TRUE
)}

tema_base_network <- function(base_size = 12, base_family = "Helvetica"){
  theme(
    line =               element_line(colour = cores$azulescuro, size = .5, linetype = 1,lineend = "butt"),
    rect =               element_rect(size = 0.5, fill = "white", colour = "white", linetype = 1),
    text =               element_text(family = "Helvetica", size=10, margin=margin(), face = "plain", colour = cores$cinzaescuro, hjust = 0.5, vjust = 0.5, angle = 0, debug=F, lineheight = 1),
    
    axis.line =          element_line(colour=NA),
    axis.line.x =        element_line(colour=NA, size=0),
    axis.line.y =        element_line(colour=NA),    
    axis.text =          element_text(size = 0, colour= NA),
    axis.text.x =        element_text(size = 0),
    axis.text.y =        element_text(size = 0),
    axis.ticks =         element_line(colour=NA, size=0),
    axis.ticks.y =       element_line(colour=NA, size=0),
    axis.title =         element_text(colour = cores$cinzaescuro, margin=margin(t = 10, r = 10, b = 10, l = 10)),
    axis.title.x =       element_text(size=0),
    axis.title.y =       element_text(size=0),
    axis.ticks.length =  unit(0, "lines"),
    
    legend.background =  element_blank(),
    legend.key =         element_blank(),
    legend.key.size =    unit(1, "lines"),
    legend.text =        element_text(size=10, colour=cores$cinzaescuro),
    legend.direction =   "horizontal",
    legend.justification = "center", 
    legend.position="bottom",
    
    panel.background =   element_rect(fill = "white", colour = NA),
    panel.border =       element_rect(fill = NA, size=0),
    panel.grid.major =   element_line(size=0),
    panel.grid.minor =   element_line(size=0),
    panel.grid.major.x = element_line(size=0),
    panel.grid.minor.x = element_line(size=0),
    panel.grid.major.y = element_line(size=0),
    panel.grid.minor.y = element_line(size=0),
    
    strip.background =   element_blank(),
    strip.text.x =       element_text(),
    panel.spacing = unit(1, "lines"),
    strip.switch.pad.wrap = unit(3, "lines"),
    strip.switch.pad.grid = unit(3, "lines"),
    
    plot.background =    element_rect(fill = "white"),
    plot.title =         element_text(hjust=0, face="bold.italic", size=14, colour=cores$cinzaescuro, margin=margin(0,0,10,0)),
    plot.subtitle =      element_text(hjust=0, face="bold.italic", size=10, colour=cores$cinzaescuro, margin=margin(0,0,10,0)),
    plot.caption =       element_text(size = 10, hjust=0),
    plot.margin =        unit(c(2, 2, 2, 2), "lines"),
    complete =           TRUE
  )}

