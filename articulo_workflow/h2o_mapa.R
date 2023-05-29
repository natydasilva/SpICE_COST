

# ver mod_preciosML.R

# mapa de precios  ----------------------------------------------------
library(leaflet)
qpal1 <- colorQuantile(c("skyblue2","deepskyblue1","blue","darkorange2","firebrick4"), mvd_apt$lpreciom2, n = 5)

qpal1 <- colorQuantile("Blues", mvd_apt$lpreciom2, n = 5)
qpal1 <-colorNumeric("RdBu", domain =  mvd_apt$lpreciom2,reverse = TRUE )
qpal1 <-colorNumeric("RdBu", domain =  mvd_apt$lpreciom2,reverse = TRUE )

# el codigo produce el mapa y lo salvamos en png
# con export ... habria que hacerlo en el script
leaflet(mvd_apt, options = leafletOptions(zoomControl = TRUE, minZoom = 11, maxZoom = 14)) %>% 
  addTiles() %>% 
  addCircleMarkers(radius =1, color=~qpal1(mvd_apt$lpreciom2)) %>% 
  addLegend("topright", title="Sqm Price (log)", pal = qpal1, values = ~lpreciom2) 



qpal2 <-colorNumeric("RdBu", domain =  mvd_apt$preciom2,reverse = TRUE )

# el codigo produce el mapa y lo salvamos en png
# con export ... habria que hacerlo en el script
leaflet(mvd_apt, options = leafletOptions(zoomControl = TRUE, minZoom = 11, maxZoom = 14)) %>% 
  addTiles() %>% 
  addCircleMarkers(radius =1, color=~qpal2(mvd_apt$preciom2)) %>% 
  addLegend("topright", title="Sqm Price (USD)", pal = qpal2, values = ~preciom2) 

