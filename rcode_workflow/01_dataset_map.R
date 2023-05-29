library(tidyverse)

datalink <- "https://www.dropbox.com/s/lsninjaf4t4tdqj/datos_meli.csv?dl=1"
meli_modelo <- read_csv( datalink )

parasumar <- c("ascensores", "garage", "salon_comunal" ,
               "aircond" ,"cocina","terraza" , "comedor",  
               "balcon" , "seguridad",  "calefacc", "parrillero"  ,  
               "living" , "lavadero",    "gym", "dorm_serv"  ,   
               "placard", "dormsuite", 'aircond', "piscina","dormsuite" ,"livingcomedor"  ,"dorm_serv"      ,"comercial"      ,"jardin"         ,"bano_soc"       ,"gas_natural"  
               ,"kitchenette"    ,"playro"         ,"apto_credito"   ,"estudio"        ,"losa_rad"       ,"desayunador"    ,"ano"            ,"estar"          ,"depserv"        ,"jacuzzi"        ,"toilette"       ,"escritorio"  
               ,"sala_reuniones" ,"chimenea"       ,"asc_serv"       ,"amoblado"       ,"roof_garden"    ,"tenis"          ,"baulera"        ,"spa"         
               ,"guarderia" ) |> unique()

sumaSI <- function(x) sum(x== 'Si', na.rm = TRUE)

meli_modelo$ammenities <- 
  apply(meli_modelo[,parasumar], 1, sumaSI) + (meli_modelo$cocheras != 0) +  
  (meli_modelo$ascensores != 0) +  
  (meli_modelo$luminosidad %in% c('Buena', 'Muy buena')) 

preds <-  c('lsup_constru', "ldistancia_playa", 
            "condicion","dormitorios",
            "banos","garage","ascensores","expensas", 'ammenities',
            "barrio_agrup" )

# Split the dataset into a train and test set:
df <- meli_modelo |> 
  filter(tipo_inmueble == "Apartamentos") |> 
  select( all_of( c("lprecio","lpreciom2", preds, "long", "lat") ) ) |> 
  mutate_if( is.character, as.factor)

set.seed(8888)
ind <- sample(1:nrow(df), size = floor(nrow(df)*.7))
train <- df[ind,  ]
test  <- df[-ind, ]

saveRDS(list(df= df, train= train, test= test ), file='data/mds_datos.rds')

# mapa de precios  ----------------------------------------------------
library(leaflet)
qpal1 <- colorQuantile(c("skyblue2","deepskyblue1","blue","darkorange2","firebrick4"),
                       df$lpreciom2, n = 5)

qpal1 <- colorQuantile("Blues", df$lpreciom2, n = 5)
qpal1 <-colorNumeric("RdBu", domain =  df$lpreciom2,reverse = TRUE )
qpal1 <-colorNumeric("RdBu", domain =  df$lpreciom2,reverse = TRUE )

# el codigo produce el mapa y lo salvamos en png
# con export ... habria que hacerlo en el script
leaflet(df, options = leafletOptions(zoomControl = TRUE, minZoom = 11, maxZoom = 14)) %>% 
  addTiles() %>% 
  addCircleMarkers(radius =1, color=~qpal1(df$lpreciom2)) %>% 
  addLegend("topright", title="Sqm Price (log)", pal = qpal1, values = ~lpreciom2) 



qpal2 <-colorNumeric("RdBu", domain =  df$preciom2,reverse = TRUE )


# Figure 1: create map and save it as png
df |> 
  filter( df$long > -56.3, df$long < -55.95) |> 
  mutate(preciom2 = exp(lpreciom2)) |> 
  leaflet(options = leafletOptions(zoomControl = TRUE, minZoom = 11.5, maxZoom = 11.5)) %>% 
  addTiles() %>% 
  addCircleMarkers(radius =1, color=~qpal2(preciom2)) %>% 
  addLegend("topright", title="Sqm Price (USD)", pal = qpal2, values = ~preciom2) 



