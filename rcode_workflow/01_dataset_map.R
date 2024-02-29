library(tidyverse)
datalink <- "https://www.dropbox.com/s/lsninjaf4t4tdqj/datos_meli.csv?dl=1"
meli_modelo <- read_csv( datalink )
ffpath <- 'paper/figures/'

# 1 dataset for models  ---------------------------------------

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

ind <- sample(1:nrow(df), size = floor(nrow(df)*.7))
train <- df[ind,  ]
test  <- df[-ind, ]

saveRDS(list(df= df, train= train, test= test ), file='data/mds_datos.rds')

# 2 map figure with prices  ----------------------------------------------------
library(leaflet)
library(mapview) # for saving png file

df$preciom2 <- df$lpreciom2 |> exp()
qpal2 <-colorNumeric("RdBu", domain =  df$preciom2,reverse = TRUE )

mm <- leaflet(df, options = leafletOptions(zoomControl = TRUE, minZoom = 11.5, maxZoom = 14)) %>% 
  addTiles() %>% 
  addCircleMarkers(radius =1, color=~qpal2(df$preciom2)) %>% 
  addLegend("topright", title="Sqm Price (USD)", pal = qpal2, values = ~preciom2) 

Sys.setenv(OPENSSL_CONF="/dev/null")
mapshot(mm, file = paste0(ffpath, 'mapa_preciom2.png'))
