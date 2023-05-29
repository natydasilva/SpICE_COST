
# scenarios para estimar: 
# aml con lprecio como respuesta
# aml con previom2 como respuesta 

# para correr en cuervo: 
# los datos estan en 'datos'
# salvamos resultados en modelResults

# Modelos usando h2o 
library(tidyverse)

# 1 Construir los datos para modelos  ---------------------------------------
# datafile <- here("data/muestra_modelos_new.Rda")
# load(file = datafile)
# rm(datafile)

meli_modelo <- read_csv( 'data/datos_meli.csv' )

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
  
  saveRDS(list(df= df, train= train, test= test ), file='data/h2o_datos.rds')


rm(list = ls())






# 3 Modelos autoML de h2o---------------------------------------------------------
library(h2o)


# instalar h2o (para ubuntu que no me esta funcionando)
# install.packages("h2o", type="source", repos=(c("http://h2o-release.s3.amazonaws.com/h2o/latest_stable_R")))
h2o.init( max_mem_size = "15g", nthreads = 10)
h2o.no_progress()

dts <- readRDS('data/h2o_datos.rds')
train <- as.h2o(dts$train)
test <- as.h2o(dts$test)
preds <-  c('lsup_constru', "ldistancia_playa", 
            "condicion","dormitorios",
            "banos","garage","ascensores","expensas", 'ammenities',
            "barrio_agrup" )

# modelo usando lpreciom2 como respuesta
aml.prm2<- h2o.automl(y = "lpreciom2", x = preds, 
                       training_frame = train, 
                       max_models = 25)

# Create the explanation for whole H2OAutoML object
perf.prm2 <- h2o.performance(aml.prm2@leader, newdata = test, xval=TRUE)
#explica.prm2 <- h2o.explain(aml.prm2, test)

saveRDS(list(mds=aml.prm2, perf=perf.prm2), file='data/h2o_aml_preciom2.rds')

# obtener modelos individuales para hacer los dibujos fuera de h2o
algos <- c("deeplearning", "drf", "glm", "stackedensemble", "xgboost")
ind.models <- vector(mode='list', length=length(algos))

for (i in 1:length(algos) ) {
  mm <- h2o.get_best_model(aml.prm2, algo=algos[i])
  ind.models[[i]] <- h2o.saveModel(mm,  path= paste0('data/ind_', algos[i] ), force = TRUE  )
  }

saveRDS(ind.models, 'data/h2o_individual.rds')




h2o.shutdown(FALSE)
