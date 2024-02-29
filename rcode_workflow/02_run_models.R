
library(h2o)
h2o.init( max_mem_size = "15g", nthreads = -1)
h2o.no_progress()

dts <- readRDS('data/mds_datos.rds')
train <- as.h2o(dts$train)
test <- as.h2o(dts$test)
preds <-  c('lsup_constru', "ldistancia_playa", 
            "condicion", "dormitorios",
            "banos", "garage", "ascensores", "expensas", 'ammenities',
            "barrio_agrup" )

# AML models
aml.prm2<- h2o.automl(y = "lpreciom2", x = preds, 
                      training_frame = train, 
                      max_models = 25)

# Create the explanation for whole H2OAutoML object
perf.prm2 <- h2o.performance(aml.prm2@leader, newdata = test, xval=TRUE)

saveRDS(list(mds = aml.prm2, perf = perf.prm2), file = 'data/aml_results.rds')

# individual models 
algos <- c("deeplearning", "drf", "glm", "stackedensemble", "xgboost")
ind.models <- vector(mode = 'list', length=length(algos))

for (i in 1:length(algos) ) {
  mm <- h2o.get_best_model(aml.prm2, algo = algos[i])
  ind.models[[i]] <- h2o.saveModel(mm,  path = paste0('data/ind_', algos[i] ), force = TRUE  )
}

saveRDS(ind.models, 'data/h2o_individual.rds')

h2o.shutdown(FALSE)

# h2o information session: ------------------------------
# 
#   R is connected to the H2O cluster: 
#   H2O cluster uptime:         2 seconds 2 milliseconds 
# H2O cluster timezone:       America/Argentina/Buenos_Aires 
# H2O data parsing timezone:  UTC 
# H2O cluster version:        3.44.0.3 
# H2O cluster version age:    2 months and 8 days 
# H2O cluster name:           H2O_started_from_R_nachalca_yed043 
# H2O cluster total nodes:    1 
# H2O cluster total memory:   3.88 GB 
# H2O cluster total cores:    4 
# H2O cluster allowed cores:  4 
# H2O cluster healthy:        TRUE 
# H2O Connection ip:          localhost 
# H2O Connection port:        54321 
# H2O Connection proxy:       NA 
# H2O Internal Security:      FALSE 
# R Version:                  R version 4.3.2 (2023-10-31) 