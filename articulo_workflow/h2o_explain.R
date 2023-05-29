
# Generamos dibujos para interpretar resultados del autoML


# Modelos usando h2o 
library(tidyverse)
library(h2o)

h2o.init( )
h2o.no_progress()


dts <- readRDS('data/h2o_datos.rds')
train <- as.h2o(dts$train)
test <- as.h2o(dts$test)

# resultados autoML
aml <- readRDS(file='data/h2o_aml_preciom2.rds')
aml$mds

# modelos individuales
indi.path <- readRDS(file = 'data/h2o_individual.rds')
ind.mods <- lapply( indi.path, h2o.loadModel )
names(ind.mods) <- c("deeplearning", "drf", "glm", "stackedensemble", "xgboost")

# Performance y variable importancia ---------------------

# performance de modelo en CV y testing
mape_fun <- function(ypred, ytrue) 100*mean( abs(ypred-ytrue)/ytrue) 

h2o.performance(ind.mods$drf, newdata = test)

perf_fn <- function(mm) {
  zz <- as.data.frame(test)
  zz$yhat <- h2o.predict(mm, newdata = test) |> as.vector()
  zz |> summarise(
    rMSE = mean( (yhat-lpreciom2)^2 ) |> sqrt(), 
    R2   =1 -  mean( (yhat-lpreciom2)^2 ) / var(lpreciom2), 
    MAEo = abs(exp(yhat)-exp(lpreciom2)) |> mean(),
    MAPEo = 100*(abs(exp(yhat)-exp(lpreciom2))/exp(lpreciom2)) |> mean()
  )
}

md.perf <- lapply(ind.mods, perf_fn) |> bind_rows(.id = 'modelo') |> 
  arrange(rMSE)

library(xtable)
options(xtable.comment=FALSE)
xtable(md.perf, caption = 'Predictive performance measures by model',
       label = 'comparo') |> 
  print(file='articulo/figuras/table-performance.tex', caption.placement='top')


importance <- lapply(ind.mods, h2o.permutation_importance, newdata=test) |> 
  bind_rows(.id = 'modelo')


importance |> data.frame() |> 
  select(modelo, Variable, Scaled.Importance)  |> 
  ggplot( ) + 
  geom_col(aes(y=Scaled.Importance, x = reorder(Variable, Scaled.Importance)), fill='steelblue' ) + 
  coord_flip() + 
  facet_grid(~modelo) + 
  labs(y = 'Importance (scaled to 1)', x = '', fill='') +
  scale_fill_brewer(palette = 'Set1') + 
  theme_bw() + theme(aspect.ratio = 2)

ggsave(file='articulo/figuras/fig-importance.pdf', height = 7, width = 7)




# PDP y ALE ------------------------
library(pdp)
library(ALEPlot) 
 
# predicciones con los modelos individuales
yhat_fn <- function(X.model, newdata) { 
  dd <- as.h2o(newdata)
  h2o.predict(X.model, dd) |> as.matrix() |> as.numeric()
  }
yhat_fn(ind.mods$drf, test[1:10,])

aleplot_fn <- function(mm, j) {
  x<-ALEPlot(X = as.data.frame(test), X.model = mm, pred.fun = yhat_fn, J=j)
  data.frame(x=x$x.values, f=x$f.values)
}

pdpplot_fn <- function(mm, j){
  x <- PDPlot(X = as.data.frame(test), X.model = mm, pred.fun = yhat_fn, J=j, K=39)
  data.frame(x=x$x.values, f=x$f.values)
}

# Efectos de superficie construida, distancia a la playa, ammenities
jjs <- c(3,4,10)

ll <- vector(mode = 'list', length = length(jjs))
for (i in 1:length(jjs)) {
  ll[[i]] <- lapply(ind.mods, aleplot_fn, j=jjs[i]) %>% bind_rows(.id = 'modelo')
}
names(ll) <- colnames(test)[jjs]
aleinfo <- bind_rows(ll, .id = 'variable')

ll <- vector(mode = 'list', length = length(jjs))
for (i in 1:length(jjs)) {
  ll[[i]] <- lapply(ind.mods, pdpplot_fn, j=jjs[i]) %>% bind_rows(.id = 'modelo')
}
names(ll) <- colnames(test)[jjs]
pdpinfo <- bind_rows(ll, .id = 'variable')

saveRDS( list(pdp=pdpinfo, ale=aleinfo), "data/alepdp_info.rds")


# Figuras ALE y PDP -------------
xx <- readRDS( "data/alepdp_info.rds" )
pdpinfo <- xx$pdp
aleinfo <- xx$ale

# los pdp
pdpinfo |> 
  filter(variable != 'expensas') |> 
  ggplot() + 
  geom_line( mapping  = aes(x=x, y=f, color=modelo) ) + 
  facet_wrap(~variable, ncol = 3, scales = 'free') + 
  scale_color_brewer(palette = 'Set1') + 
  labs(x='', y='Expected price (in log)', color='', title ='PDP for selected variables') + 
  theme_bw() + 
  theme(aspect.ratio = 1)

ggsave('articulo/figuras/fig-lospdp.pdf', height = 7, width = 7)

aleinfo |> 
  filter(variable != 'expensas') |> 
  ggplot() + 
  geom_line( mapping  = aes(x=x, y=f, color=modelo) ) + 
  facet_wrap(~variable, ncol = 3, scales = 'free') + 
  scale_color_brewer(palette = 'Set1') + 
  labs(x='', y='Expected price (in log)', color='', title = 'ALE for selected variables') + 
  theme_bw() + 
  theme(aspect.ratio = 1)

ggsave('articulo/figuras/fig-losale.pdf', height = 7, width = 7)

# figura superficie construida
list('PDP' = filter(pdpinfo, variable == 'lsup_constru'), 
       'ALE' =  filter(aleinfo, variable == 'lsup_constru')
  ) |> 
    bind_rows(.id = 'met') |> 
    ggplot() + 
    geom_line( mapping  = aes(x=x, y=f, color=met) ) + 
    facet_wrap(~modelo, ncol = 5) + scale_color_brewer(palette = 'Dark2') + 
    labs(x='Apartment area (in log)', y='Expected price (in log)', color='Method') + 
    geom_rug(data=as.data.frame(test), aes(x=lsup_constru),alpha=1/50,inherit.aes = F)  + 
    theme(aspect.ratio = 1)

ggsave('articulo/figuras/fig-efecto-lsub.pdf', height = 7, width = 7)

# figura playa
list('PDP' = filter(pdpinfo, variable == 'ldistancia_playa'), 
     'ALE' =  filter(aleinfo, variable == 'ldistancia_playa')
) |> 
  bind_rows(.id = 'met') |> 
  ggplot() + 
  geom_line( mapping  = aes(x=x, y=f, color=met) ) + 
  facet_wrap(~modelo, ncol = 5) + scale_color_brewer(palette = 'Dark2') + 
  labs(x='Distance to the beach (in log)', y='Expected price (in log)', color='Method') + 
  geom_rug(data=as.data.frame(test), aes(x=ldistancia_playa),alpha=1/50,inherit.aes = F)  + 
  theme(aspect.ratio = 1)

ggsave('articulo/figuras/fig-efecto-lplaya.pdf', height = 7, width = 7)





# Curvas ICE para todo el conjunto test  -------------------------


# (ojo: capaz que yhat_fn necesita cambiar el nombre del argumento x.model)
yhat_fn <- function(object, newdata) { 
  dd <- as.h2o(newdata)
  h2o.predict(object, dd) |> as.matrix() |> as.numeric()
}


ice.lsup <- vector(mode='list', length=5)
  for (i in 1:5) {
    m <- names(ind.mods)[i]
    
    ice.lsup[[i]] <- pdp::partial(object = ind.mods[[m]], pred.var= 'lsup_constru',
                                  train = as.data.frame(test),
                                  grid.resolution = 30, pred.fun=yhat_fn, ice = TRUE)
    
  }
  names(ice.lsup) <- names(ind.mods)
  saveRDS(ice.lsup, "data/ice_lsup.rds")

  ice.lplaya <- vector(mode='list', length=5)  
  for (i in 1:5) {
    m <- names(ind.mods)[i]
    ice.lplaya[[i]] <- pdp::partial(object = ind.mods[[m]], pred.var= 'ldistancia_playa',
                                  train = as.data.frame(test), grid.resolution = 10, 
                                  pred.fun=yhat_fn, ice = TRUE)
    
  }
  names(ice.lplaya) <- names(ind.mods)
  saveRDS(ice.lplaya, "data/ice_lplaya.rds")
  

  ice.expe <- vector(mode='list', length=5)  
  for (i in 1:5) {
    m <- names(ind.mods)[i]
    ice.expe[[i]] <- pdp::partial(object = ind.mods[[m]], pred.var= 'expensas',
                                    train = as.data.frame(test), grid.resolution = 10, 
                                    pred.fun=yhat_fn, ice = TRUE)
    
  }
  names(ice.expe) <- names(ind.mods)
  saveRDS(ice.expe, "data/ice_expe.rds")
  


# Figuras de las curvas ICE  

ice.lsup <- readRDS("data/ice_lsup.rds")
test <- dts$test  

zz <- test |> as.data.frame() |>
  mutate(yhat.id = 1:nrow(test), 
         estrato = cut(lpreciom2, quantile(lpreciom2, probs=seq(0,1, .1) ), include.lowest=TRUE ) 
         ) |> 
  select( yhat.id, estrato, lpreciom2 ) |> 
  group_by(estrato) |> 
  sample_n( size = 500)

ice.lsup |> bind_rows(.id = 'modelo') |>
  filter(modelo != 'glm', yhat.id %in% zz$yhat.id) |> 
  ggplot(aes(x=lsup_constru, y=yhat, group=yhat.id) ) +
  geom_line( alpha=1/100 , color = 'steelblue') + 
  labs(x='Apartment area (in log)', y='Expected price (in log)') +
  facet_wrap( ~modelo) + 
  theme_bw()

ggsave(file='articulo/figuras/fig-ice-sup-dec.pdf', height = 7, width = 7)  
  
ice.lsup$drf |> 
  filter( yhat.id %in% zz$yhat.id) |> 
  ggplot(aes(x=lsup_constru, y=yhat, group=yhat.id) ) +
  geom_line( alpha=1/50 , color = 'steelblue') +  
  labs(x='Apartment area (in log)', y='Expected price (in log)') +
  theme_bw()


# ww <- list('PDP' = filter(pdpinfo, variable == 'lsup_constru', modelo == 'drf'), 
#      'ALE' =  filter(aleinfo, variable == 'lsup_constru', modelo == 'drf')
# ) |> 
#   bind_rows(.id = 'met') |> 
#   mutate(f2 = f + mean(dts$train$lpreciom2) )
# 
# ggplot() + 
#   geom_line( data=filter(ice.lsup$drf, yhat.id %in% zz$yhat.id),
#              mapping = aes(x=lsup_constru, y=yhat, group=yhat.id), alpha=1/50 , color = 'steelblue' ) +
#   geom_line( data = ww, 
#              mapping = aes(x=x, y=f2, color=met) ) + 
#   labs(x='Apartment area (in log)', y='Expected price (in log)') +
#   scale_color_brewer(palette = 'Dark2') +
#   theme_bw()

ggsave(file='articulo/figuras/fig-ice-sup-decRF.pdf', height = 7, width = 7)  

# ice.lsup$drf  |> 
#   ggplot(aes(x=lsup_constru, y=yhat, group=yhat.id)) +
#   geom_line( alpha=1/250 ) 
# 
# ice.lsup |> bind_rows(.id = 'modelo') |> 
#   filter(modelo != 'glm') |> 
#   ggplot(aes(x=lsup_constru, y=yhat, group=yhat.id)) +
#   geom_line( alpha=1/500, size=.5 ) + 
#   facet_wrap( ~modelo, scale='free') + 
#   theme_bw()
# 
# ggsave(file='articulo/figuras/fig-ice-sup.pdf', height = 7, width = 7)


# Usar ICEbox package  ----------------------------------------------------
#install.packages('ICEbox')
# library(ICEbox)

# curvas ICE centradas, derivadas de ICE
# yhat_fn <- function(object, newdata) { 
#   dd <- as.h2o(newdata)
#   h2o.predict(object, dd) |> as.matrix() |> as.numeric()
# }
# yhat_fn(ind.mods$drf, test[1:10,])
# 
# ice.rf <- ice(ind.mods$drf, X= as.data.frame(test), predictor='lsup_constru', 
#     predictfcn = yhat_fn, y = as.numeric(as.vector(test['lpreciom2'])), 
#     frac_to_build = .1, num_grid_pts = 10)
# 
# plot(ice.rf, centered=TRUE)
# 
# derivadas <- dice(ice.rf)
# 
# plot(derivadas)


h2o.shutdown(FALSE)


