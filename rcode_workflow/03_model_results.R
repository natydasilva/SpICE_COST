

# predictive power and variable importance

library(tidyverse)
library(h2o)

h2o.init( )
h2o.no_progress()

dts <- readRDS('data/mds_datos.rds')
test <- as.h2o(dts$test)

#================================================
# 3.1 Performance measure of h2o models --------------------
indi.path <- readRDS(file = 'data/h2o_individual.rds')
ind.mods <- lapply( indi.path, h2o.loadModel )
names(ind.mods) <- c("deeplearning", "drf", "glm", "stackedensemble", "xgboost")

# Model performance in test data
mape_fun <- function(ypred, ytrue) 100*mean( abs(ypred-ytrue)/ytrue) 
# h2o.performance(ind.mods$drf, newdata = test)

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

# Performance table 
library(xtable)
options(xtable.comment=FALSE)
xtable(md.perf, caption = 'Predictive performance measures by model',
       label = 'comparo') |> 
  print(file='paper/figures/table-performance.tex', caption.placement='top')
#================================================
# 3.2 Variable importance h2o models --------------------
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

ggsave(file='paper/figures/fig-importance.pdf', height = 7, width = 7)
#================================================
# 3.3 PDP and ALE plots for selected variables  -----------------

library(pdp)
library(ALEPlot) 

yhat_fn <- function(X.model, newdata) { 
  dd <- as.h2o(newdata)
  h2o.predict(X.model, dd) |> as.matrix() |> as.numeric()
}
# yhat_fn(ind.mods$drf, test[1:10,])

aleplot_fn <- function(mm, j) {
  x<-ALEPlot(X = as.data.frame(test), X.model = mm, pred.fun = yhat_fn, J=j)
  data.frame(x=x$x.values, f=x$f.values)
}

pdpplot_fn <- function(mm, j){
  x <- PDPlot(X = as.data.frame(test), X.model = mm, pred.fun = yhat_fn, J=j, K=39)
  data.frame(x=x$x.values, f=x$f.values)
}

# Profile effects for: "lsup_constru", "ldistancia_playa" and "expensas" 
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


# Figure: lsup profile effect with ALE and PDP
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
ggsave('paper/figures/fig-efecto-lsub.pdf', height = 7, width = 7)

# Other possible figures: 
# los pdp
# pdpinfo |> 
#   filter(variable != 'expensas') |> 
#   ggplot() + 
#   geom_line( mapping  = aes(x=x, y=f, color=modelo) ) + 
#   facet_wrap(~variable, ncol = 3, scales = 'free') + 
#   scale_color_brewer(palette = 'Set1') + 
#   labs(x='', y='Expected price (in log)', color='', title ='PDP for selected variables') + 
#   theme_bw() + 
#   theme(aspect.ratio = 1)
# 
# aleinfo |> 
#   filter(variable != 'expensas') |> 
#   ggplot() + 
#   geom_line( mapping  = aes(x=x, y=f, color=modelo) ) + 
#   facet_wrap(~variable, ncol = 3, scales = 'free') + 
#   scale_color_brewer(palette = 'Set1') + 
#   labs(x='', y='Expected price (in log)', color='', title = 'ALE for selected variables') + 
#   theme_bw() + 
#   theme(aspect.ratio = 1)
# 
# # figura playa
# list('PDP' = filter(pdpinfo, variable == 'ldistancia_playa'), 
#      'ALE' =  filter(aleinfo, variable == 'ldistancia_playa')
# ) |> 
#   bind_rows(.id = 'met') |> 
#   ggplot() + 
#   geom_line( mapping  = aes(x=x, y=f, color=met) ) + 
#   facet_wrap(~modelo, ncol = 5) + scale_color_brewer(palette = 'Dark2') + 
#   labs(x='Distance to the beach (in log)', y='Expected price (in log)', color='Method') + 
#   geom_rug(data=as.data.frame(test), aes(x=ldistancia_playa),alpha=1/50,inherit.aes = F)  + 
#   theme(aspect.ratio = 1)

#================================================
# 3.4 ICE curves for lsup_constru

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

# Figuras de las curvas ICE  

# test <- dts$test  

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
ggsave(file='paper/figures/fig-ice-sup-dec.pdf', height = 7, width = 7)  

ice.lsup$drf |> 
  filter( yhat.id %in% zz$yhat.id) |> 
  ggplot(aes(x=lsup_constru, y=yhat, group=yhat.id) ) +
  geom_line( alpha=1/50 , color = 'steelblue') +  
  labs(x='Apartment area (in log)', y='Expected price (in log)') +
  theme_bw()
ggsave(file='paper/figures/fig-ice-sup-decRF.pdf', height = 7, width = 7)  

h2o.shutdown(FALSE)




