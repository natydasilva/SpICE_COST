

# predictive power and variable importance

library(tidyverse)
library(h2o)
ffpath <- 'paper/figures/'

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
# performance de modelo en CV y testing
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

md.perf <- lapply(ind.mods, perf_fn) |> bind_rows(.id = 'model') |> 
  arrange(rMSE)

library(xtable)
options(xtable.comment=FALSE)
xtable(md.perf, caption = 'Predictive performance measures by model',
       label = 'comparo') |> 
  print(file=paste0(ffpath, 'table-performance.tex') , caption.placement='top')

#================================================
# 3.2 Variable importance h2o models --------------------

# variable labels
vvlabs <- c('ammenities', 'elevators', 'bathroom', 'neighborhood', 'condition', 'restrooms',
            'expenses', 'garage', 'ldistance_beach', 'larea_apt')

importance <- lapply(ind.mods, h2o.permutation_importance, newdata=test) |> 
  bind_rows(.id = 'modelo') |> 
  mutate(varname = factor(Variable, labels = vvlabs))

importance |> data.frame() |> 
  select(modelo, varname, Scaled.Importance)  |> 
  ggplot( ) + 
  geom_col(aes(y=Scaled.Importance, x = reorder(varname, Scaled.Importance)), fill='steelblue' ) + 
  coord_flip() + 
  facet_wrap(~modelo) + 
  labs(y = 'Importance (scaled to 1)', x = '', fill='') +
  theme_bw() 

ggsave(file=paste0(ffpath, 'fig-importance.pdf'), height = 7, width = 7)
saveRDS(data.frame(importance) , "data/importance_info.rds")



#================================================
# 3.3 PDP and ALE plots for selected variables  -----------------

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
xx <- readRDS( "data/alepdp_info.rds" )
pdpinfo <- xx$pdp
aleinfo <- xx$ale

# correct var names
aleinfo$varname <- NULL
pdpinfo$varname <- NULL

aleinfo$varname = factor(aleinfo$variable, labels = vvlabs[c(7, 9, 10)] )
pdpinfo$varname = factor(pdpinfo$variable, labels = vvlabs[c(7, 9, 10)] )

list('PDP' = filter(pdpinfo, variable == 'lsup_constru'), 
     'ALE' =  filter(aleinfo, variable == 'lsup_constru')
) |> 
  bind_rows(.id = 'met') |> 
  ggplot() + 
  geom_line( mapping  = aes(x=x, y=f, color=met) ) + 
  # facet_wrap(~modelo, ncol = 5) + scale_color_brewer(palette = 'Dark2') + 
  facet_wrap(~modelo) + scale_color_brewer(palette = 'Dark2') + 
  labs(x='Apartment area (in log)', y='Expected price (in log)', color='Method') + 
  geom_rug(data=as.data.frame(test), aes(x=lsup_constru),alpha=1/50,inherit.aes = F)  + 
  theme(aspect.ratio = 1, legend.position = c(.8, .2))

ggsave(paste0(ffpath,'fig-efecto-lsub.pdf'), height = 7, width = 7)

#================================================
# 3.4 ICE curves for lsup_constru
# (warning: yhat_fn might need to rename x.model argument)
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

# ICE curves figures
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

ggsave(file=paste0(ffpath,'fig-ice-sup-dec.pdf'), height = 7, width = 7)  

ice.lsup$drf |> 
  filter( yhat.id %in% zz$yhat.id) |> 
  ggplot(aes(x=lsup_constru, y=yhat, group=yhat.id) ) +
  geom_line( alpha=1/50 , color = 'steelblue') +  
  labs(x='Apartment area (in log)', y='Expected price (in log)') +
  theme_bw() + 
  theme(axis.text.y = element_text(size = I(10)), 
        axis.text.x = element_text(size = I(10)),
        axis.title.x = element_text(size = I(20)),
        axis.title.y = element_text(size = I(20))
  )

ggsave(file=paste0(ffpath,'fig-ice-sup-decRF.pdf'), height = 7, width = 7)  


# plot 3 ICE curves to ilustrated method

ice.lsup$drf |> 
  #filter( yhat.id %in% zz ) |> 
  filter( yhat.id %in% c(8204,5283,19632) ) |> 
  ggplot(aes(x=lsup_constru, y=yhat) ) +
  #geom_line(aes(group=yhat.id, color = as.factor(yhat.id))) +  
  geom_line(aes(group=yhat.id, linetype=as.factor(yhat.id)) ) +  
  #  geom_point(data = yy, aes(lsup_constru, lpreciom2)) + 
  labs(x='Apartment area (in log)', y='Expected price (in log)') +
  theme_bw() + 
  scale_linetype_manual(values = c(3,1,2)) + 
  theme(axis.title.y = element_text(size = I(15)), 
        axis.title.x = element_text(size = I(15)), 
        legend.position = 'none')

ggsave(file=paste0(ffpath, 'fig-3ice-exmp.pdf'), height = 7, width = 7)  


h2o.shutdown(FALSE)




