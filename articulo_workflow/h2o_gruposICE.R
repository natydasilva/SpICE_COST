# Grupos de curvas ICE con restricción espacial

library(ggmap)
library(tidyverse)
library(rlang)
library(RColorBrewer)
library(patchwork)
library(KernSmooth)
library(ClustGeo)

# 1 Datos ICE ------------------
dt.ll <-readRDS("data/h2o_datos.rds")
df <- dt.ll$test[complete.cases(dt.ll$test), ]

ice.lsup <- readRDS("data/ice_lsup.rds")

ICERF_matriz <- ice.lsup$drf |> 
  pivot_wider(names_from = lsup_constru, values_from = yhat) |> 
  select(-yhat.id) |> 
  filter( complete.cases(dt.ll$test) ) |> 
   set_names(nm = paste0('x', 1:30 ) )

grilla <- ice.lsup[[2]]$lsup_constru |> unique()


# ICE derivadas y suavizadas --------------

? locpoly
ICEsuave <- apply( ICERF_matriz, 1,  FUN=function(crv) {
  ll <- locpoly(x=grilla, y=crv, gridsize = length(grilla), bandwidth = diff(grilla)[1] )
   ll$y |> as_tibble() |> t()
}, simplify = FALSE ) |> 
  do.call(rbind, args=_)


ICEderiva <- apply( ICERF_matriz, 1, FUN=function(crv) {
  ll <- locpoly(x=grilla, y=crv, gridsize = length(grilla), bandwidth = diff(grilla)[1], drv=1)
  ll$y |> as_tibble() |> t()
} , simplify = FALSE ) |> 
  do.call(rbind, args=_)

# algunos dibujos para entender
# ICEsuave[1:5, ] |> as_tibble() |>
#   # mutate(lsup_constru = grilla) |>
#   pivot_longer(cols=1:10, names_to = 'yhat.id', values_to = 'yhat.ss') |>
#   mutate(yhat.id = as.numeric(gsub('V', '', yhat.id))) |>
#   inner_join( filter(ice.lsup$drf , yhat.id < 6) ) |>
#   ggplot( ) +
#   geom_point(aes(lsup_constru, yhat)) +
#   geom_line(aes(lsup_constru, yhat.ss)) +
#   facet_wrap(~yhat.id)
  

# ICEderiva |> as_tibble() |>
#   mutate(lsup_constru = grilla) |>
#   pivot_longer(cols=1:10, names_to = 'yhat.id', values_to = 'yhat.drv') |>
#   mutate(yhat.id = as.numeric(gsub('V', '', yhat.id))) |>
#   ggplot( ) +
#   geom_line(aes(lsup_constru, yhat.drv)) +
#   facet_wrap(~yhat.id)


# Distancias y grupos -------------------

# Distancia euclidia en coordenadas
DD1<- df[,c("long","lat")]  |> scale() |>  dist()

# Distancias Sovolev entre ICEs
DD0 <- sqrt( dist(ICEsuave)^2 + dist(ICEderiva)^2 )

saveRDS(DD1, file='data/geoDist.rds') 
saveRDS(DD0, file='data/iceDist.rds') 

DD1 <- readRDS('data/geoDist.rds')
DD0 <- readRDS('data/iceDist.rds')

# Elegir alpha optimo 
# el objeto dist es un vector

attr(DD0, 'Size') # cantidad de puntos
# distancia entre i y j:  do[n*(i-1) - i*(i-1)/2 + j-i]. 

# muestra para obtener alpha optimo
qs = quantile(df$lpreciom2, probs=seq(0,1,.1))
id.smp <- df |> mutate(qq = cut(lpreciom2, breaks = qs, include.lowest = TRUE),
             rr = 1:nrow(df) ) |>
  group_by(qq) |> sample_frac(.3) |> pull(rr)
  
DD1.smp <- df[id.smp, c("long","lat")]  |> scale() |>  dist()
DD0.smp <- sqrt( dist(ICEsuave[id.smp,])^2 + dist(ICEderiva[id.smp, ])^2 )


alpha.info <- NULL
for(kk in 3:5) {
  alp.opt <- choicealpha(D0=DD0.smp, D1=DD1.smp, 
                         range.alpha = seq(0, 1, .1), K = kk, 
                         scale = TRUE, graph = FALSE)
  alpha.info <- rbind(alpha.info ,
                      cbind(k=alp.opt$K, alpha=alp.opt$range.alpha, alp.opt$Q,  alp.opt$Qnorm)
  )
  
}

as_tibble(alpha.info) |> 
  #select(-Q0norm, -Q1norm) |> 
  select(-Q0, -Q1) |> 
  pivot_longer(cols = 3:4) |>  
  ggplot( aes(x=alpha, y=value, color=name) ) + 
  geom_point() + geom_line() + 
  labs(y = 'Cluster homogeneity', color = '') + 
  facet_wrap(~k) + 
  scale_x_continuous(breaks = seq(0,1, .1)) + 
  scale_color_brewer(palette = 'Dark2')
  
ggsave(file='articulo/figuras/fig-alphaoptimo.pdf', height = 7, width = 7)


# hacer grupos 

pr <- data.frame( alps = c(.5, .4, .6), k = 3:5)
grupos.info <- NULL
for (j in 1:3) {
  tt  <- hclustgeo(D0=DD0, D1=DD1, alpha=pr$alp[j] ) 
  gg <- cutree(tt , k=pr$k[j])
  grupos.info <- rbind(grupos.info, 
                       data.frame(alpha=pr$alp[j],k=pr$k[j], grupos=gg ) )
  }

# salvamos las cosas para hacer gr'aficos con grupos
saveRDS(grupos.info, file = "data/grupos_infos.rds")


# Gráficos ------------------------------
pr <- data.frame( alps = c(.5, .4, .6), k = 3:5)
grupos.info <- readRDS("data/grupos_infos.rds")

# agregamos columnas con grupos en df y curvas ice
df <- df |> 
  mutate(g1 = factor(grupos.info$grupos[grupos.info$alpha==.5 & grupos.info$k == 3]), 
         g2 = factor(grupos.info$grupos[grupos.info$alpha==.4 & grupos.info$k == 4]),
         g3 = factor(grupos.info$grupos[grupos.info$alpha==.6 & grupos.info$k == 5])  )   

colnames(df)[15:17] <- paste('grupos', apply( pr, 1, function(x) paste(x, collapse = "_") ), sep="_")

ICERF_matriz$g1 <- factor(grupos.info$grupos[grupos.info$alpha==.5 & grupos.info$k == 3])
ICERF_matriz$g2 <- factor(grupos.info$grupos[grupos.info$alpha==.4 & grupos.info$k == 4])
ICERF_matriz$g3 <- factor(grupos.info$grupos[grupos.info$alpha==.6 & grupos.info$k == 5])

colnames(ICERF_matriz)[31:33] <- paste('grupos', apply( pr, 1, function(x) paste(x, collapse = "_") ), sep="_")


# funciones para dibujar
pl_curvas_fn <- function(gg, gr = 1:6, sz=0.5, aa = 1/100, facetas=TRUE, ICERF_matriz=NULL) { 
  
  fc <- paste("~", as_name(quo( {{gg}} )) ) |> as.formula()
  colores <- palette.colors(palette="Set1")[gr]
  
  # Dibujar las curvas por grupo
  pp <- ICERF_matriz |>
    mutate(yhat.id = 1:nrow(ICERF_matriz) ) |>
    filter( {{gg}} %in% gr ) |>
    pivot_longer(cols=1:30, values_to = 'ice', names_to = 'vv') |>
    inner_join( data.frame(vv=paste0('x',1:30), lsup_constru=grilla ) ) |>
    ggplot( aes(x=lsup_constru, y=ice, group=yhat.id, color={{gg}} )) +
    geom_line(linewidth=sz, alpha=aa ) +
    labs(x='Apartment area (in log)', y='Expected price (in log)') +
    scale_color_manual(values = colores) +
    # theme_bw() + 
    theme(legend.position = 'none')
  
  if (facetas) {
    pp <- pp + facet_wrap(fc) 
  }
  return(pp)
}

pl_mapa_fn <- function(gg, gr=1:6, sz=0.5, facetas=TRUE, zz=13, df=NULL) { 
  # Dibujar los mapas
  
  fc <- paste("~", as_name(quo( {{gg}} )) ) |> as.formula()
  colores <- palette.colors(palette="Set1")[gr]
  montevideo <- c(left = -56.286532, bottom = -34.95, right = -56.004532, top =-34.801112 )
  
  pp <- get_stamenmap(bbox = montevideo, zoom = zz) |> 
    ggmap() + 
    geom_point(data= filter(df, {{gg}} %in% gr ) ,
               aes(x=long, y=lat, color={{gg}}), alpha=0.7,  size=sz)+
    scale_color_manual(values = colores) +
    theme_nothing()
  
  if (facetas) {
    pp <- pp + facet_wrap(fc, nrow = 1) 
  }
  return(pp)
}

# Todos los grupos
# "grupos_0.5_3" "grupos_0.4_4" "grupos_0.6_5"

p1 <- pl_mapa_fn(gg=grupos_0.5_3, gr=1:3, sz=1, df=df, facetas=FALSE)
p2 <- pl_curvas_fn(gg=grupos_0.5_3, gr=1:3, ICERF_matriz=ICERF_matriz)
p2 + p1 + plot_annotation(title = 'k=3, aplpha = 0.5')

ggsave(file='articulo/figuras/fig-3grupos.pdf', height = 7, width = 7)


p1 <- pl_mapa_fn(gg=grupos_0.4_4, gr=1:4, sz=1, df=df, facetas=FALSE)
p2 <- pl_curvas_fn(gg=grupos_0.4_4, gr=1:4, ICERF_matriz=ICERF_matriz, sz=1.5, aa=1/150)
p1 / p2 

ggsave(file='articulo/figuras/fig-4grupos.pdf', height = 7, width = 7)


p1 <- pl_mapa_fn(gg=grupos_0.6_5, gr=1:5, sz=1, df=df, facetas=FALSE)
p2 <- pl_curvas_fn(gg=grupos_0.6_5, gr=1:5, ICERF_matriz=ICERF_matriz)
p2 + p1 + plot_annotation(title = 'k=5, aplpha = 0.6')

ggsave(file='articulo/figuras/fig-5grupos.pdf', height = 7, width = 7)
