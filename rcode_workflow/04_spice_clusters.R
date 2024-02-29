# ICE clusters with geographical constrains
library(ggmap)
library(tidyverse)
library(rlang)
library(RColorBrewer)
library(patchwork)
library(KernSmooth)
library(ClustGeo)
library(sf)
ffpath <- 'articulo_revision/figures/'

# 1 load datasets ------------------
dt.ll <-readRDS("data/mds_datos.rds")
df <- dt.ll$test[complete.cases(dt.ll$test), ]

ice.lsup <- readRDS("data/ice_lsup.rds")


ICERF_matriz <- ice.lsup$drf |> 
  pivot_wider(names_from = lsup_constru, values_from = yhat) |> 
  select(-yhat.id) |> 
  filter( complete.cases(dt.ll$test) ) |> 
  set_names(nm = paste0('x', 1:30 ) )

grilla <- ice.lsup[[2]]$lsup_constru |> unique()


# ICE: derivatives and smoothing --------------
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

# Distance matrices and clusters  -------------------

DD1 <- df[,c("long","lat")] |> 
  st_as_sf( coords = c("lat", "long"), crs = 4326) |> 
  st_distance()

# Distancias Sovolev entre ICEs
DD0 <- sqrt( dist(ICEsuave)^2 + dist(ICEderiva)^2 )

saveRDS(DD1, file='data/geoDist.rds') 
saveRDS(DD0, file='data/iceDist.rds') 

DD1 <- readRDS('data/geoDist.rds')
DD0 <- readRDS('data/iceDist.rds')


# choosing alpha: use a random sample 
qs = quantile(df$lpreciom2, probs=seq(0,1,.1))
set.seed(8)
id.smp <- df |> mutate(qq = cut(lpreciom2, breaks = qs, include.lowest = TRUE),
                       rr = 1:nrow(df) ) |>
  group_by(qq) |> sample_frac(.2) |> pull(rr)


DD1.smp <- df[id.smp, c("long","lat")] |> 
  st_as_sf( coords = c("lat", "long"), crs = 4326) |> 
  st_distance() |> 
  as.dist()

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
saveRDS(alpha.info, file='data/alpha_info.rds') 


alpha.info <- readRDS('data/alpha_info.rds')
as_tibble(alpha.info) |> filter(k %in% 3:5) |> 
  select(-Q0norm, -Q1norm) |> 
  #select(-Q0, -Q1) |> 
  pivot_longer(cols = 3:4) |>  
  ggplot( aes(x=alpha, y=value, color=name) ) + 
  geom_point() + geom_line() + 
  labs(y = 'Cluster homogeneity', color = '') + 
  facet_wrap(~k) + 
  scale_color_brewer(palette = 'Dark2') + 
  theme_bw() + theme(aspect.ratio = 1)

ggsave(file= paste0(ffpath, 'fig-alphaoptimo.pdf'), height = 7, width = 7)




# Cluster options
pr <- data.frame( alps = .5, k = 3:5 )
grupos.info <- NULL
for (j in 1:3) {
  tt  <- hclustgeo(D0=DD0, D1=DD1, alpha=pr$alp[j] ) 
  gg <- cutree(tt , k=pr$k[j])
  grupos.info <- rbind(grupos.info, 
                       data.frame(alpha=pr$alp[j],k=pr$k[j], grupos=gg ) )
}
saveRDS(grupos.info, file = "data/grupos_infos.rds")

# Prepare data for ploting ------------------------------
pr <- data.frame( alps = .5, k = 3:5 )
grupos.info <- readRDS("data/grupos_infos.rds")
#grupos.info <-readRDS("../SpICE_COST/data/grupos_infos.rds")

grupos.info$gr.nm <- apply(grupos.info, 1, function(x) paste0('gr', x[2], '_al', 100*x[1]) )
gr.nm <- unique(grupos.info$gr.nm)

nn <- ncol(df)
for (g in 1:10) {
  df[ ,nn+g] <-factor(grupos.info$grupos[grupos.info$gr.nm == gr.nm[g]])
}
colnames(df)[15:24] <- gr.nm

nn <- ncol(ICERF_matriz)
for (g in 1:10) {
  ICERF_matriz[ ,nn+g] <-factor(grupos.info$grupos[grupos.info$gr.nm == gr.nm[g]])
}
colnames(ICERF_matriz)[31:40] <- gr.nm


# ploting functions --------------
pl_curvas_fn <- function(gg, gr = 1:6, sz=0.5, aa = 1/100, 
                         facetas=TRUE, ICERF_matriz=NULL, colores=NULL) { 
  
  fc <- paste("~", as_name(quo( {{gg}} )) ) |> as.formula()
  
  if (is.null(colores)) {
    colores <- palette.colors(palette="Set1")[gr]
  }
  
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

pl_mapa_fn <- function(gg, gr=1:6, sz=0.5, facetas=TRUE, zz=13, df=NULL, colores=NULL) { 
  # Dibujar los mapas
  
  fc <- paste("~", as_name(quo( {{gg}} )) ) |> as.formula()
  if(is.null(colores)) {
    colores <- palette.colors(palette="Set1")[gr]
  }
  montevideo <- c(left = -56.286532, bottom = -34.95, right = -56.004532, top =-34.82 )
  
  pp <- get_stadiamap(bbox = montevideo, zoom = zz) |> 
    ggmap() + 
    geom_point(data= filter(df, {{gg}} %in% gr ) ,
               aes(x=long, y=lat, color={{gg}}), alpha=0.7,  size=sz)+
    scale_color_manual(values = colores) +
    theme_nothing()
  
  if (facetas) {
    pp <- pp + facet_wrap(fc) 
  }
  return(pp)
}

# Create figures -------------------------

# need stadia map key: see help(register_stadiamaps)
mapakey<- 'get-your-key' 
register_stadiamaps(mapakey)

# 4 clusters with alpha = 0.5
p1 <- pl_mapa_fn(gg=grupos_0.5_4, gr=1:4, sz=.5, df=df, facetas=FALSE)
p2 <- pl_curvas_fn(gg=grupos_0.5_4, gr=1:4, ICERF_matriz=ICERF_matriz, sz=.5, aa=1/150)
p1 / p2 

ggsave(file='paper/figures/fig-4grupos-alpha5.pdf', height = 7, width = 7)
