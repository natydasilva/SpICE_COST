# ICE clusters with geographical constrains
library(tidyverse)
library(patchwork)
library(ggmap)

library(SpICE)
ffpath <- 'paper/figures/'

# 1 load datasets ------------------
dt.ll  <-readRDS("data/h2o_datos.rds")
Dcoord <- dt.ll$test[complete.cases(dt.ll$test), ]

ice.lsup <- readRDS("data/ice_lsup.rds")
Dcurve <- ice.lsup$drf

# sample to test if everithing works
# lim <- 1000
# Dcurve   <- filter(ice.lsup$drf, yhat.id <= lim)
# Dcoord <- Dcoord[1:lim, ] 
#### #### #### ####

# 2 Obtain aplpha value and groups  --------------------------

# choose alpha value
alp.info <- plot_alpha(DD1=Dcoord, DD0=Dcurve, resp = Dcoord$lpreciom2, 
                       yhat = "yhat.id", grid = "lsup_constru", icevalue = "yhat" ) 

alp.info$alpha_plot  # alpha=0.5, 4 groups
ggsave(file= paste0(ffpath, 'fig-alphaoptimo.pdf'), height = 7, width = 7)

# ice groups with Sobolev distance
ice.groups <- cl_sobcurve(DD1=Dcoord, DD0=Dcurve, alp = 0.5, 
                          yhat = "yhat.id", grid = "lsup_constru", icevalue = "yhat")
gg <- cutree(ice.groups , k=4)

Dcoord$gr.id <- factor(gg)
Dcurve <- data.frame(yhat.id = unique(Dcurve$yhat.id), gr.id = factor(gg) ) |> 
  inner_join(Dcurve) 
#### #### #### ####

# 3 Plots: create map and curve plots -----------------

# need stadia map key: see help(register_stadiamaps)
mapakey<- 'get-your-key' 
register_stadiamaps(mapakey)
montevideo <- c(left = -56.286532, bottom = -34.95, right = -56.004532, top =-34.801112 )

# map with points colored by group.id
p1 <- plot_clcoord(data = Dcoord, gg = gr.id, gr = 1:4, aa=1,
                   long =  long, lat = lat, region = montevideo, fct = FALSE)

# ice curves by group
p2 <- plot_clcurve(data = Dcurve, icevalue = yhat, gg = gr.id, yhat = yhat.id, 
                   gr = 1:4, xvalue = lsup_constru, aa =1/20 )

p1/p2
ggsave(file= paste0(ffpath,'fig-4grupos-alpha5.pdf'), height = 7, width = 7)             

#### #### #### ####