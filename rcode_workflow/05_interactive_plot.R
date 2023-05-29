# Interactive plot for spice curves

library(tidyverse)
dt.ll <-readRDS("data/mds_datos.rds")
grupos.info <- readRDS("data/grupos_infos.rds")


df <- dt.ll$test[complete.cases(dt.ll$test), ] |> 
  mutate(gr = factor(grupos.info$grupos[grupos.info$alpha==.5 & grupos.info$k == 4]), 
         rr.id  = 1:nrwon)   

ice.lsup <- readRDS("data/ice_lsup.rds")

ICERF_matriz <- ice.lsup$drf |> 
  pivot_wider(names_from = lsup_constru, values_from = yhat) |> 
  select(-yhat.id) |> 
  filter( complete.cases(dt.ll$test) ) |> 
  set_names(nm = paste0('x', 1:30 ) )

ICERF_matriz$gr <- factor(grupos.info$grupos[grupos.info$alpha==.5 & grupos.info$k == 4])
grilla <- ice.lsup[[2]]$lsup_constru |> unique()

