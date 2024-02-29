library(tidyverse)
library(RColorBrewer)
library(KernSmooth)
library(cluster)

# recreate part of Hichcock (2007), clustering studuy

# 1) Functions  ---------------
# 4 mean functions, t in [0, 5]
mu1_fn <- function(t) { -sin(t-1)*log(t + .5) }
mu2_fn <- function(t) { cos(t)*log(t + .5) }
mu3_fn <- function(t) { -.25 - .1*cos(.5*(t-1) )*t^(1.5) * sqrt(5*t^(.5) + .5) }
mu4_fn <- function(t) { .6*cos(t)*log(t + .5)*sqrt(t+.5) }

# function to simulate observed and smoothed data
data_fn <- function(N=40, n=50, GG, bb = diff(GG)[1]) {
  
  # true mean functions
  mu.data <- rbind(mu1 = mu1_fn(GG), mu2 = mu2_fn(GG), mu3 = mu3_fn(GG), mu4 = mu4_fn(GG) )
  
  # obsserved data
  obs.data = matrix(NA, nrow=N, ncol = n)
  rr <- rep(1:4, each=10)
  for (i in 1:N) {
    # obs.data[i, ] <- mu.data[ rr[i], ] + rnorm(n)
    obs.data[i, ] <- mu.data[ rr[i], ] + rnorm(1, sd=.5) + rnorm(n, sd = .1)
  }
  
  # smoothed data
  sms.data <- apply(obs.data, 1,  FUN=function(crv) {
    ll <- locpoly(x=GG, y=crv, gridsize = length(GG), bandwidth =  bb)
    ll$y |> as_tibble() |> t()
  }, simplify = FALSE ) |> 
    do.call(rbind, args=_)
  
  return(list(obs = obs.data, sms = sms.data, grs = rr))
}

# function to compute derivatives 
deriva_fn <- function(DD, GG, bb = diff(GG)[1]) {
  apply(DD, 1, FUN=function(crv) {
    ll <- locpoly(x=GG, y=crv, gridsize = length(GG), bandwidth = bb, drv=1)
    ll$y |> as_tibble() |> t()
  } , simplify = FALSE ) |> 
    do.call(rbind, args=_)
}

# function to compute prop of correctly pairs matching
valclus_fn <- function(DS, gr) {
  
  gr.hat <- pam(DS, k=4, cluster.only=TRUE)
  
  # Pairs in same cluster
  true.pairs <- outer(gr, gr, FUN="-") == 0
  tp <- true.pairs[lower.tri(true.pairs)] 
  
  # Pairs clasified in same cluster
  class.pairs <- outer(gr.hat, gr.hat, FUN="-") == 0
  cp <- class.pairs[lower.tri(class.pairs)]
  
  sum(cp[tp==1])/sum(tp)
}



# Simulations:  ----------------------------------
# y = mu + eps
# N = 40, 10 each group
# n = 50 points from each curve, in [0,5]
# eps ~ random effect structure 
# reps = 500
N <- 40
n <- 50
R <- 500
b = .25 # smoothing parameter
grilla <- seq(0, 5, length.out = n)

set.seed(888)

res <- vector(length = R, mode = 'list')
for (r in 1:R) {
  dts <- data_fn(GG=grilla, bb = b)
  obs.drv <- deriva_fn(DD=dts$obs, GG=grilla)
  sms.drv <- deriva_fn(DD=dts$sms, GG=grilla, bb = b)
  
  d1 <- dist(dts$sms)
  d2 <- sqrt( dist(dts$sms)^2 + dist(sms.drv)^2 )
  ww <- mean(d1)/mean(d2)
  
  res[[r]] <- c(
    rep = r, 
    # obs.L2 = dist(dts$obs) |> valclus_fn(gr = dts$grs),
    L2 = dist(dts$sms) |> valclus_fn(gr = dts$grs),
    # obs.Sob = sqrt( dist(dts$obs)^2 + dist(obs.drv)^2 ) |> valclus_fn(gr = dts$grs), 
    Sob = sqrt( dist(dts$sms)^2 + dist(sms.drv)^2 ) |> valclus_fn(gr = dts$grs)
  )
}

# Get results and save
dts <- data_fn(GG=grilla, bb = b)
dts$res <- bind_rows(res) 
saveRDS(dts, 'data/simstudySobolev.rds')

#=============================================================
#  Figures from sim study   ----------------------------------
# library(tidyverse)
# library(RColorBrewer)
# 
# dts <- readRDS( 'data/simstudySobolev.rds') 
# mu1_fn <- function(t) { -sin(t-1)*log(t + .5) }
# mu2_fn <- function(t) { cos(t)*log(t + .5) }
# mu3_fn <- function(t) { -.25 - .1*cos(.5*(t-1) )*t^(1.5) * sqrt(5*t^(.5) + .5) }
# mu4_fn <- function(t) { .6*cos(t)*log(t + .5)*sqrt(t+.5) }

colores <- palette.colors(n = 4, palette = "Okabe-Ito")
names(colores) <- paste0("mu", 1:4)
my.labs <- list(bquote(mu[1]),bquote(mu[2]),bquote(mu[3]), bquote(mu[4]))

# true functions figure
ggplot() + xlim(0, 5) +
  geom_function(fun=mu1_fn, aes(color = names(colores)[1]) ) +
  geom_function(fun=mu2_fn, aes(color = names(colores)[2]) ) +
  geom_function(fun=mu3_fn, aes(color = names(colores)[3]) )+
  geom_function(fun=mu4_fn, aes(color = names(colores)[4]) ) +
  scale_color_brewer(palette = 'Dark2', name='', labels=my.labs) +
  #scale_color_manual(values = colores, name='', labels=my.labs) +
  theme_bw() + theme(aspect.ratio = 1/2)

ggsave( filename = 'paper/figures/fig-simstudy1.pdf', height = 7, width = 7)


# one dataset figure
grilla <- seq(0, 5, length.out = 50)

dts$obs |>
  as_tibble() |>
  mutate(id = 1:40, gr=dts$grs) |>
  pivot_longer( cols = starts_with('V')  ) |>
  mutate(pp = as.numeric( gsub('V','', name) ) ) |>
  arrange(id, pp) |>
  mutate( grilla = rep(grilla, 40) ) |>
  mutate( grF = factor( gr, labels=names(colores) ) ) |>
  ggplot() +
  geom_line( aes(x=grilla, y=value, color=grF, group=id)) +
  #scale_color_manual(values = colores, name='') +
  scale_color_brewer(palette = "Dark2", name='') +
  #facet_wrap(~gr) +
  theme_bw() + theme(aspect.ratio = 1/2)

ggsave( filename = 'paper/figures/fig-simstudy2.pdf', height = 7, width = 7)

library(xtable)
options(xtable.comment = FALSE)

# results table
dts$res |> 
  mutate(Ratio = Sob/L2) |> 
  select(-rep) |> 
  psych::describe(skew = FALSE, ranges = FALSE, quant = c(.05, .95)) |> 
  data.frame() |> 
  select( -c(vars,n,se) ) |> 
  xtable(digits = 2, caption = 'Summary of results', label = 'sim-results') |> 
  print(file='paper/figures/tab-simstudy-res.tex',  caption.placement = 'top')


#=============================================================
