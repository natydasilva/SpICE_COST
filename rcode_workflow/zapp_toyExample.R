
# Toy example code
S <- 1000
x1<-runif(S,-1,1) 
x2<-runif(S,0,2)
epsilon<-rnorm(S,0,0.2)

hatf<-function(x){(2*x -x^2)^2 }

iin<-integrate(hatf,0,2)
C=sqrt(iin$value/8)
C=-C
Y<- -x1*(1-x1)+ (1-x1^2)*x2+ x2^(1+(x1>0)*x1) 
base<-data.frame(Y,x1,x2)
hatf <- function(x1,x2){-C*x1*(1-x1)+ (1/2)*(2-x1-x1^2)*x2+ x2^(1+(x1>0)*x1)}


xx1<-c(-1,0,1)
funcionesIce<-list()
for(jj in 1:length(xx1)){
  funcionesIce[[jj]]<-sapply(x2,hatf,x1=xx1[jj])
}

ices<-data.frame(do.call(cbind,funcionesIce))
names(ices)<-paste0( "x1=",xx1)
ices$x2<-x2
library(reshape2)
basef<-melt(ices,id="x2")

basef$variable<-as.factor(basef$variable)
library(ggplot2)
ggplot(basef) +
  geom_line(aes(x = x2, y = value, color = variable))+theme_bw()

# sum( abs(ices[,1]-ices[,2])^2 )
# sum( abs(ices[,3]-ices[,2])^2 )

library(tidyverse)
zz <- ices |> 
  pivot_longer( cols=c(1,3) )

ices |> 
  pivot_longer( cols=c(1,3) ) |> 
  mutate( caso = factor(name, labels = c('Case 1: X1=-1', 'Case 2: X1=1') )) |>
  ggplot( ) +
  geom_line(aes(x = x2, y = `x1=0` ) ) + 
  geom_line(aes(x = x2, y = value), color='blue', linetype='dashed' ) +
  labs(x = 'X2', y = 'ICE curve') + 
  facet_grid(~caso) + 
  theme_bw() + theme(aspect.ratio = 1)

ggsave('paper/figures/fig-toyexample.pdf', height = 7, width = 7)
