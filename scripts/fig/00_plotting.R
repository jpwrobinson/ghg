library(tidyverse)
library(funk)
theme_set(theme_sleek())

th<-theme(axis.text.y=element_text(size=11.5, colour='black'),
          axis.text.x=element_text(size=11.5, colour='black'),
          axis.line = element_line(colour = "black"), 
          axis.line.x.top = element_line(colour = "white"), 
          axis.title = element_text(size=12, colour='black'),
          panel.border = element_blank(),
          strip.text = element_text(face="bold", colour='black', size=10),
          legend.position ='none',
          legend.title=element_blank(),
          axis.ticks=element_blank())


colcol<-c('#377eb8', '#4daf4a')
colcol2<-c('Farmed' = '#4daf4a', 'Wild' ='#377eb8')
