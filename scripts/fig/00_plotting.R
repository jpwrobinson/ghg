library(tidyverse)
library(funk)
library(cowplot)
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


colcol<-c('#2166ac', '#006d2c')
colcol2<-c('Farmed' = '#006d2c', 'Wild' ='#2166ac')
colcol3<-c('farmed' = '#006d2c', 'landed' ='#2166ac', 'imported' = 'grey')

nut.cols<-c('Calcium'='#de2d26', 'Iron'='#636363', 'Zinc'='#3182bd', 'Vitamin A'='#31a354',
            'Omega-3' = '#F77D29', 'Selenium' = '#776EB0')
