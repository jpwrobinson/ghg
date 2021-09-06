source('scripts/fig/00_plotting.R')


drops<-c('Other marine fish')
nut<-read.csv('data/UK_GHG_nutrient_catch.csv') %>% 
  filter(top80 == TRUE & !species %in% drops & !is.na(mid))

g0<-ggplot(nut, aes(mid,portion_adq, col=farmed_wild)) +  
      ggrepel::geom_label_repel(aes(label = species), seed=43, size=2.5) +
      geom_errorbarh(aes(xmin = low, xmax=max)) +
      theme(legend.position = c(0.8, 0.8), legend.title = element_blank()) +
      labs(x = 'C02 emmissions equivalent per kg fish', y = 'Portion for 40% nutrient adequacy, g') +
      scale_colour_manual(values = colcol2)

g1<-ggplot(nut, aes(mid, fct_reorder(species, nut_score))) + 
  geom_segment(aes(x = low, xend = max, y =  fct_reorder(species, nut_score), yend= fct_reorder(species, nut_score))) +
  labs(x = 'C02 emissions equivalent per kg fish', y ='')

g2<-ggplot(nut, aes(nut_score, fct_reorder(species, nut_score))) + 
  geom_bar(stat='identity') +
  labs(x = 'Nutrient density, %', y ='') +
  theme(axis.text.y = element_blank(), axis.title.y = element_blank())

plot_grid(g1, g2, nrow = 1, align='hv', rel_widths=c(1, 0.5))



pdf(file = 'fig/final/Figure2_UK_profiles.pdf', height=5, width=8)
g0
dev.off()