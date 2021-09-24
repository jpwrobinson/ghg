source('scripts/fig/00_plotting.R')


drops<-c('Other marine fish')
nut<-read.csv('data/UK_GHG_nutrient_catch.csv') %>% 
  filter(top80 == TRUE & !species %in% drops & !is.na(mid))

g0<-ggplot(nut, aes(mid,portion_adq, col=farmed_wild)) +  
      ggrepel::geom_label_repel(aes(label = species), seed=4, size=2.5) +
      geom_errorbarh(aes(xmin = low, xmax=max)) +
      theme(legend.position = c(0.8, 0.8), legend.title = element_blank()) +
      labs(x = 'C02 emmissions equivalent per kg fish', y = 'Portion for 40% nutrient adequacy, g') +
      scale_colour_manual(values = colcol2)

g1<-ggplot(nut, aes(mid, fct_reorder(species, mid), col=farmed_wild)) + 
  geom_segment(aes(x = low, xend = max, y =  fct_reorder(species, mid), yend= fct_reorder(species, mid))) +
  geom_point(aes(x = mid, y =  fct_reorder(species, mid))) +
  labs(x = 'C02 emissions equivalent per kg fish', y ='') +
  scale_colour_manual(values = colcol2) +
  theme(legend.position = 'none')

g2<-ggplot(nut, aes(nut_score, fct_reorder(species, mid), fill=farmed_wild)) + 
  geom_bar(stat='identity') +
  labs(x = 'Nutrient density, %', y ='') +
  scale_fill_manual(values = colcol2) +
  scale_x_continuous(labels=scales::comma, expand=c(0.01, 0.05)) +
  theme(plot.margin=unit(c(0.1, 0.5, 0.1, 0.5), 'cm'), 
        axis.ticks = element_blank(), 
        legend.position = 'none', axis.text.y = element_blank(), axis.title.y = element_blank())

g3<-ggplot(nut, aes(tot, fct_reorder(species, mid), fill=farmed_wild)) +
      geom_bar(stat = 'identity') +
      labs(x = 'Seafood produced, t', y = '') +
      scale_fill_manual(values = colcol2) +
      scale_x_continuous(labels=scales::comma, expand=c(0.01,0.05)) +
      theme(plot.margin=unit(c(0.1, 0.5, 0.1, 0.5), 'cm'), 
            axis.ticks = element_blank(), 
            legend.position = 'none', axis.text.y = element_blank(), axis.title.y = element_blank())



pdf(file = 'fig/final/Figure2_UK_profiles.pdf', height=2, width=10)
plot_grid(g1, g2, g3, nrow = 1, align = 'h', rel_widths=c(1, 0.5, 0.5), labels=c('A', 'B', 'C'))
dev.off()

pdf(file = 'fig/final/FigureS2_UK_density_CO2.pdf', height=5, width=10)
print(g0)
dev.off()






