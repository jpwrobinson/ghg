source('scripts/fig/00_plotting.R')


drops<-c('Other marine fish')
nut<-read.csv('data/UK_GHG_nutrient_catch.csv') %>% 
  filter(top90 == TRUE & !species %in% drops & !is.na(mid)) %>%
  select(-tax) %>% 
  group_by(species, farmed_wild, tot) %>% 
  summarise_at(vars(low:nut_score), mean) %>% 
  mutate(species=factor(species))

g0<-ggplot(nut, aes(mid,portion_adq, col=farmed_wild)) +  
      ggrepel::geom_label_repel(aes(label = species), seed=4, size=2.5) +
      geom_errorbarh(aes(xmin = low, xmax=max)) +
      geom_point() +
      theme(legend.position = c(0.8, 0.8), legend.title = element_blank()) +
      labs(x = 'C02 emmissions equivalent per kg fish', y = 'Portion for 40% nutrient adequacy, g') +
      scale_colour_manual(values = colcol2)

g1<-ggplot(nut, aes(mid, fct_reorder(species, mid), col=farmed_wild)) + 
  geom_segment(aes(x = low, xend = max, y =  fct_reorder(species, mid), yend= fct_reorder(species, mid))) +
  geom_point(aes(x = mid, y =  fct_reorder(species, mid))) +
  labs(x = 'C02 emissions equivalent per kg fish', y ='') +
  scale_colour_manual(values = colcol2) +
  theme(legend.position = c(0.8, 0.3), legend.title=element_blank())

nut2<-nut %>% ungroup() %>%  distinct(nut_score, species)
nut2$species<-factor(nut2$species, levels=levels(fct_reorder(nut$species, nut$mid)[!duplicated(fct_reorder(nut$species, nut$mid, min))]))

g2<-ggplot(nut2, aes(nut_score,species)) + 
  geom_bar(stat='identity') +
  labs(x = 'Nutrient density, %', y ='') +
  scale_fill_manual(values = colcol2) +
  scale_x_continuous(labels=scales::comma, expand=c(0, 0.06)) +
  theme(plot.margin=unit(c(0.1, 0.5, 0.1, 0.5), 'cm'), 
        axis.ticks = element_blank(), 
        legend.position = 'none', axis.text.y = element_blank(), axis.title.y = element_blank())

# labs <- levels(nut2$species)
# levels(nut$species)<-levels(fct_reorder(nut$species, nut$mid))
g3<-ggplot(nut, aes(tot, fct_reorder(species, mid), fill=farmed_wild)) +
      geom_bar(stat = 'identity') +
      labs(x = 'Seafood produced, t', y = '') +
      scale_fill_manual(values = colcol2) +
      scale_x_continuous(labels=scales::comma, expand=c(0, 0.06)) +
      theme(plot.margin=unit(c(0.1, 0.5, 0.1, 0.5), 'cm'), 
            axis.ticks = element_blank(), 
            legend.position = 'none', 
            axis.text.y = element_blank(), axis.title.y = element_blank())



pdf(file = 'fig/final/Figure2_UK_profiles.pdf', height=2, width=10)
plot_grid(g1, g2, g3, nrow = 1, align = 'h', rel_widths=c(1, 0.6, 0.6), labels=c('A', 'B', 'C'))
dev.off()

pdf(file = 'fig/final/FigureS2_UK_density_CO2.pdf', height=5, width=10)
print(g0)
dev.off()






