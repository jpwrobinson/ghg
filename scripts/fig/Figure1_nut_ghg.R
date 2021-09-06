

load('data/nutrient_ghg_species.rds')

g0<-ggplot(all, aes(mid, portion_adq, col=farmed_wild)) + 
  # geom_text(aes(label=common_name))
  geom_point(aes(shape = tax), size=2.5, alpha=0.5) +
  # geom_label(aes(label=common_name), alpha=0.5,size=2.5) +
  labs(x = 'CO2 emission equivalent per kg of fish', y = 'Portion of fish for 40% nutrient adequacy, g') +
  theme(legend.position = c(0.8, 0.8), legend.title = element_blank()) +
  guides(point = 'legend', text='none') +
  scale_colour_manual(values = colcol2)


g1<-ggplot(all, aes(mid, portion_adq, col=farmed_wild)) + 
    # geom_text(aes(label=common_name))
    geom_point(size=0) +
    geom_label(aes(label=common_name), alpha=0.5,size=2.5) +
    labs(x = 'CO2 emission equivalent per kg of fish', y = 'Portion of fish for 40% nutrient adequacy, g') +
    theme(legend.position = c(0.8, 0.8), legend.title = element_blank()) +
    guides(point = 'legend', text='none') +
    scale_colour_manual(values = colcol2)

g2<-ggplot(all, aes(mid, nut_score, col=farmed_wild)) + 
  # geom_text(aes(label=common_name))
  geom_point(size=0) +
  geom_label(aes(label=common_name), alpha=0.5,size=2.5) +
  labs(x = 'CO2 emission equivalent per kg of fish', y = 'Nutrient density, %') +
  theme(legend.position = c(0.8, 0.8), legend.title = element_blank()) +
  guides(point = 'legend', text='none') +
  scale_colour_manual(values = colcol2)


pdf(file = 'fig/final/Figure1_nutrient_ghg.pdf', height=5, width=8)
g0
g1
g2
dev.off()