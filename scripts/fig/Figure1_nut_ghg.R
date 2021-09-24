pacman::p_load(ggrepel, tidyverse, skimr, janitor, cowplot, funk, install=FALSE)
theme_set(theme_sleek())


load('data/nutrient_ghg_species.rds')

## estimate mean and range of C02 and nutrients by groups
wild_f<-all %>% group_by(farmed_wild, tax) %>% 
        summarise(se = se(portion_adq), mean = mean(portion_adq),
                  lower = min(low), upper = max(max), mid = mean(mid)) %>% 
        mutate(lower_nut = mean - 2*se, upper_nut = mean + 2*se, x = paste0(farmed_wild,'\n', tolower(tax)))

# main panel: nutrient adequacy vs CO2 by species
g0<-ggplot(all, aes(mid, portion_adq)) + 
  # geom_text(aes(label=common_name))
  geom_point(aes(fill=farmed_wild), size=2.5, alpha=0.9, pch=21, col='black') +
  # geom_label(aes(label=common_name), alpha=0.5,size=2.5) +
  labs(x = 'CO2 emission equivalent per kg of fish', y = 'Portion of fish for 40% nutrient adequacy, g') +
  theme(legend.position = 'none') +
  guides(point = 'legend', text='none') +
  scale_fill_manual(values = colcol2)

# inset: fish vs invert and farmed vs wild
g_inset<-ggplot(wild_f, aes(x = mid, y =mean, fill=farmed_wild)) +
      geom_linerange(aes(xmin = lower, xmax = upper)) +
      geom_pointrange(aes(ymin = lower_nut, ymax = upper_nut), pch=21, col='black', fatten=10) +
      geom_text_repel(aes(label=x),size=3, force=1) +
      scale_fill_manual(values = colcol2) +
      theme(legend.position = 'none') +
      labs(x = 'CO2 emission equivalent per kg of fish', y = 'Portion of fish for 40% nutrient adequacy, g') 

  
## sup figures showing names and nutrient scores
g1<-ggplot(all, aes(mid, portion_adq, col=farmed_wild)) + 
    # geom_text(aes(label=common_name))
    geom_point(size=0) +
    geom_label(aes(label=common_name), alpha=0.5,size=2.5) +
    labs(x = 'CO2 emission equivalent per kg of fish', y = 'Portion of fish\nfor 40% nutrient adequacy, g') +
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


pdf(file = 'fig/final/Figure1_nutrient_ghg.pdf', height=4, width=9)
plot_grid(g0, g_inset, nrow=1, rel_widths=c(1, 1))
dev.off()

pdf(file = 'fig/final/FigureS1_nutrient_ghg.pdf', height=5, width=12)
plot_grid(g1, g2, nrow=2)
dev.off()