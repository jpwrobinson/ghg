pacman::p_load(ggrepel, tidyverse, skimr, janitor, cowplot, funk, install=FALSE)
theme_set(theme_sleek())
source('scripts/fig/00_plotting.R')

load('data/nutrient_ghg_species.rds')
all<-all %>% rowwise() %>% 
      mutate(n_targets = sum(c(ca_rda, fe_rda, se_rda, zn_rda, om_rda) > 25),  ## estimate nutrition targets (25% RDA) for each species)
             nt_co2 = mid / n_targets ) %>% ## estimate the CO2 equivalent per RDA target
      ungroup() %>% droplevels()

all$common_name<-factor(all$common_name, levels = levels(fct_reorder(all$common_name, all$nt_co2)))

gl<-ggplot(all, aes(common_name, nt_co2)) + 
      geom_point(data = all %>% filter(farmed_wild == 'Wild'), aes(fill = n_targets),size=2, col='black', pch=21) +
      geom_point(data = all %>% filter(farmed_wild != 'Wild'), aes(col = n_targets),size=2, pch=19) +
      coord_flip() +
      scale_x_discrete(limits=levels(all$common_name)) +
      labs(x = '', y = 'CO2 equivalent per RDA target', fill='Number of RDA targets') +
      # scale_shape_manual(values = c(21, 19)) +
      scale_fill_distiller(palette='RdYlGn',direction=1) +
      scale_color_distiller(palette='RdYlGn',direction=1) +
      guides(color='none') +
      theme(legend.position = c(0.8, 0.4))

## now UK only
drops<-c('Other marine fish')
nut<-read.csv('data/UK_GHG_nutrient_catch.csv') %>% 
  filter(top90 == TRUE & !species %in% drops & !is.na(mid)) %>%
  select(-tax) %>% 
  rowwise() %>% 
  mutate(n_targets = sum(c(ca_rda, fe_rda, se_rda, zn_rda, om_rda) > 25),  ## estimate nutrition targets (25% RDA) for each species)
         nt_co2 = mid / n_targets ) %>% ## estimate the CO2 equivalent per RDA target
  group_by(species, farmed_wild, tot) %>% 
  summarise_at(vars(low:nt_co2), mean) %>% 
  mutate(species=factor(species))

nut$species<-factor(nut$species, levels = levels(fct_reorder(nut$species, nut$nt_co2)))

gr<-ggplot(nut, aes(log10(tot), nt_co2)) + 
  geom_point(data = nut %>% filter(farmed_wild == 'Wild'), aes(fill = n_targets), size=2, col='black', pch=21) +
  geom_point(data = nut %>% filter(farmed_wild != 'Wild'), aes(col = n_targets), size=2, pch=19) +
  geom_label_repel(aes(label=species)) +
  labs(x = 'Seafood produced, log10(tonnes)', y = 'CO2 equivalent per RDA target', fill='Number of RDA targets') +
  # scale_shape_manual(values = c(21, 19)) +
  scale_fill_distiller(palette='RdYlGn',direction=1) +
  scale_color_distiller(palette='RdYlGn',direction=1) +
  guides(color='none') +
  theme(legend.position = 'none',
        plot.margin = unit(c(4, 0.5, 4, 0.5), 'cm'))


pdf(file = 'fig/final/Figure3.pdf', height=10, width=15)
plot_grid(gl, gr, nrow=1, labels=c('A', 'B'))
dev.off()