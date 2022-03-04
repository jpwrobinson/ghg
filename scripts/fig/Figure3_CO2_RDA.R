pacman::p_load(ggrepel, tidyverse, skimr, janitor, cowplot, funk, install=FALSE)
theme_set(theme_sleek())
source('scripts/fig/00_plotting.R')

load('data/nutrient_ghg_species.rds')
all<-all %>% rowwise() %>% 
      mutate(n_targets = sum(c(ca_rda, fe_rda, se_rda, zn_rda, om_rda) > 25),  ## estimate nutrition targets (25% RDA) for each species)
             nt_co2 = mid / n_targets / 10 ) %>% ## estimate the CO2 equivalent per RDA target
      ungroup() %>% droplevels() %>% 
      mutate(common_name = factor(common_name, levels = levels(fct_reorder(common_name, nt_co2))))



## setup fish groups of interest
cats<-data.frame(isscaap = unique(all$group))
cats$group<-c('Whitefish', 'Tuna', 'Pelagic (large)', 'Bivalve', 'Pelagic (small)', 'Freshwater fish', 'Salmonidae', NA,
              'Crustacean', 'Bivalve', 'Freshwater fish', 'Cephalopod', 'Whitefish', NA, 'Tilapia', 'Crustacean', 'Crustacean', 
              'Bivalve', NA, 'Bivalve', 'Crustacean','Crustacean')

all$group2<-cats$group[match(all$group, cats$isscaap)]

## estimate mean nt_co2 by group
groups<-all %>% group_by(group2) %>% 
  summarise(nt_co2 = mean(nt_co2), n_targets = mean(n_targets)) %>% 
  mutate(product = group2) %>% 
  filter(!is.na(group2))

## now UK production focus only
drops<-c('Other marine fish')
nut<-read.csv('data/UK_GHG_nutrient_catch.csv') %>%
  filter(top90 == TRUE & !species %in% drops & !is.na(mid)) %>%
  select(-tax) %>%
  rowwise() %>%
  mutate(n_targets = sum(c(ca_rda, fe_rda, se_rda, zn_rda, om_rda) > 25),  ## estimate nutrition targets (25% RDA) for each species)
         nt_co2 = mid / n_targets / 10, ## estimate the CO2 equivalent per RDA target
         common_name = factor(common_name, levels = levels(fct_reorder(common_name, nt_co2))))
         
nut$group2<-cats$group[match(nut$group, cats$isscaap)]
## estimate mean nt_co2 by group
groups_uk<-nut %>% group_by(species) %>% 
  summarise(nt_co2 = mean(nt_co2), n_targets = mean(n_targets)) %>% 
  mutate(product = species) %>% 
  filter(!is.na(species))


## read other foods
food<-read.csv('data/ghg_nutrient_other_foods.csv') %>%  rowwise() %>% 
  mutate(n_targets = sum(c(ca_rda, fe_rda, se_rda, zn_rda, om_rda) > 25),  ## estimate nutrition targets (25% RDA) for each species)
         nt_co2 = median / n_targets / 10 ) %>% ## estimate the median CO2 equivalent per RDA target (correct kg to 100g)
  ungroup() %>% droplevels() %>% 
  filter(product %in% c('Chicken', 'Pork', 'Beef', 'Lamb')) ## take ASMs only

fig_dat<-rbind(groups %>% select(product, nt_co2, n_targets),
               food %>% select(product, nt_co2, n_targets)) %>% 
  filter(n_targets > 0) %>% 
  mutate(product = factor(product, levels = levels(fct_reorder(product, nt_co2))))

fig_dat_uk<-rbind(groups_uk %>% select(product, nt_co2, n_targets),
               food %>% select(product, nt_co2, n_targets)) %>% 
  filter(n_targets > 0) %>% 
  mutate(product = factor(product, levels = levels(fct_reorder(product, nt_co2))))


gmain<-ggplot(fig_dat, aes(product, nt_co2, fill = n_targets)) + 
  geom_segment(aes(xend = product, y = -Inf, yend = nt_co2), col='#636363') +
  geom_point(size=3.5, col='black', pch=21) +
  coord_flip() +
  scale_x_discrete(limits=levels(fig_dat$product)) +
  scale_y_continuous(expand=c(0.03,0)) +
  labs(x = '', y = 'CO2 equivalent per nutrient target', fill='# nutrient targets (NT)', subtitle = 'Global') +
  # scale_shape_manual(values = c(21, 19)) +
  scale_fill_distiller(limits=c(1, 3.5),breaks=c(1,2,3), palette='RdYlGn',direction=1) +
  scale_color_distiller(limits=c(1, 3.5),breaks=c(1,2,3), palette='RdYlGn',direction=1) +
  guides(color='none') +
  th +
  theme(legend.position = c(0.8, 0.4), axis.text.y = element_text(size=11), 
        legend.title=element_text(size=10, colour='black'),
        plot.subtitle = element_text(size =11, colour='black'),
        panel.grid.major.x=element_line(size=0.2, colour='grey'))


guk<-ggplot(fig_dat_uk, aes(product, nt_co2, fill = n_targets)) + 
  geom_segment(aes(xend = product, y = -Inf, yend = nt_co2), col='#636363') +
  geom_point(size=3.5, col='black', pch=21) +
  coord_flip() +
  scale_x_discrete(limits=levels(fig_dat_uk$product)) +
  scale_y_continuous(expand=c(0.03,0)) +
  labs(x = '', y = 'CO2 equivalent per nutrient target', fill='# nutrient targets (NT)', subtitle='UK') +
  # scale_shape_manual(values = c(21, 19)) +
  scale_fill_distiller(limits=c(1, 3.5),breaks=c(1,2,3), palette='RdYlGn',direction=1) +
  scale_color_distiller(limits=c(1, 3.5),breaks=c(1,2,3), palette='RdYlGn',direction=1) +
  guides(color='none') +
  th +
  theme(legend.position = 'none', axis.text.y = element_text(size=11),
        legend.title=element_text(size=10, colour='black'),
        plot.subtitle = element_text(size =11, colour='black'),
        panel.grid.major.x=element_line(size=0.2, colour='grey'))



## supp figure - all species in the Dal database
gl<-ggplot(all, aes(common_name, nt_co2)) + 
  # geom_segment(aes(xend = common_name, y = -Inf, yend = nt_co2), col='#636363') +
  geom_point(data = all %>% filter(farmed_wild == 'Wild'), aes(fill = n_targets),size=3, col='black', pch=21) +
  geom_point(data = all %>% filter(farmed_wild != 'Wild'), aes(col = n_targets),size=3, pch=19) +
  coord_flip() +
  scale_x_discrete(limits=levels(all$common_name)) +
  scale_y_continuous(expand=c(0.01,0)) +
  labs(x = '', y = 'CO2 equivalent per nutrient target', fill='# RDA targets') +
  # scale_shape_manual(values = c(21, 19)) +
  scale_fill_distiller(limits=c(1, 3.5),breaks=c(1,2,3), palette='RdYlGn',direction=1) +
  scale_color_distiller(limits=c(1, 3.5),breaks=c(1,2,3), palette='RdYlGn',direction=1) +
  guides(color='none') +
  th +
  theme(legend.position = c(0.8, 0.4), 
    panel.grid.major=element_line(size=0.2, colour='grey'),
    panel.grid.minor.x=element_line(size=0.1, colour='grey'),
      axis.text.y = element_text(size=9), legend.title=element_text(size=10))



pdf(file = 'fig/final/Figure3.pdf', height=4, width=10)
print(plot_grid(gmain, guk, labels=c('A', 'B')))
dev.off()


pdf(file = 'fig/final/FigureS3.pdf', height=12, width=8)
# print(
#   plot_grid(gl, gr, nrow=1, labels=c('A', 'B'))
# )
print(gl)
dev.off()
