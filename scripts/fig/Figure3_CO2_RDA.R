pacman::p_load(ggrepel, tidyverse, skimr, janitor, cowplot, funk, install=FALSE)
theme_set(theme_sleek())
source('scripts/fig/00_plotting.R')

load('data/nutrient_ghg_species.rds')
all<-all %>% rowwise() %>% 
      mutate(n_targets = sum(c(ca_rda, fe_rda, se_rda, zn_rda, om_rda) > 25),  ## estimate nutrition targets (25% RDA) for each species)
             nt_co2 = mid / n_targets ) %>% ## estimate the CO2 equivalent per RDA target
      ungroup() %>% droplevels()

all$common_name<-factor(all$common_name, levels = levels(fct_reorder(all$common_name, all$nt_co2)))


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

## read other foods
food<-read.csv('data/ghg_nutrient_other_foods.csv') %>%  rowwise() %>% 
  mutate(n_targets = sum(c(ca_rda, fe_rda, se_rda, zn_rda, om_rda) > 25),  ## estimate nutrition targets (25% RDA) for each species)
         nt_co2 = median / n_targets ) %>% ## estimate the median CO2 equivalent per RDA target
  ungroup() %>% droplevels() %>% 
  filter(product %in% c('Chicken', 'Pork', 'Beef', 'Lamb')) ## take ASMs only

fig_dat<-rbind(groups %>% select(product, nt_co2, n_targets),
               food %>% select(product, nt_co2, n_targets)) %>% 
        filter(n_targets > 0)

fig_dat$product<-factor(fig_dat$product, levels = levels(fct_reorder(fig_dat$product, fig_dat$nt_co2)))


gmain<-ggplot(fig_dat, aes(product, nt_co2, fill = n_targets)) + 
  geom_segment(aes(xend = product, y = -Inf, yend = nt_co2), col='grey') +
  geom_point(size=3.5, col='black', pch=21) +
  coord_flip() +
  scale_x_discrete(limits=levels(fig_dat$product)) +
  scale_y_continuous(expand=c(0.03,0)) +
  labs(x = '', y = 'kg CO2 equivalent per RDA target', fill='# RDA targets') +
  # scale_shape_manual(values = c(21, 19)) +
  scale_fill_distiller(palette='RdYlGn',direction=1) +
  scale_color_distiller(palette='RdYlGn',direction=1) +
  guides(color='none') +
  th +
  theme(legend.position = c(0.8, 0.4), axis.text.y = element_text(size=11), legend.title=element_text(size=10))




## supp figure - all species in the Dal database
gl<-ggplot(all, aes(common_name, nt_co2)) + 
      geom_segment(aes(xend = common_name, y = -Inf, yend = nt_co2), col='grey') +
      geom_point(data = all %>% filter(farmed_wild == 'Wild'), aes(fill = n_targets),size=2.5, col='black', pch=21) +
      geom_point(data = all %>% filter(farmed_wild != 'Wild'), aes(col = n_targets),size=2.5, pch=19) +
      coord_flip() +
      scale_x_discrete(limits=levels(all$common_name)) +
      scale_y_continuous(expand=c(0.01,0)) +
      labs(x = '', y = 'CO2 equivalent per RDA target', fill='# RDA targets') +
      # scale_shape_manual(values = c(21, 19)) +
      scale_fill_distiller(palette='RdYlGn',direction=1) +
      scale_color_distiller(palette='RdYlGn',direction=1) +
      guides(color='none') +
      th +
      theme(legend.position = c(0.8, 0.4), axis.text.y = element_text(size=9), legend.title=element_text(size=10))

## now UK production focus only
# drops<-c('Other marine fish')
# nut<-read.csv('data/UK_GHG_nutrient_catch.csv') %>% 
#   filter(top90 == TRUE & !species %in% drops & !is.na(mid)) %>%
#   select(-tax) %>% 
#   rowwise() %>% 
#   mutate(n_targets = sum(c(ca_rda, fe_rda, se_rda, zn_rda, om_rda) > 25),  ## estimate nutrition targets (25% RDA) for each species)
#          nt_co2 = mid / n_targets ) %>% ## estimate the CO2 equivalent per RDA target
#   group_by(species, farmed_wild, tot) %>% 
#   summarise_at(vars(low:nt_co2), mean) %>% 
#   mutate(species=factor(species))
# 
# nut$species<-factor(nut$species, levels = levels(fct_reorder(nut$species, nut$nt_co2)))
# 
# gr<-ggplot(nut, aes(log10(tot), nt_co2)) + 
#   geom_point(data = nut %>% filter(farmed_wild == 'Wild'), aes(fill = n_targets), size=2, col='black', pch=21) +
#   geom_point(data = nut %>% filter(farmed_wild != 'Wild'), aes(col = n_targets), size=2, pch=19) +
#   geom_label_repel(aes(label=species)) +
#   labs(x = 'Seafood produced, log10(tonnes)', y = 'CO2 equivalent per RDA target', fill='Number of RDA targets') +
#   # scale_shape_manual(values = c(21, 19)) +
#   scale_fill_distiller(palette='RdYlGn',direction=1) +
#   scale_color_distiller(palette='RdYlGn',direction=1) +
#   guides(color='none') +
#   th +
#   theme(legend.position = 'none',
#         plot.margin = unit(c(4, 0.5, 4, 0.5), 'cm'))


pdf(file = 'fig/final/Figure3.pdf', height=4, width=4)
print(gmain)
dev.off()


pdf(file = 'fig/final/FigureS3.pdf', height=12, width=8)
# print(
#   plot_grid(gl, gr, nrow=1, labels=c('A', 'B'))
# )
print(gl)
dev.off()
