pacman::p_load(tidyverse, skimr, janitor, cowplot, funk, install=FALSE)
theme_set(theme_sleek())
source('scripts/fig/00_plotting.R')
set.seed(43)

gfg<-readxl::read_excel('data/gfg/GFG_Export_2021-04-12.xlsx') %>% clean_names() %>% 
    rename(scientific_name = latin_name) %>% 
    filter(! total_score %in% c('Unknown', 'Under Review', 'Default Red Rating', 'FIP Improver')) %>% 
    group_by(common_name, scientific_name, farmed_wild) %>% 
    summarise(total_score = median(as.numeric(total_score))) %>% 
    mutate(farmed_wild = recode(farmed_wild, 'Caught at sea' = 'Wild'), 
          scientific_name = recode(scientific_name, 'Euthynnus pelamis, Katsuwonus pelamis' = 'Katsuwonus pelamis',
                                                      'Theragra chalcogramma' = 'Gadus chalcogrammus'),
          id = paste(farmed_wild, scientific_name, sep='_'))


drops<-c('Other marine fish')
nut<-read.csv('data/UK_GHG_nutrient_catch.csv') %>%
  filter(top90 == TRUE & !species %in% drops & !is.na(mid)) %>%
  select(-tax) %>%
  rowwise() %>%
  mutate(n_targets = sum(c(ca_rda, fe_rda, se_rda, zn_rda, om_rda) > 25),  ## estimate nutrition targets (25% RDA) for each species)
         nt_co2 = mid / n_targets / 10, ## estimate the CO2 equivalent per RDA target
         common_name = factor(common_name, levels = levels(fct_reorder(common_name, nt_co2)))) %>% 
  mutate(id = paste(farmed_wild, scientific_name, sep='_')) %>% 
  left_join(gfg %>% select(-farmed_wild), by = 'id')
        




gg<-ggplot(nut, aes(nt_co2, total_score, col=farmed_wild)) + 
        geom_point(size=3) + 
        geom_label_repel(aes(label = common_name.x), force=2,show.legend = FALSE) +
        th +
        scale_y_continuous(breaks=seq(0, 16, by = 2)) +
        scale_color_manual(values = colcol2) +
        labs(y = 'Sustainability Score (Good Fish Guide)', x = 'kg CO2 per nutrient target') +
          theme(legend.position = c(0.8, 0.8), 
                legend.title=element_blank()) 

pdf(file = 'fig/Figure4.pdf', height = 7, width=8)
print(gg)
dev.off()