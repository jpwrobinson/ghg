pacman::p_load(tidyverse, skimr, janitor, cowplot, funk, ggrepel, scales, fmsb, install=FALSE)
theme_set(theme_sleek())
source('scripts/fig/00_plotting.R')
set.seed(43)

readxl::read_excel('data/gfg/GFG_Export_2021-04-12.xlsx') %>% clean_names() %>% filter(total_score=='Default Red Rating') %>% 
    group_by(common_name, farmed_wild) %>% summarise(n = n_distinct(id)) %>% data.frame()

gfg<-readxl::read_excel('data/gfg/GFG_Export_2021-04-12.xlsx') %>% clean_names() %>% 
    rename(scientific_name = latin_name) %>% 
    filter(! total_score %in% c('Unknown', 'Under Review', 'Default Red Rating', 'FIP Improver')) %>% 
    group_by(common_name, scientific_name, farmed_wild) %>% 
    summarise(se = se(as.numeric(total_score)), total_score = mean(as.numeric(total_score))) %>% ungroup() %>% 
    mutate(lower = total_score - 2*se, upper = total_score + 2*se,
          farmed_wild = recode(farmed_wild, 'Caught at sea' = 'Wild'), 
          scientific_name = recode(scientific_name, 'Euthynnus pelamis, Katsuwonus pelamis' = 'Katsuwonus pelamis',
                                                      'Theragra chalcogramma' = 'Gadus chalcogrammus'),
          id = paste(farmed_wild, scientific_name, sep='_'))


# read nutrient/ghg data, join with gfg
drops<-c('Other marine fish')
nut<-read.csv('data/UK_GHG_nutrient_catch.csv') %>%
  filter(top90 == TRUE & !species %in% drops & !is.na(mid)) %>%
  select(-tax) %>%
  rowwise() %>%
  mutate(n_targets = sum(c(ca_rda, fe_rda, se_rda, zn_rda, om_rda) > 25),  ## estimate nutrition targets (25% RDA) for each species)
         nt_co2 = mid / n_targets / 10, ## estimate the CO2 equivalent per RDA target
         nt_co2_low = low / n_targets / 10, ## estimate the CO2 equivalent per RDA target
         nt_co2_hi = max / n_targets / 10, ## estimate the CO2 equivalent per RDA target
         common_name = factor(common_name, levels = levels(fct_reorder(common_name, nt_co2)))) %>% 
  mutate(id = paste(farmed_wild, scientific_name, sep='_')) %>% 
  left_join(gfg %>% select(-farmed_wild, -common_name, -scientific_name), by = 'id')


nut_rad<-nut %>% filter(!is.na(total_score)) %>% 
      ungroup() %>% 
      mutate(CO2 = rescale(mid, to=c(1,0)), N = rescale(nut_score), S = rescale(total_score), P = rescale(tot)) %>% 
      select(id, common_name, scientific_name, farmed_wild, CO2, N, S, P, tot) #%>% 
      # pivot_longer(-c(scientific_name, farmed_wild), names_to = 'variable', values_to = 'value')

## catch weighted average values
nut_avg<-nut_rad %>% group_by(farmed_wild) %>% 
      summarise(across(where(is.numeric), ~ weighted.mean(.x, w = tot)))

## replace production with mean value (not catch weighted, illogical)
nut_avg_prod<-nut_rad %>% group_by(farmed_wild) %>% 
      summarise(across(where(is.numeric), ~ mean(.x)))

nut_avg$P<-nut_avg_prod$P
nut_avg$tot<-NULL

## average radars
for(i in 1:2){

  dat<-nut_avg[i,]
  ppcol<-ifelse(unique(nut_avg$farmed_wild[i])=='Farmed', colcol[2], colcol[1])
  tit<-nut_avg$farmed_wild[i]
  # gridlab<-ifelse(i == 1, 3, 0)
  gridlab = 4

  gg<-ggradar(dat, 
    group.colours = ppcol,
    base.size = 1,
    group.point.size = 2,
    group.line.width = 1,
    background.circle.colour = "white",
    axis.labels=c('CO2', 'Nutrients', 'Sustainability', 'Production'),
    axis.label.size = 4,
    grid.label.size = gridlab,
    fill=TRUE,
  gridline.mid.colour = "grey") + 
  labs(subtitle = tit) +
  theme(plot.subtitle = element_text(size=14, colour='black', face=2),
    plot.margin =unit(c(0.1, .01, 0.01, -.5), 'cm'))
  assign(paste0('gAvg', i), gg)
}

## species radars
sp<-unique(nut_rad$id)
for(i in 1:length(sp)){

  dat<-nut_rad[i, 4:8]
  ppcol<-ifelse(unique(nut_rad$farmed_wild[i])=='Farmed', colcol[2], colcol[1])
  tit<-nut_rad$common_name[i]
  # gridlab<-ifelse(i == 1, 3, 0)
  gridlab = 0

  gg<-ggradar(dat, 
    group.colours = ppcol,
    base.size = 1,
    group.point.size = 2,
    group.line.width = 1,
    background.circle.colour = "white",
    axis.label.size = 3,
    grid.label.size = gridlab,
    fill=TRUE,
  gridline.mid.colour = "grey") + 
  labs(subtitle = tit) +
  theme(plot.subtitle = element_text(size=11, colour='black', face=1),
    plot.margin =unit(c(0.01, 0.01, 0.01, 0.01), 'cm'))
  assign(paste0('g', i), gg)
}

ggblank<-ggradar(dat, 
    group.colours = 'transparent',
    base.size = 1,
    group.point.size = 2,
    group.line.width = 1,
    background.circle.colour = "white",
    axis.label.size = 3,
    grid.label.size = gridlab,
    grid.line.width= 0,
    fill=TRUE,
  gridline.mid.colour = "transparent") + 
  labs(subtitle = tit) +
  theme(plot.subtitle = element_text(size=10, colour='transparent'),
    plot.margin =unit(c(0.01, 0.01, 0.01, 0.01), 'cm'))

pdf(file = 'fig/final/Figure4_radar.pdf', height=7, width=16)
gavg<-plot_grid(gAvg2, gAvg1, nrow=2)
gsp<-plot_grid(g1, g2, g3, g4, g5, g6, g7, g8, g9, g10, g11, g12)
print(
  plot_grid(gavg, gsp, nrow=1, rel_widths=c(0.5, 1))
  )
dev.off()
