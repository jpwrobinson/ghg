pacman::p_load(tidyverse, skimr, janitor, cowplot, funk, ggrepel, scales, ggradar, install=FALSE)
theme_set(theme_sleek())
source('scripts/fig/00_plotting.R')
set.seed(43)

readxl::read_excel('data/gfg/GFG_Export_2021-04-12.xlsx') %>% clean_names() %>% filter(total_score=='Default Red Rating') %>% 
    group_by(common_name, farmed_wild) %>% summarise(n = n_distinct(id)) %>% data.frame()

gfg<-readxl::read_excel('data/gfg/GFG_Export_2021-04-12.xlsx') %>% clean_names() %>% 
    rename(scientific_name = latin_name) %>% 
    filter(! total_score %in% c('Unknown', 'Under Review', 'Default Red Rating', 'FIP Improver')) %>% 
    group_by(common_name, scientific_name, farmed_wild) %>% 
    mutate(total_score = as.numeric(total_score),
          farmed_wild = recode(farmed_wild, 'Caught at sea' = 'Wild'), 
          scientific_name = recode(scientific_name, 'Euthynnus pelamis, Katsuwonus pelamis' = 'Katsuwonus pelamis',
                                                      'Theragra chalcogramma' = 'Gadus chalcogrammus'),
          id = paste(farmed_wild, scientific_name, sep='_')) %>% 
    group_by(farmed_wild) %>% 
    ## rescale ratings between 0-1, but inverse for farmed
    mutate(total_score = ifelse(farmed_wild == 'Farmed', rescale(total_score, to = c(0,1)),rescale(total_score, to = c(1,0))))  %>% 
    group_by(common_name, farmed_wild, scientific_name, id) %>% 
    summarise(lower = min(total_score), upper = max(total_score), total_score = median(total_score)) %>% ungroup()

    

# read nutrient/ghg data, join with gfg
drops<-c('Other marine fish')
nut<-read.csv('data/UK_GHG_nutrient_catch.csv') %>%
  filter(top90 == TRUE & !species %in% drops & !is.na(mid)) %>%
  select(-tax) %>%
  rowwise() %>%
  mutate(nut_score = sum(c(ca_rda, fe_rda, se_rda, zn_rda, om_rda, vita_rda, vitd_rda, vitb12_rda, folate_rda))) %>% 
  mutate(id = paste(farmed_wild, scientific_name, sep='_')) %>% 
  left_join(gfg %>% select(-farmed_wild, -common_name, -scientific_name), by = 'id') %>% 
  filter(id != 'Wild_Mytilus edulis')


## rescale variables to 0-1. Sustainability is already on 0-1 scale.
nut_rad<-nut %>% filter(!is.na(total_score)) %>% 
      ungroup() %>% 
      mutate(GHG = rescale(mid, to=c(1,0)), N = rescale(nut_score), S = (total_score), P = rescale(tot)) %>% 
      select(id, common_name, scientific_name, farmed_wild, GHG, N, S, P, tot) #%>% 
      # pivot_longer(-c(scientific_name, farmed_wild), names_to = 'variable', values_to = 'value')

## add price per kg (Seafish in retail 2021 update)
nut_rad$price_key_kg<-c(16.34, 8.03, 8.54, 6.45, 9.64, 5.76, 5.08, 10.42, 24.56, 5.47, 16.12)
nut_rad$A<-rescale(nut_rad$price_key_kg, to=c(1,0))
nut_rad$price_key_kg<-NULL

 # [1] Atlantic salmon   Atlantic mackerel Atlantic cod      Skipjack tuna    
 # [5] Haddock           Atlantic herring  Alaska pollock    Norway lobster   
 # [9] Queen scallop     Blue mussel       Blue mussel       Rainbow trout  

## catch weighted average values
nut_avg<-nut_rad %>% group_by(farmed_wild) %>% 
      summarise(across(where(is.numeric), ~ weighted.mean(.x, w = tot)))

## replace production with mean value (not catch weighted, illogical)
nut_avg_prod<-nut_rad %>% group_by(farmed_wild) %>% 
      summarise(across(where(is.numeric), ~ mean(.x)))

nut_avg$P<-nut_avg_prod$P


## average radars
for(i in 1:2){

  ppcol<-ifelse(unique(nut_avg$farmed_wild[i])=='Farmed', colcol[2], colcol[1])
  tit<-nut_avg$farmed_wild[i]
  # gridlab<-ifelse(i == 1, 3, 0)
  gridlab = 4
  if(i == 1){
  cap <- paste0('Average profile, ', scales::comma(round(nut_avg$tot[i],0)), ' t')}

  if(i == 2){
  cap <- paste0('Average profile, ', scales::comma(round(nut_avg$tot[i],0)), ' t (total annual production)')}

  dat<-nut_avg[i,]
  dat$tot<-NULL

  gg<-ggradar(dat, 
    group.colours = ppcol,
    base.size = 1,
    group.point.size = 2,
    group.line.width = 1,
    background.circle.colour = "white",
    axis.labels=c('Low GHG', 'Nutrients', 'Sustainability', 'Production', 'Affordable'),
    axis.label.size = 4,
    grid.label.size = gridlab,
    fill=TRUE,
  gridline.mid.colour = "grey") + 
  labs(title = tit, subtitle = cap) +
  coord_equal(clip='off') +
  theme(
    plot.title = element_text(size=12, colour='black', face=2),
    plot.subtitle = element_text(size=9, colour='#636363', face=1),
    plot.margin =unit(c(0.1, .01, 0.01, -1), 'cm'))
  assign(paste0('gAvg', i), gg)
}

## species radars
sp<-unique(nut_rad$id)
for(i in 1:length(sp)){

  dat<-nut_rad[i, 4:10]
  ppcol<-ifelse(unique(nut_rad$farmed_wild[i])=='Farmed', colcol[2], colcol[1])
  tit<-nut_rad$common_name[i]
  # gridlab<-ifelse(i == 1, 3, 0)
  gridlab = 0
  cap <- paste0(scales::comma(round(dat$tot,0)), ' t')

  dat$tot<-NULL

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
  labs(title = tit, subtitle = cap) +
  theme(
    plot.title = element_text(size=12, colour='black', face=2),
    plot.subtitle = element_text(size=9, colour='#636363', face=1),
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

pdf(file = 'fig/final/Figure4_radar.pdf', height=7, width=15)
gavg<-plot_grid(gAvg2, gAvg1, nrow=2)
gsp<-plot_grid(g1, g2, g3, g4, g5, g6, g7, g8, g9, g10, g11)
print(
  plot_grid(gavg, gsp, nrow=1, rel_widths=c(0.5, 1))
  )
dev.off()
