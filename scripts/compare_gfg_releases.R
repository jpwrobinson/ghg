pacman::p_load(tidyverse, skimr, janitor, cowplot, funk, ggrepel, scales, ggradar, install=FALSE)
theme_set(theme_sleek())
source('scripts/fig/00_plotting.R')
set.seed(43)

gfg<-readxl::read_excel('data/gfg/GFG_Export_2022-11-09.xlsx') %>% clean_names() %>% 
  rename(common_name = species_common_name, 
         scientific_name = species_scientific_name, 
         farmed_wild = farmed_or_wild_caught) %>% 
  mutate(farmed_wild = recode(farmed_wild, 'Wild caught' = 'Wild')) %>% 
  filter(! public_rating %in% c('Unknown', 'Under Review', 'Default Red Rating', 'FIP Improver')) %>%
  group_by(common_name, scientific_name, farmed_wild) %>% 
  mutate(public_rating = as.numeric(public_rating),
         farmed_wild = recode(farmed_wild, 'Caught at sea' = 'Wild'), 
         scientific_name = recode(scientific_name, 'Euthynnus pelamis, Katsuwonus pelamis' = 'Katsuwonus pelamis',
                                  'Theragra chalcogramma' = 'Gadus chalcogrammus'),
         id = paste(farmed_wild, scientific_name, sep='_'),
         total_score = as.numeric(public_rating)) %>% 
  group_by(farmed_wild) %>% 
  ## rescale ratings between 0-1, but inverse for farmed
  mutate(total_score = ifelse(farmed_wild == 'Farmed', rescale(total_score, to = c(1,0)),rescale(total_score, to = c(1,0))))  %>%
  group_by(common_name, farmed_wild, scientific_name, id) %>% 
  summarise(lower = min(total_score), upper = max(total_score), total_score = median(total_score)) %>% ungroup()



gfg_old<-readxl::read_excel('data/gfg/GFG_Export_2021-04-12.xlsx') %>% clean_names() %>% 
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


g<-left_join(gfg, gfg_old %>% select(id, total_score), by = 'id')

ggplot(g, aes(total_score.x, total_score.y)) + 
  geom_point(aes(col=farmed_wild)) + labs(x = '2022', y = '2021') +
  geom_abline(intercept = 0, slope=1)


g %>% filter(scientific_name %in% sps) %>% data.frame()


## salmons

sps<-c("Aequipecten opercularis", 'Salmo salar')

gfg<-readxl::read_excel('data/gfg/GFG_Export_2022-11-09.xlsx') %>% clean_names() %>% 
  rename(common_name = species_common_name, 
         scientific_name = species_scientific_name, 
         farmed_wild = farmed_or_wild_caught) %>% 
  mutate(farmed_wild = recode(farmed_wild, 'Wild caught' = 'Wild')) %>% 
  filter(! public_rating %in% c('Unknown', 'Under Review', 'Default Red Rating', 'FIP Improver')) %>%
  group_by(common_name, scientific_name, farmed_wild) %>% 
  mutate(public_rating = as.numeric(public_rating),
         farmed_wild = recode(farmed_wild, 'Caught at sea' = 'Wild'), 
         scientific_name = recode(scientific_name, 'Euthynnus pelamis, Katsuwonus pelamis' = 'Katsuwonus pelamis',
                                  'Theragra chalcogramma' = 'Gadus chalcogrammus'),
         id = paste(farmed_wild, scientific_name, sep='_'),
         total_score = as.numeric(public_rating)) %>% 
  filter(scientific_name %in% sps)


gfg_old<-readxl::read_excel('data/gfg/GFG_Export_2021-04-12.xlsx') %>% clean_names() %>% 
  rename(scientific_name = latin_name) %>% 
  filter(! total_score %in% c('Unknown', 'Under Review', 'Default Red Rating', 'FIP Improver')) %>% 
  group_by(common_name, scientific_name, farmed_wild) %>% 
  mutate(total_score = as.numeric(total_score),
         farmed_wild = recode(farmed_wild, 'Caught at sea' = 'Wild'), 
         scientific_name = recode(scientific_name, 'Euthynnus pelamis, Katsuwonus pelamis' = 'Katsuwonus pelamis',
                                  'Theragra chalcogramma' = 'Gadus chalcogrammus'),
         id = paste(farmed_wild, scientific_name, sep='_')) %>% 
  filter(scientific_name %in% sps)
