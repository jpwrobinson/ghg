pacman::p_load(tidyverse, skimr, janitor, cowplot, funk, install=FALSE)
theme_set(theme_sleek())


load('data/uk_seafood.rds')
load('data/nutrient_ghg_species.rds')


unique(all$common_name)

sp<-tot_post$species[tot_post$top80 == 'TRUE']
sp

## need species info on
# 1) shrimp (warm vs cold)
# 2) crab
# 3) Squid

## missing GHG info on
# 1) Squid
# 2) Whelks
# 3) Monk

## missing nutrient info on
# 1) Squid
# 2) Whelks
# 3) Crab
# 4) Shrimp
# 5) Mussel
# 6) Norway Lobster

all<-all  %>%  mutate(uk_name = common_name)  %>% 
		mutate(uk_name = case_when(
			str_detect(uk_name, 'shrimp') ~ 'Shrimp, miscellaneous',
			str_detect(uk_name, 'Atlantic mackerel') ~ 'Mackerel',
			str_detect(uk_name, 'Atlantic cod') ~ 'Cod',
			str_detect(uk_name, 'Skipjack tuna') ~ 'Tuna, skipjack',
			str_detect(uk_name, 'Atlantic herring') ~ 'Herring',
			str_detect(uk_name, 'Norway lobster') ~ 'Lobster, Norway',
			str_detect(uk_name, 'Queen scallop') ~ 'Scallop',
			TRUE ~ uk_name))

# sp
# all[str_detect(all$common_name, tolower(sp[12])),]
# all[str_detect(all$common_name, 'tuna'),]


all_uk<-left_join(tot_post %>% mutate(uk_name = species), by = 'uk_name', all) %>% 
		filter(!is.na(tot))

## Missing nutrients n = 8
all_uk  %>% filter(is.na(Selenium_mu))  %>% pull(common_name)
## Missing GHG n = 
all_uk  %>% filter(is.na(mid))  %>% pull(common_name)
## Missing nutrients n = 8
all_uk  %>% filter(is.na(Selenium_mu))  %>% pull(common_name)
