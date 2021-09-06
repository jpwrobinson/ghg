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
			# str_detect(uk_name, 'shrimp|prawn') ~ 'Shrimp, miscellaneous',
			str_detect(uk_name, 'Atlantic mackerel') ~ 'Mackerel',
			str_detect(uk_name, 'Atlantic cod') ~ 'Cod',
			str_detect(uk_name, 'Skipjack tuna') ~ 'Tuna, skipjack',
			str_detect(uk_name, 'Atlantic herring') ~ 'Herring',
			str_detect(uk_name, 'Norway lobster') ~ 'Lobster, Norway',
			str_detect(uk_name, 'Queen scallop') ~ 'Scallop',
			str_detect(uk_name, 'Atlantic salmon') ~ 'Salmon',
			str_detect(uk_name, 'Saithe') ~ 'Saithe',
			TRUE ~ uk_name)) 

# create a new 'shrimp misc.' category for GHG based on all available GHG estimates for shrimps
shrimp<-all %>% filter(str_detect(common_name, 'shrimp|prawn')) %>% group_by(farmed_wild) %>% summarise_at(vars(low:Vitamin_A_mu), mean) %>% 
        mutate(uk_name = 'Shrimp, miscellaneous',
               common_name = 'Shrimp, miscellaneous',
               scientific_name = 'Shrimp, miscellaneous')

warmies<-c('Fenneropenaeus merguiensis', 'Penaeus esculentus', 'Macrobrachium rosenbergii', 
           'Penaeus monodon', 'Farfantepenaeus aztecus', 'Litopenaeus setiferus', 'Farfantepenaeus notialis', 'Penaeus vannamei')
cold<-c('Pleoticus muelleri', 'Pandalus borealis')

shrimp_warm<-all %>% filter(scientific_name %in% warmies) %>% 
    group_by(farmed_wild) %>% summarise_at(vars(low:vitamin_a), mean) %>%
  mutate(uk_name = 'Shrimp, warmwater',
         common_name = 'Shrimp, warmwater',
         scientific_name = 'Shrimp, warmwater')

## replace shrimp in all with new averaged values
all<-all %>% filter(!str_detect(common_name, 'shrimp'))
all<-rbind(all, shrimp %>% select(names(all)), shrimp_warm %>% select(names(all)))

# sp
# all[str_detect(all$common_name, tolower(sp[12])),]
# all[str_detect(all$common_name, 'tuna'),]

all_uk<-left_join(tot_post %>% mutate(uk_name = species), by = 'uk_name', all) %>% 
		filter(!is.na(tot))
write.csv(all_uk, file = 'data/UK_GHG_nutrient_catch.csv', row.names=FALSE)

## Missing nutrients n = 0
all_uk  %>% filter(is.na(selenium) & top80 == TRUE)  %>% distinct(common_name)

## Missing GHG n = 7
all_uk  %>% filter(is.na(mid) & top80 == TRUE)  %>% distinct(species)
## Squid data for tropical South America catch
## Whelk missing
## Blue whiting missing, but can this be approximated by a pelagic fishery?
## Shrimp misc. are global imported - so no info on warm/cold and species composition
## What is the species composition for crab, shrimp (warmwater)?
