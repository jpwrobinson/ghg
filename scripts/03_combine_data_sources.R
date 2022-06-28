pacman::p_load(tidyverse, skimr, janitor, cowplot, funk, install=FALSE)
theme_set(theme_sleek())
source('scripts/rda_reader.R')

load('data/uk_seafood.rds')
load('data/nutrient_ghg_species.rds')


unique(all$common_name)
head(all)

## need species info on
# 1) shrimp (warm vs cold)
# 2) crab
# 3) Squid

## missing GHG info on
# 1) Squid
# 2) Whelks
# 3) Monk
# 4) Sardine

## missing nutrient info on
# 1) Squid
# 2) Whelks
# 3) Crab
# 4) Shrimp
# 5) Mussel
# 6) Norway Lobster

all<-all  %>%  ungroup()  %>%  mutate(uk_name = common_name)  %>% 
		mutate(uk_name = case_when(
			# str_detect(uk_name, 'shrimp|prawn') ~ 'Shrimp, miscellaneous',
			str_detect(uk_name, 'Atlantic mackerel') ~ 'Mackerel',
			str_detect(uk_name, 'Atlantic cod') ~ 'Cod',
			str_detect(uk_name, 'Skipjack tuna') ~ 'Tuna, skipjack',
			str_detect(uk_name, 'Atlantic herring') ~ 'Herring',
			str_detect(uk_name, 'Norway lobster') ~ 'Lobster, Norway',
			str_detect(uk_name, 'Queen scallop') ~ 'Scallop',
			str_detect(uk_name, 'Atlantic salmon') ~ 'Salmon',
			str_detect(uk_name, 'Sea trout') ~ 'Trout',
			str_detect(uk_name, 'Rainbow trout') ~ 'Trout',
			str_detect(uk_name, 'Saithe') ~ 'Saithe',
			str_detect(uk_name, 'Blue mussel') ~ 'Sea mussels',
			TRUE ~ uk_name)) 

# create a new 'shrimp misc.' category for GHG based on all available GHG estimates for shrimps
shrimp<-all %>% filter(str_detect(common_name, 'shrimp|prawn')) %>% group_by(farmed_wild) %>% summarise_at(vars(low:nut_score), mean) %>% 
        mutate(uk_name = 'Shrimp, miscellaneous',
               common_name = 'Shrimp, miscellaneous',
               scientific_name = 'Shrimp, miscellaneous',
               group = 'Shrimps, prawns')

warmies<-c('Fenneropenaeus merguiensis', 'Penaeus esculentus', 'Macrobrachium rosenbergii', 
           'Penaeus monodon', 'Farfantepenaeus aztecus', 'Litopenaeus setiferus', 'Farfantepenaeus notialis', 'Penaeus vannamei')
cold<-c('Pleoticus muelleri', 'Pandalus borealis')

shrimp_warm<-all %>% filter(scientific_name %in% warmies) %>% 
    group_by(group, farmed_wild) %>% summarise_at(vars(low:nut_score), mean) %>%
  mutate(uk_name = 'Shrimp, warmwater',
         common_name = 'Shrimp, warmwater',
         scientific_name = 'Shrimp, warmwater',
         group = 'Shrimps, prawns')

## replace shrimp in all with new averaged values
all<-all %>% filter(!str_detect(common_name, 'shrimp'))
all<-rbind(all, shrimp %>% select(names(all)), shrimp_warm %>% select(names(all)))

# sp
# all[str_detect(all$common_name, tolower(sp[12])),]
# all[str_detect(all$common_name, 'tuna'),]

## now subset UK food
all_uk<-left_join(tot_post,
			all %>% mutate(species=uk_name),
			by = 'species') %>% 
		filter(!is.na(tot))

all_uk_bysector<-left_join(tot ,#%>% mutate(uk_name = species), 
			all %>% mutate(id = paste(uk_name, tolower(farmed_wild), sep = '_')) %>% 
			group_by(id, farmed_wild) %>% 
  				summarise_at(vars(mid, nut_score), mean),
			by = 'id') 

## join export to all_uk
ex<-readxl::read_excel('data/uk/uk_export.xlsx') %>% clean_names() %>% 
	pivot_longer(-c(species, indicator), names_to = 'year', values_to='value') %>% 
	mutate(year = as.numeric(str_replace_all(year, 'x', ''))) %>% 
	filter(indicator == 'live_weight_tonnes' & year == 2019) %>% 
	group_by(species, indicator) %>% 
	summarise(value = mean(value))

all_uk$exported<-ex$value[match(all_uk$species, ex$species)]
all_uk$apparent_consumption <- all_uk$tot - all_uk$exported

## add vitamin data for major UK products
vit<-read.csv('data/nut/uk_vitamin_data.csv') %>% janitor::clean_names()

## match vitamins, extracted only for top products
all_uk$vitamin_a[all_uk$species=='Lobster, Norway'] <- vit$vitamin_a[vit$uk_name=='Lobster, Norway']
all_uk$vitamin_a[all_uk$species=='Scallop'] <- vit$vitamin_a[vit$uk_name=='Scallop']
all_uk$vitamin_a[all_uk$species=='Sea mussels'] <- vit$vitamin_a[vit$uk_name=='Sea mussels']
all_uk$vitamin_a[all_uk$species=='Shrimp, miscellaneous'] <- vit$vitamin_a[vit$uk_name=='Shrimp, miscellaneous']
all_uk$vitamin_a[all_uk$species=='Shrimp, warmwater'] <- vit$vitamin_a[vit$uk_name=='Shrimp, warmwater']

all_uk$vitamin_d <- vit$vitamin_d[match(all_uk$species, vit$uk_name)]
all_uk$folate <- vit$folate[match(all_uk$species, vit$uk_name)]
all_uk$vitamin_b12 <- vit$vitamin_b12[match(all_uk$species, vit$uk_name)]
all_uk$iodine <- vit$iodine[match(all_uk$species, vit$uk_name)]

## now RDA
all_uk$vita_rda<-all_uk$vitamin_a/rda$rni_women[rda$nutrient=='vitamin_a']*100
all_uk$vitd_rda<-all_uk$vitamin_d/rda$rni_women[rda$nutrient=='vitamin_d']*100
all_uk$vitb12_rda<-all_uk$vitamin_b12/rda$rni_women[rda$nutrient=='vitamin_b12']*100
all_uk$folate_rda<-all_uk$folate/rda$rni_women[rda$nutrient=='folate']*100
all_uk$iodine_rda<-all_uk$iodine/rda$rni_women[rda$nutrient=='iodine']*100

## now cap nutrient RDA at 100% (i.e. a species either meets (100%) or doesn't meet (<100%) the RDA)
all_uk$vita_rda[all_uk$vita_rda>100]<-100
all_uk$vitd_rda[all_uk$vitd_rda>100]<-100
all_uk$vitb12_rda[all_uk$vitb12_rda>100]<-100
all_uk$folate_rda[all_uk$folate_rda>100]<-100
all_uk$iodine_rda[all_uk$iodine_rda>100]<-100

write.csv(all_uk, file = 'data/UK_GHG_nutrient_catch.csv', row.names=FALSE)
write.csv(all_uk_bysector, file = 'data/UK_GHG_nutrient_catch_bysector.csv', row.names=FALSE)

## Missing nutrients n = 0
all_uk  %>% filter(is.na(selenium) & top90 == TRUE)  %>% distinct(common_name)

## Missing GHG n = 14
all_uk  %>% filter(is.na(mid) & top90 == TRUE)  %>% distinct(species)
## Squid data for tropical South America catch
## Whelk missing
## Blue whiting missing, but can this be approximated by a pelagic fishery?
## Shrimp misc. are global imported - so no info on warm/cold and species composition
## What is the species composition for crab, shrimp (warmwater)?
