pacman::p_load(tidyverse, skimr, janitor, cowplot, funk, install=FALSE)
theme_set(theme_sleek())
source('scripts/rda_reader.R')
#### GHG data


#### Nutrient data
nut<-read.csv('data/nut/Species_Nutrient_Predictions.csv') %>%
			select(species, ends_with('mu')) %>%
			rename(scientific_name = species) %>%
			select(-Protein_mu)


#### GHG data
farm<-read.csv('data/ghg/Specie_List_07_05_2021_Farmed.csv') %>%
				clean_names() %>%
				select(farmed_wild, common_name, scientific_name, system, ghg_low:pr_ghg_high, data_certainty) %>%
				rename(gear = 'system')

wild<-read.csv('data/ghg/Specie_List_07_05_2021_Wild.csv') %>%
			clean_names() %>%
			select(farmed_wild, common_name, scientific_name, gear, ghg_low:pr_ghg_high, data_certainty)

ghg<-rbind(farm %>% select(names(wild)), wild)

## fix some species names
ghg <- ghg %>% mutate(scientific_name = recode(scientific_name, 'Theragra chalcogramma' = 'Gadus chalcogrammus',
																'Seriola quinqueriadata' = 'Seriola quinqueradiata',
																'Psetta maxima' = 'Scophthalmus maximus'))

pdf(file = 'fig/ghg_all_species.pdf', height=8, width=8)
ggplot(ghg, aes(common_name, ymin = ghg_low, ymax = ghg_high, col=farmed_wild)) + 
			geom_errorbar() +
			coord_flip() + 
			facet_grid(~data_certainty)
dev.off()

## get midpoint of GHG range for each species - gear combo
ghg<-ghg %>% group_by(common_name, scientific_name, gear, farmed_wild) %>%
			summarise(low = min(ghg_low), max = max(ghg_high)) %>%
			mutate(mid = low + ((max - low)/2))

## join nutrients
all<-ghg %>% left_join(nut)

## export species without nutrient estimates (mostly inverts)
write.csv(
	all %>% filter(is.na(Selenium_mu)) %>% ungroup() %>% distinct(common_name, scientific_name) %>% data.frame(),
	file = 'data/nut/missing_nutrient_species.csv', row.names=FALSE)

## long format
all <- all %>% pivot_longer(Selenium_mu:Vitamin_A_mu, names_to = 'nutrient', values_to = 'conc')

pdf(file = 'fig/ghg_by_nutrients.pdf', height=3, width=15)
ggplot(all, aes(mid, conc, colour=farmed_wild)) + 
		geom_point() +
		labs(x = 'kg CO2 (midpoint of lower and upper estimates)', y = 'nutrient concentration per 100g') +
		facet_wrap(~nutrient, scales='free_y', nrow=1) +
		theme(legend.position = 'top')

dev.off()

## species filter for UK products
uk_prods<-c("Anchovy", "Herring", "Mackerel", "Sardine", "Tuna", "Crab", "Nephrops", "Prawn", "Scallop",
            "Alaska Pollock", "Cod", "Haddock", "Atlantic Salmon", "Mussels", "Prawns_warm", "Pangasius", "Tilapia", "Sea bass")

## fuzzy grepl on UK common and scientific names in ghg dataset
all_uk<-all %>% filter(grepl(paste(uk_prods, collapse="|"), common_name, ignore.case = TRUE) |
                         grepl(paste(uk_prods, collapse="|"), scientific_name, ignore.case = TRUE))

all_uk$product<-NA
for(i in 1:length(uk_prods)){
  all_uk$product[grep(uk_prods[i] , all_uk$common_name, ignore.case = TRUE)]<-uk_prods[i]
}

## need invert nutrients

pdf(file = 'fig/ghg_nutrients_uk.pdf', height=3, width=15)
ggplot(all_uk %>%  filter(!is.na(conc)), aes(mid, conc, colour=farmed_wild)) + 
  geom_point() +
  labs(x = 'kg CO2 (midpoint of lower and upper estimates)', y = 'nutrient concentration per 100g') +
  facet_wrap(~nutrient, scales='free_y', nrow=1) +
  theme(legend.position = 'top')

all_uk %>% group_by(product, farmed_wild, nutrient) %>% summarise(conc = median(conc), mid = median(mid)) %>% 
  filter(!is.na(conc)) %>%
  ggplot(aes(mid, conc, colour=farmed_wild, label=product)) + 
  # geom_point() +
  geom_text() +
  labs(x = 'kg CO2 (midpoint of lower and upper estimates)', y = 'nutrient concentration per 100g') +
  facet_wrap(~nutrient, scales='free_y', nrow=1) +
  theme(legend.position = 'top')

dev.off()



## nutrient density

all_uk<-all_uk %>% mutate(nutrient = tolower(str_replace_all(nutrient, '_mu', ''))) %>%  
        pivot_wider(names_from = 'nutrient', values_from = 'conc')
all_uk$ca_rda<-all_uk$calcium/rda$rni_women[rda$nutrient=='calcium']*100
all_uk$fe_rda<-all_uk$iron/rda$rni_women[rda$nutrient=='iron']*100
all_uk$se_rda<-all_uk$selenium/rda$rni_women[rda$nutrient=='selenium']*100
all_uk$zn_rda<-all_uk$zinc/rda$rni_women[rda$nutrient=='zinc']*100
all_uk$vita_rda<-all_uk$vitamin_a/rda$rni_women[rda$nutrient=='vitamin_a']*100
all_uk$om_rda<-all_uk$omega_3/rda$rni_women[rda$nutrient=='omega_3']*100

## now cap nutrient RDA at 100% (i.e. a species either meets (100%) or doesn't meet (<100%) the RDA)
# TY BEAL: https://twitter.com/TyRBeal/status/1344662877458886656
all_uk$ca_rda[all_uk$ca_rda>100]<-100
all_uk$fe_rda[all_uk$fe_rda>100]<-100
all_uk$se_rda[all_uk$se_rda>100]<-100
all_uk$zn_rda[all_uk$zn_rda>100]<-100
all_uk$vita_rda[all_uk$vita_rda>100]<-100
all_uk$om_rda[all_uk$om_rda>100]<-100

## now add prob nutrient adequacy (mean rda), and price to reach 40% nutrient adequacy
all_uk <- all_uk %>% rowwise() %>%
  mutate(#nut_adq = mean(c(ca_rda, fe_rda, se_rda, zn_rda, vita_rda, om_rda)),
    nut_score = sum(c(ca_rda, fe_rda, se_rda, zn_rda, vita_rda, om_rda))) %>%
  filter(!is.na(nut_score)) %>% 
  filter(!is.na(mid))

pdf(file = 'fig/ghg_nutrient_density_uk.pdf', height=5, width=6)

all_uk %>% group_by(product, farmed_wild) %>% summarise(nut_score = median(nut_score), mid = mean(mid)) %>% 
  ggplot(aes(mid, nut_score, colour=farmed_wild, label=product)) + 
  # geom_point() +
  geom_text() +
  labs(x = 'kg CO2 (midpoint of lower and upper estimates)', y = 'nutrient density (combined % RDA for 5 nutrients)', subtitle='Mean GHG per product') +
  theme(legend.position = 'top')


  ggplot(all_uk, aes(mid, nut_score, colour=farmed_wild, label=product)) + 
  geom_point() +
  labs(x = 'kg CO2 (midpoint of lower and upper estimates)', y = 'nutrient density (combined % RDA for 5 nutrients)', subtitle='All GHG data points') +
  theme(legend.position = 'top')

dev.off()
