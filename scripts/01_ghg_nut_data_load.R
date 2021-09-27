pacman::p_load(tidyverse, skimr, janitor, cowplot, funk, install=FALSE)
theme_set(theme_sleek())
source('scripts/rda_reader.R')
#### GHG data


#### Nutrient data
nut<-read.csv('data/nut/Species_Nutrient_Predictions.csv') %>%
      left_join(read.csv('data/nut/all_traits_active.csv') %>% clean_names()) %>% 
			select(species, family, ends_with('mu')) %>%
			rename(scientific_name = species) %>%
			select(-Protein_mu) %>% 
      mutate(tax = 'Fish') 

nut_fam<-nut %>% group_by(family) %>% 
      summarise_at(vars(Selenium_mu:Vitamin_A_mu), mean) %>% 
      mutate(tax = 'Fish')

nut_inv<-read.csv('data/nut/invertebrate_SaU_species_nutrient_list_withCI.csv') %>% 
        select(scientific_name, common_name, family, ends_with('g'), -protein.g) %>% 
        rename(Selenium_mu = selenium.mcg, Zinc_mu = zinc.mg, Iron_mu = iron.mg, Calcium_mu = calcium.mg,
               Omega_3_mu = omega3.g) %>% 
        mutate(Vitamin_A_mu = NA, tax = 'Invertebrate') 

## read species level invert nutrients
nut_inv_high<-read.csv('data/nut/biocomp_hicks_03032020.csv') %>% filter(BiblioID != 'fi165')
cn<-colnames(nut_inv_high)
cn<-str_replace_all(cn, '\\(', '_')
cn<-str_replace_all(cn, '\\)', '')
colnames(nut_inv_high)<-cn

## nutrients of interest, raw meat and wild caught
## matching species-level estimates
nut_inv_high <- nut_inv_high %>% filter(Subgroup %in% c('Crustacean', 'Molluscs'),
                             Processing == 'r' & Type == 'W') %>%
  filter(!Scientific.name == '') %>%
  select(Scientific.name, PROTCNT.g., CA.mg., FE.mg., SE.mcg., 
         ZN.mg., VITA_RAE.mcg., FAPUN3.g.) %>% 
  gather(nutrient, value, PROTCNT.g.:FAPUN3.g.) %>%
  filter(!value %in% c('', 'tr', 'nd'))

nut_inv_high$value<-str_replace_all(nut_inv_high$value, '\\[', '')
nut_inv_high$value<-str_replace_all(nut_inv_high$value, '\\]', '')
nut_inv_high$value<-as.numeric(nut_inv_high$value)

## estimate species mean
nut_inv_high<-nut_inv_high %>% 
  group_by(Scientific.name, nutrient) %>% 
  summarise(value = mean(value, na.rm=TRUE)) %>% 
  pivot_wider(names_from = nutrient, values_from = value) %>% 
  rename(scientific_name = Scientific.name,
         Calcium_mu = CA.mg., Iron_mu = FE.mg., Selenium_mu = SE.mcg., Zinc_mu = ZN.mg., Omega_3_mu = FAPUN3.g.) %>% 
  mutate(Vitamin_A_mu = NA, tax = 'Invertebrate', family=NA)

#estimate family mean
nut_inv_fam<-nut_inv %>%
      group_by(family) %>% summarise_at(vars(Iron_mu:Vitamin_A_mu), mean) %>% 
      mutate(Vitamin_A_mu = NA, tax = 'Invertebrate')

# add family to species-level nut inv
nut_inv_high$family<-nut_inv$family[match(nut_inv_high$scientific_name, nut_inv$scientific_name)]

nut<-rbind(nut, nut_inv_high %>% select(names(nut)))
nut_coarse<-rbind(nut, nut_inv %>% select(names(nut)))
nut_fam<-rbind(nut_fam, nut_inv_fam %>% select(names(nut_fam)))

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
ghg <- ghg %>% mutate(scientific_name = recode(scientific_name, 
                                'Theragra chalcogramma' = 'Gadus chalcogrammus',
																'Seriola quinqueriadata' = 'Seriola quinqueradiata',
																'Psetta maxima' = 'Scophthalmus maximus', 
																'Pampus' = 'Pampus argenteus'))

pdf(file = 'fig/ghg/ghg_all_species.pdf', height=8, width=8)
ggplot(ghg, aes(common_name, ymin = ghg_low, ymax = ghg_high, col=farmed_wild)) + 
			geom_errorbar() +
			coord_flip() + 
			facet_grid(~data_certainty)
dev.off()

## get midpoint of GHG range for each species, separate farm + wild
ghg<-ghg %>% group_by(common_name, scientific_name, farmed_wild) %>%
			summarise(low = min(ghg_low), max = max(ghg_high)) %>%
			mutate(mid = low + ((max - low)/2))

## join nutrients (species-level)
all<-ghg %>% left_join(nut)

## add some family info
all<-all %>% 
  mutate(family = case_when(
    str_detect(scientific_name, 'Portunus') ~ 'Portunidae',
    str_detect(scientific_name, 'Penaeus') ~ 'Penaeidae',
    str_detect(scientific_name, 'Macrobrachium rosenbergii') ~ 'Palaemonidae',
    str_detect(scientific_name, 'Lutjanus') ~ 'Lutjanidae', 
    str_detect(scientific_name, 'Nemipterus') ~ 'Nemipteridae',
    TRUE ~ family))

## check missing species (no species-level nut data for inverts)
missing<-all %>% filter(is.na(tax)) 

## add family data, rebind
fam_level_nut<-left_join(missing %>% select(common_name:family), nut_fam, by='family') %>% mutate(nutrient_source = 'Family-level')
all<-rbind(all %>% filter(!is.na(tax)) %>% mutate(nutrient_source = 'Species-level'), fam_level_nut)

# add class data and rebind
# missing<-all %>% filter(is.na(nutrient_source)) 
# class_level_nut<-left_join(missing %>% select(common_name:mid), nut_coarse) %>% mutate(nutrient_source = 'Class-level')
# all<-rbind(all %>% filter(!is.na(tax)), class_level_nut)

## still some nutrients missing for species-level nut inverts
all %>% filter(nutrient_source=='Species-level' & tax =='Invertebrate') %>% 
      filter_at(vars(Selenium_mu:Vitamin_A_mu), any_vars(is.na(.)))

## fill in NAs by each nutrient
all$Selenium_mu[is.na(all$Selenium_mu)]<-nut_inv$Selenium_mu[match(all$scientific_name[is.na(all$Selenium_mu)], nut_inv$scientific_name)]
all$Zinc_mu[is.na(all$Zinc_mu)]<-nut_inv$Zinc_mu[match(all$scientific_name[is.na(all$Zinc_mu)], nut_inv$scientific_name)]
all$Omega_3_mu[is.na(all$Omega_3_mu)]<-nut_inv$Omega_3_mu[match(all$scientific_name[is.na(all$Omega_3_mu)], nut_inv$scientific_name)]
all$Calcium_mu[is.na(all$Calcium_mu)]<-nut_inv$Calcium_mu[match(all$scientific_name[is.na(all$Calcium_mu)], nut_inv$scientific_name)]
all$Iron_mu[is.na(all$Iron_mu)]<-nut_inv$Iron_mu[match(all$scientific_name[is.na(all$Iron_mu)], nut_inv$scientific_name)]

all %>% filter(is.na(Selenium_mu))

## export species without nutrient estimates (should be zero)
write.csv(
  all %>% filter(is.na(Selenium_mu)) %>% ungroup() %>% distinct(common_name, scientific_name) %>% data.frame(),
  file = 'data/nut/missing_nutrient_species.csv', row.names=FALSE)

## long format
all <- all %>% pivot_longer(Selenium_mu:Vitamin_A_mu, names_to = 'nutrient', values_to = 'conc')

pdf(file = 'fig/ghg/ghg_by_nutrients.pdf', height=3, width=15)
ggplot(all, aes(mid, conc, colour=farmed_wild)) + 
  geom_point() +
  labs(x = 'kg CO2 (midpoint of lower and upper estimates)', y = 'nutrient concentration per 100g') +
  facet_wrap(~nutrient, scales='free_y', nrow=1) +
  theme(legend.position = 'top')
dev.off()

all<-all %>% mutate(nutrient = tolower(str_replace_all(nutrient, '_mu', ''))) %>%  
  pivot_wider(names_from = 'nutrient', values_from = 'conc')
all$ca_rda<-all$calcium/rda$rni_women[rda$nutrient=='calcium']*100
all$fe_rda<-all$iron/rda$rni_women[rda$nutrient=='iron']*100
all$se_rda<-all$selenium/rda$rni_women[rda$nutrient=='selenium']*100
all$zn_rda<-all$zinc/rda$rni_women[rda$nutrient=='zinc']*100
all$vita_rda<-all$vitamin_a/rda$rni_women[rda$nutrient=='vitamin_a']*100
all$om_rda<-all$omega_3/rda$rni_women[rda$nutrient=='omega_3']*100

## now cap nutrient RDA at 100% (i.e. a species either meets (100%) or doesn't meet (<100%) the RDA)
# TY BEAL: https://twitter.com/TyRBeal/status/1344662877458886656
all$ca_rda[all$ca_rda>100]<-100
all$fe_rda[all$fe_rda>100]<-100
all$se_rda[all$se_rda>100]<-100
all$zn_rda[all$zn_rda>100]<-100
all$vita_rda[all$vita_rda>100]<-100
all$om_rda[all$om_rda>100]<-100

## now add prob nutrient adequacy (mean rda), and price to reach 40% nutrient adequacy
all <- all %>% rowwise() %>%
  mutate(nut_adq = mean(c(ca_rda, fe_rda, se_rda, zn_rda, om_rda)),
         portion_adq = 40 / nut_adq * 100,
    nut_score = sum(c(ca_rda, fe_rda, se_rda, zn_rda, om_rda))) #%>%
  # filter(!is.na(nut_score)) %>% 
  # filter(!is.na(mid))

## export species with nutrient estimates
save(all, file = 'data/nutrient_ghg_species.rds')

# ------- END OF DATA CLEAN AND EXPORT ------------ #

## Now figures exploring GHG
## species filter for UK products
# uk_prods<-c("Anchovy", "Herring", "Mackerel", "Sardine", "Tuna", "Crab", "Nephrops", "Prawn", "Scallop",
#             "Alaska Pollock", "Cod", "Haddock", "Atlantic Salmon", "Mussels", "Prawns_warm", "Pangasius", "Tilapia", "Sea bass")
# 
# ## fuzzy grepl on UK common and scientific names in ghg dataset
# all_uk<-all %>% filter(grepl(paste(uk_prods, collapse="|"), common_name, ignore.case = TRUE) |
#                          grepl(paste(uk_prods, collapse="|"), scientific_name, ignore.case = TRUE))
# 
# all_uk$product<-NA
# for(i in 1:length(uk_prods)){
#   all_uk$product[grep(uk_prods[i] , all_uk$common_name, ignore.case = TRUE)]<-uk_prods[i]
# }
# 
# ## need invert nutrients
# 
# pdf(file = 'fig/ghg/ghg_nutrients_uk.pdf', height=3, width=15)
# ggplot(all_uk %>%  filter(!is.na(conc)), aes(mid, conc, colour=farmed_wild)) + 
#   geom_point() +
#   labs(x = 'kg CO2 (midpoint of lower and upper estimates)', y = 'nutrient concentration per 100g') +
#   facet_wrap(~nutrient, scales='free_y', nrow=1) +
#   theme(legend.position = 'top')
# 
# all_uk %>% group_by(product, farmed_wild, nutrient) %>% summarise(conc = median(conc), mid = median(mid)) %>% 
#   filter(!is.na(conc)) %>%
#   ggplot(aes(mid, conc, colour=farmed_wild, label=product)) + 
#   # geom_point() +
#   geom_text() +
#   labs(x = 'kg CO2 (midpoint of lower and upper estimates)', y = 'nutrient concentration per 100g') +
#   facet_wrap(~nutrient, scales='free_y', nrow=1) +
#   theme(legend.position = 'top')
# 
# dev.off()
# 
# 
# 
# ## nutrient density
# 
# all_uk<-all_uk %>% mutate(nutrient = tolower(str_replace_all(nutrient, '_mu', ''))) %>%  
#         pivot_wider(names_from = 'nutrient', values_from = 'conc')
# all_uk$ca_rda<-all_uk$calcium/rda$rni_women[rda$nutrient=='calcium']*100
# all_uk$fe_rda<-all_uk$iron/rda$rni_women[rda$nutrient=='iron']*100
# all_uk$se_rda<-all_uk$selenium/rda$rni_women[rda$nutrient=='selenium']*100
# all_uk$zn_rda<-all_uk$zinc/rda$rni_women[rda$nutrient=='zinc']*100
# all_uk$vita_rda<-all_uk$vitamin_a/rda$rni_women[rda$nutrient=='vitamin_a']*100
# all_uk$om_rda<-all_uk$omega_3/rda$rni_women[rda$nutrient=='omega_3']*100
# 
# ## now cap nutrient RDA at 100% (i.e. a species either meets (100%) or doesn't meet (<100%) the RDA)
# # TY BEAL: https://twitter.com/TyRBeal/status/1344662877458886656
# all_uk$ca_rda[all_uk$ca_rda>100]<-100
# all_uk$fe_rda[all_uk$fe_rda>100]<-100
# all_uk$se_rda[all_uk$se_rda>100]<-100
# all_uk$zn_rda[all_uk$zn_rda>100]<-100
# all_uk$vita_rda[all_uk$vita_rda>100]<-100
# all_uk$om_rda[all_uk$om_rda>100]<-100
# 
# ## now add prob nutrient adequacy (mean rda), and price to reach 40% nutrient adequacy
# all_uk <- all_uk %>% rowwise() %>%
#   mutate(#nut_adq = mean(c(ca_rda, fe_rda, se_rda, zn_rda, vita_rda, om_rda)),
#     nut_score = sum(c(ca_rda, fe_rda, se_rda, zn_rda, vita_rda, om_rda))) %>%
#   filter(!is.na(nut_score)) %>% 
#   filter(!is.na(mid))
# 
# pdf(file = 'fig/ghg/ghg_nutrient_density_uk.pdf', height=5, width=6)
# 
# all_uk %>% group_by(product, farmed_wild) %>% summarise(nut_score = median(nut_score), mid = mean(mid)) %>% 
#   ggplot(aes(mid, nut_score, colour=farmed_wild, label=product)) + 
#   # geom_point() +
#   geom_text() +
#   labs(x = 'kg CO2 (midpoint of lower and upper estimates)', y = 'nutrient density (combined % RDA for 5 nutrients)', subtitle='Mean GHG per product') +
#   theme(legend.position = 'top')
# 
# 
#   ggplot(all_uk, aes(mid, nut_score, colour=farmed_wild, label=product)) + 
#   geom_point() +
#   labs(x = 'kg CO2 (midpoint of lower and upper estimates)', y = 'nutrient density (combined % RDA for 5 nutrients)', subtitle='All GHG data points') +
#   theme(legend.position = 'top')
# 
# dev.off()
