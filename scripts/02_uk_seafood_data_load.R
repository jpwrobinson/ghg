pacman::p_load(tidyverse, readxl, janitor)
theme_set(theme_bw())
load('data/nutrient_ghg_species.rds')

land<-read_excel('data/uk/UK_landings_2015_2019.xlsx')  %>% clean_names()  %>% 
		rename_with(~ str_replace_all(.x, 'x', ''), starts_with('x'))

land_tot<-land  %>% group_by(species, name)  %>% 	
			summarise(catch = sum(`2019_landings_tonnes`, na.rm=TRUE))  %>% 
			ungroup()  %>% 
			arrange(desc(catch))  %>% 
			mutate(tc = sum(catch), t80 = cumsum(catch), pos = t80/tc, top80 = ifelse(pos <= 0.8, TRUE, FALSE))

## most species groups have a dominant gear (>90% of catch)

g1<-ggplot(land_tot, aes(fct_reorder(name, catch), catch)) +
		geom_bar(stat = 'identity', aes(fill = top80)) +
		coord_flip() +
		labs(y = 'Landings, live weight (tonnes)', x = '', caption = 'Total UK landings 2015-19, summed across all gears.\nTop 80% of catch highlighted blue.') +
		scale_y_continuous(labels=scales::comma) +
		theme(legend.position = 'none')

pdf(file = 'fig/uk/UK_landed_weight_by_species.pdf', height=8, width=5)
g1
dev.off()


save(land, land_tot, file = 'data/uk_landings.rds')


## now imports
## need to check how CF should be used
imp<-read_excel('data/uk/UK_seafood_imports_2019.xlsx')  %>% clean_names()  %>% 
    mutate(species = case_when(
    str_detect(species, 'ussel') ~ 'Sea mussels',
    str_detect(species, 'eam') ~ 'Bream',
    str_detect(species, 'deep-water rose') ~ 'Shrimp, miscellaneous',
    str_detect(species, 'Shrimp, coldwater') ~ 'Shrimp, miscellaneous',
    str_detect(species, 'Crangon spp') ~ 'Shrimp, miscellaneous',
    TRUE ~ species)) %>% 
			group_by(species, species_group, presentation)  %>% 
			summarise(w = sum(kg)/1e3, value_gbp = sum(value))  %>% 
			group_by(species)  %>% 
			mutate(w_sp = sum(w), value_gbp_sp = sum(value_gbp)) 

imp_post<-read_excel('data/uk/UK_seafood_imports_2019.xlsx')  %>% clean_names()  %>% 
			group_by(species)  %>% 
			summarise(w = sum(kg)/1e3, value_gbp = sum(value))  %>% 
			ungroup()  %>% 
			arrange(desc(w))  %>% 
			mutate(tc = sum(w), t80 = cumsum(w), pos = t80/tc, top80 = ifelse(pos <= 0.8, TRUE, FALSE))

imp_plot<-left_join(imp, imp_post %>% select(species, top80))


g1<-ggplot(imp_plot, aes(fct_reorder(species, w_sp), w)) +
		geom_bar(stat = 'identity', aes(fill =presentation)) +
		geom_text(data = imp_post  %>% filter(top80 == TRUE), 
			aes(species, w, label = paste0(round(pos,2)*100, '%')), hjust=-.3, size=2.5) +
		coord_flip() +
		labs(y = 'Imported live weight (tonnes)', x = '', caption = 'Total UK imports 2019, summed by species.\nTop 80% of catch annotated.') +
		scale_y_continuous(labels=scales::comma,expand=c(0.1, 0.1)) +
		theme(legend.position = c(0.6, 0.4)) 

pdf(file = 'fig/uk/UK_imported_weight_by_species.pdf', height=10, width=5)
g1
dev.off()

##  summarise
imp_post<-imp_post %>% 
  group_by(species)  %>% 
  summarise(w = sum(w)) %>% 
  mutate(farmed_wild = 'Imported')

save(imp, file = 'data/uk_imports.rds')


## now aquaculture
aq<-read.csv('data/uk/aquaculture_production_weight_2015_2018.csv') %>% clean_names() %>% 
        rename(species = species_name) %>% 
        select(species, average_tonnes) %>% 
        mutate(species = recode(species,
                                'Salmonids nei' = 'Other salmonids',
                                'Atlantic salmon' = 'Salmon',
                                'Common carp' = 'Carp',
                                'Cyprinids nei' = 'Carp',
                                'Crucian carp' = 'Carp',
                                'Sea trout' = 'Trout',
                                'Rainbow trout' = 'Trout',
                                'Atlantic halibut' = 'Halibut, Atlantic',
                                "European flat oyster" = 'Oyster',
                                'Freshwater bream' = 'Bream',
                                'Sea mussels nei' = 'Sea mussels',
                                'Great Atlantic scallop' = 'Scallop',
                                'Queen scallop' = 'Scallop',
                                'Nile tilapia' = 'Tilapia',
                                'Northern quahog(=Hard clam)' = "Clam",
                                'Marine crustaceans nei' = 'Other crustaceans',
                                'European seabass' = 'Seabass, European')) %>% 
          group_by(species) %>% summarise(average_tonnes = sum(average_tonnes)) %>% 
          mutate(farmed_wild = 'Farmed')

### now total available seafood

## need to change some species names. priority = make landed species match up with imported
land_19<-read_excel('data/uk/UK_landings_2015_2019.xlsx')  %>% clean_names()  %>% 
		rename_with(~ str_replace_all(.x, 'x', ''), starts_with('x')) %>% 
		filter(!is.na(`2019_landings_tonnes`)) %>% 
		group_by(species, name)  %>% 	
		summarise(catch = sum(`2019_landings_tonnes`))  %>%  
		mutate(species = name) %>% 
		mutate(species = recode(species,  
					'Nephrops (Norway Lobster)' = 'Lobster, Norway',
					"Crabs (C.P.Mixed Sexes)" = 'Crab',
					'Scallops' = 'Scallop',
					"Monks or Anglers" = 'Monk',
					'Plaice' = 'Plaice, European',
					'Saithe' = 'Saithe (=Coalfish)',
					'Horse Mackerel' = 'Horse mackerel, Atlantic',
					'Queen Scallops' = 'Scallop',
					'Sprats' = 'Sprat (=Brisling)',
					'Lobsters' = 'Lobster Homarus spp',
					'Patagonian squid' = 'Squid',
					'Sole' = 'Sole, common',
					"Crabs - Velvet (Swim)" = 'Crab',
					'Lesser Spotted Dog' = 'Dogfish',
					'Thornback Ray' = 'Ray',
					'Blonde Ray' = 'Ray',
					'Brown Shrimps' = 'Shrimps, miscellaneous',
					'Bass' = 'Seabass, European',
					'Catfish' = 'Freshwater catfish'))  %>% 
				group_by(species)  %>% 
				summarise(catch = sum(catch)) %>% 
				mutate(farmed_wild = 'Wild')


## join imports and landings and aquaculture
tot<-full_join(
        full_join(land_19  %>% select(species, catch),
				imp_post %>% select(species, w)),
				aq %>% select(species, average_tonnes))  %>% 
	rename(imported = w, wild = catch, farmed = average_tonnes)  %>% 
  mutate(imported = ifelse(is.na(imported), 0, imported),
         wild = ifelse(is.na(wild), 0, wild),
         farmed = ifelse(is.na(farmed), 0, farmed),
         prop_imported = imported / (imported + wild + farmed)) %>% 
	pivot_longer(-c(species,prop_imported), values_to = 'catch', names_to = 'source')  %>% 
	group_by(species) %>% 
	mutate(tot = sum(catch), id = paste(species, source, sep = '_'))  

## get the species cumulative landings - summed across imports + landings
tot_post<- tot %>% 
	group_by(species)  %>% 
	summarise(tot = sum(catch), prop_imported  = unique(prop_imported)) %>% 
	arrange(desc(tot))  %>% 
	mutate(all = sum(tot), t80 = cumsum(tot), pos = t80/all, 
		top90 = ifelse(pos <= 0.90, TRUE, FALSE))


save(tot, tot_post, file = 'data/uk_seafood.rds')

tot_plot<-left_join(tot, tot_post %>% select(species, top90))

g1<-ggplot(tot_plot  %>% filter(top90 == TRUE), aes(fct_reorder(species, tot), catch, fill=source)) + geom_bar(stat='identity') +
		coord_flip() +
		labs(y = 'Total available live weight (tonnes)', x = '', 
		     caption = 'Total seafood available in UK, summed by species.\nTop 90% species only') +
		scale_y_continuous(labels=scales::comma,expand=c(0.1, 0.1)) +
		theme(legend.position = c(0.6, 0.4)) 

pdf(file = 'fig/uk/UK_available_seafood_top90percent_species.pdf', height=10, width=5)
print(g1)
dev.off()