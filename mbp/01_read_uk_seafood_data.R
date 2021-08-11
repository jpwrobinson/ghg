
pacman::p_load(tidyverse, readxl, janitor)
theme_set(theme_bw())

land<-read_excel('UK_landings_2015_2019.xlsx')  %>% clean_names()  %>% 
		rename_with(~ str_replace_all(.x, 'x', ''), starts_with('x'))

land_tot<-land  %>% group_by(species, name)  %>% 	
			summarise(catch = sum(sum_of_total_live_weight_landed_tonnes))  %>% 
			ungroup()  %>% 
			arrange(desc(catch))  %>% 
			mutate(tc = sum(catch), t80 = cumsum(catch), pos = t80/tc, top80 = ifelse(pos <= 0.8, TRUE, FALSE))

value_tot<-land  %>% group_by(species, name)  %>% 	
			summarise(value = sum(sum_of_total_value_of_landings_euro))  %>% 
			ungroup()  %>% 
			arrange(desc(value))  %>% 
			mutate(tc = sum(value), t80 = cumsum(value), pos = t80/tc, top80 = ifelse(pos <= 0.8, TRUE, FALSE))

## most species groups have a dominant gear (>90% of catch)

g1<-ggplot(land_tot, aes(fct_reorder(name, catch), catch)) +
		geom_bar(stat = 'identity', aes(fill = top80)) +
		coord_flip() +
		labs(y = 'Landings, live weight (tonnes)', x = '', caption = 'Total UK landings 2015-19, summed across all gears.\nTop 80% of catch highlighted blue.') +
		scale_y_continuous(labels=scales::comma) +
		theme(legend.position = 'none')

pdf(file = 'UK_landed_weight_by_species.pdf', height=8, width=5)
g1
dev.off()

g2<-ggplot(value_tot, aes(fct_reorder(name, value), value)) +
		geom_bar(stat = 'identity', aes(fill = top80)) +
		coord_flip() +
		labs(y = 'Value, live weight ($ euros)', x = '', caption = 'Total UK landings 2015-19, summed across all gears.\nTop 80% of catch highlighted blue.') +
		scale_y_continuous(labels=scales::comma) +
		theme(legend.position = 'none')

pdf(file = 'UK_landed_value_by_species.pdf', height=8, width=5)
g2
dev.off()

save(land, land_tot, value_tot, file = 'uk_landings.rds')


## now imports
## need to check how CF should be used
imp<-read_excel('UK_seafood_imports_2019.xlsx')  %>% clean_names()  %>% 
			group_by(species, species_group, presentation)  %>% 
			summarise(w = sum(kg), value_gbp = sum(value))  %>% 
			group_by(species)  %>% 
			mutate(w_sp = sum(w), value_gbp_sp = sum(value_gbp)) 

imp_post<-read_excel('UK_seafood_imports_2019.xlsx')  %>% clean_names()  %>% 
			group_by(species)  %>% 
			summarise(w = sum(kg), value_gbp = sum(value))  %>% 
			ungroup()  %>% 
			arrange(desc(w))  %>% 
			mutate(tc = sum(w), t80 = cumsum(w), pos = t80/tc, top80 = ifelse(pos <= 0.8, TRUE, FALSE))

imp_plot<-left_join(imp, imp_post %>% select(species, top80))


g1<-ggplot(imp_plot, aes(fct_reorder(species, w_sp), w/1e3)) +
		geom_bar(stat = 'identity', aes(fill =presentation)) +
		geom_text(data = imp_post  %>% filter(top80 == TRUE), 
			aes(species, w/1e3, label = paste0(round(pos,2)*100, '%')), hjust=-.3, size=2.5) +
		coord_flip() +
		labs(y = 'Imported live weight (tonnes)', x = '', caption = 'Total UK imports 2019, summed by species.\nTop 80% of catch annotated.') +
		scale_y_continuous(labels=scales::comma,expand=c(0.1, 0.1)) +
		theme(legend.position = c(0.6, 0.4)) 

pdf(file = 'UK_imported_weight_by_species.pdf', height=8, width=5)
g1
dev.off()