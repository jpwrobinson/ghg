pacman::p_load(tidyverse, skimr, janitor, cowplot, funk, ggrepel, scales, ggradar, install=FALSE)
theme_set(theme_sleek())
source('scripts/fig/00_plotting.R')

# library(tabulizer)

# # Extract the table
# tabel <- extract_tables('data/uk/Guillen2019_seafood_footprint.pdf')
# # Extract the first element of the variable
# head(tabel[[2]])
# write.csv(tabel[[1]], file = 'data/uk/guillen1.csv')
# write.csv(rbind(tabel[[2]], tabel[[3]]), file = 'data/uk/guillen2.csv')


## put together in excel, into:
dat<-read.csv('data/uk/guillen_extracted.csv') %>% janitor::clean_names() %>% 
		pivot_longer(-c(country, code, total_consumption_footprint_tonnes), names_to = 'footprint', values_to = 'value') %>% 
		mutate(lab = str_replace_all(footprint, '_consumption_footprint_per_capita_kg', ''))

dat$footprint<-factor(dat$footprint, levels=unique(dat$footprint)[c(1,2,3,4)])
dat$lab<-factor(dat$lab, levels=unique(dat$lab)[c(1,2,3,4)])

pdf(file = 'fig/national_seafood_consumption_footprint.pdf', height=15, width=14)
ggplot(dat %>% filter(country!='World'), 
			aes(fct_reorder(country,total_consumption_footprint_tonnes), value, fill=lab)) +
	geom_bar(stat='identity') +
	facet_grid(~lab) +
	scale_y_continuous(expand=c(0,0)) +
	coord_flip() +
	labs(x= '',y = 'kg per capita') +
	theme(legend.position = 'none')
dev.off()