pacman::p_load(tidyverse, skimr, janitor, cowplot, funk, install=FALSE)
theme_set(theme_sleek())

source('scripts/fig/00_plotting.R')


drops<-c('Other marine fish')
nut<-read.csv('data/UK_GHG_nutrient_catch.csv') %>% 
  mutate(prop_tot = tot / all * 100, class = ifelse(prop_tot >= 5, 'High production', 'Low production')) %>% 
  filter(top90 == TRUE & !species %in% drops & !is.na(mid)) %>%
  select(-tax) %>% 
  rowwise() %>% 
  ## redo nut_score
  mutate(nut_score = sum(c(ca_rda, fe_rda, se_rda, zn_rda, om_rda, iodine_rda, vita_rda, vitd_rda, vitb12_rda, folate_rda)),
        nut_score2 = sum(c(ca_rda, fe_rda, se_rda, zn_rda, om_rda))) %>% 
  group_by(species, farmed_wild, tot, class) %>% 
  summarise_at(vars(low:nut_score2, vitamin_d:iodine_rda), mean) %>% 
  mutate(species=factor(species), id = paste0(species, ' (', farmed_wild, ')'))


## read export data
ex<-readxl::read_excel('data/uk/uk_export.xlsx') %>% clean_names() %>% 
	pivot_longer(-c(species, indicator), names_to = 'year', values_to='value') %>% 
	mutate(year = as.numeric(str_replace_all(year, 'x', ''))) %>% 
	filter(indicator == 'live_weight_tonnes') %>% 
	group_by(species, indicator) %>% 
	summarise(value = mean(value))


## order according to nut GHG
ex$species<-factor(ex$species, levels=levels(fct_reorder(nut$species, nut$mid)[!duplicated(fct_reorder(nut$species, nut$mid))]))

## 3. production 
gg<-ggplot(ex, aes(value, species)) +
      geom_bar(stat = 'identity', fill='grey50') +
      labs(x = expression(tonnes~yr^{'-1'}), y = '', parse=TRUE) +
      scale_x_continuous(labels=scales::comma, expand=c(0, 0.06)) +
      th+ 
      theme(plot.margin=unit(c(0.1, 0.5, 0.1, 0.5), 'cm'), 
            axis.ticks = element_blank(), 
            legend.position = c(0.85,0.5), 
            panel.grid.major.x = element_line(size=0.2, colour ='grey'),
            # axis.text.y = element_blank(), 
            axis.title.y = element_blank(), strip.text.y = element_blank())

gg