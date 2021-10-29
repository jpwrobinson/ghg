pacman::p_load(tidyverse, skimr, janitor, cowplot, funk, install=FALSE)
theme_set(theme_sleek())


load('data/nutrient_ghg_species.rds')
all<-read.csv(file = 'data/UK_GHG_nutrient_catch.csv') %>%
  filter(top90==TRUE & !is.na(common_name)) %>% 
  group_by(species, scientific_name, farmed_wild, tot) %>% 
  summarise_at(vars(low:nut_score), mean) 

# estimate the total emissions for each seafood, based on annual production and low/max GHG
## production (tot) is in tonnes, GHG is kg equivalent
all<-all %>% mutate(low_emit = low * tot * 1000, max_emit = max * tot * 1000,
                    mid_emit = mid * tot * 1000,
                    low_diff = low_emit/mid_emit*100,
                    max_diff = max_emit/mid_emit*100,
                    range = max - low,
                    prop = (max-low)/low*100) 

# select species where the lowest GHG production is a tenfold improvement on the highest GHG production
fold<-all %>% filter(prop >= 100)
load('data/ghg_all_dal_data.rds')
ghg <- ghg %>% filter(scientific_name %in% fold$scientific_name) %>% 
        mutate(ghg_mid = (ghg_low + ghg_high) / 2,
               id = paste(common_name, gear))

ghg_mean<-ghg %>% group_by(common_name) %>% summarise(ghg = mean(ghg_mid))

pos<-position_dodge(width=0.4)
g0<-ggplot(ghg_mean, 
      # geom_segment(aes(xend = fct_reorder(species, mid_emit), y = low_diff, yend = max_diff), col='grey') +
       aes(fct_reorder(common_name, ghg_mid), ghg_mid), position = pos) +
      # geom_pointrange(aes(ymin = ghg_low, ymax = ghg_high), position = pos, fill='#377eb8', size=2,pch=21)+
      geom_point(data = ghg, aes(y = ghg_high), fill='#377eb8', position = pos, size=2,pch=21)+
      geom_point(data = ghg, aes(y = ghg_low), fill='#377eb8', position = pos, size=2,pch=21)+
      # coord_flip() +
      # facet_wrap(~farmed_wild) +
      th +
      labs(x = '', y = 'GHG emissions for lowest and highest emitting production methods, %')


pdf(file = 'fig/final/Figure4_emissions_change.pdf', height=7, width=10)
print(g0)
dev.off()