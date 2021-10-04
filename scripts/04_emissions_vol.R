pacman::p_load(tidyverse, skimr, janitor, cowplot, funk, install=FALSE)
theme_set(theme_sleek())


load('data/nutrient_ghg_species.rds')
all<-read.csv(file = 'data/UK_GHG_nutrient_catch.csv') %>%
  filter(top90==TRUE & !is.na(common_name)) %>% 
  group_by(species, farmed_wild, tot) %>% 
  summarise_at(vars(low:nut_score), mean) 

# estimate the total emissions for each seafood, based on annual production and low/max GHG
## production (tot) is in tonnes, GHG is kg equivalent
all<-all %>% mutate(low_emit = low * tot * 1000, max_emit = max * tot * 1000,
                    mid_emit = mid * tot * 1000,
                    low_diff = low_emit/mid_emit*100,
                    max_diff = max_emit/mid_emit*100) 

g0<-ggplot(all, 
       aes(fct_reorder(species, mid_emit), low_diff), position = position_dodge(width=0.2)) +
      geom_segment(aes(xend = fct_reorder(species, mid_emit), y = low_diff, yend = max_diff), col='grey') +
      geom_point(fill='#377eb8', size=3,pch=21)+
      geom_point(aes(y = max_diff), fill='#e41a1c', size=3,pch=21) +
      coord_flip() +
      facet_wrap(~farmed_wild) +
      th +
      labs(x = '', y = 'GHG emissions for lowest and highest emitting production methods, %')


pdf(file = 'fig/final/Figure4_emissions_change.pdf', height=7, width=10)
print(g0)
dev.off()