library(tidyverse)
library(readxl)
library(janitor)
theme_set(theme_bw())

uk<-read.csv('data/UK_GHG_nutrient_catch.csv') %>% filter(top90==TRUE)

ghg2<-read_excel('data/ghg/seafood_watch_ghg_by_lw_and_edible_JamesRobinson_20221024.xlsx') %>% 
  clean_names() %>% 
  group_by(common_name, scientific_name, species_group, category) %>% 
  summarise(ghg_kg = median(ghg_kg), ghg_100g_ed = median(ghg_100g_ed), edible_fraction = mean(edible_fraction)) %>% 
  mutate(diff = ghg_100g_ed / ghg_kg)


ggplot(ghg2, aes(ghg_kg, ghg_100g_ed, col=category)) + geom_point() +
  ggrepel::geom_text_repel(data= ghg2 %>% filter(diff > 0.2),
                  aes(label = common_name), size=2) +
  labs(x = 'CO2-eq per kg live weight', y = 'CO2-eq per 100 g edible portion', subtitle = 'All species')

ghg3<-ghg2 %>% filter(scientific_name %in% uk$scientific_name)
ggplot(ghg3, aes(ghg_kg, ghg_100g_ed, col=category)) + geom_point() +
  ggrepel::geom_text_repel(data= ghg3,
                           aes(label = common_name), size=2) +
  labs(x = 'CO2-eq per kg live weight', y = 'CO2-eq per 100 g edible portion', subtitle = 'All species')

pdf(file = 'fig/final/Figure_SX_GHG_edible.pdf', height=6, width=8)
ggplot(ghg3, aes(fct_reorder(common_name,ghg_kg), ghg_100g_ed*10)) + 
  geom_segment(aes(fct_reorder(common_name,ghg_kg), ghg_kg, xend=fct_reorder(common_name,ghg_kg), yend=ghg_100g_ed*10), col='grey') +
  geom_point(size=2.5, position = position_dodge(width=0.5), col='#e41a1c') +
  geom_point(size=2.5, aes(fct_reorder(common_name,ghg_kg), ghg_kg), position = position_dodge(width=0.5)) +
  coord_flip() +
  labs(x = '', y='CO2-eq per kg (black) and per kg edible portion (red)') +
  theme(panel.grid.major.y = element_blank()) +
  scale_y_continuous(breaks=seq(0, 60, 5))
dev.off()