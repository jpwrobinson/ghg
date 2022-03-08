
source('scripts/fig/Figure5_radar.R')
source('scripts/fig/00_plotting.R')

drops<-c('Other marine fish')
nut<-read.csv('data/UK_GHG_nutrient_catch.csv') %>% 
  mutate(prop_tot = tot / all * 100, class = ifelse(prop_tot >= 5, 'High production', 'Low production')) %>% 
  filter(top90 == TRUE & !species %in% drops & !is.na(mid)) %>%
  select(-tax) %>% 
  group_by(species, farmed_wild, tot, class) %>% 
  summarise_at(vars(low:nut_score), mean) %>% 
  mutate(species=factor(species), id = paste0(species, ' (', farmed_wild, ')'))



nutS<-read.csv('data/UK_GHG_nutrient_catch_bysector.csv') %>% 
  filter(species %in% nut$species) %>%
  select(-tax) %>% 
  group_by(species, farmed_wild, tot, catch, source) %>% 
  summarise_at(vars(low:nut_score), mean) %>% 
  mutate(species=factor(species), id = paste0(species, ' (', farmed_wild, ')')) %>% 
  mutate(source = case_when(
			str_detect(source, 'farmed') ~ 'UK',
  			str_detect(source, 'wild') ~ 'UK',
  			str_detect(source, 'imported') ~ 'Imported'))

nutS$source<-factor(nutS$source, levels=unique(nutS$source)[c(1,2)])

# ## 1. GHG
# g1<-ggplot(nutS, aes(mid, fct_reorder(id, mid), col=farmed_wild)) + 
#   geom_segment(aes(x = low, xend = max, y =  fct_reorder(id, mid), yend= fct_reorder(id, mid))) +
#   geom_point(aes(x = mid, y =  fct_reorder(id, mid)), size=3) +
#   labs(x = 'CO2 emissions equivalent\nper kg seafood', y ='') +
#   facet_grid(rows = vars(source), scales='free_y', space = 'free_y') + 
#   scale_colour_manual(values = colcol2) +
#   # scale_y_discrete(labels=nut$species[as.factor(levels(fct_reorder(nut$id, nut$mid)))]) +
#   th+ 
#   theme(legend.position = c(0.8, 0.8), legend.title=element_blank(), strip.text.y = element_blank())


# nutS3<-nutS %>% ungroup() %>% select(species, farmed_wild, source, mid, id, ca_rda:om_rda) %>%   
#     pivot_longer(ca_rda:om_rda, names_to = 'nutrient', values_to = 'rda') 

# nutS3$nutrient<-factor(nutS3$nutrient, levels = rev(unique(nutS3$nutrient)[c(3,5,2,4,1)]))
# nutS3$lab<-nutS3$nutrient; levels(nutS3$lab) <-rev(c('Selenium','Omega-3', 'Iron', 'Zinc','Calcium')) 

# nutS3<-nutS3 %>% group_by(id) %>% 
#   arrange(factor(nutrient, levels = rev(levels(nutrient))), .by_group=TRUE) %>% 
#   mutate(label_ypos=cumsum(rda) - 0.5*rda)


# ## 2. nutrient density
# g2<-ggplot(nutS3, aes(rda, fct_reorder(id, mid), fill=lab)) + 
#   geom_bar(stat='identity') +
#   # geom_text(data = nutS3 %>% filter(rda >= 15),
#   #           aes(x = label_ypos, label= paste0(round(rda, 0), '%')),  color="white", size=2) +
#   labs(x = 'Nutrient density, %', y ='') +
#   facet_grid(rows = vars(source), scales='free_y', space = 'free_y') + 
#   scale_fill_manual(values = nut.cols) +
#   scale_x_continuous(labels=scales::comma, expand=c(0, 0.06)) +
#   th+ 
#   theme(plot.margin=unit(c(0.1, 0.5, 0.1, 0.5), 'cm'), 
#         axis.ticks = element_blank(), 
#         legend.position = 'none', axis.text.y = element_blank(), axis.title.y = element_blank(), strip.text.y = element_blank())

## 3. production 
g3<-ggplot(nutS, aes(catch, fct_reorder(species, tot), fill=farmed_wild)) +
      geom_bar(stat = 'identity') +
      labs(x = 'Seafood produced, t', y = '') +
      facet_grid(cols = vars(source), scales='free_y', space = 'free_y') + 
      scale_fill_manual(values = colcol2) +
      scale_x_continuous(labels=scales::comma, expand=c(0, 0.06)) +
      th+ 
      theme(plot.margin=unit(c(0.1, 0.5, 0.1, 0.5), 'cm'), 
            axis.ticks = element_blank(), 
            legend.position = c(0.95,0.2), 
            panel.grid.major.x = element_line(colour ='grey'),
            # axis.text.y = element_blank(), 
            axis.title.y = element_blank(), strip.text.x = element_text(colour='black',angle=360))



pdf(file = 'fig/final/FigureS2_UK_profiles_bysector.pdf', height=4, width=12)
# print(
#   plot_grid(g1, g2, g3, nrow = 1, align = 'h', rel_widths=c(1, 0.6, 0.8), labels=c('A', 'B', 'C'))
# )
g3

dev.off()