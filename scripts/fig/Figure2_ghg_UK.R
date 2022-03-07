source('scripts/fig/00_plotting.R')


drops<-c('Other marine fish')
nut<-read.csv('data/UK_GHG_nutrient_catch.csv') %>% 
  mutate(prop_tot = tot / all * 100, class = ifelse(prop_tot >= 5, 'High production', 'Low production')) %>% 
  filter(top90 == TRUE & !species %in% drops & !is.na(mid)) %>%
  select(-tax) %>% 
  group_by(species, farmed_wild, tot, class) %>% 
  summarise_at(vars(low:nut_score), mean) %>% 
  mutate(species=factor(species), id = paste0(species, ' (', farmed_wild, ')'))


g0<-ggplot(nut, aes(mid,portion_adq, col=farmed_wild)) +  
      ggrepel::geom_label_repel(aes(label = species), seed=4, size=2.5) +
      geom_errorbarh(aes(xmin = low, xmax=max)) +
      geom_point() +
      th+ 
      theme(legend.position = c(0.8, 0.8), legend.title = element_blank()) +
      labs(x = 'C02 emmissions equivalent per kg seafood', y = 'Portion for 40% nutrient adequacy, g') +
      scale_colour_manual(values = colcol2)

## 1. GHG
g1<-ggplot(nut, aes(mid, fct_reorder(id, mid), col=farmed_wild)) + 
  geom_segment(aes(x = low, xend = max, y =  fct_reorder(id, mid), yend= fct_reorder(id, mid))) +
  geom_point(aes(x = mid, y =  fct_reorder(id, mid)), size=3) +
  labs(x = 'CO2 emissions equivalent\nper kg seafood', y ='') +
  facet_grid(rows = vars(class), scales='free_y', space = 'free_y') + 
  scale_colour_manual(values = colcol2) +
  # scale_y_discrete(labels=nut$species[as.factor(levels(fct_reorder(nut$id, nut$mid)))]) +
  th+ 
  theme(legend.position = c(0.8, 0.8), legend.title=element_blank(), strip.text.y = element_blank())

nut2<-nut %>% ungroup() %>%  distinct(nut_score, species, id, mid, class) 
nut3<-nut %>% ungroup() %>% select(species, farmed_wild, class, mid, id, ca_rda:om_rda) %>%   
    pivot_longer(ca_rda:om_rda, names_to = 'nutrient', values_to = 'rda') 

nut3$nutrient<-factor(nut3$nutrient, levels = rev(unique(nut3$nutrient)[c(3,5,2,4,1)]))
nut3$lab<-nut3$nutrient; levels(nut3$lab) <-rev(c('Selenium','Omega-3', 'Iron', 'Zinc','Calcium')) 

nut3<-nut3 %>% group_by(id) %>% 
  arrange(factor(nutrient, levels = rev(levels(nutrient))), .by_group=TRUE) %>% 
  mutate(label_ypos=cumsum(rda) - 0.5*rda)


## 2. nutrient density
g2<-ggplot(nut3, aes(rda, fct_reorder(id, mid), fill=lab)) + 
  geom_bar(stat='identity') +
  geom_text(data = nut3 %>% filter(rda >= 15),
            aes(x = label_ypos, label= paste0(round(rda, 0), '%')),  color="white", size=2) +
  labs(x = 'Nutrient density, %', y ='') +
  facet_grid(rows = vars(class), scales='free_y', space = 'free_y') + 
  scale_fill_manual(values = nut.cols) +
  scale_x_continuous(labels=scales::comma, expand=c(0, 0.06)) +
  th+ 
  theme(plot.margin=unit(c(0.1, 0.5, 0.1, 0.5), 'cm'), 
        axis.ticks = element_blank(), 
        legend.position = 'none', axis.text.y = element_blank(), axis.title.y = element_blank(), strip.text.y = element_blank())

## 3. production 
# g3<-ggplot(nut, aes(tot, fct_reorder(id, mid), fill=farmed_wild)) +
#       geom_bar(stat = 'identity') +
#       labs(x = 'Seafood produced, t', y = '') +
#       facet_grid(rows = vars(class), scales='free_y', space = 'free_y') + 
#       scale_fill_manual(values = colcol2) +
#       scale_x_continuous(labels=scales::comma, expand=c(0, 0.06)) +
#       th+ 
#       theme(plot.margin=unit(c(0.1, 0.5, 0.1, 0.5), 'cm'), 
#             axis.ticks = element_blank(), 
#             legend.position = 'none', 
#             axis.text.y = element_blank(), axis.title.y = element_blank(), strip.text.y = element_text(colour='black',angle=360))

source('scripts/fig/FigureSX_GHG_nut_bysector.R')


pdf(file = 'fig/final/Figure2_UK_profiles.pdf', height=7, width=9)
top<-plot_grid(g1, g2, nrow = 1, align = 'h', rel_widths=c(1, 0.6), labels=c('A', 'B'))
print(
  plot_grid(top, g3, nrow =2, labels=c('', 'C'), rel_heights=c(1, 0.7))
)
dev.off()

pdf(file = 'fig/final/FigureS2_UK_density_CO2.pdf', height=5, width=10)
print(g0)
dev.off()











