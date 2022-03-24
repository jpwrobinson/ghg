source('scripts/fig/00_plotting.R')


drops<-c('Other marine fish')
nut<-read.csv('data/UK_GHG_nutrient_catch.csv') %>% 
  mutate(prop_tot = tot / all * 100, class = ifelse(prop_tot >= 5, 'High production', 'Low production')) %>% 
  filter(top90 == TRUE & !species %in% drops & !is.na(mid)) %>%
  select(-tax) %>% 
  ## redo nut_score
  mutate(nut_score = sum(c(ca_rda, fe_rda, se_rda, zn_rda, om_rda, vita_rda, vitd_rda, vitb12_rda, folate_rda))) %>% 
  group_by(species, farmed_wild, tot, class) %>% 
  summarise_at(vars(low:nut_score, vitamin_d:folate_rda), mean) %>% 
  mutate(species=factor(species), id = paste0(species, ' (', farmed_wild, ')'))


nutS<-read.csv('data/UK_GHG_nutrient_catch_bysector.csv') %>% 
  filter(species %in% nut$species) %>%
  select(-tax) %>% 
  group_by(species, farmed_wild, tot, catch, source) %>% 
  mutate(species=factor(species), id = paste0(species, ' (', farmed_wild, ')')) %>% 
  mutate(source = case_when(
      str_detect(source, 'farmed') ~ 'Farmed (UK)',
        str_detect(source, 'wild') ~ 'Wild (UK)',
        str_detect(source, 'imported') ~ 'Imported'))

nutS$source<-factor(nutS$source, levels=unique(nutS$source)[c(2,1,3)])
nutS$class<-nut$class[match(nutS$species, nut$species)]
nutS<-nutS %>% group_by(species, class, source, tot, mid) %>% summarise(catch = sum(catch))

nutS$species<-factor(nutS$species, levels=levels(fct_reorder(nut$species, nut$mid)[!duplicated(fct_reorder(nut$species, nut$mid))]))


## 1. GHG
g1<-ggplot(nut, aes(mid, fct_reorder(species, mid), col=farmed_wild)) + 
  geom_segment(aes(x = low, xend = max, y =  fct_reorder(species, mid), yend= fct_reorder(species, mid))) +
  geom_point(data = nut %>% filter(species != 'Sea mussels'),aes(x = mid, y =  fct_reorder(species, mid)), size=3,) +
  geom_point(data =nut %>% filter(species == 'Sea mussels'),
    aes(x = mid, y =  fct_reorder(species, mid)), size=3, position = position_dodge(width=0.5)) +
  labs(x = expression(paste(kg~CO[2],'-',eq)), y ='') +
  facet_grid(rows = vars(class), scales='free_y', space = 'free_y') + 
  scale_colour_manual(values = colcol2) +
  # scale_y_discrete(labels=nut$species[as.factor(levels(fct_reorder(nut$id, nut$mid)))]) +
  scale_x_continuous(expand=c(0.01, 0.01)) +
  th+ 
  theme(legend.position = c(0.8, 0.2), panel.grid.major.x = element_line(size=0.2, colour ='grey'),
    legend.title=element_blank(), strip.text.y = element_blank())

nut3<-nut %>% ungroup() %>% group_by(species, class) %>% 
    summarise_at(vars(ca_rda:om_rda, vita_rda:folate_rda), mean) %>%   
    pivot_longer(ca_rda:folate_rda, names_to = 'nutrient', values_to = 'rda') 

nut3$nutrient<-factor(nut3$nutrient, levels = rev(unique(nut3$nutrient)[c(8,3,5,7,4,2,1,9,6)]))
nut3$lab<-nut3$nutrient; levels(nut3$lab) <-rev(c('Vitamin B12', 'Selenium','Omega-3', 'Vitamin D', 'Zinc', 'Iron','Calcium','Folate', 'Vitamin A')) 

nut3<-nut3 %>% group_by(species) %>% 
  arrange(factor(nutrient, levels = rev(levels(nutrient))), .by_group=TRUE) %>% 
  mutate(label_ypos=cumsum(rda) - 0.5*rda)

nut3$species<-factor(nut3$species, levels=levels(fct_reorder(nut$species, nut$mid)[!duplicated(fct_reorder(nut$species, nut$mid))]))

## 2. nutrient density
g2<-ggplot(nut3, aes(rda, species, col=lab, fill=lab)) + 
  geom_bar(stat='identity') +
  geom_text(data = nut3 %>% filter(rda >= 15),
            aes(x = label_ypos, label= paste0(round(rda, 0), '%')),  color="white", size=2) +
  labs(x = 'Nutrient density, %', y ='') +
  facet_grid(rows = vars(class), scales='free_y', space = 'free_y') + 
  scale_fill_manual(values = nut.cols2) +
  scale_colour_manual(values = nut.cols2) +
  scale_x_continuous(labels=scales::comma, expand=c(0, 0.06)) +
  th+ 
  theme(plot.margin=unit(c(0.1, 0.5, 0.1, 0.5), 'cm'), 
        axis.ticks = element_blank(), 
        legend.position = 'none', axis.text.y = element_blank(), 
        axis.title.y = element_blank(), strip.text.y = element_blank())

## 3. production 
g3<-ggplot(nutS, aes(catch, species, fill=source)) +
      geom_bar(stat = 'identity') +
      labs(x = expression(tonnes~yr^{'-1'}), y = '', parse=TRUE) +
      # facet_grid(cols = vars(source), scales='free_y', space = 'free_y') + 
      facet_grid(rows = vars(class), scales='free_y', space = 'free_y') + 
      scale_fill_manual(values = colcol3) +
      scale_x_continuous(labels=scales::comma, expand=c(0, 0.06)) +
      th+ 
      theme(plot.margin=unit(c(0.1, 0.5, 0.1, 0.5), 'cm'), 
            axis.ticks = element_blank(), 
            legend.position = c(0.85,0.5), 
            panel.grid.major.x = element_line(size=0.2, colour ='grey'),
            axis.text.y = element_blank(), 
            axis.title.y = element_blank(), strip.text.y = element_blank())



pdf(file = 'fig/final/Figure2_UK_profiles.pdf', height=5, width=13)
top<-plot_grid(g1, g3, g2, nrow = 1, align = 'h', 
  rel_widths=c(0.9, 0.6, 0.8), labels=c('A', 'B', 'C'), hjust=0)
print(
  # plot_grid(top, g3, nrow =2, labels=c('', 'C'), rel_heights=c(1, 0.7))
  top
)
dev.off()











