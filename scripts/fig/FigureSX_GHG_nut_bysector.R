
# source('scripts/fig/Figure5_radar.R')
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


## 1. all products
all<-read.csv('data/UK_GHG_nutrient_catch.csv') %>% 
      mutate(uk = (1-prop_imported) * tot, imp = prop_imported * tot) %>% 
      select(species, tot, uk, imp, common_name, scientific_name, tax) %>% 
      pivot_longer(uk:imp, names_to = 'type', values_to = 'catch') %>% 
      filter(tot > 100 & catch !=0)

gsup<-ggplot(all, aes(catch, fct_reorder(species, tot), col = type)) +
      # geom_bar(stat = 'identity')+# position = position_dodge()) +
      geom_segment(size=0.5, aes(x= 0.01, xend = catch, yend = fct_reorder(species, tot))) +
      geom_point(size=2.5, position = position_dodge()) +
      labs(x = expression(tonnes~yr^{'-1'}), y = '', parse=TRUE) +
      scale_color_manual(values = colcol4, labels=c('UK', 'Imported')) +
      scale_x_log10(breaks=c(10^(2:5)), labels=c('100', '1,000', '10K', '100K'), limits=c(10,10e5), expand=c(0, 0.06)) +
      th+ 
      theme(plot.margin=unit(c(0.1, 0.5, 0.1, 0.5), 'cm'), 
            axis.ticks = element_blank(), 
            legend.position = c(0.9,0.4), 
            panel.grid.major.x = element_line(size=0.2, colour ='grey'),
            axis.text.y = element_text(size=9), 
            axis.title.y = element_blank())


## 2. production source (for Fig2)
g3<-ggplot(nutS, aes(catch, fct_reorder(species, tot), fill=farmed_wild)) +
      geom_bar(stat = 'identity') +
      labs(x = expression(tonnes~yr^{'-1'}), y = '', parse=TRUE) +
      facet_grid(cols = vars(source), scales='free_y', space = 'free_y') + 
      scale_fill_manual(values = colcol2) +
      scale_x_continuous(labels=scales::comma, expand=c(0, 0.06)) +
      th+ 
      theme(plot.margin=unit(c(0.1, 0.5, 0.1, 0.5), 'cm'), 
            axis.ticks = element_blank(), 
            legend.position = c(0.95,0.2), 
            panel.grid.major.x = element_line(size=0.2, colour ='grey'),
            # axis.text.y = element_blank(), 
            axis.title.y = element_blank(), strip.text.x = element_text(colour='black',angle=360))



pdf(file = 'fig/final/FigureS2_UK_seafood.pdf', height=12, width=6)
# print(
#   plot_grid(g1, g2, g3, nrow = 1, align = 'h', rel_widths=c(1, 0.6, 0.8), labels=c('A', 'B', 'C'))
# )
print(gsup)

dev.off()