pacman::p_load(tidyverse, skimr, janitor, cowplot, funk, ggrepel, scales, fmsb, install=FALSE)
theme_set(theme_sleek())
source('scripts/fig/00_plotting.R')
set.seed(43)

gfg<-readxl::read_excel('data/gfg/GFG_Export_2021-04-12.xlsx') %>% clean_names() %>% 
    rename(scientific_name = latin_name) %>% 
    filter(! total_score %in% c('Unknown', 'Under Review', 'Default Red Rating', 'FIP Improver')) %>% 
    group_by(common_name, scientific_name, farmed_wild) %>% 
    summarise(se = se(as.numeric(total_score)), total_score = mean(as.numeric(total_score))) %>% ungroup() %>% 
    mutate(lower = total_score - 2*se, upper = total_score + 2*se,
          farmed_wild = recode(farmed_wild, 'Caught at sea' = 'Wild'), 
          scientific_name = recode(scientific_name, 'Euthynnus pelamis, Katsuwonus pelamis' = 'Katsuwonus pelamis',
                                                      'Theragra chalcogramma' = 'Gadus chalcogrammus'),
          id = paste(farmed_wild, scientific_name, sep='_'))


# read nutrient/ghg data, join with gfg
drops<-c('Other marine fish')
nut<-read.csv('data/UK_GHG_nutrient_catch.csv') %>%
  filter(top90 == TRUE & !species %in% drops & !is.na(mid)) %>%
  select(-tax) %>%
  rowwise() %>%
  mutate(n_targets = sum(c(ca_rda, fe_rda, se_rda, zn_rda, om_rda) > 25),  ## estimate nutrition targets (25% RDA) for each species)
         nt_co2 = mid / n_targets / 10, ## estimate the CO2 equivalent per RDA target
         nt_co2_low = low / n_targets / 10, ## estimate the CO2 equivalent per RDA target
         nt_co2_hi = max / n_targets / 10, ## estimate the CO2 equivalent per RDA target
         common_name = factor(common_name, levels = levels(fct_reorder(common_name, nt_co2)))) %>% 
  mutate(id = paste(farmed_wild, scientific_name, sep='_')) %>% 
  left_join(gfg %>% select(-farmed_wild, -common_name, -scientific_name), by = 'id')


nut_rad<-nut %>% filter(!is.na(total_score)) %>% 
      ungroup() %>% 
      mutate(CO2 = rescale(mid, to=c(1,0)), Nutrients = rescale(nut_score), Sustainability = rescale(total_score), Production = rescale(tot)) %>% 
      select(id, common_name, scientific_name, farmed_wild, CO2, Nutrients, Sustainability, Production) #%>% 
      # pivot_longer(-c(scientific_name, farmed_wild), names_to = 'variable', values_to = 'value')

create_beautiful_radarchart <- function(data, color = "#00AFBB", 
                                        vlabels = colnames(data), vlcex = 0.7,
                                        caxislabels = NULL, title = NULL, ...){
  radarchart(
    data, axistype = 1,
    # Customize the polygon
    pcol = color, pfcol = scales::alpha(color, 0.5), plwd = 2, plty = 1,
    # Customize the grid
    cglcol = "grey", cglty = 1, cglwd = 0.8,
    # Customize the axis
    axislabcol = "grey", 
    # Variable labels
    vlcex = vlcex, vlabels = vlabels,
    caxislabels = caxislabels, title = title, ...
  )
}

sp<-unique(nut_rad$id)

for(i in 1:length(sp)){

  dat<-nut_rad[i, 4:8]
  ppcol<-ifelse(unique(nut_rad$farmed_wild[i])=='Farmed', colcol[2], colcol[1])
  tit<-nut_rad$common_name[i]
  gridlab<-ifelse(i == 1, 3, 0)

  gg<-ggradar(dat, 
    group.colours = ppcol,
    base.size = 1,
    group.point.size = 2,
    group.line.width = 1,
    background.circle.colour = "white",
    axis.label.size = 2.5,
    grid.label.size = gridlab,
    fill=TRUE,
  gridline.mid.colour = "grey") + 
  labs(subtitle = tit) +
  theme(plot.subtitle = element_text(size=10, colour='black'),
    plot.margin =unit(c(0.01, 0.01, 0.01, 0.01), 'cm'))
  assign(paste0('g', i), gg)
}

pdf(file = 'fig/final/Figure4_radar.pdf', height=7, width=10)
print(
  plot_grid(g1, g2, g3, g4, g5, g6, g7, g8, g9, g10, g11, g12, nrow=4)
  )
dev.off()
