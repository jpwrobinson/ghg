pacman::p_load(tidyverse, skimr, janitor, cowplot, funk, ggrepel, install=FALSE)
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


# nut2<-nut %>% filter(!is.na(total_score))
# pp<-prcomp(~nut2$nut_score + nut2$total_score + nut2$mid)
# plot(pp)




gg<-ggplot(nut, aes(nt_co2, total_score, col=farmed_wild)) + 
        # geom_pointrange(size=0.2, aes(ymin = lower, ymax = upper)) + 
        # geom_errorbarh(size=0.2, aes(xmin = nt_co2_low, xmax = nt_co2_hi)) + 
        geom_point(size=3) + 
        geom_label_repel(aes(label = common_name), force=2,show.legend = FALSE) +
        th +
        scale_y_continuous(breaks=seq(0, 16, by = 2)) +
        scale_color_manual(values = colcol2) +
        labs(y = 'Sustainability Score (Good Fish Guide)', x = 'kg CO2 per nutrient target') +
          theme(legend.position = c(0.8, 0.8), 
                legend.title=element_blank()) 


## read stock assessment indicators
# https://data.cefas.co.uk/view/20996
# https://jncc.gov.uk/our-work/ukbi-b2-sustainable-fisheries/#indicator-description
stock<-read.csv('data/ices/stock_data_timeries.csv') %>% 
      filter(Year >= 1990 & Year < 2020) %>% 
      mutate(F_stat = ifelse(FishingPressure > FMSY, 'Over', 'Under'),
            B_stat = ifelse(StockSize > Blim, 'Over', 'Under'),
            F_stat = ifelse(is.na(F_stat), 'Unknown', F_stat),
            B_stat = ifelse(is.na(B_stat), 'Unknown', B_stat))

stock$group<-nut$group[match(stock$SpeciesName, nut$scientific_name)]
stock$F_stat<-factor(stock$F_stat, levels=c('Unknown', 'Over', 'Under'))

# ## recreate CEFAS fig
# ggplot(stock, aes(Year, fill=F_stat)) + geom_bar(position='fill')
# ggplot(stock, aes(Year, fill=B_stat)) + geom_bar(position='fill') ## B_stat mostly unknown

## check only species in nut/ghg
g2<-ggplot(stock %>% filter(SpeciesName %in% nut$scientific_name & group!='Salmons, trouts, smelts'), aes(Year, fill=F_stat)) + 
    geom_bar(position='fill') +
    facet_wrap(~group) +
    labs(x = '', y ='% stocks of UK interest') +
    scale_fill_manual(values = rev(c('#4daf4a', '#e41a1c', '#999999')), labels=c( 'Unknown', '>Fmsy', '<Fmsy')) + 
    scale_x_continuous(breaks=seq(1990, 2019, by = 5), expand=c(0,0)) + 
    scale_y_continuous(labels=scales::percent, expand=c(0,0)) + th +
    theme(legend.position = 'top')

g3<-ggplot(stock %>% filter(SpeciesName %in% nut$scientific_name & group!='Salmons, trouts, smelts'), aes(Year, fill=B_stat)) + 
    geom_bar(position='fill') +
    facet_wrap(~group) +
    labs(x = '', y ='% stocks of UK interest')  +
    scale_fill_manual(values = rev(c('#4daf4a', '#e41a1c', '#999999')), labels=c( 'Unknown', '>Blim', '<Blim')) + 
    scale_x_continuous(breaks=seq(1990, 2019, by = 5), expand=c(0,0)) + 
    scale_y_continuous(labels=scales::percent, expand=c(0,0)) + th +
    theme(legend.position = 'top')


pdf(file = 'fig/final/Figure4.pdf', height = 7, width=8)
print(gg)
dev.off()

pdf(file = 'fig/final/FigureSX_stock_status.pdf', height = 7, width=12)
print(plot_grid(g2, g3, labels=c('A', 'B'), nrow=1))
dev.off()
