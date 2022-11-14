pacman::p_load(tidyverse, skimr, janitor, cowplot, funk, ggrepel, install=FALSE)
theme_set(theme_sleek())
source('scripts/fig/00_plotting.R')
set.seed(43)


## good fish guide scores

gfg<-readxl::read_excel('data/gfg/GFG_Export_2022-11-09.xlsx') %>% clean_names() %>% 
  rename(common_name = species_common_name, 
         scientific_name = species_scientific_name, 
         farmed_wild = farmed_or_wild_caught) %>% 
  mutate(farmed_wild = recode(farmed_wild, 'Wild caught' = 'Wild')) %>% 
  filter(! public_rating %in% c('Under review')) %>%
  filter(!(farmed_production_methods == 'Closed system, RAS' & common_name == 'Atlantic salmon')) %>% 
  filter(!(wild_capture_methods == 'Net (gill or fixed)' & common_name == 'Skipjack tuna')) %>% 
  group_by(common_name, scientific_name, farmed_wild) %>% 
  mutate(public_rating = as.numeric(public_rating),
         farmed_wild = recode(farmed_wild, 'Caught at sea' = 'Wild'), 
         scientific_name = recode(scientific_name, 'Euthynnus pelamis, Katsuwonus pelamis' = 'Katsuwonus pelamis',
                                  'Theragra chalcogramma' = 'Gadus chalcogrammus'),
         id = paste(farmed_wild, scientific_name, sep='_'),
         total_score = as.numeric(public_rating)) %>% 
  group_by(farmed_wild) %>% 
  ## rescale ratings between 0-1, but inverse for farmed
  # mutate(total_score = ifelse(farmed_wild == 'Farmed', rescale(total_score, to = c(1,0)),rescale(total_score, to = c(1,0))))  %>%
  mutate(total_score = rescale(total_score, to = c(1,0))) %>% 
  group_by(common_name, farmed_wild, scientific_name, id) %>% 
  summarise(lower = min(total_score), upper = max(total_score), total_score = median(total_score)) %>% ungroup()



# read nutrient/ghg data, join with gfg
drops<-c('Other marine fish')
nutCO<-read.csv('data/UK_GHG_nutrient_catch.csv') %>%
  mutate(edible_fraction = edible_fraction / 100) %>% 
  filter(top90 == TRUE & !species %in% drops & !is.na(mid)) %>%
  select(-tax) %>%
  rowwise() %>%
  mutate(n_targets = sum(c(ca_rda, fe_rda, se_rda, zn_rda, om_rda, vita_rda, vitb12_rda, vitd_rda, folate_rda) > 25),  ## estimate nutrition targets (25% RDA) for each species)
        n_targets2 = sum(c(ca_rda, fe_rda, se_rda, zn_rda, om_rda) > 25), 
         nt_co2 = mid / edible_fraction / n_targets / 10, ## estimate the CO2 equivalent per RDA target
         common_name = factor(common_name, levels = levels(fct_reorder(common_name, nt_co2)))) %>% 
  mutate(id = paste(farmed_wild, scientific_name, sep='_')) %>% 
  left_join(gfg %>% select(-farmed_wild, -common_name, -scientific_name), by = 'id') %>% 
  filter(id != 'Wild_Mytilus edulis')


# nut2<-nut %>% filter(!is.na(total_score))
# pp<-prcomp(~nut2$nut_score + nut2$total_score + nut2$mid)
# plot(pp)


g0<-ggplot(nutCO, aes(nt_co2, total_score, col=farmed_wild)) + 
        # geom_pointrange(size=0.4, aes(ymin = lower, ymax = upper)) + 
        # geom_errorbarh(size=0.4, aes(xmin = nt_co2_low, xmax = nt_co2_hi)) + 
        geom_point(size=3) + 
        geom_text_repel(aes(label = common_name), col='black',segment.color='grey',max.overlaps=Inf, box.padding = 1.75, size=2.5,show.legend = FALSE) +
        th +
        # scale_y_continuous(breaks=seq(0, 16, by = 2)) +
        scale_color_manual(values = colcol2) +
        labs(y = 'Sustainability score', x  = expression(paste(kg~CO[2],'-',eq~per~nutrient~target))) +
          theme(legend.position = c(0.8, 0.75), 
            plot.margin=unit(c(0.1, 0.5, 0.1, 0.1), 'cm'), 
            axis.ticks=element_line(colour='black'),
                legend.title=element_blank()) 

g0B<-ggplot(nutCO %>% filter(!is.na(total_score)), 
        aes(fct_reorder(common_name, total_score),total_score, col=farmed_wild)) + 
        geom_pointrange(size=0.4, aes(ymin = lower, ymax = upper)) + 
        geom_point(size=3) + 
        th +
        coord_flip(clip='off') +
        scale_x_discrete(position = 'top') +
        scale_y_continuous(limits=c(0,1.1),
            sec.axis = sec_axis(~ . * 1, breaks=c(0, 1), labels=c('Less\nsustainable', 'More\nsustainable'))) +
        scale_color_manual(values = colcol2) +
        labs(y = 'Sustainability score', x  = '') +
          theme(legend.position = 'none', 
            plot.margin=unit(c(0.1, 0.1, 0.1, 1), 'cm'), 
            axis.ticks=element_line(colour='black'),
            axis.ticks.x.top=element_blank(),
                legend.title=element_blank()) 

source('scripts/fig/Figure4_radar.R')
nut_rad$price_key_kg<-c(16.34, 8.03, 8.54, 6.45, 9.64, 5.76, 5.08, 10.42, 24.56, 5.47, 16.12)
nutCO$price_key_kg<-nut_rad$price_key_kg[match(nutCO$common_name, nut_rad$common_name)]

g1<-ggplot(nutCO, aes(nt_co2, price_key_kg, col=farmed_wild)) + 
        # geom_pointrange(size=0.2, aes(ymin = lower, ymax = upper)) + 
        # geom_errorbarh(size=0.2, aes(xmin = nt_co2_low, xmax = nt_co2_hi)) + 
        geom_point(size=3) + 
        # geom_label_repel(aes(label = common_name), size=2.5, force=2,show.legend = FALSE) +
        geom_text_repel(aes(label = common_name), col='black',segment.color='grey',max.overlaps=0, box.padding = 1.75, size=2.5,show.legend = FALSE) +
        th +
        # scale_y_continuous(breaks=seq(0, 16, by = 2)) +
        scale_color_manual(values = colcol2) +
        labs(y = 'GBP per kg', x  = expression(paste(kg~CO[2],'-',eq~per~nutrient~target))) +
          theme(legend.position = c(0.8, 0.8), 
            plot.margin=unit(c(1, 1.5, 0.1, 1.5), 'cm'), 
            axis.ticks=element_line(colour='black'),
                legend.title=element_blank()) 


## read stock assessment indicators
# https://data.cefas.co.uk/view/20996
# https://jncc.gov.uk/our-work/ukbi-b2-sustainable-fisheries/#indicator-description
stock<-read.csv('data/ices/stock_data_timeries.csv') %>% 
      filter(Year >= 1990 & Year < 2020) %>% 
      mutate(F_stat = ifelse(FishingPressure > FMSY, 'Over', 'Under'),
            B_stat = ifelse(StockSize < Blim, 'Over', 'Under'),
            F_stat = ifelse(is.na(F_stat), 'Unknown', F_stat),
            B_stat = ifelse(is.na(B_stat), 'Unknown', B_stat))

stock$group<-nut$group[match(stock$SpeciesName, nut$scientific_name)]
stock$common_name<-nut$common_name[match(stock$SpeciesName, nut$scientific_name)]
stock$F_stat<-factor(stock$F_stat, levels=c('Unknown', 'Over', 'Under'))

# ## recreate CEFAS fig
# ggplot(stock, aes(Year, fill=F_stat)) + geom_bar(position='fill')
# ggplot(stock, aes(Year, fill=B_stat)) + geom_bar(position='fill') ## B_stat mostly unknown

## check only species in nut/ghg
g2<-ggplot(stock %>% filter(SpeciesName %in% nut$scientific_name & group!='Salmons, trouts, smelts'), aes(Year, fill=F_stat)) + 
    geom_bar(position='fill') +
    facet_wrap(~common_name, nrow=1) +
    labs(x = '', y ='% stocks of UK interest') +
    scale_fill_manual(values = rev(c('#4daf4a', '#e41a1c', '#999999')), labels=c( 'Unknown', 'F > Fmsy', 'F < Fmsy')) + 
    scale_x_continuous(breaks=seq(1990, 2019, by = 5), expand=c(0,0)) + 
    scale_y_continuous(labels=scales::percent, expand=c(0,0)) + th +
    theme(legend.position = 'right', axis.text.x = element_text(size=9, angle=0, hjust=0.5, vjust=0.5),
            axis.ticks = element_line(colour='black'),
            strip.text.x = element_text(colour='black', size=11, face=1))

g3<-ggplot(stock %>% filter(SpeciesName %in% nut$scientific_name & group!='Salmons, trouts, smelts'), aes(Year, fill=B_stat)) + 
    geom_bar(position='fill') +
    facet_wrap(~common_name, nrow=1) +
    labs(x = '', y ='% stocks of UK interest')  +
    scale_fill_manual(values = rev(c('#e41a1c','#4daf4a', '#999999')), labels=c( 'Unknown', 'B > Blim', 'B < Blim')) + 
    scale_x_continuous(breaks=seq(1990, 2019, by = 5), expand=c(0,0)) + 
    scale_y_continuous(labels=scales::percent, expand=c(0,0)) + th +
    theme(legend.position = 'right', axis.text.x = element_text(size=9, angle=0, hjust=0.5, vjust=0.5),
            axis.ticks = element_line(colour='black'),
            strip.text.x = element_blank())


pdf(file = 'fig/final/FigureSX_GFGscore.pdf', height = 9, width=9)
top<-plot_grid(g0, g0B, labels=c('A', 'B'), nrow=1, rel_widths = c(1, 0.8), align='h')
print(plot_grid(top, g1, labels=c('', 'C'), nrow=2))
dev.off()

pdf(file = 'fig/final/FigureSX_stock_status.pdf', height = 7, width=12)
print(plot_grid(g2, g3, labels=c('A', 'B'), nrow=2))
dev.off()
