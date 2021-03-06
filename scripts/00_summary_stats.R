




## fig 4
load('data/nutrient_ghg_species.rds')
all<-all %>% rowwise() %>% 
      mutate(n_targets = sum(c(ca_rda, fe_rda, se_rda, zn_rda, om_rda) > 25),  ## estimate nutrition targets (25% RDA) for each species)
             nt_co2 = mid / n_targets / 10 ) %>% ## estimate the CO2 equivalent per RDA target
      ungroup() %>% droplevels() %>% 
      mutate(common_name = factor(common_name, levels = levels(fct_reorder(common_name, nt_co2))))

all %>% filter(n_targets>=3 & fe_rda > 25) %>% data.frame()
all %>% filter(n_targets>=3 & zn_rda > 25) %>% data.frame()
all %>% filter(n_targets>=3 & ca_rda > 25) %>% data.frame()
all %>% filter(om_rda > 25) %>% dim()
all %>% filter(se_rda > 25) %>% dim()

## uk catch
nut<-read.csv('data/UK_GHG_nutrient_catch.csv') %>%
  filter(top90 == TRUE & !species %in% drops & !is.na(mid)) %>%
  select(-tax) %>%
  rowwise() %>%
  mutate(n_targets = sum(c(ca_rda, fe_rda, se_rda, zn_rda, om_rda) > 25),  ## estimate nutrition targets (25% RDA) for each species)
         nt_co2 = mid / n_targets / 10, ## estimate the CO2 equivalent per RDA target
         common_name = factor(common_name, levels = levels(fct_reorder(common_name, nt_co2))))

nut %>% ungroup() %>% slice_min(nt_co2, n = 5) %>% data.frame()

food<-read.csv('data/ghg_nutrient_other_foods.csv') %>%  rowwise() %>% 
  mutate(n_targets = sum(c(ca_rda, fe_rda, se_rda, zn_rda, om_rda) > 25),  ## estimate nutrition targets (25% RDA) for each species)
         nt_co2 = median / n_targets / 10 ) %>% ## estimate the median CO2 equivalent per RDA target (correct kg to 100g)
  ungroup() %>% droplevels() %>% 
  filter(product %in% c('Chicken', 'Pork', 'Beef', 'Lamb')) ## take ASMs only

food %>% data.frame()

## vitamin D, mug per 100g
# from Willer et al. 2022 PLOS
# salmon = 11, herring = 10.1, mackerel = 8.5, sprat = 13
# convert to 40g portion
11/100*40
10.1/100*40
8.5/100*40
# 13/100*40
# RNI = 7 mug per day (Gibsen & Sidnell, 2014)

3.4 / 7 * 100
4.4 / 7 * 100
# 49 - 63% of RNI


## beef = 0.5
(0.5/100*400) / 7 * 100

## lamb = 0.8
(0.8/100*400) / 7 * 100

## chicken = 0.1
(0.1/100*400) / 7 * 100

