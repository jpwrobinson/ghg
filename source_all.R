

## Data clean
clean<-list.files('scripts/')  %>% .[matches(".R", vars=.)]
for(i in 1:3){
  print(paste('Running', clean[i]))
  source(paste0('scripts/', clean[i]))
}

## Analysis
# price_mod<-list.files('scripts/price_mod')
# price_mod<-price_mod[-2] # drop model compile
# for(i in 1:length(price_mod)){
#   print(paste('Running', price_mod[i]))
#   source(paste0('scripts/price_mod/', price_mod[i]))
# }

## Figures for MS
fig<-list.files('scripts/fig')
for(i in 1:length(fig)){
  print(paste('Running', fig[i]))
  source(paste0('scripts/fig/', fig[i]))
}

