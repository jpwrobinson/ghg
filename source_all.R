

## Data clean
clean<-list.files('scripts/')  %>% .[matches(".R", vars=.)]
for(i in 1:4){
  print(paste('Running', clean[i]))
  source(paste0('scripts/', clean[i]))
}

## Figures for MS
fig<-list.files('scripts/fig')
for(i in 1:length(fig)){
  print(paste('Running', fig[i]))
  source(paste0('scripts/fig/', fig[i]))
}

