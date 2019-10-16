library(tidyverse)
library(raster)
library(tmap)

source('parallel_stack.R')

phy <- read_csv('Data/PHYLACINE_1.2.0/Data/Traits/Trait_data.csv') %>%
  filter(Terrestrial == 1, Marine == 0, Freshwater == 0, Aerial == 0) %>% #only terrestrial
  filter(Genus.1.2 != 'Homo') #without Homo

# Present natural ---------------------------------------------------------
species <- phy %>% 
  pull(Binomial.1.2)

setwd('Data/PHYLACINE_1.2.0/Data/Ranges/Present_natural')
present.natural <- stack(paste0(species, '.tif'))
present.natural <- sum(present.natural)
registerDoParallel(6)
blocks <- length(species) / 15
foreach(i = 1:blocks) %dopar% {
  sum(present.natural[[]])  
}

setwd('../Current')
current <- stack(paste0(species, '.tif'))
current <- sum(current)

setwd('../../../../../Results')
writeRaster(present.natural, 'pn_mammal_richness.tif', overwrite = T)
writeRaster(current, 'cu_mammal_richness.tif', overwrite = T)
rm(present.natural, current)
gc(verbose = F)

setwd('..')


