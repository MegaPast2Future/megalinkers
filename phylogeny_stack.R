library(stringi)
library(raster)
library(tidyverse)

phy <- read_csv("/home/GIT/pantheria/phylacine.csv")

phylogeny <- phy %>% 
  filter(Mass.g >= 10000) %>% 
  filter(Terrestrial == 1, Marine == 0, Freshwater == 0, Aerial == 0) %>% 
  filter(Genus.1.2 != "Homo") %>% 
  select(Binomial.1.2, Family.1.2)

setwd("/home/GIT/megalinkers/code_publication/Data/PHYLACINE_1.2.0/Data/Ranges/Present_natural/")

families <- phylogeny %>% 
  pull(Family.1.2) %>% 
  unique()

phylogeny_list <- 1:length(families) %>% 
  map(function(x) phylogeny %>% 
        filter(Family.1.2 == families[x]) %>% 
        pull(Binomial.1.2))

phylogeny_stack <- lapply(phylogeny_list, function(x){stack(paste0(x, ".tif"))})
names(phylogeny_stack) <- families