# ____Scope and flow of the code____
#
# For this code, you need the data from the PHYLACINE database, avaialble at
# https://github.com/MegaPast2Future/PHYLACINE_1.2 The code is divided into 3
# parts:

# 1 - Initialization: loading libraries, functions that I wrote to make the
# main.R script more compact, and the PHYLACINE .csv file with mammals traits,
# of which I use columns: `Binomial.1.2`, `Mass.g`, `Diet.Plant`, habitat
# information, and `Genus.1.2`. I create two other columns, `Diet`, derived
# according to the thresholds discussed in the main text (namely, a species is
# carnivore if `Diet.Plant` < 10, herbivore if `Diet.Plant` > 90, omnivore
# otherwise), and Home range`, derived from Kelt & Van Vuren 2001
# (https://doi.org/10.1086/320621).

# 2 - Average and maximum home range evaluation for each scenario examined. I
# loaded, for each scenario, the .tif files of the species that are not
# considered extinct, multiplied the RasterLayers for their home range value,
# stacked them into a RasterStack, and calculated the average and maximum home
# range per cell. Because the behavior of a multiplication of a RasterStack for
# a vector is anomalous, i.e. the time for number of RasterLayers follows a
# linear relationship only for RasterStacks of less than 600 elements, I split
# the process into blocks of 500 species, plus one accounting for the rest and
# parallelized it.

# 3 - Statistical analyses. Contrasts between scenarios are evaluated performing
# multiple non-parametric rank sum tests and quantifying the magnitute of the
# difference using the probabilistic sample superiority effect size. 

#____Micsellaneous____

# Troughout all the code I remove memory expensive RasterStacks and list of
# RasterBricks, but you can comment all "rm(...)" and "gc(verbose = F)". I
# provided the code to make the figures and tables for the main manuscript and
# the appendix in figure.R script and at the end of the main.R script,
# respectively.

# Init --------------------------------------------------------------------
library(tidyverse)
library(raster)
library(doParallel)
library(foreach) 
library(skimr)
library(tmap)
library(plotrix)
library(cowplot)

source('homerange.R') # functions for estimating home range
source('parallel_stack.R') # function to parallelize the stack

# load PHYLACINE trait database and filtering
phy <- read_csv('https://raw.githubusercontent.com/MegaPast2Future/PHYLACINE_1.2/master/Data/Traits/Trait_data.csv', col_types = cols()) %>%
  filter(
    Terrestrial == 1, Marine == 0, Freshwater == 0, Aerial == 0, #only terrestrial
    Genus.1.2 != 'Homo'
  ) %>%  #without Homo
  mutate(Diet = map(Diet.Plant, function(x){ #diet categorization: herbivores, omnivores, or carnivores
    if(x >= 90){
      "H"
    } else if(x <= 10){
      "C"
    } else{
      "O"
    }
  }) %>% unlist()) %>% 
  mutate(Diet = factor(Diet, levels = c("H", "O", "C")))

# home range estimation
phy %>% 
  mutate(`Home range` = map2(Diet, Mass.g, function(x, y){
    homerange(x, y)[1]
  }) %>% unlist()) -> phy

source('analysis.R') # evaluate connectivity of each scenario
source('new_effect_size.R') # functions for probabilistic effect size
source('statistical_analyses.R') # perform statistical analyses

# Tables of results -------------------------------------------------------
read_csv('Results/10_90/Mean/table_mean.csv', col_types = cols(),
         col_names = c('p', 'r', 'P(X > Y)', 'Area', 'Fraction', 'Comparison', 'Effect size')) %>%
  mutate(
    Comparison = factor(Comparison, levels = c('PN - CU', 'CU - VU', 'CU - RW', 'CU - PNE', 'PN - RW', 'PN - PNE', 'PNE - RW')),
    Area = factor(Area, levels = sort(unique(Area)))
  ) %>%
  filter(p < 0.001) %>%
  dplyr::select(Area, Comparison, `Effect size`) %>%
  spread(key = Comparison, value = `Effect size`, fill = '-') %>%
  mutate_all(~ replace(., . == 'No difference', '-')) %>%
  mutate_all(~ replace(., . == 'Small', '*')) %>%
  mutate_all(~ replace(., . == 'Medium', '**')) %>%
  mutate_all(~ replace(., . == 'Large', '***')) %>%
  mutate_all(~ replace(., . == 'Very large', '****')) %>%
  mutate_all(~ replace(., . == 'Huge', '*****')) %>%
  kable()

read_csv('Results/10_90/Mean/table_mean.csv', col_types = cols(),
         col_names = c('p', 'r', 'P(X > Y)', 'Area', 'Fraction', 'Comparison', 'Effect size')) %>%
  mutate(
    Comparison = factor(Comparison, levels = c('PN - CU', 'CU - VU', 'CU - RW', 'CU - PNE', 'PN - RW', 'PN - PNE', 'PNE - RW')),
    Area = factor(Area, levels = sort(unique(Area)))
  ) %>%
  filter(p < 0.001) %>%
  dplyr::select(Area, Comparison, Fraction) %>%
  mutate(Fraction = round(Fraction, 2)) %>%
  spread(key = Comparison, value = Fraction, fill = '-') %>%
  kable('latex', booktabs = T)

read_csv('Results/Max/table_max.csv', col_types = cols(),
         col_names = c('p', 'r', 'P(X > Y)', 'Area', 'Fraction', 'Comparison', 'Effect size')) %>%
  mutate(
    Comparison = factor(Comparison, levels = c('PN - CU', 'CU - VU', 'CU - RW', 'CU - PNE', 'PN - RW', 'PN - PNE', 'PNE - RW')),
    Area = factor(Area, levels = sort(unique(Area)))
  ) %>%
  filter(p < 0.001) %>%
  dplyr::select(Area, Comparison, `Effect size`) %>%
  unique() %>% 
  spread(key = Comparison, value = `Effect size`, fill = '-') %>%
  mutate_all(~ replace(., . == 'No difference', '-')) %>%
  mutate_all(~ replace(., . == 'Small', '*')) %>%
  mutate_all(~ replace(., . == 'Medium', '**')) %>%
  mutate_all(~ replace(., . == 'Large', '***')) %>%
  mutate_all(~ replace(., . == 'Very large', '****')) %>%
  mutate_all(~ replace(., . == 'Huge', '*****')) %>%
  kable()

read_csv('Results/Max/table_max.csv', col_types = cols(),
         col_names = c('p', 'r', 'P(X > Y)', 'Area', 'Fraction', 'Comparison', 'Effect size')) %>%
  mutate(
    Comparison = factor(Comparison, levels = c('PN - CU', 'CU - VU', 'CU - RW', 'CU - PNE', 'PN - RW', 'PN - PNE', 'PNE - RW')),
    Area = factor(Area, levels = sort(unique(Area)))
  ) %>%
  filter(p < 0.001) %>%
  dplyr::select(Area, Comparison, Fraction) %>%
  mutate(Fraction = round(Fraction, 2)) %>%
  spread(key = Comparison, value = Fraction, fill = '-') %>%
  kable('latex', booktabs = T)


# Sensitivity analyses mean ------------------------------------------------
five <- read_csv('Results/5_95/Mean/table_mean.csv', col_types = cols(),
         col_names = c('p', 'r', 'P(X > Y)', 'Area', 'Fraction', 'Comparison', 'Effect size')) %>%
  mutate(
    Th = 5,
    Comparison = factor(Comparison, levels = c('PN - CU', 'CU - VU', 'CU - RW', 'CU - PNE', 'PN - RW', 'PN - PNE', 'PNE - RW')),
    Area = factor(Area, levels = sort(unique(Area)))
  )

ten <- read_csv('Results/10_90/Mean/table_mean.csv', col_types = cols(),
                 col_names = c('p', 'r', 'P(X > Y)', 'Area', 'Fraction', 'Comparison', 'Effect size')) %>%
  mutate(
    Th = 10,
    Comparison = factor(Comparison, levels = c('PN - CU', 'CU - VU', 'CU - RW', 'CU - PNE', 'PN - RW', 'PN - PNE', 'PNE - RW')),
    Area = factor(Area, levels = sort(unique(Area)))
  )

twenty <- read_csv('Results/10_90/Mean/table_mean.csv', col_types = cols(),
                   col_names = c('p', 'r', 'P(X > Y)', 'Area', 'Fraction', 'Comparison', 'Effect size')) %>%
  mutate(
    Th = 20,
    Comparison = factor(Comparison, levels = c('PN - CU', 'CU - VU', 'CU - RW', 'CU - PNE', 'PN - RW', 'PN - PNE', 'PNE - RW')),
    Area = factor(Area, levels = sort(unique(Area)))
  )

bind_rows(five, ten, twenty) %>% 
  group_by(Comparison, Area) %>% 
  summarize(P = round(max(p), 4)) %>% 
  spread(Comparison, P)

# just did it manually
i <- 1
comp <- levels(five$Comparison)[i]

bind_rows(five, ten, twenty) %>% 
  dplyr::select(Area, Comparison, `Effect size`) %>% 
  arrange(desc(Area, Comparison)) %>% 
  filter(Comparison == comp) %>% 
  unique() %>% 
  kable()

i <- i + 1

# Sensitivity analyses max ------------------------------------------------
five <- read_csv('Results/5_95/Max/table_max.csv', col_types = cols(),
                 col_names = c('p', 'r', 'P(X > Y)', 'Area', 'Fraction', 'Comparison', 'Effect size')) %>%
  mutate(
    Th = 5,
    Comparison = factor(Comparison, levels = c('PN - CU', 'CU - VU', 'CU - RW', 'CU - PNE', 'PN - RW', 'PN - PNE', 'PNE - RW')),
    Area = factor(Area, levels = sort(unique(Area)))
  )

ten <- read_csv('Results/10_90/Max/table_max.csv', col_types = cols(),
                col_names = c('p', 'r', 'P(X > Y)', 'Area', 'Fraction', 'Comparison', 'Effect size')) %>%
  mutate(
    Th = 10,
    Comparison = factor(Comparison, levels = c('PN - CU', 'CU - VU', 'CU - RW', 'CU - PNE', 'PN - RW', 'PN - PNE', 'PNE - RW')),
    Area = factor(Area, levels = sort(unique(Area)))
  )

twenty <- read_csv('Results/10_90/Max/table_max.csv', col_types = cols(),
                   col_names = c('p', 'r', 'P(X > Y)', 'Area', 'Fraction', 'Comparison', 'Effect size')) %>%
  mutate(
    Th = 20,
    Comparison = factor(Comparison, levels = c('PN - CU', 'CU - VU', 'CU - RW', 'CU - PNE', 'PN - RW', 'PN - PNE', 'PNE - RW')),
    Area = factor(Area, levels = sort(unique(Area)))
  )

bind_rows(five, ten, twenty) %>% 
  group_by(Comparison, Area) %>% 
  summarize(P = round(max(p), 4)) %>% 
  spread(Comparison, P)

# just did it manually
i <- 1
comp <- levels(five$Comparison)[i]

bind_rows(five, ten, twenty) %>% 
  dplyr::select(Area, Comparison, `Effect size`) %>% 
  arrange(desc(Area, Comparison)) %>% 
  filter(Comparison == comp) %>% 
  unique() %>% 
  kable()

i <- i + 1
