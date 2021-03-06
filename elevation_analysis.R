# statistical_analyses.R performs the statistics:
# 1 - multiple rank-sum test with bonferroni correction 
# 2 - probabilistic effect size

# Every map of continents will work for this step, but it should be a raster of
# the same extent and resolution of the PHYLACINE ranges. We used the map
# provided by Matt Davis
continents <- raster('Data/continents.tif')
cont <- stack("Data/continents-stack.envi")
names(cont) <- c("Africa", "Asia", "Oceania", "Europe", "NorthAmerica", "SouthAmerica")
el <- raster("elevation.tif")
el[el >= 2000] <- NA
el[!is.na(el)] <- 1

setwd('Results')

# Mean --------------------------------------------------------------------
pn_mean <- raster('present_natural_mean.tif') * el
pne_mean <- raster('pn_extant_mean.tif') * el
cu_mean <- raster('current_mean.tif') * el
vu_mean <- raster('vulnerable_mean.tif') * el
rw_mean <- raster('rw_extant_mean.tif') * el

#take only cells that are valid for all scenarios
valid_cells <- pn_mean
valid_cells[valid_cells > 0] <- 1
valid_cells[is.na(pne_mean)] <- NA
valid_cells[is.na(cu_mean)] <- NA
valid_cells[is.na(vu_mean)] <- NA
valid_cells[is.na(rw_mean)] <- NA

pn_mean[is.na(valid_cells)] <- NA
pne_mean[is.na(valid_cells)] <- NA
cu_mean[is.na(valid_cells)] <- NA
vu_mean[is.na(valid_cells)] <- NA
rw_mean[is.na(valid_cells)] <- NA

global <- stack(pn_mean, cu_mean, vu_mean, rw_mean, pne_mean)
get_values(global, "Global", "mean")
get_values(global * cont$Africa, "Africa", "mean")
get_values(global * cont$Asia, "Asia", "mean")
get_values(global * cont$Oceania, "Oceania", "mean")
get_values(global * cont$Europe, "Europe", "mean")
get_values(global * cont$NorthAmerica, "NorthAmerica", "mean")
get_values(global * cont$SouthAmerica, "SouthAmerica", "mean")
global * continents[[1]]
comparisons <- matrix(c(1, 2, 1, 4, 1, 5, 2, 3, 4, 2, 5, 2, 5, 4), ncol = 2, byrow = T)

# Global ------------------------------------------------------------------
results <- map(1:nrow(comparisons), function(x){
  new_effect_size(global[[comparisons[x, 1]]], 
                  global[[comparisons[x, 2]]])
})

do.call('rbind', results) %>% 
  add_column(Comparison = c('PN - CU', 'PN - RW', 'PN - PNE', 'CU - VU', 
                            'CU - RW', 'CU - PNE', 'PNE - RW')) %>% 
  mutate(`Effect size` = magnitude(superiority = round(`P(X > Y)`, 2))) %>% 
  write_csv('Mean/global_statistics_el.csv')

# Continental ------------------------------------------------------------------
for(cont.name in c('Africa', 'Asia', 'Europe', 'North America', 'South America', 'Oceania')){
  results <- map(1:nrow(comparisons), function(x){
    new_effect_size(
      global[[comparisons[x, 1]]], 
      global[[comparisons[x, 2]]],
      global = F, area = cont.name
    )
  })
  
  do.call('rbind', results) %>% 
    add_column(Comparison = c('PN - CU', 'PN - RW', 'PN - PNE', 'CU - VU', 
                              'CU - RW', 'CU - PNE', 'PNE - RW')) %>% 
    mutate(`Effect size` = magnitude(superiority = round(`P(X > Y)`, 2))) %>% 
    write_csv(paste0('Mean/', cont.name, '_statistics_el.csv'))
}

# Table mean ---------------------------------------------------------------
setwd('Mean')

if('table)mean.csv' %in% list.files(pattern = 'csv')){
  system("rm table.csv")
}

system('cat *csv | grep ^[^"p"] > table_mean.csv')

read_csv('table_mean.csv', col_types = cols(),
         col_names = c('p', 'r', 'P(X > Y)', 'Area', 'Fraction', 'Comparison', 'Effect size')) %>% 
  filter(p > 0.001) %>% 
  kable()

setwd('..')

# Max --------------------------------------------------------------------
pn_max <- raster('present_natural_max.tif') * el
pne_max <- raster('pn_extant_max.tif') * el
cu_max <- raster('current_max.tif') * el
vu_max <- raster('vulnerable_max.tif') * el
rw_max <- raster('rw_extant_max.tif') * el

#take only cells that are valid for all scenarios
valid_cells <- pn_max
valid_cells[valid_cells > 0] <- 1
valid_cells[is.na(pne_max)] <- NA
valid_cells[is.na(cu_max)] <- NA
valid_cells[is.na(vu_max)] <- NA
valid_cells[is.na(rw_max)] <- NA

pn_max[is.na(valid_cells)] <- NA
pne_max[is.na(valid_cells)] <- NA
cu_max[is.na(valid_cells)] <- NA
vu_max[is.na(valid_cells)] <- NA
rw_max[is.na(valid_cells)] <- NA

global <- stack(pn_max, cu_max, vu_max, rw_max, pne_max)
get_values(global, "Global", "max")
get_values(global * cont$Africa, "Africa", "max")
get_values(global * cont$Asia, "Asia", "max")
get_values(global * cont$Oceania, "Oceania", "max")
get_values(global * cont$Europe, "Europe", "max")
get_values(global * cont$NorthAmerica, "NorthAmerica", "max")
get_values(global * cont$SouthAmerica, "SouthAmerica", "max")
comparisons <- matrix(c(1, 2, 1, 4, 1, 5, 2, 3, 4, 2, 5, 2, 5, 4), ncol = 2, byrow = T)

# Global ------------------------------------------------------------------
results <- map(1:nrow(comparisons), function(x){
  new_effect_size(global[[comparisons[x, 1]]], 
                  global[[comparisons[x, 2]]])
})

do.call('rbind', results) %>% 
  add_column(Comparison = c('PN - CU', 'PN - RW', 'PN - PNE', 'CU - VU', 
                            'CU - RW', 'CU - PNE', 'PNE - RW')) %>% 
  mutate(`Effect size` = magnitude(superiority = round(`P(X > Y)`, 2))) %>% 
  write_csv('Max/global_statistics_el.csv')

# Continental ------------------------------------------------------------------
for(cont.name in c('Africa', 'Asia', 'Europe', 'North America', 'South America', 'Oceania')){
  results <- map(1:nrow(comparisons), function(x){
    new_effect_size(
      global[[comparisons[x, 1]]], 
      global[[comparisons[x, 2]]],
      global = F, area = cont.name
    )
  })
  
  do.call('rbind', results) %>% 
    add_column(Comparison = c('PN - CU', 'PN - RW', 'PN - PNE', 'CU - VU', 
                              'CU - RW', 'CU - PNE', 'PNE - RW')) %>% 
    mutate(`Effect size` = magnitude(superiority = round(`P(X > Y)`, 2))) %>% 
    write_csv(paste0('Max/', cont.name, '_statistics_el.csv'))
}

# Table max ---------------------------------------------------------------
setwd('Max')

if('table_max.csv' %in% list.files(pattern = 'csv')){
  system("rm table.csv")
}

system('cat *csv | grep ^[^"p"] > table_max.csv')

read_csv('table_max.csv', col_names = c('p', 'r', 'P(X > Y)', 'Area', 'Fraction', 'Comparison', 'Effect size')) %>% 
  filter(p > 0.001) %>% 
  kable()

setwd('..')

# Return to parent Megalinkers --------------------------------------------

setwd('..')


