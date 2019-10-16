########################### analysis.R  valuates the average and maximum
#ecosystem connectivity for all scenarios #########################

# Present natural  ----------------------------------------------------
setwd('Data/PHYLACINE_1.2.0/Data/Ranges/Present_natural')
species <- phy %>%
  pull(Binomial.1.2)

# This takes around 6 minutes with 6 cores
registerDoParallel(cores = 6)
blocks <- floor(length(species)/500)
H <- phy %>% filter(Binomial.1.2 %in% species) %>% pull(`Home range`)
pn_list <- foreach(i = 1:(blocks + 1), .combine = c) %dopar%{
  parallel_stack(i, H)
}

pn_stack <- stack(pn_list)
values(pn_stack)[which(values(pn_stack) == 0)] <- NA
pn_mean <- mean(pn_stack, na.rm = T)
pn_max <- max(pn_stack, na.rm = T)

setwd('../../../../../Results/')
writeRaster(pn_mean, 'present_natural_mean.tif', overwrite = T)
writeRaster(pn_max, 'present_natural_max.tif', overwrite = T)

rm(pn_list, pn_stack,
   pn_mean, pn_max)

gc(verbose = F)

# Present natural extant species ---------------------------------------------------------------------
setwd('../Data/PHYLACINE_1.2.0/Data/Ranges/Present_natural')

species <- phy %>% 
  filter(!IUCN.Status.1.2 %in% c('EP', 'EX', 'EW')) %>% 
  pull(Binomial.1.2)

blocks <- floor(length(species)/500)
H <- phy %>% filter(Binomial.1.2 %in% species) %>% pull(`Home range`)

pn_extant_list <- foreach(i = 1:(blocks + 1), .combine = c) %dopar%{
  parallel_stack(i, H)
}

pn_extant_stack <- stack(pn_extant_list)
values(pn_extant_stack)[which(values(pn_extant_stack) == 0)] <- NA
pn_extant_mean <- mean(pn_extant_stack, na.rm = T)
pn_extant_max <- max(pn_extant_stack, na.rm = T)

setwd('../../../../../Results/')
writeRaster(pn_extant_mean, 'pn_extant_mean.tif', overwrite = T)
writeRaster(pn_extant_max, 'pn_extant_max.tif', overwrite = T)

rm(pn_extant_list, pn_extant_stack,
   pn_extant_mean, pn_extant_max)
gc(verbose = F)

# Current -----------------------------------------------------------------
setwd('../Data/PHYLACINE_1.2.0/Data/Ranges/Current')

species <- phy %>% 
  filter(!IUCN.Status.1.2 %in% c('EP', 'EX', 'EW')) %>% 
  pull(Binomial.1.2)

blocks <- floor(length(species)/500)
H <- phy %>% filter(Binomial.1.2 %in% species) %>% pull(`Home range`)

cu_list <- foreach(i = 1:(blocks + 1), .combine = c) %dopar%{
  parallel_stack(i, H)
}

cu_stack <- stack(cu_list)
values(cu_stack)[which(values(cu_stack) == 0)] <- NA
cu_mean <- mean(cu_stack, na.rm = T)
cu_max <- max(cu_stack, na.rm = T)

setwd('../../../../../Results/')
writeRaster(cu_mean, 'current_mean.tif', overwrite = T)
writeRaster(cu_max, 'current_max.tif', overwrite = T)

rm(cu_list, cu_stack,
   cu_mean, cu_max)
gc(verbose = F)

# Threatened extinct -------------------------------------------
setwd('../Data/PHYLACINE_1.2.0/Data/Ranges/Current')

species <- phy %>% 
  filter(!IUCN.Status.1.2 %in% c('EP', 'EX', 'EW', 'CR', 'EN', 'VU')) %>% 
  pull(Binomial.1.2)

blocks <- floor(length(species)/500)
H <- phy %>% filter(Binomial.1.2 %in% species) %>% pull(`Home range`)

vu_list <- foreach(i = 1:(blocks + 1), .combine = c) %dopar%{
  parallel_stack(i, H)
}

vu_stack <- stack(vu_list)
values(vu_stack)[which(values(vu_stack) == 0)] <- NA
vu_mean <- mean(vu_stack, na.rm = T)
vu_max <- max(vu_stack, na.rm = T)

setwd('../../../../../Results/')
writeRaster(vu_mean, 'vulnerable_mean.tif', overwrite = T)
writeRaster(vu_max, 'vulnerable_max.tif', overwrite = T)

rm(vu_list, vu_stack,
   vu_mean, vu_max)
gc(verbose = F)

# Conservative rewilding ---------------------------------------------------------------------
setwd('../Data/PHYLACINE_1.2.0/Data/Ranges/Present_natural')

species <- phy %>% 
  filter(
    !IUCN.Status.1.2 %in% c('EP', 'EX', 'EW'),
    !(Diet == "H" & Mass.g > 500000),
    !(Diet == "C" & Mass.g > 100000),
    !(Diet == "O" & Mass.g > 100000)
  ) %>% 
  pull(Binomial.1.2)

species_current <- phy %>% 
  filter(
    !IUCN.Status.1.2 %in% c('EP', 'EX', 'EW'),
    !Binomial.1.2 %in% species
  ) %>% 
  pull(Binomial.1.2)

blocks <- floor(length(species)/500)
H <- phy %>% filter(Binomial.1.2 %in% species) %>% pull(`Home range`)
rw_extant_list1 <- foreach(i = 1:(blocks + 1), .combine = c) %dopar%{
  parallel_stack(i, H)
}
setwd('../Current')
H <- phy %>% filter(Binomial.1.2 %in% species_current) %>% pull(`Home range`)
rw_extant_list2 <- stack(paste0(species_current, '.tif'), quick = T) * H #don't need to parallelize so few species

rw_extant_stack <- stack(rw_extant_list1)
rw_extant_stack <- stack(rw_extant_stack, rw_extant_list2)
values(rw_extant_stack)[which(values(rw_extant_stack) == 0)] <- NA
rw_extant_mean <- mean(rw_extant_stack, na.rm = T)
rw_extant_max <- max(rw_extant_stack, na.rm = T)

setwd('../../../../../Results/')
writeRaster(rw_extant_mean, 'rw_extant_mean.tif', overwrite = T)
writeRaster(rw_extant_max, 'rw_extant_max.tif', overwrite = T)

rm(rw_extant_list1, rw_extant_list2, rw_extant_stack,
   rw_extant_mean, rw_extant_max)
gc(verbose = F)

setwd('..')

stopImplicitCluster()
