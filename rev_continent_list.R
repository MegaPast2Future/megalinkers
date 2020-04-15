continents <- stack("Data/continents-stack.envi")
names(continents) <- c("africa", "asia", "australasia", "europe", "north.america", "south.america")

registerDoParallel(3) #on my personal laptop, COVID outbreak. Around 1.25 hours per scenario

# present-natural -----
setwd('Data/PHYLACINE_1.2.0/Data/Ranges/Present_natural')

species <- phy %>%
  pull(Binomial.1.2)
pn <- stack(paste0(species, ".tif"), quick = TRUE)
T0 <- Sys.time()
foreach (i = species, .combine = "rbind") %dopar% {
  r <- continents * pn[[i]]
  r_cont <- names(continents)[apply(values(r), MARGIN = 2, max, na.rm = TRUE) == 1]
  if (!is_empty(r_cont)) {
    data.frame(Continent = r_cont, Species = i)
  }
} -> pn_cont
Sys.time() - T0
rm(pn, cont.list, pn_list)
gc()
pn_cont %>% 
  as_tibble() %>%
  write_csv("../../../../../Results/pn_list.csv")

# full rewilding  -----
pne_cont <- list()
species <- phy %>%
  filter(!IUCN.Status.1.2 %in% c('EP', 'EX', 'EW')) %>% 
  pull(Binomial.1.2)
pne <- stack(paste0(species, ".tif"), quick = TRUE)
foreach (i = species, .combine = "rbind") %dopar% {
  r <- continents * pne[[i]]
  r_cont <- names(continents)[apply(values(r), MARGIN = 2, max, na.rm = TRUE) == 1]
  if (!is_empty(r_cont)) {
    data.frame(Continent = r_cont, Species = i)
  }
} -> pne_cont
rm(pne, cont.list, pne_list)
gc()

pne_cont %>% 
  as_tibble() %>%
  write_csv("../../../../../Results/frw_list.csv")

# conservative rewilding  -----
crw_cont <- list()
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
crw1 <- stack(paste0(species, ".tif"), quick = TRUE)
setwd('../Current')
crw2 <- stack(paste0(species_current, ".tif"), quick = TRUE)
crw <- stack(crw1, crw2, quick = TRUE)
species <- union(species, species_current)
foreach (i = species, .combine = "rbind") %dopar% {
  r <- continents * crw[[i]]
  r_cont <- names(continents)[apply(values(r), MARGIN = 2, max, na.rm = TRUE) == 1]
  if (!is_empty(r_cont)) {
    data.frame(Continent = r_cont, Species = i)
  }
} -> crw_cont
rm(crw, cont.list, crw_list)
gc()

crw_cont %>% 
  as_tibble() %>%
  write_csv("../../../../../Results/crw_list.csv")

# current -----
cu_cont <- list()
species <- phy %>%
  filter(!IUCN.Status.1.2 %in% c('EP', 'EX', 'EW')) %>% 
  pull(Binomial.1.2)
cu <- stack(paste0(species, ".tif"), quick = TRUE)
foreach (i = species, .combine = "rbind") %dopar% {
  r <- continents * cu[[i]]
  r_cont <- names(continents)[apply(values(r), MARGIN = 2, max, na.rm = TRUE) == 1]
  if (!is_empty(r_cont)) {
    data.frame(Continent = r_cont, Species = i)
  }
} -> cu_cont
rm(cu, cont.list, cu_list)
gc()

cu_cont %>% 
  as_tibble() %>%
  write_csv("../../../../../Results/cu_list.csv")

# threatened species -----
th_cont <- list()
species <- phy %>% 
  filter(!IUCN.Status.1.2 %in% c('EP', 'EX', 'EW', 'CR', 'EN', 'VU')) %>% 
  pull(Binomial.1.2)
th <- stack(paste0(species, ".tif"), quick = TRUE)
foreach (i = species, .combine = "rbind") %dopar% {
  r <- continents * th[[i]]
  r_cont <- names(continents)[apply(values(r), MARGIN = 2, max, na.rm = TRUE) == 1]
  if (!is_empty(r_cont)) {
    data.frame(Continent = r_cont, Species = i)
  }
} -> th_cont
rm(th, cont.list, th_list)
gc()

th_cont %>% 
  as_tibble() %>%
  write_csv("../../../../../Results/th_list.csv")

stopImplicitCluster()

# combine results and write csv -----
setwd("../../../../..")

bind_rows(
  pn_cont %>% as_tibble %>% mutate(Scenario = "PN"),
  pne_cont %>% as_tibble %>% mutate(Scenario = "FRW"),
  crw_cont %>% as_tibble %>% mutate(Scenario = "CRW"),
  cu_cont %>% as_tibble %>% mutate(Scenario = "CU"),
  th_cont %>% as_tibble %>% mutate(Scenario = "TH")
) %>% 
  left_join(transmute(phy, 
                      Species = Binomial.1.2, 
                      Mass.g,
                      Diet,
                      IUCN.Status.1.2,
                      `Home range` = map2(Diet, Mass.g, function(x, y){
                        homerange(x, y)[1]
                      }) %>% unlist())) %>% 
  #filter_all(any_vars(is.na(.)))
  write_csv("Results/continent_list.csv")

stopImplicitCluster()
