continents <- stack("Data/continents-stack.envi")
names(continents) <- c("africa", "asia", "australasia", "europe", "north.america", "south.america")

registerDoParallel(3) #on my personal laptop, COVID outbreak. Around ---- hours per scenario

# present-natural -----
setwd('Data/PHYLACINE_1.2.0/Data/Ranges/Present_natural')

mc <- phy %>%
  filter(Mass.g >= 10^5,
         Diet == "C" | Diet == "O") %>% 
  pull(Binomial.1.2)
lc <- phy %>%
  filter(Mass.g < 10^5,
         Mass.g >= 21500,
         Diet == "C" | Diet == "O") %>% 
  pull(Binomial.1.2)
sc <- phy %>%
  filter(Mass.g < 21500,
         Diet == "C" | Diet == "O") %>% 
  pull(Binomial.1.2)
mh <- phy %>%
  filter(Mass.g >= 10^6,
         Diet == "H") %>% 
  pull(Binomial.1.2)
lh <- phy %>%
  filter(Mass.g < 10^6,
         Mass.g >= 45000,
         Diet == "H") %>% 
  pull(Binomial.1.2)
sh <- phy %>%
  filter(Mass.g < 45000,
         Diet == "H") %>% 
  pull(Binomial.1.2)
pn <- list(stack(paste0(mc, ".tif"), quick = TRUE),
           stack(paste0(lc, ".tif"), quick = TRUE),
           stack(paste0(sc, ".tif"), quick = TRUE),
           stack(paste0(mh, ".tif"), quick = TRUE),
           stack(paste0(lh, ".tif"), quick = TRUE),
           stack(paste0(sh, ".tif"), quick = TRUE))
names(pn) <- c("Megacarnivores", 
               "Large carnivores",
               "Small carnivores",
               "Megaherbivores",
               "Large herbivores",
               "Small herbivores")
T0 <- Sys.time()
foreach (i = names(pn), .combine = "rbind") %do% {
  if (dim(pn[[i]])[3] < 150) {
    r <- sum(pn[[i]])
    r <- continents * r
  } else {
    foreach (j = 1:ceiling(dim(pn[[i]])[3] / 150), .combine = "list") %dopar% {
      sub_pn <- list()
      if (j != ceiling(dim(pn[[i]])[3] / 150)) {
        sub_pn[[j]] <- pn[[i]][[(1 + 150 * (j - 1)):(150 * j)]]
        sub_pn[[j]] <- sum(sub_pn[[j]])
      } else {
        sub_pn[[j]] <- pn[[i]][[(1 + 150 * (j - 1)):(dim(pn[[i]])[3])]]
        sub_pn[[j]] <- sum(sub_pn[[j]])
      }
      sub_pn[[j]]
    } -> r
    r <- unlist(r)
    r <- sum(stack(r))
    r <- continents * r
  }
  Mean <- apply(values(r), MARGIN = 2, mean, na.rm = TRUE)
  Std <- apply(values(r), MARGIN = 2, sd, na.rm = TRUE)
  Median <- apply(values(r), MARGIN = 2, median, na.rm = TRUE)
  data.frame(Continent = names(continents),
             Mean = Mean,
             Std = Std,
             Median = Median,
             Class = i)
} -> pn_cont
Sys.time() - T0
rm(pn, cont.list, pn_list)
gc()
pn_cont %>% 
  as_tibble() %>%
  write_csv("../../../../../Results/pn_richness.csv")

# present-natural extant species -----
mc <- phy %>%
  filter(Mass.g >= 10^5,
         Diet == "C" | Diet == "O",
         !IUCN.Status.1.2 %in% c('EP', 'EX', 'EW')) %>% 
  pull(Binomial.1.2)
lc <- phy %>%
  filter(Mass.g < 10^5,
         Mass.g >= 21500,
         Diet == "C" | Diet == "O",
         !IUCN.Status.1.2 %in% c('EP', 'EX', 'EW')) %>% 
  pull(Binomial.1.2)
sc <- phy %>%
  filter(Mass.g < 21500,
         Diet == "C" | Diet == "O",
         !IUCN.Status.1.2 %in% c('EP', 'EX', 'EW')) %>% 
  pull(Binomial.1.2)
mh <- phy %>%
  filter(Mass.g >= 10^6,
         Diet == "H",
         !IUCN.Status.1.2 %in% c('EP', 'EX', 'EW')) %>% 
  pull(Binomial.1.2)
lh <- phy %>%
  filter(Mass.g < 10^6,
         Mass.g >= 45000,
         Diet == "H",
         !IUCN.Status.1.2 %in% c('EP', 'EX', 'EW')) %>% 
  pull(Binomial.1.2)
sh <- phy %>%
  filter(Mass.g < 45000,
         Diet == "H",
         !IUCN.Status.1.2 %in% c('EP', 'EX', 'EW')) %>% 
  pull(Binomial.1.2)
pne <- list(stack(paste0(mc, ".tif"), quick = TRUE),
            stack(paste0(lc, ".tif"), quick = TRUE),
            stack(paste0(sc, ".tif"), quick = TRUE),
            stack(paste0(mh, ".tif"), quick = TRUE),
            stack(paste0(lh, ".tif"), quick = TRUE),
            stack(paste0(sh, ".tif"), quick = TRUE))
names(pne) <- c("Megacarnivores", 
                "Large carnivores",
                "Small carnivores",
                "Megaherbivores",
                "Large herbivores",
                "Small herbivores")
foreach (i = names(pne), .combine = "rbind") %do% {
  if (dim(pne[[i]])[3] < 150) {
    r <- sum(pne[[i]])
    r <- continents * r
  } else {
    foreach (j = 1:ceiling(dim(pne[[i]])[3] / 150), .combine = "list") %dopar% {
      sub_pn <- list()
      if (j != ceiling(dim(pne[[i]])[3] / 150)) {
        sub_pn[[j]] <- pne[[i]][[(1 + 150 * (j - 1)):(150 * j)]]
        sub_pn[[j]] <- sum(sub_pn[[j]])
      } else {
        sub_pn[[j]] <- pne[[i]][[(1 + 150 * (j - 1)):(dim(pne[[i]])[3])]]
        sub_pn[[j]] <- sum(sub_pn[[j]])
      }
      sub_pn[[j]]
    } -> r
    r <- unlist(r)
    r <- sum(stack(r))
    r <- continents * r
  }
  Mean <- apply(values(r), MARGIN = 2, mean, na.rm = TRUE)
  Std <- apply(values(r), MARGIN = 2, sd, na.rm = TRUE)
  Median <- apply(values(r), MARGIN = 2, median, na.rm = TRUE)
  data.frame(Continent = names(continents),
             Mean = Mean,
             Std = Std,
             Median = Median,
             Class = i)
} -> pne_cont
Sys.time() - T0
rm(pne, cont.list, pne_list)
gc()
pne_cont %>% 
  as_tibble() %>%
  write_csv("../../../../../Results/pne_richness.csv")

# current -----
setwd('../Current')
mc <- phy %>%
  filter(Mass.g >= 10^5,
         Diet == "C" | Diet == "O",
         !IUCN.Status.1.2 %in% c('EP', 'EX', 'EW')) %>% 
  pull(Binomial.1.2)
lc <- phy %>%
  filter(Mass.g < 10^5,
         Mass.g >= 21500,
         Diet == "C" | Diet == "O",
         !IUCN.Status.1.2 %in% c('EP', 'EX', 'EW')) %>% 
  pull(Binomial.1.2)
sc <- phy %>%
  filter(Mass.g < 21500,
         Diet == "C" | Diet == "O",
         !IUCN.Status.1.2 %in% c('EP', 'EX', 'EW')) %>% 
  pull(Binomial.1.2)
mh <- phy %>%
  filter(Mass.g >= 10^6,
         Diet == "H",
         !IUCN.Status.1.2 %in% c('EP', 'EX', 'EW')) %>% 
  pull(Binomial.1.2)
lh <- phy %>%
  filter(Mass.g < 10^6,
         Mass.g >= 45000,
         Diet == "H",
         !IUCN.Status.1.2 %in% c('EP', 'EX', 'EW')) %>% 
  pull(Binomial.1.2)
sh <- phy %>%
  filter(Mass.g < 45000,
         Diet == "H",
         !IUCN.Status.1.2 %in% c('EP', 'EX', 'EW')) %>% 
  pull(Binomial.1.2)

cu <- list(stack(paste0(mc, ".tif"), quick = TRUE),
           stack(paste0(lc, ".tif"), quick = TRUE),
           stack(paste0(sc, ".tif"), quick = TRUE),
           stack(paste0(mh, ".tif"), quick = TRUE),
           stack(paste0(lh, ".tif"), quick = TRUE),
           stack(paste0(sh, ".tif"), quick = TRUE))
names(cu) <- c("Megacarnivores", 
               "Large carnivores",
               "Small carnivores",
               "Megaherbivores",
               "Large herbivores",
               "Small herbivores")
T0 <- Sys.time()
foreach (i = names(cu), .combine = "rbind") %do% {
  if (dim(cu[[i]])[3] < 150) {
    r <- sum(cu[[i]])
    r <- continents * r
  } else {
    foreach (j = 1:ceiling(dim(cu[[i]])[3] / 150), .combine = "list") %dopar% {
      sub_pn <- list()
      if (j != ceiling(dim(cu[[i]])[3] / 150)) {
        sub_pn[[j]] <- cu[[i]][[(1 + 150 * (j - 1)):(150 * j)]]
        sub_pn[[j]] <- sum(sub_pn[[j]])
      } else {
        sub_pn[[j]] <- cu[[i]][[(1 + 150 * (j - 1)):(dim(cu[[i]])[3])]]
        sub_pn[[j]] <- sum(sub_pn[[j]])
      }
      sub_pn[[j]]
    } -> r
    r <- unlist(r)
    r <- sum(stack(r))
    r <- continents * r
  }
  Mean <- apply(values(r), MARGIN = 2, mean, na.rm = TRUE)
  Std <- apply(values(r), MARGIN = 2, sd, na.rm = TRUE)
  Median <- apply(values(r), MARGIN = 2, median, na.rm = TRUE)
  data.frame(Continent = names(continents),
             Mean = Mean,
             Std = Std,
             Median = Median,
             Class = i)
} -> cu_cont
Sys.time() - T0
rm(cu, cont.list, cu_list)
gc()
pn_cont %>% 
  as_tibble() %>%
  write_csv("../../../../../Results/cu_richness.csv")


# threatened species -----
mc <- phy %>%
  filter(Mass.g >= 10^5,
         Diet == "C" | Diet == "O",
         !IUCN.Status.1.2 %in% c('EP', 'EX', 'EW', 'CR', 'EN', 'VU')) %>% 
  pull(Binomial.1.2)
lc <- phy %>%
  filter(Mass.g < 10^5,
         Mass.g >= 21500,
         Diet == "C" | Diet == "O",
         !IUCN.Status.1.2 %in% c('EP', 'EX', 'EW', 'CR', 'EN', 'VU')) %>% 
  pull(Binomial.1.2)
sc <- phy %>%
  filter(Mass.g < 21500,
         Diet == "C" | Diet == "O",
         !IUCN.Status.1.2 %in% c('EP', 'EX', 'EW', 'CR', 'EN', 'VU')) %>% 
  pull(Binomial.1.2)
mh <- phy %>%
  filter(Mass.g >= 10^6,
         Diet == "H",
         !IUCN.Status.1.2 %in% c('EP', 'EX', 'EW', 'CR', 'EN', 'VU')) %>% 
  pull(Binomial.1.2)
lh <- phy %>%
  filter(Mass.g < 10^6,
         Mass.g >= 45000,
         Diet == "H",
         !IUCN.Status.1.2 %in% c('EP', 'EX', 'EW', 'CR', 'EN', 'VU')) %>% 
  pull(Binomial.1.2)
sh <- phy %>%
  filter(Mass.g < 45000,
         Diet == "H",
         !IUCN.Status.1.2 %in% c('EP', 'EX', 'EW', 'CR', 'EN', 'VU')) %>% 
  pull(Binomial.1.2)

th <- list(stack(paste0(mc, ".tif"), quick = TRUE),
           stack(paste0(lc, ".tif"), quick = TRUE),
           stack(paste0(sc, ".tif"), quick = TRUE),
           stack(paste0(mh, ".tif"), quick = TRUE),
           stack(paste0(lh, ".tif"), quick = TRUE),
           stack(paste0(sh, ".tif"), quick = TRUE))
names(th) <- c("Megacarnivores", 
               "Large carnivores",
               "Small carnivores",
               "Megaherbivores",
               "Large herbivores",
               "Small herbivores")
foreach (i = names(th), .combine = "rbind") %do% {
  if (dim(th[[i]])[3] < 150) {
    r <- sum(th[[i]])
    r <- continents * r
  } else {
    foreach (j = 1:ceiling(dim(th[[i]])[3] / 150), .combine = "list") %dopar% {
      sub_pn <- list()
      if (j != ceiling(dim(th[[i]])[3] / 150)) {
        sub_pn[[j]] <- th[[i]][[(1 + 150 * (j - 1)):(150 * j)]]
        sub_pn[[j]] <- sum(sub_pn[[j]])
      } else {
        sub_pn[[j]] <- th[[i]][[(1 + 150 * (j - 1)):(dim(th[[i]])[3])]]
        sub_pn[[j]] <- sum(sub_pn[[j]])
      }
      sub_pn[[j]]
    } -> r
    r <- unlist(r)
    r <- sum(stack(r))
    r <- continents * r
  }
  Mean <- apply(values(r), MARGIN = 2, mean, na.rm = TRUE)
  Std <- apply(values(r), MARGIN = 2, sd, na.rm = TRUE)
  Median <- apply(values(r), MARGIN = 2, median, na.rm = TRUE)
  data.frame(Continent = names(continents),
             Mean = Mean,
             Std = Std,
             Median = Median,
             Class = i)
} -> th_cont
Sys.time() - T0
rm(th, cont.list, th_list)
gc()
th_cont %>% 
  as_tibble() %>%
  write_csv("../../../../../Results/th_richness.csv")

stopImplicitCluster()

# combine results and write csv -----
setwd("../../../../..")

bind_rows(
  pn_cont %>% as_tibble %>% mutate(Scenario = "PN"),
  pne_cont %>% as_tibble %>% mutate(Scenario = "PNE"),
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
  write_csv("Results/strat_sampl.csv")

# analysis ------
res <- read_csv("Results/continent_list.csv")

x <- res %>% 
  filter(Scenario == "PN", Continent == "north.america") %>% 
  pull(`Home range`)

y <- res %>% 
  filter(Scenario == "CU", Continent == "north.america") %>% 
  pull(`Home range`)

es <- new_effect_size(x, y)
magnitude(es$`P(X > Y)`)
