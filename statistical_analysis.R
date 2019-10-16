#this is obsolete, and it has been replaced by analyses.R

new_effect_size(pn_mean, pne_mean) %>% add_column(`Statistic` = 'Mean', Comparison = 'PN - PNE') %>% 
  full_join(new_effect_size(pn_mean, cu_mean) %>% add_column(`Statistic` = 'Mean', Comparison = 'PN - CU')) %>% 
  full_join(new_effect_size(pne_mean, cu_mean) %>% add_column(`Statistic` = 'Mean', Comparison = 'PNE - CU')) %>% 
  full_join(new_effect_size(cu_mean, cr_mean) %>% add_column(`Statistic` = 'Mean', Comparison = 'CU - CR')) %>% 
  full_join(new_effect_size(cu_mean, en_mean) %>% add_column(`Statistic` = 'Mean', Comparison = 'CU - EN')) %>% 
  full_join(new_effect_size(cu_mean, vu_mean) %>% add_column(`Statistic` = 'Mean', Comparison = 'CU - VU')) %>%
  full_join(new_effect_size(pn_max, pne_max) %>% add_column(`Statistic` = 'Max', Comparison = 'PN - PNE')) %>% 
  full_join(new_effect_size(pn_max, cu_max) %>% add_column(`Statistic` = 'Max', Comparison = 'PN - CU')) %>% 
  full_join(new_effect_size(pne_max, cu_max) %>% add_column(`Statistic` = 'Max', Comparison = 'PNE - CU')) %>% 
  full_join(new_effect_size(cu_max, cr_max) %>% add_column(`Statistic` = 'Max', Comparison = 'CU - CR')) %>% 
  full_join(new_effect_size(cu_max, en_max) %>% add_column(`Statistic` = 'Max', Comparison = 'CU - EN')) %>% 
  full_join(new_effect_size(cu_max, vu_max) %>% add_column(`Statistic` = 'Max', Comparison = 'CU - VU')) %>%
  full_join(new_effect_size(pn_mean, pne_mean, global = F, area = 'Africa') %>% add_column(`Statistic` = 'Mean', Comparison = 'PN - PNE')) %>% 
  full_join(new_effect_size(pn_mean, cu_mean, global = F, area = 'Africa') %>% add_column(`Statistic` = 'Mean', Comparison = 'PN - CU')) %>% 
  full_join(new_effect_size(pne_mean, cu_mean, global = F, area = 'Africa') %>% add_column(`Statistic` = 'Mean', Comparison = 'PNE - CU')) %>% 
  full_join(new_effect_size(cu_mean, cr_mean, global = F, area = 'Africa') %>% add_column(`Statistic` = 'Mean', Comparison = 'CU - CR')) %>% 
  full_join(new_effect_size(cu_mean, en_mean, global = F, area = 'Africa') %>% add_column(`Statistic` = 'Mean', Comparison = 'CU - EN')) %>% 
  full_join(new_effect_size(cu_mean, vu_mean, global = F, area = 'Africa') %>% add_column(`Statistic` = 'Mean', Comparison = 'CU - VU')) %>%
  full_join(new_effect_size(pn_max, pne_max, global = F, area = 'Africa') %>% add_column(`Statistic` = 'Max', Comparison = 'PN - PNE')) %>% 
  full_join(new_effect_size(pn_max, cu_max, global = F, area = 'Africa') %>% add_column(`Statistic` = 'Max', Comparison = 'PN - CU')) %>% 
  full_join(new_effect_size(pne_max, cu_max, global = F, area = 'Africa') %>% add_column(`Statistic` = 'Max', Comparison = 'PNE - CU')) %>% 
  full_join(new_effect_size(cu_max, cr_max, global = F, area = 'Africa') %>% add_column(`Statistic` = 'Max', Comparison = 'CU - CR')) %>% 
  full_join(new_effect_size(cu_max, en_max, global = F, area = 'Africa') %>% add_column(`Statistic` = 'Max', Comparison = 'CU - EN')) %>% 
  full_join(new_effect_size(cu_max, vu_max, global = F, area = 'Africa') %>% add_column(`Statistic` = 'Max', Comparison = 'CU - VU')) %>%
  full_join(new_effect_size(pn_mean, pne_mean, global = F, area = 'Asia') %>% add_column(`Statistic` = 'Mean', Comparison = 'PN - PNE')) %>% 
  full_join(new_effect_size(pn_mean, cu_mean, global = F, area = 'Asia') %>% add_column(`Statistic` = 'Mean', Comparison = 'PN - CU')) %>% 
  full_join(new_effect_size(pne_mean, cu_mean, global = F, area = 'Asia') %>% add_column(`Statistic` = 'Mean', Comparison = 'PNE - CU')) %>% 
  full_join(new_effect_size(cu_mean, cr_mean, global = F, area = 'Asia') %>% add_column(`Statistic` = 'Mean', Comparison = 'CU - CR')) %>% 
  full_join(new_effect_size(cu_mean, en_mean, global = F, area = 'Asia') %>% add_column(`Statistic` = 'Mean', Comparison = 'CU - EN')) %>% 
  full_join(new_effect_size(cu_mean, vu_mean, global = F, area = 'Asia') %>% add_column(`Statistic` = 'Mean', Comparison = 'CU - VU')) %>%
  full_join(new_effect_size(pn_max, pne_max, global = F, area = 'Asia') %>% add_column(`Statistic` = 'Max', Comparison = 'PN - PNE')) %>% 
  full_join(new_effect_size(pn_max, cu_max, global = F, area = 'Asia') %>% add_column(`Statistic` = 'Max', Comparison = 'PN - CU')) %>% 
  full_join(new_effect_size(pne_max, cu_max, global = F, area = 'Asia') %>% add_column(`Statistic` = 'Max', Comparison = 'PNE - CU')) %>% 
  full_join(new_effect_size(cu_max, cr_max, global = F, area = 'Asia') %>% add_column(`Statistic` = 'Max', Comparison = 'CU - CR')) %>% 
  full_join(new_effect_size(cu_max, en_max, global = F, area = 'Asia') %>% add_column(`Statistic` = 'Max', Comparison = 'CU - EN')) %>% 
  full_join(new_effect_size(cu_max, vu_max, global = F, area = 'Asia') %>% add_column(`Statistic` = 'Max', Comparison = 'CU - VU')) %>% 
  full_join(new_effect_size(pn_mean, pne_mean, global = F, area = 'Europe') %>% add_column(`Statistic` = 'Mean', Comparison = 'PN - PNE')) %>% 
  full_join(new_effect_size(pn_mean, cu_mean, global = F, area = 'Europe') %>% add_column(`Statistic` = 'Mean', Comparison = 'PN - CU')) %>% 
  full_join(new_effect_size(pne_mean, cu_mean, global = F, area = 'Europe') %>% add_column(`Statistic` = 'Mean', Comparison = 'PNE - CU')) %>% 
  full_join(new_effect_size(cu_mean, cr_mean, global = F, area = 'Europe') %>% add_column(`Statistic` = 'Mean', Comparison = 'CU - CR')) %>% 
  full_join(new_effect_size(cu_mean, en_mean, global = F, area = 'Europe') %>% add_column(`Statistic` = 'Mean', Comparison = 'CU - EN')) %>% 
  full_join(new_effect_size(cu_mean, vu_mean, global = F, area = 'Europe') %>% add_column(`Statistic` = 'Mean', Comparison = 'CU - VU')) %>%
  full_join(new_effect_size(pn_max, pne_max, global = F, area = 'Europe') %>% add_column(`Statistic` = 'Max', Comparison = 'PN - PNE')) %>% 
  full_join(new_effect_size(pn_max, cu_max, global = F, area = 'Europe') %>% add_column(`Statistic` = 'Max', Comparison = 'PN - CU')) %>% 
  full_join(new_effect_size(pne_max, cu_max, global = F, area = 'Europe') %>% add_column(`Statistic` = 'Max', Comparison = 'PNE - CU')) %>% 
  full_join(new_effect_size(cu_max, cr_max, global = F, area = 'Europe') %>% add_column(`Statistic` = 'Max', Comparison = 'CU - CR')) %>% 
  full_join(new_effect_size(cu_max, en_max, global = F, area = 'Europe') %>% add_column(`Statistic` = 'Max', Comparison = 'CU - EN')) %>% 
  full_join(new_effect_size(cu_max, vu_max, global = F, area = 'Europe') %>% add_column(`Statistic` = 'Max', Comparison = 'CU - VU')) %>% 
  full_join(new_effect_size(pn_mean, pne_mean, global = F, area = 'North America') %>% add_column(`Statistic` = 'Mean', Comparison = 'PN - PNE')) %>% 
  full_join(new_effect_size(pn_mean, cu_mean, global = F, area = 'North America') %>% add_column(`Statistic` = 'Mean', Comparison = 'PN - CU')) %>% 
  full_join(new_effect_size(pne_mean, cu_mean, global = F, area = 'North America') %>% add_column(`Statistic` = 'Mean', Comparison = 'PNE - CU')) %>% 
  full_join(new_effect_size(cu_mean, cr_mean, global = F, area = 'North America') %>% add_column(`Statistic` = 'Mean', Comparison = 'CU - CR')) %>% 
  full_join(new_effect_size(cu_mean, en_mean, global = F, area = 'North America') %>% add_column(`Statistic` = 'Mean', Comparison = 'CU - EN')) %>% 
  full_join(new_effect_size(cu_mean, vu_mean, global = F, area = 'North America') %>% add_column(`Statistic` = 'Mean', Comparison = 'CU - VU')) %>%
  full_join(new_effect_size(pn_max, pne_max, global = F, area = 'North America') %>% add_column(`Statistic` = 'Max', Comparison = 'PN - PNE')) %>% 
  full_join(new_effect_size(pn_max, cu_max, global = F, area = 'North America') %>% add_column(`Statistic` = 'Max', Comparison = 'PN - CU')) %>% 
  full_join(new_effect_size(pne_max, cu_max, global = F, area = 'North America') %>% add_column(`Statistic` = 'Max', Comparison = 'PNE - CU')) %>% 
  full_join(new_effect_size(cu_max, cr_max, global = F, area = 'North America') %>% add_column(`Statistic` = 'Max', Comparison = 'CU - CR')) %>% 
  full_join(new_effect_size(cu_max, en_max, global = F, area = 'North America') %>% add_column(`Statistic` = 'Max', Comparison = 'CU - EN')) %>% 
  full_join(new_effect_size(cu_max, vu_max, global = F, area = 'North America') %>% add_column(`Statistic` = 'Max', Comparison = 'CU - VU')) %>% 
  full_join(new_effect_size(pn_mean, pne_mean, global = F, area = 'South America') %>% add_column(`Statistic` = 'Mean', Comparison = 'PN - PNE')) %>% 
  full_join(new_effect_size(pn_mean, cu_mean, global = F, area = 'South America') %>% add_column(`Statistic` = 'Mean', Comparison = 'PN - CU')) %>% 
  full_join(new_effect_size(pne_mean, cu_mean, global = F, area = 'South America') %>% add_column(`Statistic` = 'Mean', Comparison = 'PNE - CU')) %>% 
  full_join(new_effect_size(cu_mean, cr_mean, global = F, area = 'South America') %>% add_column(`Statistic` = 'Mean', Comparison = 'CU - CR')) %>% 
  full_join(new_effect_size(cu_mean, en_mean, global = F, area = 'South America') %>% add_column(`Statistic` = 'Mean', Comparison = 'CU - EN')) %>% 
  full_join(new_effect_size(cu_mean, vu_mean, global = F, area = 'South America') %>% add_column(`Statistic` = 'Mean', Comparison = 'CU - VU')) %>%
  full_join(new_effect_size(pn_max, pne_max, global = F, area = 'South America') %>% add_column(`Statistic` = 'Max', Comparison = 'PN - PNE')) %>% 
  full_join(new_effect_size(pn_max, cu_max, global = F, area = 'South America') %>% add_column(`Statistic` = 'Max', Comparison = 'PN - CU')) %>% 
  full_join(new_effect_size(pne_max, cu_max, global = F, area = 'South America') %>% add_column(`Statistic` = 'Max', Comparison = 'PNE - CU')) %>% 
  full_join(new_effect_size(cu_max, cr_max, global = F, area = 'South America') %>% add_column(`Statistic` = 'Max', Comparison = 'CU - CR')) %>% 
  full_join(new_effect_size(cu_max, en_max, global = F, area = 'South America') %>% add_column(`Statistic` = 'Max', Comparison = 'CU - EN')) %>% 
  full_join(new_effect_size(cu_max, vu_max, global = F, area = 'South America') %>% add_column(`Statistic` = 'Max', Comparison = 'CU - VU')) %>%
  full_join(new_effect_size(pn_mean, pne_mean, global = F, area = 'Oceania') %>% add_column(`Statistic` = 'Mean', Comparison = 'PN - PNE')) %>% 
  full_join(new_effect_size(pn_mean, cu_mean, global = F, area = 'Oceania') %>% add_column(`Statistic` = 'Mean', Comparison = 'PN - CU')) %>% 
  full_join(new_effect_size(pne_mean, cu_mean, global = F, area = 'Oceania') %>% add_column(`Statistic` = 'Mean', Comparison = 'PNE - CU')) %>% 
  full_join(new_effect_size(cu_mean, cr_mean, global = F, area = 'Oceania') %>% add_column(`Statistic` = 'Mean', Comparison = 'CU - CR')) %>% 
  full_join(new_effect_size(cu_mean, en_mean, global = F, area = 'Oceania') %>% add_column(`Statistic` = 'Mean', Comparison = 'CU - EN')) %>% 
  full_join(new_effect_size(cu_mean, vu_mean, global = F, area = 'Oceania') %>% add_column(`Statistic` = 'Mean', Comparison = 'CU - VU')) %>%
  full_join(new_effect_size(pn_max, pne_max, global = F, area = 'Oceania') %>% add_column(`Statistic` = 'Max', Comparison = 'PN - PNE')) %>% 
  full_join(new_effect_size(pn_max, cu_max, global = F, area = 'Oceania') %>% add_column(`Statistic` = 'Max', Comparison = 'PN - CU')) %>% 
  full_join(new_effect_size(pne_max, cu_max, global = F, area = 'Oceania') %>% add_column(`Statistic` = 'Max', Comparison = 'PNE - CU')) %>% 
  full_join(new_effect_size(cu_max, cr_max, global = F, area = 'Oceania') %>% add_column(`Statistic` = 'Max', Comparison = 'CU - CR')) %>% 
  full_join(new_effect_size(cu_max, en_max, global = F, area = 'Oceania') %>% add_column(`Statistic` = 'Max', Comparison = 'CU - EN')) %>% 
  full_join(new_effect_size(cu_max, vu_max, global = F, area = 'Oceania') %>% add_column(`Statistic` = 'Max', Comparison = 'CU - VU')) 



# # This function return the p-value of the comparison and the effect size,
# # according to Kerby (2014). x and y are two rasters of the interested variable.
# # method is either 'signed-rank' or 'sum-rank', the first for paired and the
# # second for non-paired test.
# effect_size <- function(x, y, method = 'sum-rank', global = T, continent){
#   cont.name <- c('Asia', 'North America', 'Europe', 'Africa', 'South America', 'Oceania')
#   if(global == F){
#     cont.value <- which(cont.name == continent)
#     clipped.continent <- clamp(continents, lower = cont.value, upper = cont.value, useValues = F) / cont.value
#     x <- mask(x, clipped.continent)
#     y <- mask(y, clipped.continent)
#   }
#   x <- 10^values(x)
#   y <- 10^values(y)
#   if(method == 'signed-rank'){
#     rem <- unique(c(which(is.na(x)), which(is.na(y))))
#     x <- x[-rem]
#     y <- y[-rem]
#     diff <- x - y
#     diff <- diff[diff != 0] # remove equal pairs, that are added to V and N as 0.5
#     diff.rank <- rank(abs(diff))
#     diff.rank.sign <- diff.rank * sign(diff)
#     rank.pos <- sum(diff.rank.sign[diff.rank.sign > 0]) # this is the V in the signed-rank sum test
#     rank.neg <- sum(diff.rank.sign[diff.rank.sign < 0])
#     return(c(wilcox.test(x, y, paired = T)$p.value,
#              (rank.pos - abs(rank.neg)) / (rank.pos + abs(rank.neg))))
#   } else if(method == 'sum-rank'){
#     x <- na.omit(x)
#     y <- na.omit(y)
#     return(c(wilcox.test(x, y, paired = F)$p.value,
#              wilcox.test(x, y, paired = F)$statistic[[1]] / sum(1:length(c(x,y)))))
#   }
# }
# 
# results <- tibble(`alpha` = NA, `Rank-biserial correlation` = NA, `Comparison` = NA, `Statistic` = NA, `Continent` = NA) %>%
#   add_row(`alpha` = effect_size(pn_mean, pne_mean)[1], `Rank-biserial correlation` = effect_size(pn_mean, pne_mean)[2], `Comparison` = 'PN - PNE', `Statistic` = 'Mean', `Continent` = 'World') %>% 
#   add_row(`alpha` = effect_size(pn_mean, cu_mean)[1], `Rank-biserial correlation` = effect_size(pn_mean, cu_mean)[2], `Comparison` = 'PN - CU', `Statistic` = 'Mean', `Continent` = 'World') %>% 
#   add_row(`alpha` = effect_size(pne_mean, cu_mean)[1], `Rank-biserial correlation` = effect_size(pne_mean, cu_mean)[2], `Comparison` = 'PNE - CU', `Statistic` = 'Mean', `Continent` = 'World') %>% 
#   add_row(`alpha` = effect_size(cu_mean, cr_mean)[1], `Rank-biserial correlation` = effect_size(cu_mean, cr_mean)[2], `Comparison` = 'CU - CR', `Statistic` = 'Mean', `Continent` = 'World') %>% 
#   add_row(`alpha` = effect_size(cu_mean, en_mean)[1], `Rank-biserial correlation` = effect_size(cu_mean, en_mean)[2], `Comparison` = 'CU - EN', `Statistic` = 'Mean', `Continent` = 'World') %>% 
#   add_row(`alpha` = effect_size(cu_mean, vu_mean)[1], `Rank-biserial correlation` = effect_size(cu_mean, vu_mean)[2], `Comparison` = 'CU - VU', `Statistic` = 'Mean', `Continent` = 'World') %>% 
#   add_row(`alpha` = effect_size(pn_max, pne_max)[1], `Rank-biserial correlation` = effect_size(pn_max, pne_max)[2], `Comparison` = 'PN - PNE', `Statistic` = 'Max', `Continent` = 'World') %>% 
#   add_row(`alpha` = effect_size(pn_max, cu_max)[1], `Rank-biserial correlation` = effect_size(pn_max, cu_max)[2], `Comparison` = 'PN - CU', `Statistic` = 'Max', `Continent` = 'World') %>% 
#   add_row(`alpha` = effect_size(pne_max, cu_max)[1], `Rank-biserial correlation` = effect_size(pne_max, cu_max)[2], `Comparison` = 'PNE - CU', `Statistic` = 'Max', `Continent` = 'World') %>% 
#   add_row(`alpha` = effect_size(cu_max, cr_max)[1], `Rank-biserial correlation` = effect_size(cu_max, cr_max)[2], `Comparison` = 'CU - CR', `Statistic` = 'Max', `Continent` = 'World') %>% 
#   add_row(`alpha` = effect_size(cu_max, en_max)[1], `Rank-biserial correlation` = effect_size(cu_max, en_max)[2], `Comparison` = 'CU - EN', `Statistic` = 'Max', `Continent` = 'World') %>% 
#   add_row(`alpha` = effect_size(cu_max, vu_max)[1], `Rank-biserial correlation` = effect_size(cu_max, vu_max)[2], `Comparison` = 'CU - VU', `Statistic` = 'Max', `Continent` = 'World') %>% 
#   add_row(`alpha` = effect_size(pn_mean, pne_mean, global = F, continent = 'Africa')[1], `Rank-biserial correlation` = effect_size(pn_mean, pne_mean, global = F, continent = 'Africa')[2], `Comparison` = 'PN - PNE', `Statistic` = 'Mean', `Continent` = 'Africa') %>% 
#   add_row(`alpha` = effect_size(pn_mean, cu_mean, global = F, continent = 'Africa')[1], `Rank-biserial correlation` = effect_size(pn_mean, cu_mean, global = F, continent = 'Africa')[2], `Comparison` = 'PN - CU', `Statistic` = 'Mean', `Continent` = 'Africa') %>% 
#   add_row(`alpha` = effect_size(pne_mean, cu_mean, global = F, continent = 'Africa')[1], `Rank-biserial correlation` = effect_size(pne_mean, cu_mean, global = F, continent = 'Africa')[2], `Comparison` = 'PNE - CU', `Statistic` = 'Mean', `Continent` = 'Africa') %>% 
#   add_row(`alpha` = effect_size(cu_mean, cr_mean, global = F, continent = 'Africa')[1], `Rank-biserial correlation` = effect_size(cu_mean, cr_mean, global = F, continent = 'Africa')[2], `Comparison` = 'CU - CR', `Statistic` = 'Mean', `Continent` = 'Africa') %>% 
#   add_row(`alpha` = effect_size(cu_mean, en_mean, global = F, continent = 'Africa')[1], `Rank-biserial correlation` = effect_size(cu_mean, en_mean, global = F, continent = 'Africa')[2], `Comparison` = 'CU - EN', `Statistic` = 'Mean', `Continent` = 'Africa') %>% 
#   add_row(`alpha` = effect_size(cu_mean, vu_mean, global = F, continent = 'Africa')[1], `Rank-biserial correlation` = effect_size(cu_mean, vu_mean, global = F, continent = 'Africa')[2], `Comparison` = 'CU - VU', `Statistic` = 'Mean', `Continent` = 'Africa') %>% 
#   add_row(`alpha` = effect_size(pn_max, pne_max, global = F, continent = 'Africa')[1], `Rank-biserial correlation` = effect_size(pn_max, pne_max, global = F, continent = 'Africa')[2], `Comparison` = 'PN - PNE', `Statistic` = 'Max', `Continent` = 'Africa') %>% 
#   add_row(`alpha` = effect_size(pn_max, cu_max, global = F, continent = 'Africa')[1], `Rank-biserial correlation` = effect_size(pn_max, cu_max, global = F, continent = 'Africa')[2], `Comparison` = 'PN - CU', `Statistic` = 'Max', `Continent` = 'Africa') %>% 
#   add_row(`alpha` = effect_size(pne_max, cu_max, global = F, continent = 'Africa')[1], `Rank-biserial correlation` = effect_size(pne_max, cu_max, global = F, continent = 'Africa')[2], `Comparison` = 'PNE - CU', `Statistic` = 'Max', `Continent` = 'Africa') %>% 
#   add_row(`alpha` = effect_size(cu_max, cr_max, global = F, continent = 'Africa')[1], `Rank-biserial correlation` = effect_size(cu_max, cr_max, global = F, continent = 'Africa')[2], `Comparison` = 'CU - CR', `Statistic` = 'Max', `Continent` = 'Africa') %>% 
#   add_row(`alpha` = effect_size(cu_max, en_max, global = F, continent = 'Africa')[1], `Rank-biserial correlation` = effect_size(cu_max, en_max, global = F, continent = 'Africa')[2], `Comparison` = 'CU - EN', `Statistic` = 'Max', `Continent` = 'Africa') %>% 
#   add_row(`alpha` = effect_size(cu_max, vu_max, global = F, continent = 'Africa')[1], `Rank-biserial correlation` = effect_size(cu_max, vu_max, global = F, continent = 'Africa')[2], `Comparison` = 'CU - VU', `Statistic` = 'Max', `Continent` = 'Africa') %>% 
#   add_row(`alpha` = effect_size(pn_mean, pne_mean, global = F, continent = 'Asia')[1], `Rank-biserial correlation` = effect_size(pn_mean, pne_mean, global = F, continent = 'Asia')[2], `Comparison` = 'PN - PNE', `Statistic` = 'Mean', `Continent` = 'Asia') %>% 
#   add_row(`alpha` = effect_size(pn_mean, cu_mean, global = F, continent = 'Asia')[1], `Rank-biserial correlation` = effect_size(pn_mean, cu_mean, global = F, continent = 'Asia')[2], `Comparison` = 'PN - CU', `Statistic` = 'Mean', `Continent` = 'Asia') %>% 
#   add_row(`alpha` = effect_size(pne_mean, cu_mean, global = F, continent = 'Asia')[1], `Rank-biserial correlation` = effect_size(pne_mean, cu_mean, global = F, continent = 'Asia')[2], `Comparison` = 'PNE - CU', `Statistic` = 'Mean', `Continent` = 'Asia') %>% 
#   add_row(`alpha` = effect_size(cu_mean, cr_mean, global = F, continent = 'Asia')[1], `Rank-biserial correlation` = effect_size(cu_mean, cr_mean, global = F, continent = 'Asia')[2], `Comparison` = 'CU - CR', `Statistic` = 'Mean', `Continent` = 'Asia') %>% 
#   add_row(`alpha` = effect_size(cu_mean, en_mean, global = F, continent = 'Asia')[1], `Rank-biserial correlation` = effect_size(cu_mean, en_mean, global = F, continent = 'Asia')[2], `Comparison` = 'CU - EN', `Statistic` = 'Mean', `Continent` = 'Asia') %>% 
#   add_row(`alpha` = effect_size(cu_mean, vu_mean, global = F, continent = 'Asia')[1], `Rank-biserial correlation` = effect_size(cu_mean, vu_mean, global = F, continent = 'Asia')[2], `Comparison` = 'CU - VU', `Statistic` = 'Mean', `Continent` = 'Asia') %>% 
#   add_row(`alpha` = effect_size(pn_max, pne_max, global = F, continent = 'Asia')[1], `Rank-biserial correlation` = effect_size(pn_max, pne_max, global = F, continent = 'Asia')[2], `Comparison` = 'PN - PNE', `Statistic` = 'Max', `Continent` = 'Asia') %>% 
#   add_row(`alpha` = effect_size(pn_max, cu_max, global = F, continent = 'Asia')[1], `Rank-biserial correlation` = effect_size(pn_max, cu_max, global = F, continent = 'Asia')[2], `Comparison` = 'PN - CU', `Statistic` = 'Max', `Continent` = 'Asia') %>% 
#   add_row(`alpha` = effect_size(pne_max, cu_max, global = F, continent = 'Asia')[1], `Rank-biserial correlation` = effect_size(pne_max, cu_max, global = F, continent = 'Asia')[2], `Comparison` = 'PNE - CU', `Statistic` = 'Max', `Continent` = 'Asia') %>% 
#   add_row(`alpha` = effect_size(cu_max, cr_max, global = F, continent = 'Asia')[1], `Rank-biserial correlation` = effect_size(cu_max, cr_max, global = F, continent = 'Asia')[2], `Comparison` = 'CU - CR', `Statistic` = 'Max', `Continent` = 'Asia') %>% 
#   add_row(`alpha` = effect_size(cu_max, en_max, global = F, continent = 'Asia')[1], `Rank-biserial correlation` = effect_size(cu_max, en_max, global = F, continent = 'Asia')[2], `Comparison` = 'CU - EN', `Statistic` = 'Max', `Continent` = 'Asia') %>% 
#   add_row(`alpha` = effect_size(cu_max, vu_max, global = F, continent = 'Asia')[1], `Rank-biserial correlation` = effect_size(cu_max, vu_max, global = F, continent = 'Asia')[2], `Comparison` = 'CU - VU', `Statistic` = 'Max', `Continent` = 'Asia') %>% 
#   add_row(`alpha` = effect_size(pn_mean, pne_mean, global = F, continent = 'Europe')[1], `Rank-biserial correlation` = effect_size(pn_mean, pne_mean, global = F, continent = 'Europe')[2], `Comparison` = 'PN - PNE', `Statistic` = 'Mean', `Continent` = 'Europe') %>% 
#   add_row(`alpha` = effect_size(pn_mean, cu_mean, global = F, continent = 'Europe')[1], `Rank-biserial correlation` = effect_size(pn_mean, cu_mean, global = F, continent = 'Europe')[2], `Comparison` = 'PN - CU', `Statistic` = 'Mean', `Continent` = 'Europe') %>% 
#   add_row(`alpha` = effect_size(pne_mean, cu_mean, global = F, continent = 'Europe')[1], `Rank-biserial correlation` = effect_size(pne_mean, cu_mean, global = F, continent = 'Europe')[2], `Comparison` = 'PNE - CU', `Statistic` = 'Mean', `Continent` = 'Europe') %>% 
#   add_row(`alpha` = effect_size(cu_mean, cr_mean, global = F, continent = 'Europe')[1], `Rank-biserial correlation` = effect_size(cu_mean, cr_mean, global = F, continent = 'Europe')[2], `Comparison` = 'CU - CR', `Statistic` = 'Mean', `Continent` = 'Europe') %>% 
#   add_row(`alpha` = effect_size(cu_mean, en_mean, global = F, continent = 'Europe')[1], `Rank-biserial correlation` = effect_size(cu_mean, en_mean, global = F, continent = 'Europe')[2], `Comparison` = 'CU - EN', `Statistic` = 'Mean', `Continent` = 'Europe') %>% 
#   add_row(`alpha` = effect_size(cu_mean, vu_mean, global = F, continent = 'Europe')[1], `Rank-biserial correlation` = effect_size(cu_mean, vu_mean, global = F, continent = 'Europe')[2], `Comparison` = 'CU - VU', `Statistic` = 'Mean', `Continent` = 'Europe') %>% 
#   add_row(`alpha` = effect_size(pn_max, pne_max, global = F, continent = 'Europe')[1], `Rank-biserial correlation` = effect_size(pn_max, pne_max, global = F, continent = 'Europe')[2], `Comparison` = 'PN - PNE', `Statistic` = 'Max', `Continent` = 'Europe') %>% 
#   add_row(`alpha` = effect_size(pn_max, cu_max, global = F, continent = 'Europe')[1], `Rank-biserial correlation` = effect_size(pn_max, cu_max, global = F, continent = 'Europe')[2], `Comparison` = 'PN - CU', `Statistic` = 'Max', `Continent` = 'Europe') %>% 
#   add_row(`alpha` = effect_size(pne_max, cu_max, global = F, continent = 'Europe')[1], `Rank-biserial correlation` = effect_size(pne_max, cu_max, global = F, continent = 'Europe')[2], `Comparison` = 'PN - CU', `Statistic` = 'Max', `Continent` = 'Europe') %>% 
#   add_row(`alpha` = effect_size(cu_max, cr_max, global = F, continent = 'Europe')[1], `Rank-biserial correlation` = effect_size(cu_max, cr_max, global = F, continent = 'Europe')[2], `Comparison` = 'CU - CR', `Statistic` = 'Max', `Continent` = 'Europe') %>% 
#   add_row(`alpha` = effect_size(cu_max, en_max, global = F, continent = 'Europe')[1], `Rank-biserial correlation` = effect_size(cu_max, en_max, global = F, continent = 'Europe')[2], `Comparison` = 'CU - EN', `Statistic` = 'Max', `Continent` = 'Europe') %>% 
#   add_row(`alpha` = effect_size(cu_max, vu_max, global = F, continent = 'Europe')[1], `Rank-biserial correlation` = effect_size(cu_max, vu_max, global = F, continent = 'Europe')[2], `Comparison` = 'CU - VU', `Statistic` = 'Max', `Continent` = 'Europe') %>% 
#   add_row(`alpha` = effect_size(pn_mean, pne_mean, global = F, continent = 'North America')[1], `Rank-biserial correlation` = effect_size(pn_mean, pne_mean, global = F, continent = 'North America')[2], `Comparison` = 'PN - PNE', `Statistic` = 'Mean', `Continent` = 'North America') %>% 
#   add_row(`alpha` = effect_size(pn_mean, cu_mean, global = F, continent = 'North America')[1], `Rank-biserial correlation` = effect_size(pn_mean, cu_mean, global = F, continent = 'North America')[2], `Comparison` = 'PN - CU', `Statistic` = 'Mean', `Continent` = 'North America') %>% 
#   add_row(`alpha` = effect_size(pne_mean, cu_mean, global = F, continent = 'North America')[1], `Rank-biserial correlation` = effect_size(pne_mean, cu_mean, global = F, continent = 'North America')[2], `Comparison` = 'PNE - CU', `Statistic` = 'Mean', `Continent` = 'North America') %>% 
#   add_row(`alpha` = effect_size(cu_mean, cr_mean, global = F, continent = 'North America')[1], `Rank-biserial correlation` = effect_size(cu_mean, cr_mean, global = F, continent = 'North America')[2], `Comparison` = 'CU - CR', `Statistic` = 'Mean', `Continent` = 'North America') %>% 
#   add_row(`alpha` = effect_size(cu_mean, en_mean, global = F, continent = 'North America')[1], `Rank-biserial correlation` = effect_size(cu_mean, en_mean, global = F, continent = 'North America')[2], `Comparison` = 'CU - EN', `Statistic` = 'Mean', `Continent` = 'North America') %>% 
#   add_row(`alpha` = effect_size(cu_mean, vu_mean, global = F, continent = 'North America')[1], `Rank-biserial correlation` = effect_size(cu_mean, vu_mean, global = F, continent = 'North America')[2], `Comparison` = 'CU - VU', `Statistic` = 'Mean', `Continent` = 'North America') %>% 
#   add_row(`alpha` = effect_size(pn_max, pne_max, global = F, continent = 'North America')[1], `Rank-biserial correlation` = effect_size(pn_max, pne_max, global = F, continent = 'North America')[2], `Comparison` = 'PN - PNE', `Statistic` = 'Max', `Continent` = 'North America') %>% 
#   add_row(`alpha` = effect_size(pn_max, cu_max, global = F, continent = 'North America')[1], `Rank-biserial correlation` = effect_size(pn_max, cu_max, global = F, continent = 'North America')[2], `Comparison` = 'PN - CU', `Statistic` = 'Max', `Continent` = 'North America') %>% 
#   add_row(`alpha` = effect_size(pne_max, cu_max, global = F, continent = 'North America')[1], `Rank-biserial correlation` = effect_size(pne_max, cu_max, global = F, continent = 'North America')[2], `Comparison` = 'PNE - CU', `Statistic` = 'Max', `Continent` = 'North America') %>% 
#   add_row(`alpha` = effect_size(cu_max, cr_max, global = F, continent = 'North America')[1], `Rank-biserial correlation` = effect_size(cu_max, cr_max, global = F, continent = 'North America')[2], `Comparison` = 'CU - CR', `Statistic` = 'Max', `Continent` = 'North America') %>% 
#   add_row(`alpha` = effect_size(cu_max, en_max, global = F, continent = 'North America')[1], `Rank-biserial correlation` = effect_size(cu_max, en_max, global = F, continent = 'North America')[2], `Comparison` = 'CU - EN', `Statistic` = 'Max', `Continent` = 'North America') %>% 
#   add_row(`alpha` = effect_size(cu_max, vu_max, global = F, continent = 'North America')[1], `Rank-biserial correlation` = effect_size(cu_max, vu_max, global = F, continent = 'North America')[2], `Comparison` = 'CU - CR', `Statistic` = 'Max', `Continent` = 'North America') %>% 
#   add_row(`alpha` = effect_size(pn_mean, pne_mean, global = F, continent = 'South America')[1], `Rank-biserial correlation` = effect_size(pn_mean, pne_mean, global = F, continent = 'South America')[2], `Comparison` = 'PN - PNE', `Statistic` = 'Mean', `Continent` = 'South America') %>% 
#   add_row(`alpha` = effect_size(pn_mean, cu_mean, global = F, continent = 'South America')[1], `Rank-biserial correlation` = effect_size(pn_mean, cu_mean, global = F, continent = 'South America')[2], `Comparison` = 'PN - CU', `Statistic` = 'Mean', `Continent` = 'South America') %>% 
#   add_row(`alpha` = effect_size(pne_mean, cu_mean, global = F, continent = 'South America')[1], `Rank-biserial correlation` = effect_size(pne_mean, cu_mean, global = F, continent = 'South America')[2], `Comparison` = 'PNE - CU', `Statistic` = 'Mean', `Continent` = 'South America') %>% 
#   add_row(`alpha` = effect_size(cu_mean, cr_mean, global = F, continent = 'South America')[1], `Rank-biserial correlation` = effect_size(cu_mean, cr_mean, global = F, continent = 'South America')[2], `Comparison` = 'CU - CR', `Statistic` = 'Mean', `Continent` = 'South America') %>% 
#   add_row(`alpha` = effect_size(cu_mean, en_mean, global = F, continent = 'South America')[1], `Rank-biserial correlation` = effect_size(cu_mean, en_mean, global = F, continent = 'South America')[2], `Comparison` = 'CU - EN', `Statistic` = 'Mean', `Continent` = 'South America') %>% 
#   add_row(`alpha` = effect_size(cu_mean, vu_mean, global = F, continent = 'South America')[1], `Rank-biserial correlation` = effect_size(cu_mean, vu_mean, global = F, continent = 'South America')[2], `Comparison` = 'CU - VU', `Statistic` = 'Mean', `Continent` = 'South America') %>% 
#   add_row(`alpha` = effect_size(pn_max, pne_max, global = F, continent = 'South America')[1], `Rank-biserial correlation` = effect_size(pn_max, pne_max, global = F, continent = 'South America')[2], `Comparison` = 'PN - PNE', `Statistic` = 'Max', `Continent` = 'South America') %>% 
#   add_row(`alpha` = effect_size(pn_max, cu_max, global = F, continent = 'South America')[1], `Rank-biserial correlation` = effect_size(pn_max, cu_max, global = F, continent = 'South America')[2], `Comparison` = 'PN - CU', `Statistic` = 'Max', `Continent` = 'South America') %>% 
#   add_row(`alpha` = effect_size(pne_max, cu_max, global = F, continent = 'South America')[1], `Rank-biserial correlation` = effect_size(pne_max, cu_max, global = F, continent = 'South America')[2], `Comparison` = 'PNE - CU', `Statistic` = 'Max', `Continent` = 'South America') %>% 
#   add_row(`alpha` = effect_size(cu_max, cr_max, global = F, continent = 'South America')[1], `Rank-biserial correlation` = effect_size(cu_max, cr_max, global = F, continent = 'South America')[2], `Comparison` = 'CU - CR', `Statistic` = 'Max', `Continent` = 'South America') %>% 
#   add_row(`alpha` = effect_size(cu_max, en_max, global = F, continent = 'South America')[1], `Rank-biserial correlation` = effect_size(cu_max, en_max, global = F, continent = 'South America')[2], `Comparison` = 'CU - EN', `Statistic` = 'Max', `Continent` = 'South America') %>% 
#   add_row(`alpha` = effect_size(cu_max, vu_max, global = F, continent = 'South America')[1], `Rank-biserial correlation` = effect_size(cu_max, vu_max, global = F, continent = 'South America')[2], `Comparison` = 'CU - VU', `Statistic` = 'Max', `Continent` = 'South America') %>% 
#   add_row(`alpha` = effect_size(pn_mean, pne_mean, global = F, continent = 'Oceania')[1], `Rank-biserial correlation` = effect_size(pn_mean, pne_mean, global = F, continent = 'Oceania')[2], `Comparison` = 'PN - PNE', `Statistic` = 'Mean', `Continent` = 'Oceania') %>% 
#   add_row(`alpha` = effect_size(pn_mean, cu_mean, global = F, continent = 'Oceania')[1], `Rank-biserial correlation` = effect_size(pn_mean, cu_mean, global = F, continent = 'Oceania')[2], `Comparison` = 'PN - CU', `Statistic` = 'Mean', `Continent` = 'Oceania') %>% 
#   add_row(`alpha` = effect_size(pne_mean, cu_mean, global = F, continent = 'Oceania')[1], `Rank-biserial correlation` = effect_size(pne_mean, cu_mean, global = F, continent = 'Oceania')[2], `Comparison` = 'PNE - CU', `Statistic` = 'Mean', `Continent` = 'Oceania') %>% 
#   add_row(`alpha` = effect_size(cu_mean, cr_mean, global = F, continent = 'Oceania')[1], `Rank-biserial correlation` = effect_size(cu_mean, cr_mean, global = F, continent = 'Oceania')[2], `Comparison` = 'CU - CR', `Statistic` = 'Mean', `Continent` = 'Oceania') %>% 
#   add_row(`alpha` = effect_size(cu_mean, en_mean, global = F, continent = 'Oceania')[1], `Rank-biserial correlation` = effect_size(cu_mean, en_mean, global = F, continent = 'Oceania')[2], `Comparison` = 'CU - EN', `Statistic` = 'Mean', `Continent` = 'Oceania') %>% 
#   add_row(`alpha` = effect_size(cu_mean, vu_mean, global = F, continent = 'Oceania')[1], `Rank-biserial correlation` = effect_size(cu_mean, vu_mean, global = F, continent = 'Oceania')[2], `Comparison` = 'CU - VU', `Statistic` = 'Mean', `Continent` = 'Oceania') %>% 
#   add_row(`alpha` = effect_size(pn_max, pne_max, global = F, continent = 'Oceania')[1], `Rank-biserial correlation` = effect_size(pn_max, pne_max, global = F, continent = 'Oceania')[2], `Comparison` = 'PN - PNE', `Statistic` = 'Max', `Continent` = 'Oceania') %>% 
#   add_row(`alpha` = effect_size(pn_max, cu_max, global = F, continent = 'Oceania')[1], `Rank-biserial correlation` = effect_size(pn_max, cu_max, global = F, continent = 'Oceania')[2], `Comparison` = 'PN - CU', `Statistic` = 'Max', `Continent` = 'Oceania') %>% 
#   add_row(`alpha` = effect_size(pne_max, cu_max, global = F, continent = 'Oceania')[1], `Rank-biserial correlation` = effect_size(pne_max, cu_max, global = F, continent = 'Oceania')[2], `Comparison` = 'PNE - CU', `Statistic` = 'Max', `Continent` = 'Oceania') %>% 
#   add_row(`alpha` = effect_size(cu_max, cr_max, global = F, continent = 'Oceania')[1], `Rank-biserial correlation` = effect_size(cu_max, cr_max, global = F, continent = 'Oceania')[2], `Comparison` = 'CU - CR', `Statistic` = 'Max', `Continent` = 'Oceania') %>% 
#   add_row(`alpha` = effect_size(cu_max, en_max, global = F, continent = 'Oceania')[1], `Rank-biserial correlation` = effect_size(cu_max, en_max, global = F, continent = 'Oceania')[2], `Comparison` = 'CU - EN', `Statistic` = 'Max', `Continent` = 'Oceania') %>% 
#   add_row(`alpha` = effect_size(cu_max, vu_max, global = F, continent = 'Oceania')[1], `Rank-biserial correlation` = effect_size(cu_max, vu_max, global = F, continent = 'Oceania')[2], `Comparison` = 'CU - CR', `Statistic` = 'Max', `Continent` = 'Oceania') %>%  
#   filter(!is.na(Statistic)) %>% 
#   mutate(Comparison = factor(Comparison, levels = unique(Comparison)))
