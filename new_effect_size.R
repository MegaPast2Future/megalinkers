new_effect_size <- function(x, y, global = T, area = 'World'){
  cont.name <- c('Asia', 'North America', 'Europe', 'Africa', 'South America', 'Oceania')
  if(global == F){
    cont.value <- which(cont.name == area)
    clipped.continent <- clamp(continents, lower = cont.value, upper = cont.value, useValues = F) / cont.value
    x <- mask(x, clipped.continent)
    y <- mask(y, clipped.continent)
  }
  x <- na.omit(values(x))
  y <- na.omit(values(y))
  # rank-sum test
  p.value <- wilcox.test(x, y)$p.value
  # all favorable pairs
  P <- sum(unlist(1:length(x) %>% map(function(i) sum(x[i] > y))))
  # all unfavorable pairs
  N <- sum(unlist(1:length(x) %>% map(function(i) sum(x[i] < y))))
  # all ties
  E <- sum(unlist(1:length(x) %>% map(function(i) sum(x[i] == y))))
  # total
  Tot <- P + N + E
  r = (P - N) / (Tot)
  prob = P / Tot
  fraction = 10^mean(y - x)
  return(tibble(`p` = p.value, `r` = r, `P(X > Y)` = prob, Area = area, Fraction = fraction))
}

magnitude <- function(superiority){
  x <- rep(NA, length(superiority))
  for(i in 1:length(superiority)){
    if(superiority[i] >= 0.92){
      x[i] <- 'Huge'
    } else if(superiority[i] >= 0.80){
      x[i] <- 'Very large'
    } else if(superiority[i] >= 0.71){
      x[i] <- 'Large'
    } else if(superiority[i] >= 0.64){
      x[i] <- 'Medium'
    } else if(superiority[i] >= 0.56){
      x[i] <- 'Small'
    } else if(superiority[i] < 0.56){
      x[i] <- ('No difference')
    }
  }
  return(x)
}
