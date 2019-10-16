library(tmap)
library(raster)
library(tidyverse)

source('new_effect_size.R')

setwd('Results')

# Mean --------------------------------------------------------------------
pn_mean <- raster('present_natural_mean.tif')
pne_mean <- raster('pn_extant_mean.tif')
cu_mean <- raster('current_mean.tif')
pn_no_megafauna_mean <- raster('pn_no_megafauna_mean.tif')

#take only cells that are valid for all scenarios
valid_cells <- intersect(
  intersect(
    intersect(which(!is.na(values(pn_mean))), which(!is.na(values(pne_mean)))),
    which(!is.na(values(cu_mean)))),
  which(!is.na(values(pn_no_megafauna_mean))))

values(pn_mean)[-valid_cells] <- NA
values(pne_mean)[-valid_cells] <- NA
values(cu_mean)[-valid_cells] <- NA
values(pn_no_megafauna_mean)[-valid_cells] <- NA

pn_max <- raster('present_natural_max.tif')
pne_max <- raster('pn_extant_max.tif')
cu_max <- raster('current_max.tif')
pn_no_megafauna_max <- raster('pn_no_megafauna_max.tif')

#take only cells that are valid for all scenarios
valid_cells <- intersect(
  intersect(
    intersect(which(!is.na(values(pn_max))), which(!is.na(values(pne_max)))),
    which(!is.na(values(cu_max)))),
  which(!is.na(values(pn_no_megafauna_max))))

values(pn_max)[-valid_cells] <- NA
values(pne_max)[-valid_cells] <- NA
values(cu_max)[-valid_cells] <- NA
values(pn_no_megafauna_max)[-valid_cells] <- NA

setwd('..')

# Present natural without megafauna ---------------------------------------
library(cowplot)

p1 <- tibble(value = c(na.omit(values(pn_mean)), 
                 na.omit(values(pn_no_megafauna_mean)),
                 na.omit(values(cu_mean))),
       Scenario = factor(c(rep('Present natural', 16920),
                           rep('\nPresent natural \nwithout \nmegafauna\n', 16920),
                           rep('Current', 16920)),
                         levels = c('Present natural', '\nPresent natural \nwithout \nmegafauna\n', 'Current'))) %>% 
  ggplot() +
  geom_histogram(aes(value, fill = Scenario), col = 'black', binwidth = 0.1) +
  facet_grid(Scenario ~ .) +
  xlab(expression('log(Average Ecosystem Connectivity (km'^2*'))')) +
  ylab('Number of cells') +
  theme(legend.position = 'none', axis.title.x = element_text(hjust = 0.5))
# p2 <- tibble(value = c(na.omit(values(pn_max)), 
#                  na.omit(values(pn_no_megafauna_max)),
#                  na.omit(values(cu_max))),
#        Scenario = factor(c(rep('Present natural', 16920),
#                            rep('\nPresent natural \nwithout \nmegafauna\n', 16920),
#                            rep('Current', 16920)),
#                          levels = c('Present natural', '\nPresent natural \nwithout \nmegafauna\n', 'Current'))) %>% 
#   ggplot() +
#   geom_histogram(aes(value, fill = Scenario), col = 'black', binwidth = 0.2) +
#   facet_grid(Scenario ~ .) +
#   xlab('') +
#   ylab('') +
#   theme(legend.position = 'none')

pdf('Figures/megafauna_contribution.pdf', width = 7, height = 5)
p1
dev.off()

svg('Figures/megafauna_contribution.svg', width = 7, height = 5)
p1
dev.off()

# Present natural without megafauna for continents ---------------------------------
library(cowplot)

continents <- raster('Data/continents.tif')
cont.name <- c('Africa', 'Asia', 'Europe', 'North America', 'South America', 'Oceania')

p <- list()

for(area in cont.name){
  cont.value <- which(cont.name == area)
  clipped.continent <- clamp(continents, lower = cont.value, upper = cont.value, useValues = F) / cont.value
  
  pn <- mask(pn_mean, clipped.continent) %>% values() %>% na.omit()
  pn_no <- mask(pn_no_megafauna_mean, clipped.continent) %>% values() %>% na.omit() 
  cu <- mask(cu_mean, clipped.continent) %>% values() %>% na.omit()
  p1 <- tibble(value = c(pn, pn_no, cu),
               Scenario = factor(c(rep('Present natural', length(pn)),
                                   rep('\nPresent natural \nwithout \nmegafauna\n', length(pn)),
                                   rep('Current', length(pn))),
                                 levels = c('Present natural', '\nPresent natural \nwithout \nmegafauna\n', 'Current'))) %>% 
    ggplot() +
    geom_histogram(aes(value, fill = Scenario), col = 'black', binwidth = 0.1) +
    facet_grid(Scenario ~ .) +
    xlab('') +
    ylab('Number of cells') +
    ggtitle(area) +
    theme(legend.position = 'none', axis.title.x = element_text(hjust = 0.5), plot.title = element_text(hjust = 0.5))
  
  # pn <- mask(pn_max, clipped.continent) %>% values() %>% na.omit()
  # pn_no <- mask(pn_no_megafauna_max, clipped.continent) %>% values() %>% na.omit()
  # cu <- mask(cu_max, clipped.continent) %>% values() %>% na.omit()
  # p2 <- tibble(value = c(pn, pn_no, cu),
  #              Scenario = factor(c(rep('Present natural', length(pn)),
  #                                  rep('\nPresent natural \nwithout \nmegafauna\n', length(pn)),
  #                                  rep('Current', length(pn))),
  #                                levels = c('Present natural', '\nPresent natural \nwithout \nmegafauna\n', 'Current'))) %>% 
  #   ggplot() +
  #   geom_histogram(aes(value, fill = Scenario), col = 'black', binwidth = 0.2) +
  #   facet_grid(Scenario ~ .) +
  #   xlab('') +
  #   ylab('') +
  #   ggtitle('')+
  #   theme(legend.position = 'none')
  
  p[[cont.value]] <- p1
}

svg('Figures/megafauna_contribution_continents.svg', width = 14, height = 16)
plot_grid(p[[1]], p[[2]], p[[3]], p[[4]], p[[5]], p[[6]], ncol = 2)
dev.off()

# Continental figures -----------------------------------------------------
library(cowplot)

continents <- raster('Data/continents.tif')
cont.name <- c('Africa', 'Asia', 'Europe', 'North America', 'South America', 'Oceania')

# area <- cont.name[6]
# cont.value <- which(cont.name == area)
# clipped.continent <- clamp(continents, lower = cont.value, upper = cont.value, useValues = F) / cont.value
# pn <- mask(pn_mean, clipped.continent)
# pn_no <- mask(pn_no_megafauna_mean, clipped.continent)
# cu <- mask(cu_mean, clipped.continent)
# c(area, round(new_effect_size(cu, pn_no)$`P(X > Y)`, 2))

p <- list()

for(area in cont.name){
  cont.value <- which(cont.name == area)
  clipped.continent <- clamp(continents, lower = cont.value, upper = cont.value, useValues = F) / cont.value
  pn <- mask(pn_mean, clipped.continent) %>% values() %>% na.omit()
  pn_no <- mask(pn_no_megafauna_mean, clipped.continent) %>% values() %>% na.omit()
  cu <- mask(cu_mean, clipped.continent) %>% values() %>% na.omit()
  p[[cont.value]] <- tibble(value = c(pn, pn_no, cu),
                            Scenario = factor(c(rep('Present natural', length(pn)),
                                                rep('\nPresent natural \nwithout \nmegafauna\n', length(pn)),
                                                rep('Current', length(pn))),
                                              levels = c('Present natural', '\nPresent natural \nwithout \nmegafauna\n', 'Current'))) %>% 
    ggplot() +
    geom_histogram(aes(value, fill = Scenario), col = 'black', binwidth = 0.1) +
    facet_grid(Scenario ~ .) +
    xlab('') +
    ylab('') +
    theme(legend.position = 'none') +
    ggtitle(area)
}

pdf('Figures/continental_megafaun_contribution.pdf', width = 10, height = 14)
plot_grid(p[[1]], p[[2]], p[[3]], p[[4]], p[[5]], p[[6]], ncol = 2)
plot.new()
text(x = 0.5, y = 0, labels = expression('log(Average Ecosystem Connectivity (km'^2*'))'), cex = 1.4)
text(x = 0.5, y = 0.5, labels = 'Number of cells', cex = 1.4)
dev.off()


# South America without big cats ------------------------------------------
library(tmap)
library(viridis)
library(plotrix)

values(continents)[which(values(continents) != 5)] <- NA
cu_no_pan_mean <- trim(mask(cu_no_pan_mean, continents))
cu_mean <- crop(trim(mask(cu_mean, continents)), cu_no) 
cu_no_pan_max <- trim(mask(cu_no_pan_max, continents))
cu_max <- crop(trim(mask(cu_max, continents)), cu_no)

figure.stack <- stack(cu_mean, cu_no_pan_mean)
pdf("../Figures/SA_no_cats_mean.pdf", width = 7)
plot.new()
tm_shape(10^figure.stack) +
  tm_raster(breaks = 10^seq(-1, 2, length = 1000), palette = inferno(1000), legend.show = F) +
  tm_layout(main.title = 'Average Connectivity', legend.position = c('left', 'bottom'),
            main.title.position = 'center', main.title.size = 2, panel.show = T,
            panel.labels = c('Current', 'No Large Felidae'),
            panel.label.size = 1.6, outer.margins = c(0.1,0,0,0))
color.legend(0, -0.16, 0.95, -0.135, legend = round(10^seq(-1, 2, by = 1), 2) / 100, 
             rect.col = inferno(1000), align = "rb", cex = 1.5) +
  tm_facets(ncol = 2)
par(xpd=TRUE)
graphics::text(x = 0.5, y = -0.2, labels = expression(km^2), cex = 1.5)
dev.off()

figure.stack <- stack(cu_max, cu_no_pan_max)
pdf("../Figures/SA_no_cats_max.pdf", width = 7)
plot.new()
tm_shape(10^figure.stack) +
  tm_raster(breaks = 10^seq(1, 4.5, length = 1000), palette = inferno(1000), legend.show = F) +
  tm_layout(main.title = 'Maximum Connectivity', legend.position = c('left', 'bottom'),
            main.title.position = 'center', main.title.size = 2, panel.show = T,
            panel.labels = c('Current', 'No Large Felidae'),
            panel.label.size = 1.6, outer.margins = c(0.1,0,0,0))
color.legend(0, -0.16, 0.95, -0.135, legend = round(10^seq(1, 4.5, by = 1), 2) / 100, 
             rect.col = inferno(1000), align = "rb", cex = 1.5) +
  tm_facets(ncol = 2)
par(xpd=TRUE)
graphics::text(x = 0.5, y = -0.2, labels = expression(km^2), cex = 1.5)
dev.off()
