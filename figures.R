setwd('Results')

pn_mean <- raster('present_natural_mean.tif')
pne_mean <- raster('pn_extant_mean.tif')
cu_mean <- raster('current_mean.tif')
vu_mean <- raster('vulnerable_mean.tif')
rw_mean <- raster('rw_extant_mean.tif')

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

pn_max <- raster('present_natural_max.tif')
pne_max <- raster('pn_extant_max.tif')
cu_max <- raster('current_max.tif')
vu_max <- raster('vulnerable_max.tif')
rw_max <- raster('rw_extant_max.tif')

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

setwd('../Figures')

# Boxplots ----------------------------------------------------------------
box1 <- 10^values(pn_mean) / 100
box2 <- 10^values(cu_mean) / 100
box3 <- 10^values(vu_mean) / 100
box4 <- 10^values(rw_mean) / 100
box5 <- 10^values(pne_mean) / 100

p1 <- tibble(
  value = c(box1, box2, box3, box4, box5), 
  scenario = factor(c(rep('PN', length(box1)), rep('CU', length(box2)), 
                      rep('TH', length(box3)), rep('RW', length(box4)), 
                      rep('PNE', length(box5))), 
                    levels = c('PN', 'CU', 'TH', 'RW', 'PNE'))) %>%
  filter(!is.na(value)) %>% 
  ggplot() +
  geom_boxplot(aes(scenario, value, group = scenario), 
               fill = c('seagreen3', 'lightgoldenrod', 'tomato', 'darkolivegreen1', 'darkolivegreen3'), 
               outlier.shape = NA) +
  xlab('Scenario') +
  ylab(expression('Biotic movement capacity '*(km^2))) +
  scale_x_discrete(name = 'Scenario', labels = c('Present\n natural', 'Current', "Threatened\n extinct",
                                                 'Conservative\n rewilding', 'Full\n rewilding')) +
  coord_cartesian(ylim = c(0.005, 1.6)) +
  theme(
    panel.background = element_blank(), 
    axis.line = element_line(), 
    plot.title = element_text(hjust = 0.5, size = 14),
    legend.position = 'none', 
    axis.title = element_text(size = 12), 
    axis.text = element_text(size = 12),
    axis.title.y = element_text(hjust = 0.5),
    panel.grid.major = element_line(colour = 'gainsboro'),
    axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 0.5)
  ) +
  ggtitle("Average movement capacity")

p1

box1 <- 10^values(pn_max) / 100
box2 <- 10^values(cu_max) / 100
box3 <- 10^values(vu_max) / 100
box4 <- 10^values(rw_max) / 100
box5 <- 10^values(pne_max) / 100

L <- range(c(boxplot.stats(c(box1))$stats[c(1,5)], 
             boxplot.stats(c(box2))$stats[c(1,5)], 
             boxplot.stats(c(box3))$stats[c(1,5)], 
             boxplot.stats(c(box4))$stats[c(1,5)],
             boxplot.stats(c(box5))$stats[c(1,5)]))

p2 <- tibble(
  value = c(box1, box2, box3, box4, box5), 
  scenario = factor(c(rep('PN', length(box1)), rep('CU', length(box2)), 
                      rep('TH', length(box3)), rep('RW', length(box4)), 
                      rep('PNE', length(box5))), 
                    levels = c('PN', 'CU', 'TH', 'RW', 'PNE'))) %>%
  filter(!is.na(value)) %>% 
  ggplot() +
  geom_boxplot(aes(scenario, value, group = scenario), 
               fill = c('seagreen3', 'lightgoldenrod', 'tomato', 'darkolivegreen1', 'darkolivegreen3'), 
               outlier.shape = NA) +
  xlab('Scenario') +
  ylab(expression('Biotic movement capacity '*(km^2))) +
  scale_x_discrete(name = 'Scenario', labels = c('Present\n natural', 'Current', "Threatened\n extinct",
                                                 'Conservative\n rewilding', 'Full\n rewilding')) +
  coord_cartesian(ylim = c(10, 1200)) +
  theme(
    panel.background = element_blank(), 
    axis.line = element_line(), 
    plot.title = element_text(hjust = 0.5, size = 14), 
    legend.position = 'none', 
    axis.title = element_text(size = 12), 
    axis.text = element_text(size = 12),
    axis.title.y = element_text(hjust = 0.5),
    panel.grid.major = element_line(colour = 'gainsboro'),
    axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 0.5)
  ) +
  ggtitle("Maximum movement capacity")

plot_grid(p1, p2, labels = NA, label_size = 9, label_x = 0)
ggsave('boxplot.png', width = 10, height = 6)
ggsave('boxplot.svg', width = 6, height = 6)

# Continental boxplots ----------------------------------------------------
continents <- raster("../Data/continents.tif")
cont_names <- c("Asia", "North America", "Europe", "Africa", "South America", "Oceania")

limty <- matrix(0, 6, 2)
limty[1, ] <- c(1.2, 1200)
limty[2, ] <- c(1.8, 2400)
limty[3, ] <- c(1.5, 1200)
limty[4, ] <- c(2.0, 700)
limty[5, ] <- c(2.4, 900)
limty[6, ] <- c(0.5, 240)

p <- list()
q <- list()

for(i in 1:length(cont_names)){
  cont <- clamp(continents, lower = i, upper = i, useValues = F)
  cont <- cont / cont
  pn_mean_cont <- pn_mean * cont
  cu_mean_cont <- cu_mean * cont
  vu_mean_cont <- vu_mean * cont
  rw_mean_cont <- rw_mean * cont
  pne_mean_cont <- pne_mean * cont
  
  box1 <- 10^values(pn_mean_cont) / 100
  box2 <- 10^values(cu_mean_cont) / 100
  box3 <- 10^values(vu_mean_cont) / 100
  box4 <- 10^values(rw_mean_cont) / 100
  box5 <- 10^values(pne_mean_cont) / 100
  
  L <- range(c(boxplot.stats(c(box1))$stats[c(1,5)], 
               boxplot.stats(c(box2))$stats[c(1,5)], 
               boxplot.stats(c(box3))$stats[c(1,5)], 
               boxplot.stats(c(box4))$stats[c(1,5)],
               boxplot.stats(c(box5))$stats[c(1,5)]))
  
  p1 <- tibble(
    value = c(box1, box2, box3, box4, box5), 
    scenario = factor(c(rep('PN', length(box1)), rep('CU', length(box2)), 
                        rep('TH', length(box3)), rep('RW', length(box4)), 
                        rep('PNE', length(box5))), 
                      levels = c('PN', 'CU', 'TH', 'RW', 'PNE'))) %>%
    filter(!is.na(value)) %>% 
    ggplot() +
    geom_boxplot(aes(scenario, value, group = scenario), 
                 fill = c('seagreen3', 'lightgoldenrod', 'tomato', 'darkolivegreen1', 'darkolivegreen3'), 
                 outlier.shape = NA) +
    xlab('') +
    ylab('') +
    ggtitle(cont_names[i]) +
    scale_x_discrete(name = '', labels = c('Present\n natural', 'Current', "Threatened\n extinct",
                                           'Conservative\n rewilding', 'Full\n rewilding')) +
    coord_cartesian(ylim = c(min(L), max(L))) +
    theme(
      panel.background = element_blank(), 
      axis.line = element_line(), 
      plot.title = element_text(hjust = 0.5, size = 20), 
      legend.position = 'none', 
      axis.title = element_text(size = 8), 
      axis.text = element_text(size = 8),
      axis.title.y = element_blank(),
      panel.grid.major = element_line(colour = 'gainsboro'),
      axis.text.x = element_blank()
    )
  
  pn_max_cont <- pn_max * cont
  cu_max_cont <- cu_max * cont
  vu_max_cont <- vu_max * cont
  rw_max_cont <- rw_max * cont
  pne_max_cont <- pne_max * cont
  
  box1 <- 10^values(pn_max_cont) / 100
  box2 <- 10^values(cu_max_cont) / 100
  box3 <- 10^values(vu_max_cont) / 100
  box4 <- 10^values(rw_max_cont) / 100
  box5 <- 10^values(pne_max_cont) / 100
  
  L <- range(c(boxplot.stats(c(box1))$stats[c(1,5)], 
               boxplot.stats(c(box2))$stats[c(1,5)], 
               boxplot.stats(c(box3))$stats[c(1,5)], 
               boxplot.stats(c(box4))$stats[c(1,5)],
               boxplot.stats(c(box5))$stats[c(1,5)]))
  
  p2 <- tibble(
    value = c(box1, box2, box3, box4, box5), 
    scenario = factor(c(rep('PN', length(box1)), rep('CU', length(box2)), 
                        rep('TH', length(box3)), rep('RW', length(box4)), 
                        rep('PNE', length(box5))), 
                      levels = c('PN', 'CU', 'TH', 'RW', 'PNE'))) %>%
    filter(!is.na(value)) %>% 
    ggplot() +
    geom_boxplot(aes(scenario, value, group = scenario), 
                 fill = c('seagreen3', 'lightgoldenrod', 'tomato', 'darkolivegreen1', 'darkolivegreen3'), 
                 outlier.alpha = 0.3, outlier.size = 0.5) +
    xlab('') +
    ylab('') +
    ggtitle(cont_names[i]) +
    scale_x_discrete(name = '', labels = c('Present\n natural', 'Current', "Threatened\n extinct",
                                           'Conservative\n rewilding', 'Full\n rewilding')) +
    coord_cartesian(ylim = c(10, max(L))) +
    theme(
      panel.background = element_blank(), 
      axis.line = element_line(), 
      plot.title = element_text(hjust = 0.5, size = 20), 
      legend.position = 'none', 
      axis.title = element_text(size = 8), 
      axis.text = element_text(size = 8),
      axis.title.y = element_blank(),
      panel.grid.major = element_line(colour = 'gainsboro'),
      axis.text.x = element_blank()
    )
  
  p[[i]] <- p1
  q[[i]] <- p2
}

pdf('../Figures/boxplot_continents_mean.pdf', width = 10, height = 12)
plot.new()
text(x = 0.5, y = 0, "Scenario")
text(x = 0.5, y = 0.35, "Average movement capacity")
text(x = 0.5, y = 0.9, 'Present\n natural\n\n Current\n\n Threatened\n extinct\n\n Conservative\n rewilding\n\n Full\n rewilding\n')
text(x = 0, y = 0.5, expression('Movement capacity '*(km^2)), srt = 90)
plot_grid(p[[1]], p[[2]], p[[3]], p[[4]], p[[5]], p[[6]], ncol = 2)
dev.off()

pdf('../Figures/boxplot_continents_max.pdf', width = 10, height = 12)
plot.new()
text(x = 0.5, y = 0, "Scenario")
text(x = 0.5, y = 0.35, "Maximum movement capacity")
text(x = 0.5, y = 0.9, 'Present\n natural\n\n Current\n\n Threatened\n extinct\n\n Conservative\n rewilding\n\n Full\n rewilding\n')
text(x = 0, y = 0.5, expression('Movement capacity '*(km^2)), srt = 90)
plot_grid(q[[1]], q[[2]], q[[3]], q[[4]], q[[5]], q[[6]], ncol = 2)
dev.off()

# Global maps -------------------------------------------------------------
Pal <- rev(wesanderson::wes_palette("Zissou1", 100, type = 'continuous'))

figure.stack <- 10^stack(cu_mean - pn_mean, vu_mean - pn_mean, rw_mean - pn_mean, pne_mean - pn_mean)
values(figure.stack)[values(figure.stack) > 1] <- 1
names(figure.stack) <- c('PN', 'PNE', 'CU', 'TH')

pdf('/home/GIT/Megalinkers/Figures/map_mean_difference.pdf', 8, 16)
plot.new()
tm_shape(figure.stack) +
  tm_raster(palette = Pal, legend.show = F, breaks = seq(0, 1, 0.1)) +
  tm_layout(
    main.title = 'Average movement capacity', 
    legend.show = T, legend.position = c('left', 'bottom'),
    main.title.position = 'center', main.title.size = 2, panel.show = T,
    panel.labels = c('Current', 'Threatened species extinct', 
                     'Conservative rewilding', 'Full rewilding'),
    panel.label.size = 1.6, outer.margins = c(0.1, 0, 0, 0),
    inner.margins = c(0, 0, 0.05, 0), frame = F
  ) +
  tm_facets(ncol = 1)
color.legend(0, -0.07, 0.95, -0.055, legend = seq(0, 1, 0.1),
             rect.col = Pal, align = "rb", cex = 1.5)
par(xpd=TRUE)
graphics::text(x = 0.5, y = -0.1, labels = 'Proportion of present-natural', cex = 1.5)
dev.off()

figure.stack <- 10^stack(cu_max - pn_max, vu_max - pn_max, rw_max - pn_max, pne_max - pn_max)
values(figure.stack)[values(figure.stack) > 1] <- 1
names(figure.stack) <- c('PN', 'PNE', 'CU', 'TH')

pdf('/home/GIT/Megalinkers/Figures/map_max_difference.pdf', 8, 16)
plot.new()
tm_shape(figure.stack) +
  tm_raster(palette = Pal, legend.show = T, breaks = seq(0, 1, 0.1)) +
  tm_layout(main.title = 'Maximum movement capacity', 
            legend.show = F, legend.position = c('left', 'bottom'),
            main.title.position = 'center', main.title.size = 2, panel.show = T,
            panel.labels = c('Current', 'Threatened species extinct', 
                             'Conservative rewilding', 'Full rewilding'),
            panel.label.size = 1.6, outer.margins = c(0.1, 0, 0, 0),
            inner.margins = c(0, 0, 0.05, 0), frame = F) +
  tm_facets(ncol = 1)
color.legend(0, -0.07, 0.95, -0.055, legend = seq(0, 1, 0.1),
             rect.col = Pal, align = "rb", cex = 1.5)
par(xpd=TRUE)
graphics::text(x = 0.5, y = -0.1, labels = 'Proportion of present-natural', cex = 1.5)
dev.off()

system("bash bind_maps.sh map_mean_difference.pdf map_max_difference.pdf")
system("inkscape -l jammed_output.svg jammed_output.pdf")
system("inkscape -f jammed_output.svg -d 600 -e jammed_output.png")

# Continental maps -------------------------------------------------------------
Pal <- rev(wesanderson::wes_palette("Zissou1", 100, type = 'continuous'))

figure.stack <- 10^stack(cu_mean - pn_mean, vu_mean - pn_mean, rw_mean - pn_mean, pne_mean - pn_mean)
values(figure.stack)[values(figure.stack) > 1] <- 1
names(figure.stack) <- c('PN', 'PNE', 'CU', 'TH')

p <- list()
for(i in 1:6){
  p[[i]] <- tm_shape(trim(figure.stack * continents[[i]])) +
    tm_raster(palette = Pal, legend.show = F, breaks = seq(0, 1, 0.1)) +
    tm_layout(
      main.title = '', 
      legend.show = T, legend.position = c('left', 'bottom'),
      main.title.position = 'center', main.title.size = 2, panel.show = T,
      panel.labels = c('Current', 'Threatened species extinct', 
                       'Conservative rewilding', 'Full rewilding'),
      panel.label.size = 1.6, outer.margins = c(0.1, 0, 0, 0),
      inner.margins = c(0, 0, 0.05, 0), frame = F
    ) +
    tm_facets(ncol = 1)
}

tmap_arrange(p)

pdf('/home/GIT/Megalinkers/Figures/map_mean_difference.pdf', 8, 16)
plot.new()
color.legend(0, -0.07, 0.95, -0.055, legend = seq(0, 1, 0.1),
             rect.col = Pal, align = "rb", cex = 1.5)
par(xpd=TRUE)
graphics::text(x = 0.5, y = -0.1, labels = 'Proportion of present-natural', cex = 1.5)
dev.off()

figure.stack <- 10^stack(cu_max - pn_max, vu_max - pn_max, rw_max - pn_max, pne_max - pn_max)
values(figure.stack)[values(figure.stack) > 1] <- 1
names(figure.stack) <- c('PN', 'PNE', 'CU', 'TH')

pdf('/home/GIT/Megalinkers/Figures/map_max_difference.pdf', 8, 16)
plot.new()
tm_shape(figure.stack) +
  tm_raster(palette = Pal, legend.show = T, breaks = seq(0, 1, 0.1)) +
  tm_layout(main.title = 'Maximum movement capacity', 
            legend.show = F, legend.position = c('left', 'bottom'),
            main.title.position = 'center', main.title.size = 2, panel.show = T,
            panel.labels = c('Current', 'Threatened species extinct', 
                             'Conservative rewilding', 'Full rewilding'),
            panel.label.size = 1.6, outer.margins = c(0.1, 0, 0, 0),
            inner.margins = c(0, 0, 0.05, 0), frame = F) +
  tm_facets(ncol = 1)
color.legend(0, -0.07, 0.95, -0.055, legend = seq(0, 1, 0.1),
             rect.col = Pal, align = "rb", cex = 1.5)
par(xpd=TRUE)
graphics::text(x = 0.5, y = -0.1, labels = 'Proportion of present-natural', cex = 1.5)
dev.off()

system("bash bind_maps.sh map_mean_difference.pdf map_max_difference.pdf")
system("inkscape -l jammed_output.svg jammed_output.pdf")
system("inkscape -f jammed_output.svg -d 600 -e jammed_output.png")

# Home range --------------------------------------------------------------
h <- read_tsv('~/Downloads/Mammal_Home_Ranges.txt', col_types = cols(), 
              col_names = c('Species', 'Order', 'Family', 'TG', 'Mass', 'H', 'Ref', 'Note')) %>% 
  mutate(H = H - 2)

ggplot(h) +
  geom_smooth(aes(Mass, H, col = TG), method = 'lm', alpha = 0.2) +
  geom_point(aes(Mass, H, col = TG), alpha = 0.7) + 
  xlab(expression('log'[10]*'(mass (g))')) +
  ylab(expression('log'[10]*'(home range (km'^2*'))')) +
  scale_x_continuous(expand = c(0, 0.1)) +
  scale_y_continuous(expand = c(0, 0.1)) +
  scale_color_discrete('', labels = c('Carnivores', 'Herbivores', 'Omnivores')) +
  theme(
    panel.background = element_blank(), 
    axis.line = element_line(), 
    axis.title = element_text(size = 20), 
    axis.text = element_text(size = 20),
    panel.grid.major = element_line(colour = 'gainsboro'),
    legend.key = element_blank(),
    legend.text = element_text(size = 16),
    legend.position = c(0.85, 0.18),
    legend.title = element_text(size = 0),
    legend.background = element_rect(colour = 'black')
  )

ggsave('Figures/homerange.png', width = 8, height = 5.5)

h %>% 
  group_by(TG)  %>% 
  tally()
