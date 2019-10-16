parallel_stack <- function(x, H){
  interval <- ((500 * x) - 500 + 1):(500 * x)
  if(x == blocks + 1){
    interval <- ((500 * x) - 500 + 1):length(species)
  }
  return(raster::stack(paste0(species[interval], '.tif')) * H[interval])
}