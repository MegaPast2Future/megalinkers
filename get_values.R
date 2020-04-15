get_values <- function(r, area, stat) {
  names(r) <- c("PN", "CU", "TH", "CRW", "FRW")
  ans <- list()
  for (x in names(global)) {
    ans[[x]] <- global[[x]] %>% 
      values() %>% 
      na.omit %>% 
      as.numeric()
  }
  bind_rows(ans) %>% 
    write_csv(paste0(area, "_", stat, "_el.csv"))
}
