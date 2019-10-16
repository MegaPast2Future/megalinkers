phy_diet <- function(species, th = 10){
	diet <- phy %>% 
	  filter(Binomial.1.2 %in% species) %>% 
	  mutate(Diet = modify(Diet.Plant, function(x){
	    if(x >= 90){
	      "H"
	    } else if(x <= 10){
	      "C"
	    } else{
	      "O"
	    }
	  }))
	
	H <- which(diet >= 100 - th)
	C <- which(diet <= th)
	O <- which(diet < (100 - th) & diet > th)
	diet[H] <- "H"
	diet[C] <- "C"
	diet[O] <- "O"
	return(diet)
}
