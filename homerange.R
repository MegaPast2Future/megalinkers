H <- data.frame(
  c(-2.64,-2.17,-1.84), #intercepts
  c(0.180,0.245,0.328), #intercepts SE
  c(1.08,1.21,1.23), #slopes
  c(0.049,0.073,0.090) #slopes SE
)
colnames(H) <- c("intercept","SE_intercept","slope","SE_slope")
rownames(H) <- c("H","O","C")

homerange <- function(diet, mass){
  avg <- H[diet, "intercept"] + H[diet, "slope"]*log10(mass)
  var <- H[diet, "SE_intercept"]^2 + H[diet, "SE_slope"]^2*log10(mass)^2
  return(c(avg, var))
}