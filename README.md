# Rstudio_Codes

################### van Genuchten Parameters Optimization ####################
require(pacman)
p_load(lubridate, DEoptim)

## %%%%%%%%%%%%%%%%%%
## PROFILE Pressure at -30cm

## Function

Genuchten_30 <- function(alpha){
  alpha <- 0.13706
  n <- 1.14035
  theta_s <- 0.37315
  100 * (n - alpha * alpha)^2 + (1 - alpha)^2
}

## lower and upper bounds
## parameter vector.
lower <- c(0.2704)
upper <- c(0.2723)

## run DEoptim and set a seed first for replicability
set.seed(1234)
DEoptim(Genuchten_30, lower, upper)

## increase the population size
DEoptim(Genuchten_30, lower, upper, DEoptim.control(NP = 300))

## change other settings and store the output
outDEoptim <- DEoptim(Genuchten_30, lower, upper, DEoptim.control(NP = NA,
                                                                itermax = 300,
                                                                F = 0.8, 
                                                                CR = 0.5))

## plot the output
plot(outDEoptim)



##%%%%%%%%%%%%%%%%%%%%%%%

## %%%%%%%%%%%%%%%%%%
## PROFILE -60cm

# Function 

genuchten_60 <- function(alpha){
  alpha <- 0.02290
  n <- 1.20680
  theta_s <- 0.28688
  100 * (n - alpha * alpha)^2 + (1 - alpha)^2
}

## lower and upper bounds

lower <- c(0.0189)
upper <- c(0.0269)

## run DEoptim and set a seed first for replicability
set.seed(1234)
DEoptim(genuchten_60, lower, upper)

## increase the population size
DEoptim(genuchten_60, lower, upper, DEoptim.control(NP = 100))

## change other settings and store the output
outDEoptim <- DEoptim(genuchten_60, lower, upper, DEoptim.control(NP = NA,
                                                                itermax = 200,
                                                                F = 0.8,
                                                                CR = 0.5))

## plot the output
plot(outDEoptim)

##%%%%%%%%%%%%%%%%%%%%%%%
