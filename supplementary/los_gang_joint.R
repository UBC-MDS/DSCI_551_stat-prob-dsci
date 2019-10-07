# Joint distribution for length of stay and gang demand
# To come up with the joint distribution so that the marginals remain as they are, use a Gaussian copula.
# Sorry for the lack of documentation. I wrote this in a hurry.
library(tidyverse)
Phi <- function(x, y, rho) {
	if (x == Inf)  return(pnorm(y))
	if (x == -Inf) return(0)
	if (y == Inf)  return(pnorm(x))
	if (y == -Inf) return(0)
	pbivnorm::pbivnorm(x, y, rho = rho)
}
heavyside <- function(p) {
	n <- length(p)
	Vectorize(function(x) sum(p[1:n <= x]))
}
joint <- function(rho,
				  p_los = c(0.25, 0.35, 0.2, 0.1, 0.1), 
				  p_gang = c(0.2, 0.4, 0.3, 0.1)) {
	F_los <- heavyside(p_los)
	F_gang <- heavyside(p_gang)
	F_joint <- function(los, gang, rho) 
		Phi(qnorm(F_los(los)), qnorm(F_gang(gang)), rho = rho)
	p_joint <- function(los, gang, rho) {
		F_joint(los, gang, rho) - 
			F_joint(los - 1, gang, rho) - 
			F_joint(los, gang - 1, rho) +
			F_joint(los - 1, gang - 1, rho)
	}
	crossing(los  = 1:length(p_los),
			 gang = 1:length(p_gang)) %>% 
		group_by_all() %>% 
		mutate(p = p_joint(los, gang, rho = rho)) %>% 
		ungroup()
}