# Low Purity Octane example
library(tidyverse)
set.seed(20)
octane <- lst(
	ddist   = function(x) if_else(x < 0 | x > 1, 0, 2 * x),
	ddist_symm  = function(x) if_else(x < 0 | x > 1, 0, pmin(4 * x, 4 * (1 - x))),
	ddist_right = function(x) if_else(x < 0 | x > 1, 0, 2 * (1 - x)),
	ddist_unif  = function(x) if_else(x < 0 | x > 1, 0, 1),
	qdist       = function(x) sqrt(x),
	pdist       = function(x) case_when(
		x <= 0        ~ 0,
		x > 0 & x < 1 ~ x ^ 2,
		x >= 1        ~ 1
	),
	n = 100,
	sample       = sqrt(runif(n)),
	sample_unif  = runif(n),
	sample_right = 1 - sqrt(runif(n)),
	plot_ddist = tibble(
		A = sample,
		B = sample_right,
		C = sample_unif,
		rng = seq(-0.5, 1.5, length.out = n)
	) %>% 
		pivot_longer(cols = A:C, names_to = "scenario", values_to = "measurement") %>% 
		ggplot(aes(rng)) +
		stat_function(fun = ddist, n = 1000) +
		theme_bw() +
		labs(x = "Octane Purity",
			 y = "Density")
)