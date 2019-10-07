# Monthly Expense example
library(tidyverse)
set.seed(2)
expense <- lst(
	meanlog = 8,
	sdlog = 0.5,
	mean  = exp(meanlog + sdlog^2/2),
	ddist = function(x) dlnorm(x, meanlog = meanlog, sdlog = sdlog),
	qdist = function(x) qlnorm(x, meanlog = meanlog, sdlog = sdlog),
	rdist = function(x) rlnorm(x, meanlog = meanlog, sdlog = sdlog),
	pdist = function(x) plnorm(x, meanlog = meanlog, sdlog = sdlog),
	sample = round(rdist(20), 2),
	plot_ddist = tibble(x = qdist(c(0, 0.99))) %>% 
		ggplot(aes(x)) +
		stat_function(fun = ddist) +
		theme_bw() +
		labs(x = "Monthly Expense", y = "Density")
)
