# Ships arriving at port example
library(tidyverse)
## Length of Stay variable
los <- lst(
	pmf = tribble(
		~ ndays, ~ p,
		0, 0,
		1, 0.25,
		2, 0.35,
		3, 0.2,
		4, 0.1,
		5, 0.1,
		6, 0
	) %>% mutate(
		cdf   = cumsum(p),
		right = ndays,
		left  = lag(ndays) %>% 
			replace_na(-Inf)
	)
)
## Gang demand
gang <- lst(
	pmf = tibble(
		gangs = 1:4,
		p     = c(0.2, 0.4, 0.3, 0.1)
	)
)