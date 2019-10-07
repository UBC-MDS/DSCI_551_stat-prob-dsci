suppressPackageStartupMessages(library(tidyverse))
suppressPackageStartupMessages(library(janitor))

set.seed(88)
k <- 5
p <- 0.7
n <- 10000
rand_sample <- rnbinom(n, size = 5, prob = 0.7)
head(rand_sample, 100)


## ------ Mean ------
(1 - p) * k / p   # True, distribution-based
mean(rand_sample) # Approximate, empirical

## ------ Variance ------
(1 - p) * k / p^2 # True, distribution-based
var(rand_sample)  # Approximate, empirical

## ------ Standard deviation ------
sqrt((1 - p) * k / p^2) # True, distribution-based
sd(rand_sample)         # Approximate, empirical

## ------ Probability of seeing 0 ------
mean(rand_sample == 0)         # Approximate, empirical
dnbinom(0, size = k, prob = p) # True, distribution-based

## ------ pmf ------
## Code without using the tidyverse:
table(rand_sample)
pmf <- janitor::tabyl(rand_sample)               # Empirical
pmf$n <- NULL
pmf <- setNames(pmf, c("x", "empirical"))
pmf$actual <- dnbinom(pmf$x, size = k, prob = p) # True

## Code using the tidyverse:
pmf <- janitor::tabyl(rand_sample) %>% 
	select(x = rand_sample, empirical = percent) %>% 
	mutate(actual = dnbinom(x, size = k, prob = p))
pmf %>% 
	mutate(actual    = round(actual, 4),         # Empirical
		   empirical = round(empirical, 4))      # True


## Plot
pmf %>% 
	gather(key = "method", value = "Probability", empirical, actual) %>% 
	ggplot(aes(x, Probability)) +
	facet_wrap(~ method) +
	geom_col(fill = "maroon") +
	theme_bw()

## ------ Entropy ------
## Emprical (the actual is hard to compute)
- sum(pmf$empirical * log(pmf$empirical))

## ------ Mode ------
## Actual
pmf %>% 
	filter(actual == max(actual)) %>% 
	pull(x)
## Empirical
pmf %>% 
	filter(empirical == max(empirical)) %>% 
	pull(x)
