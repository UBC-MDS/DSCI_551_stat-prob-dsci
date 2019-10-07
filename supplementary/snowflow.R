# Toy flow vs. snow example
# 
# This script specifies the joint distribution between flow and snow, as well
# as the marginals in the objects `snow` and `flow`.

library(tidyverse)
library(CopulaModel)

snow <- lst(
	meanlog = log(5),
	sdlog = 1,
	mean = exp(meanlog + sdlog^2/2),
	ddist = function(x) dlnorm(x, meanlog = meanlog, sdlog = sdlog),
	pdist = function(x) plnorm(x, meanlog = meanlog, sdlog = sdlog),
	qdist = function(x) qlnorm(x, meanlog = meanlog, sdlog = sdlog),
	rdist = function(n) rlnorm(n, meanlog = meanlog, sdlog = sdlog)
)
flow <- lst(
	meanlog = log(150),
	sdlog = 1,
	mean = exp(meanlog + sdlog^2/2),
	ddist = function(x) dlnorm(x, meanlog = meanlog, sdlog = sdlog),
	pdist = function(x) plnorm(x, meanlog = meanlog, sdlog = sdlog),
	qdist = function(x) qlnorm(x, meanlog = meanlog, sdlog = sdlog),
	rdist = function(n) rlnorm(n, meanlog = meanlog, sdlog = sdlog)
)
snowflow <- lst(
	cpar = 3,
	ddist = function(x, y) {
		dfrk(snow$pdist(x), flow$pdist(y), cpar) * 
			snow$ddist(x) * 
			flow$ddist(y)
	},
	pdist = function(x, y) {
		pfrk(snow$pdist(x), flow$pdist(y), cpar)
	},
	dcond = function(y, x) {
		dfrk(snow$pdist(x), flow$pdist(y), cpar) * 
			flow$ddist(y)
	},
	pcond = function(y, x) {
		pcondfrk(flow$pdist(y), snow$pdist(x), cpar)
	},
	qcond = function(p, x) {
		flow$qdist(qcondfrk(p, snow$pdist(x), cpar))
	},
	rdist = function(n) {
		x <- snow$rdist(n)
		u <- snow$pdist(x)
		tau <- runif(n)
		v <- qcondfrk(tau, u, cpar = cpar)
		y <- flow$qdist(v)
		tibble(x = x, y = y)
	},
	meancond = Vectorize(function(x) {
		integrand <- function(t) qcond(t, x)
		integrate(integrand, lower = 0, upper = 1)$value
	}),
	ggframe = crossing(
		x = seq(0.1, snow$qdist(0.8), length.out = 100),
		y = seq(0.1, flow$qdist(0.9), length.out = 100)
	) %>% 
		mutate(z = ddist(x, y)) %>% 
		ggplot(aes(x, y)) +
		labs(x = "Snowmelt (mm)",
			 y = expression(paste("River Flow ", (m^3/s)))) +
		theme_bw()
)