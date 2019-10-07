#' Plot a joint density and marginals
#' 
#' A makeshift way of producing a contour plot with marginals alongside it. Not 
#' meant to be robust and customizable. 
#' 
#' @param .data Data frame with columns x, y, and z, corresponding to the aesthetics in geom_contour().
#' z should be the density evaluated at x, y.
#' @param dmargx,dmargy Functions of the marginal densities of x and y.
#' @param .sample Optional data frame of a random sample of x and y. Columns
#' should be named x and y. NULL if you don't want this (the default).
#' @param p21_layers Extra layers to add to the bottom-left joint plot. NULL if nothing.
#' @return Contour plot with marginals at top and to the right, constructed with cowplot::plot_grid() and
#' ggplot() objects. 
ggjointmarg <- function(.data, dmargx, dmargy, .sample = NULL, p21_layers = list()) {
	if (is.null(.sample)) {
		rug_layer <- point_layer <- list()
	} else {
		rug_layer <- geom_rug(data = .sample, mapping = aes(x))
		point_layer <- geom_point(data = .sample, mapping = aes(x, y), alpha = 0.25)
	}
	p11 <- ggplot(.data, aes(x)) +
		stat_function(fun = dmargx) +
		rug_layer +
		theme_minimal() +
		theme(axis.title.x = element_blank(), 
			  panel.grid   = element_blank(),
			  axis.text.x  = element_blank(),
			  axis.text.y  = element_text(colour = "white"),
			  axis.title.y = element_text(colour = "white"))
	p22 <- ggplot(.data, aes(y)) +
		stat_function(fun = dmargy) +
		rug_layer +
		theme_minimal() +
		theme(panel.grid   = element_blank(),
			  axis.text.x  = element_blank(),
			  axis.text.y  = element_text(colour = "white"),
			  axis.title.x = element_text(colour = "white"),
			  axis.title.y = element_blank()) +
		coord_flip() 
	p12 <- ggplot() +
		theme_minimal() 
	p21 <- ggplot(.data, aes(x, y)) +
		geom_contour(aes(z = z, colour = ..level..)) +
		point_layer +
		theme_bw() +
		theme(axis.title.y = element_text(angle = 0, vjust = 0.5)) +
		scale_colour_continuous(guide = FALSE) +
		p21_layers
	cowplot::plot_grid(
		p11, p12,
		p21, p22,
		ncol = 2,
		rel_widths = c(4, 1),
		rel_heights = c(1, 4)
	)
}
