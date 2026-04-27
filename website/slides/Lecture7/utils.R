# Load functions in env
draw_sample <- function(n){
  return(rnorm(n, 175, 7))
}

example_quantile_cdf <- function(q){
  data <- tibble(quantile = seq(-3, 3, 0.1), F = pnorm(quantile))
  data %>% 
    ggplot() + 
    geom_line(aes(quantile, F)) +
    geom_segment(aes(x = -Inf, y = q, xend = qnorm(q), yend = q), color = "red", linetype='dotted') +
    geom_segment(aes(x = qnorm(q), y = -Inf, xend = qnorm(q), yend = q), color = "red", linetype='dotted') +
    scale_x_continuous(breaks = c(qnorm(q)), labels = c(paste0("Q(", q,") = ", round(qnorm(q), 4)))) + 
    theme(plot.title = element_text(size=24), axis.text = element_text(size=18), axis.title = element_text(size=18))    
}

example_stochastic_relation <- function() {
  prob <- tibble(
    x = seq(0, 4, 0.01),
    y = 2 * x,
    y1 = 2 + dnorm(x, mean = 1, sd = 0.25),
    y2 = 6 + dnorm(x, mean = 3, sd = 0.25),
    y3 = 4 + dnorm(x, mean = 2, sd = 0.25)
  )
  plot <- prob %>%
    ggplot() +
    geom_line(aes(x, y)) +
    ggtitle("Stochastic Relation") +
    geom_line(aes(x, y1), color = "red") +
    geom_line(aes(x, y2), color = "red") +
    geom_line(aes(x, y3), color = "red") +
    geom_segment(aes(x = 1, y = 2, xend = 1, yend = 3.6), color = "red", linetype = "dotted") +
    geom_segment(aes(x = 2, y = 4, xend = 2, yend = 5.6), color = "red", linetype = "dotted") +
    geom_segment(aes(x = 3, y = 6, xend = 3, yend = 7.6), color = "red", linetype = "dotted") +
    coord_flip() +
    scale_y_continuous(
      name = "x",
      breaks = c(2, 4, 6),
      labels = c("1", "2", "3")
    ) +
    scale_x_continuous(
      name = "y",
      breaks = c(1, 2, 3),
      labels = c("2", "4", "6")
    ) +
    theme(
      plot.title = element_text(size = 30, face = "bold"),
      axis.text = element_text(size = 21),
      axis.title = element_text(size = 27)
    )
  return(plot)
}


example_quantile_regression <- function() {
  prob <- tibble(x = seq(0, 4, 0.01), 
                 y = 2 * x, 
                 y1 = 2 + dnorm(x, mean = 1, sd = 0.25), 
                 y2 = 6 + dnorm(x, mean = 3, sd = 0.25),
                 y3 = 4 + dnorm(x, mean = 2, sd = 0.25))
  
  prob %>% 
    ggplot() + 
    geom_line(aes(x, y - 2*1.64*0.25)) + ggtitle("0.95 Quantile regression") +
    geom_line(aes(x, y1), color = "red") +
    geom_line(aes(x,y2), color = "red") + 
    geom_line(aes(x,y3), color = "red") + 
    geom_segment(aes(x = 1 + 1.64*0.25, y = 2, xend = 1 + 1.64*0.25, yend = 2.4), color = "red") +
    geom_segment(aes(x = 2 + 1.64*0.25, y = 4, xend = 2 + 1.64*0.25, yend = 4.4), color = "red") +
    geom_segment(aes(x = 3 + 1.64*0.25, y = 6, xend = 3 + 1.64*0.25, yend = 6.4), color = "red") +
    
    geom_segment(aes(x = 1 + 1.64*0.25 - 0.1, y = 2, xend = 1 + 1.64*0.25 - 0.1, yend = 2.7), color = "red", linetype='dotted') +
    geom_segment(aes(x = 1 + 1.64*0.25 - 0.2, y = 2, xend = 1 + 1.64*0.25 - 0.2, yend = 3.1), color = "red", linetype='dotted') +
    geom_segment(aes(x = 1 + 1.64*0.25 - 0.3, y = 2, xend = 1 + 1.64*0.25 - 0.3, yend = 3.4), color = "red", linetype='dotted') +
    geom_segment(aes(x = 1 + 1.64*0.25 - 0.4, y = 2, xend = 1 + 1.64*0.25 - 0.4, yend = 3.6), color = "red", linetype='dotted') +
    geom_segment(aes(x = 1 + 1.64*0.25 - 0.5, y = 2, xend = 1 + 1.64*0.25 - 0.5, yend = 3.5), color = "red", linetype='dotted') +
    geom_segment(aes(x = 1 + 1.64*0.25 - 0.6, y = 2, xend = 1 + 1.64*0.25 - 0.6, yend = 3.2), color = "red", linetype='dotted') +
    geom_segment(aes(x = 1 + 1.64*0.25 - 0.7, y = 2, xend = 1 + 1.64*0.25 - 0.7, yend = 2.8), color = "red", linetype='dotted') +
    geom_segment(aes(x = 1 + 1.64*0.25 - 0.8, y = 2, xend = 1 + 1.64*0.25 - 0.8, yend = 2.5), color = "red", linetype='dotted') +
    geom_segment(aes(x = 1 + 1.64*0.25 - 0.9, y = 2, xend = 1 + 1.64*0.25 - 0.9, yend = 2.2), color = "red", linetype='dotted') +
    annotate("text", x = 1, y = 2.5, label = c("95%"), size = 10) +
    
    
    geom_segment(aes(x = 2 + 1.64*0.25 - 0.1, y = 4, xend = 2 + 1.64*0.25 - 0.1, yend = 4.7), color = "red", linetype='dotted') +
    geom_segment(aes(x = 2 + 1.64*0.25 - 0.2, y = 4, xend = 2 + 1.64*0.25 - 0.2, yend = 5.1), color = "red", linetype='dotted') +
    geom_segment(aes(x = 2 + 1.64*0.25 - 0.3, y = 4, xend = 2 + 1.64*0.25 - 0.3, yend = 5.4), color = "red", linetype='dotted') +
    geom_segment(aes(x = 2 + 1.64*0.25 - 0.4, y = 4, xend = 2 + 1.64*0.25 - 0.4, yend = 5.6), color = "red", linetype='dotted') +
    geom_segment(aes(x = 2 + 1.64*0.25 - 0.5, y = 4, xend = 2 + 1.64*0.25 - 0.5, yend = 5.5), color = "red", linetype='dotted') +
    geom_segment(aes(x = 2 + 1.64*0.25 - 0.6, y = 4, xend = 2 + 1.64*0.25 - 0.6, yend = 5.2), color = "red", linetype='dotted') +
    geom_segment(aes(x = 2 + 1.64*0.25 - 0.7, y = 4, xend = 2 + 1.64*0.25 - 0.7, yend = 4.8), color = "red", linetype='dotted') +
    geom_segment(aes(x = 2 + 1.64*0.25 - 0.8, y = 4, xend = 2 + 1.64*0.25 - 0.8, yend = 4.5), color = "red", linetype='dotted') +
    geom_segment(aes(x = 2 + 1.64*0.25 - 0.9, y = 4, xend = 2 + 1.64*0.25 - 0.9, yend = 4.2), color = "red", linetype='dotted') +
    annotate("text", x = 2, y = 4.5, label = c("95%"), size = 10) +
    
    geom_segment(aes(x = 3 + 1.64*0.25 - 0.1, y = 6, xend = 3 + 1.64*0.25 - 0.1, yend = 6.7), color = "red", linetype='dotted') +
    geom_segment(aes(x = 3 + 1.64*0.25 - 0.2, y = 6, xend = 3 + 1.64*0.25 - 0.2, yend = 7.1), color = "red", linetype='dotted') +
    geom_segment(aes(x = 3 + 1.64*0.25 - 0.3, y = 6, xend = 3 + 1.64*0.25 - 0.3, yend = 7.4), color = "red", linetype='dotted') +
    geom_segment(aes(x = 3 + 1.64*0.25 - 0.4, y = 6, xend = 3 + 1.64*0.25 - 0.4, yend = 7.6), color = "red", linetype='dotted') +
    geom_segment(aes(x = 3 + 1.64*0.25 - 0.5, y = 6, xend = 3 + 1.64*0.25 - 0.5, yend = 7.5), color = "red", linetype='dotted') +
    geom_segment(aes(x = 3 + 1.64*0.25 - 0.6, y = 6, xend = 3 + 1.64*0.25 - 0.6, yend = 7.2), color = "red", linetype='dotted') +
    geom_segment(aes(x = 3 + 1.64*0.25 - 0.7, y = 6, xend = 3 + 1.64*0.25 - 0.7, yend = 6.8), color = "red", linetype='dotted') +
    geom_segment(aes(x = 3 + 1.64*0.25 - 0.8, y = 6, xend = 3 + 1.64*0.25 - 0.8, yend = 6.5), color = "red", linetype='dotted') +
    geom_segment(aes(x = 3 + 1.64*0.25 - 0.9, y = 6, xend = 3 + 1.64*0.25 - 0.9, yend = 6.2), color = "red", linetype='dotted') +
    annotate("text", x = 3, y = 6.5, label = c("95%"), size = 10) +
    
    
    coord_flip() +
    scale_y_continuous(name ="x",
                       breaks = c(2, 4, 6),
                       labels = c("1","2","3")) + 
    scale_x_continuous(name ="y",
                       breaks = c(1, 2, 3),
                       labels = c("2","4","6")) + 
    theme(plot.title = element_text(size=24), axis.text = element_text(size=18), axis.title = element_text(size=18))
}

