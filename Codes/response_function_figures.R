library(ggplot2)

f_high <- function(x){
  y = 250 * (1 - exp(-.035 * (20 + x)))
  return(y)
}

response_high <- ggplot() +
  xlim(0, 200) +
  ylim(100, 255) +
  geom_function(fun = f_high) +
  xlab("Input Rate") +
  ylab("Yield") +
  theme(axis.title.y=element_blank())


f_mid <- function(x){
  y = 250 * (1 - exp(-.02 * (45 + x)))
  return(y)
}

response_mid <- ggplot() +
  xlim(0, 200) +
  ylim(100, 255) +
  geom_function(fun = f_mid) +
  xlab("Input Rate") +
  ylab("Yield") +
  theme(axis.title.y=element_blank())

f_low <- function(x){
  y = 250 * (1 - exp(-.009 * (160 + x)))
  return(y)
}

response_low <- ggplot() +
  xlim(0, 200) +
  ylim(100, 255) +
  geom_function(fun = f_low) +
  xlab("Input Rate") +
  ylab("Yield") 
response_low

# f_seed <- function(x){
#   y = 150 + 5.2*x - 0.068*(x^2)
#   return(y)
# }
# 
# response_seed <- ggplot() +
#   xlim(0, 60) +
#   ylim(100, 255) +
#   geom_function(fun = f_seed) +
#   xlab("Input Rate") +
#   ylab("Yield") 
# response_seed

library(patchwork)
response_figure <- (response_low + response_mid + response_high) +
  plot_annotation(tag_levels = 'A')
saveRDS(response_figure, here("Results/response_figure.rds"))

