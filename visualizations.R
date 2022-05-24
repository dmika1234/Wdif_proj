source('funs.R')
library(ggplot2)
library(dplyr)


K <- 48
S0 <- 50
delta_t <- 1 / 12
T <- 2
sigma <- 0.3
u <- exp(sigma * sqrt(delta_t))
d <- exp(-sigma * sqrt(delta_t))
r <- 0.02


res <- analize_opt(S0 = S0, K = K, T = T, delta_t = delta_t,
                   u = u, d = d, r = r, opt_type = "A", call_opt = TRUE)
# Cost
res %>% filter(time == 0)





ggplot(res) +
  theme_bw() +
  geom_point(aes(x = time, y = price, color = if_executed)) +
  geom_hline(yintercept = K)

