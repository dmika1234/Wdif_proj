source('funs.R')
library(dplyr)

# Params
K <- 48
S0 <- 50
delta_t <- 1 / 12
T <- 2
sigma <- 0.3
u <- exp(sigma * sqrt(delta_t))
d <- exp(-sigma * sqrt(delta_t))
r <- 0.02



#==================== Calculating option prices ========================
eu_call_price <- as.numeric(analize_opt(S0 = S0, K = K, T = T, delta_t = delta_t,
                             u = u, d = d, r = r, opt_type = "E", call_opt = TRUE) %>%
  filter(time == 0) %>%
  select(costs))
# 10.19118
eu_call_price

eu_put_price <- as.numeric(analize_opt(S0 = S0, K = K, T = T, delta_t = delta_t,
                            u = u, d = d, r = r, opt_type = "E", call_opt = FALSE) %>%
  filter(time == 0) %>%
  select(costs))
# 6.309078
eu_put_price

am_put_price <- as.numeric(analize_opt(S0 = S0, K = K, T = T, delta_t = delta_t,
                            u = u, d = d, r = r, opt_type = "A", call_opt = FALSE) %>%
  filter(time == 0) %>%
  select(costs))
# 6.470605
am_put_price

am_call_price <- as.numeric(analize_opt(S0 = S0, K = K, T = T, delta_t = delta_t,
                             u = u, d = d, r = r, opt_type = "A", call_opt = TRUE) %>%
  filter(time == 0) %>%
  select(costs))
# 10.19118
am_call_price
# ========================






