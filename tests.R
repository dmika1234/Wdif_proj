source('funs.R')


# Przykład z wykładu dla modelu dwuokresowego
res <- calculate_call_cost(20, 21, 1 / 2, 3 /12, 1.1, 0.9, 0.12)
# V_0
res[[1]]
res <-calculate_cost(20, 21, 1 / 2, 3 /12, 1.1, 0.9, 0.12)
res
  
  
res <- calculate_put_cost(20, 21, 1 / 2, 3 /12, 1.1, 0.9, 0.12)

i <- 0.03
v <- 1 / (1+i)

(1 - v ^ 4) /i*100

# Tests
K <- 48
S0 <- 50
delta_t <- 1 / 12
T <- 2
sigma <- 0.3
u <- exp(sigma * sqrt(delta_t))
d <- exp(-sigma * sqrt(delta_t))
r <- 0.02
n
res1 <- calculate_call_cost(S0 = S0, K = K, T = T, delta_t = delta_t,
                            u = u, d = d, r = r)
res2 <- calculate_put_cost(S0 = S0, K = K, T = T, delta_t = delta_t,
                            u = u, d = d, r = r)
res1[[1]] + K * exp(-r * T)
res2[[1]] + S0
res2 <- calculate_ex_moments_am_put(S0 = S0, K = K, T = T, delta_t = delta_t,
                           u = u, d = d, r = r)

K * exp(-r * T) - S0
library("derivmkts")
binomplot(S0, K, 0.3, r, 2, 0, 25, american = FALSE, putopt = FALSE, specifyupdn = TRUE, up = u, dn = d)
binomplot(S0, K, 0.3, r, 2, 0, 25, american = TRUE, putopt = TRUE, specifyupdn = TRUE,  up = u, dn = d)



library(ggplot2)
am_ex <- calculate_execution_am_call_cost(50,K,2,delta_t,u,d,0.02)
am_price <- calculate_american_call_cost(50,K,2,delta_t,u,d,0.02)
am_ex
#length(am_ex)
#length(am_price)
p <- ggplot()
for (i in seq(0,length(am_price)-1)) 
{
  for (j in seq(1,length(am_price[[i+1]])))
  {
    color = '#333399'
    if (am_ex[[i+1]][j] == TRUE)
    {
      color = '#66cc00'
    }
    force(p)
    p <- p + geom_point(aes(i*delta_t,am_price[[i+1]][j]),colour=color,size=3)
  }
  
}
p <- p + geom_point(aes(i*delta_t,am_price[[i+1]][1]),colour=color,size=3)
p <- p + geom_point(aes(i*delta_t,am_price[[i+1]][2]),colour=color,size=3)
p <- p + geom_point(aes(i*delta_t,am_price[[i+1]][3]),colour=color,size=3)
p

