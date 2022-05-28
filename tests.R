# File contains functions to test constraints of option prices, and testing of prices from our project
source('funs.R')


# Params
K <- 48
S0 <- 50
delta_t <- 1 / 12
Ttime <- 2
sigma <- 0.3
u <- exp(sigma * sqrt(delta_t))
d <- exp(-sigma * sqrt(delta_t))
r <- 0.02


check_conditions <- function(Ce, Pe, Ca, Pa, S0, K, Ttime, r){
  results <- matrix(NA, nrow = 1, ncol = 11)
  colnames(results) <- c("Ce <= S0", "Ca <= S0", "Pe <= Ke^(-rT)", "Pa <= K",
                         "Ce >= S0 - Ke^(-rT)", "Pe >= Ke^(-rT) - S0",
                         "Ca >= Ce", "Pa >= Pe", "Pa >= K - S0", "parytetE", "parytetA")
  results[1] <- Ce <= S0
  results[2] <- Ca <= S0
  results[3] <- Pe <= K * exp(-r*Ttime)
  results[4] <- Pa <= K
  results[5] <- Ce >= S0 - K*exp(-r*Ttime)
  results[6] <- Pe >= K*exp(-r*Ttime) - S0
  results[7] <- Ca >= Ce
  results[8] <- Pa >= Pe
  results[9] <- Pa >= K - S0
  results[10] <- abs(Ce + K*exp(-r*Ttime) - (Pe + S0)) <= 1e-10
  results[11] <- abs(Ca + K*exp(-r*Ttime) - (Pa + S0)) <= 1e-10
  
  return(results)
}


res_cond <- check_conditions(eu_call_price, eu_put_price, am_call_price, am_put_price, S0, K, Ttime, r)
res_cond

am_call_price + K*exp(-r*T) - (am_put_price+ S0)
