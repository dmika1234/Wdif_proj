
K=48
T=2
S0=50
r=0.02
delta_t=1/12
sigma=0.3
u=exp(sigma*sqrt(delta_t))
d=exp(-sigma*sqrt(delta_t))

calc_payoff_call <- function(K, St){
  pmax(St - K, 0)
}


calc_payoff_put <- function(K, St){
  pmax(K- St, 0)
}


calc_prices <- function(n, S0, u, d){
  a <- 0:n
  return(S0 * (u ^ (rev(a))) * (d ^ (a)))
}


generate_price_tree <- function(S0, u, d, periods){
  list_tree <- sapply(0:periods, calc_prices, S0 = S0, u = u, d = d)
  return(list_tree)
}


calculate_call_cost <- function(S0, K, T, delta_t, u, d, r){
  
  time_vec <- seq(from = 0 , to = T, by = delta_t)
  n <- length(time_vec)
  price_tree <- generate_price_tree(S0, u, d, n - 1)
  
  costs <- vector(mode = "list", length = n)
  p <- (price_tree[[n-1]][1] * exp(r * delta_t) - price_tree[[n]][2]) /
    (price_tree[[n]][1] - price_tree[[n]][2])
  costs[[n]] <- calc_payoff_call(K, price_tree[[n]])
  
  for(i in rev(2:n)){
    costs_matrix <- matrix(c(costs[[i]][1],
                             rep(costs[[i]][-c(1, i)], each = ifelse(i == 2, 0, 2)),
                             costs[[i]][i]), ncol = i - 1)
    p_matrix <- matrix(rep(c(p, 1-p), i - 1), ncol = i - 1)
    costs[[i-1]] <- exp(-r * delta_t) * apply(costs_matrix * p_matrix, 2, sum)
  }
  return(costs)
}


calculate_put_cost <- function(S0, K, T, delta_t, u, d, r){
  
  time_vec <- seq(from = 0 , to = T, by = delta_t)
  n <- length(time_vec)
  price_tree <- generate_price_tree(S0, u, d, n - 1)
  
  costs <- vector(mode = "list", length = n)
  p <- (price_tree[[n-1]][1] * exp(r * delta_t) - price_tree[[n]][2]) /
    (price_tree[[n]][1] - price_tree[[n]][2])
  costs[[n]] <- calc_payoff_put(K, price_tree[[n]])
  
  for(i in rev(2:n)){
    costs_matrix <- matrix(c(costs[[i]][1],
                             rep(costs[[i]][-c(1, i)], each = ifelse(i == 2, 0, 2)),
                             costs[[i]][i]), ncol = i - 1)
    p_matrix <- matrix(rep(c(p, 1-p), i - 1), ncol = i - 1)
    costs[[i-1]] <- exp(-r * delta_t) * apply(costs_matrix * p_matrix, 2, sum)
  }
  return(costs)
}
calculate_american_call_cost <- function(S0, K, T, delta_t, u, d, r){
  
  time_vec <- seq(from = 0 , to = T, by = delta_t)
  n <- length(time_vec)
  price_tree <- generate_price_tree(S0, u, d, n - 1)
  
  costs <- vector(mode = "list", length = n)
  p <- (price_tree[[n-1]][1] * exp(r * delta_t) - price_tree[[n]][2]) /
    (price_tree[[n]][1] - price_tree[[n]][2])
  costs[[n]] <- calc_payoff_call(K, price_tree[[n]])
  
  for(i in rev(2:n)){
    costs_matrix <- matrix(c(costs[[i]][1],
                             rep(costs[[i]][-c(1, i)], each = ifelse(i == 2, 0, 2)),
                             costs[[i]][i]), ncol = i - 1)
    p_matrix <- matrix(rep(c(p, 1-p), i - 1), ncol = i - 1)
    costs[[i-1]] <- apply(matrix(c(exp(-r * delta_t) * apply(costs_matrix * p_matrix, 2, sum),calc_payoff_call(K,price_tree[[i-1]])),byrow=T,nrow=2),2,max)
  }
  
  return(costs)
}

calculate_american_put_cost <- function(S0, K, T, delta_t, u, d, r){
  
  time_vec <- seq(from = 0 , to = T, by = delta_t)
  n <- length(time_vec)
  price_tree <- generate_price_tree(S0, u, d, n - 1)
  
  costs <- vector(mode = "list", length = n)
  p <- (price_tree[[n-1]][1] * exp(r * delta_t) - price_tree[[n]][2]) /
    (price_tree[[n]][1] - price_tree[[n]][2])
  costs[[n]] <- calc_payoff_put(K, price_tree[[n]])
  
  for(i in rev(2:n)){
    costs_matrix <- matrix(c(costs[[i]][1],
                             rep(costs[[i]][-c(1, i)], each = ifelse(i == 2, 0, 2)),
                             costs[[i]][i]), ncol = i - 1)
    p_matrix <- matrix(rep(c(p, 1-p), i - 1), ncol = i - 1)
    costs[[i-1]] <-apply(matrix(c(exp(-r * delta_t) * apply(costs_matrix * p_matrix, 2, sum),calc_payoff_put(K, price_tree[[i-1]])),byrow=T,nrow=2),2,max)
  }
  return(costs)
}


calculate_ex_moments_am_put<- function(S0, K, T, delta_t, u, d, r){
  
  time_vec <- seq(from = 0 , to = T, by = delta_t)
  n <- length(time_vec)
  price_tree <- generate_price_tree(S0, u, d, n - 1)
  
  costs <- vector(mode = "list", length = n-1)
  execution_moments<-vector(mode = "list", length = n)
  p <- (price_tree[[n-1]][1] * exp(r * delta_t) - price_tree[[n]][2]) /
    (price_tree[[n]][1] - price_tree[[n]][2])
  costs[[n]] <- calc_payoff_put(K, price_tree[[n]])
  execution_moments[[n]]<-calc_payoff_put(K, price_tree[[n]])>0
  for(i in rev(2:n)){
    costs_matrix <- matrix(c(costs[[i]][1],
                             rep(costs[[i]][-c(1, i)], each = ifelse(i == 2, 0, 2)),
                             costs[[i]][i]), ncol = i - 1)
    p_matrix <- matrix(rep(c(p, 1-p), i - 1), ncol = i - 1)
    costs[[i-1]] <-exp(-r * delta_t) * apply(costs_matrix * p_matrix, 2, sum)
    execution_moments[[i-1]]<- costs[[i-1]]<calc_payoff_put(K, price_tree[[i-1]])
    }
  return(execution_moments)
}



calculate_execution_am_call_cost <- function(S0, K, T, delta_t, u, d, r){
  
  time_vec <- seq(from = 0 , to = T, by = delta_t)
  n <- length(time_vec)
  price_tree <- generate_price_tree(S0, u, d, n - 1)
  
  costs <- vector(mode = "list", length = n)
  execution_moments<-vector(mode = "list", length = n)
  p <- (price_tree[[n-1]][1] * exp(r * delta_t) - price_tree[[n]][2]) /
    (price_tree[[n]][1] - price_tree[[n]][2])
  costs[[n]] <- calc_payoff_call(K, price_tree[[n]])
  execution_moments[[n]]<-calc_payoff_call(K, price_tree[[n]])>0
  for(i in rev(2:n)){
    costs_matrix <- matrix(c(costs[[i]][1],
                             rep(costs[[i]][-c(1, i)], each = ifelse(i == 2, 0, 2)),
                             costs[[i]][i]), ncol = i - 1)
    p_matrix <- matrix(rep(c(p, 1-p), i - 1), ncol = i - 1)
    costs[[i-1]] <- apply(matrix(c(exp(-r * delta_t) * apply(costs_matrix * p_matrix, 2, sum),calc_payoff_call(K,price_tree[[i-1]])),byrow=T,nrow=2),2,max)
    execution_moments[[i-1]]<- costs[[i-1]]<calc_payoff_call(K, price_tree[[i-1]])
  }
  
  return(execution_moments)
}


