
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
    costs[[i-1]] <- exp(-r * delta_t) * apply(costs_matrix * p_matrix, 2, sum)
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
    costs[[i-1]] <- exp(-r * delta_t) * apply(costs_matrix * p_matrix, 2, sum)
    execution_moments[[i-1]]<- costs[[i-1]]<calc_payoff_call(K, price_tree[[i-1]])
  }
  
  return(execution_moments)
}


# Ostateczna funkcja
analize_opt <- function(S0, K, T, delta_t, u, d, r, call_opt = TRUE, opt_type = "E"){
  
  time_vec <- seq(from = 0 , to = T, by = delta_t)
  n <- length(time_vec)
  price_tree <- generate_price_tree(S0, u, d, n - 1)
  
  p <- (price_tree[[n-1]][1] * exp(r * delta_t) - price_tree[[n]][2]) /
    (price_tree[[n]][1] - price_tree[[n]][2])
  
  if(call_opt){
    payoff_FUN <- calc_payoff_call
  }else{
    payoff_FUN <- calc_payoff_put
  }

  
  costs <- vector(mode = "list", length = n)
  costs[[n]] <- payoff_FUN(K, price_tree[[n]])
  
  execution_moments <- vector(mode = "list", length = n)
  execution_moments[[n]] <- payoff_FUN(K, price_tree[[n]]) > 0
  
  final_df <- data.frame(price = price_tree[[n]])
  final_df$costs <- costs[[n]]
  final_df$time <- time_vec[n]
  final_df$if_executed <- execution_moments[[n]]
    
    
  for(i in rev(2:n)){
    costs_matrix <- matrix(c(costs[[i]][1],
                             rep(costs[[i]][-c(1, i)], each = ifelse(i == 2, 0, 2)),
                             costs[[i]][i]), ncol = i - 1)
    p_matrix <- matrix(rep(c(p, 1-p), i - 1), ncol = i - 1)
    if(opt_type == "E"){
      costs[[i-1]] <- exp(-r * delta_t) * apply(costs_matrix * p_matrix, 2, sum)
      execution_moments[[i-1]] <- rep(FALSE, length(costs[[i-1]]))
    }
    if(opt_type == "A"){
      costs[[i-1]] <- apply(matrix(c(exp(-r * delta_t) * apply(costs_matrix * p_matrix, 2, sum),
                                     payoff_FUN(K,price_tree[[i-1]])), byrow = TRUE, nrow = 2), 2, max)
      cost <- exp(-r * delta_t) * apply(costs_matrix * p_matrix, 2, sum)
      execution_moments[[i-1]] <- cost < payoff_FUN(K, price_tree[[i-1]])
    }
    final_df <- rbind(final_df,
                      data.frame(price = price_tree[[i-1]], costs = costs[[i-1]],
                                 time = rep(time_vec[i-1], length(price_tree[[i-1]])),
                                 if_executed = execution_moments[[i-1]]))
  }
  return(final_df)
}

# Ostateczna funkcja 2 ( z gotówką i akcjami)
analize_opt2 <- function(S0, K, T, delta_t, u, d, r, call_opt = TRUE, opt_type = "E"){
  time_vec <- seq(from = 0 , to = T, by = delta_t)
  n <- length(time_vec)
  price_tree <- generate_price_tree(S0, u, d, n - 1)
  
  p <- (price_tree[[n-1]][1] * exp(r * delta_t) - price_tree[[n]][2]) /
    (price_tree[[n]][1] - price_tree[[n]][2])
  
  if(call_opt){
    payoff_FUN <- calc_payoff_call
  }else{
    payoff_FUN <- calc_payoff_put
  }
  
  
  costs <- vector(mode = "list", length = n)
  costs[[n]] <- payoff_FUN(K, price_tree[[n]])
  shares<-vector(mode = "list", length = n)
  shares[[n]]<-rep(NA,n)
  cash<-vector(mode = "list", length = n)
  cash[[n]]<-rep(NA, n)
  execution_moments <- vector(mode = "list", length = n)
  execution_moments[[n]] <- payoff_FUN(K, price_tree[[n]]) > 0
  
  final_df <- data.frame(price = price_tree[[n]])
  final_df$costs <- costs[[n]]
  final_df$time <- time_vec[n]
  final_df$if_executed <- execution_moments[[n]]
  final_df$shares<-shares[[n]]
  final_df$cash<-cash[[n]]
  
  for(i in rev(2:n)){
    costs_matrix <- matrix(c(costs[[i]][1],
                             rep(costs[[i]][-c(1, i)], each = ifelse(i == 2, 0, 2)),
                             costs[[i]][i]), ncol = i - 1)
    p_matrix <- matrix(rep(c(p, 1-p), i - 1), ncol = i - 1)
    if(opt_type == "E"){
      costs[[i-1]] <- exp(-r * delta_t) * apply(costs_matrix * p_matrix, 2, sum)
      execution_moments[[i-1]] <- rep(FALSE, length(costs[[i-1]]))
    }
    if(opt_type == "A"){
      costs[[i-1]] <- apply(matrix(c(exp(-r * delta_t) * apply(costs_matrix * p_matrix, 2, sum),
                                     payoff_FUN(K,price_tree[[i-1]])), byrow = TRUE, nrow = 2), 2, max)
      cost <- exp(-r * delta_t) * apply(costs_matrix * p_matrix, 2, sum)
      execution_moments[[i-1]] <- cost < payoff_FUN(K, price_tree[[i-1]])
    }
    final_df <- rbind(final_df,
                      data.frame(price = price_tree[[i-1]], costs = costs[[i-1]],
                                 shares= round((costs[[i]][1:(i-1)]-costs[[i]][2:i])/(price_tree[[i]][1:(i-1)]-price_tree[[i]][2:i]),2),
                                 cash=round((costs[[i]][2:i]*price_tree[[i]][1:(i-1)]-costs[[i]][1:(i-1)]*price_tree[[i]][2:i])/exp(r*delta_t)*(price_tree[[i]][1:(i-1)]-price_tree[[i]][2:i]),2),
                                 time = rep(time_vec[i-1], length(price_tree[[i-1]])),
                                 if_executed = execution_moments[[i-1]]))
  }
  return(final_df)
}

# analize_opt2 (S0, K, T, delta_t, u, d, r, call_opt = FALSE, opt_type = "E")
# analize_opt2 (S0, K, T, delta_t, u, d, r, call_opt = TRUE, opt_type = "E")
# analize_opt2 (S0, K, T, delta_t, u, d, r, call_opt = FALSE, opt_type = "A")
# analize_opt2 (S0, K, T, delta_t, u, d, r, call_opt = TRUE, opt_type = "A")



