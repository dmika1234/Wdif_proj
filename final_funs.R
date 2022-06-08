K=48
T=2
S0=50
r=0.02
delta_t=1/12
sigma=0.3
u=exp(sigma*sqrt(delta_t))
d=exp(-sigma*sqrt(delta_t))


##################################################################################
#             DRZEWO DWUMIANOWE I PORTFEL ZABEZPIECZAJACY
##################################################################################


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

# analize_opt (S0, K, T, delta_t, u, d, r, call_opt = FALSE, opt_type = "E")
# analize_opt (S0, K, T, delta_t, u, d, r, call_opt = TRUE, opt_type = "E")
# analize_opt (S0, K, T, delta_t, u, d, r, call_opt = FALSE, opt_type = "A")
# analize_opt (S0, K, T, delta_t, u, d, r, call_opt = TRUE, opt_type = "A")

##################################################################################
#               MOMENTY WYKONANIA
##################################################################################


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

##################################################################################
#               WIZUALIZACJE
##################################################################################
# ============= Analiza momentów wykonania i podstawowe wykresy
# Results for ==AMERICAN PUT==
res1 <- analize_opt(S0 = S0, K = K, T = 1, delta_t = delta_t,
                   u = u, d = d, r = r, opt_type = "A", call_opt = FALSE)
res3 <- analize_opt(S0 = S0, K = K, T = 3, delta_t = delta_t,
                    u = u, d = d, r = r, opt_type = "A", call_opt = FALSE)
res6 <- analize_opt(S0 = S0, K = K, T = 6, delta_t = delta_t,
                   u = u, d = d, r = r, opt_type = "A", call_opt = FALSE)


# Analazing border points
border_points1 <- res1 %>% 
  filter(if_executed == TRUE) %>% 
  group_by(time) %>% 
  summarise(maks = max(price)) %>% 
  mutate(T = 1)
border_points3 <- res3 %>% 
  filter(if_executed == TRUE) %>% 
  group_by(time) %>% 
  summarise(maks = max(price)) %>% 
  mutate(T = 3)
border_points6 <- res6 %>% 
  filter(if_executed == TRUE) %>% 
  group_by(time) %>% 
  summarise(maks = max(price)) %>% 
  mutate(T = 6)

border_points <- rbind(border_points1, border_points3, border_points6)
model_barrier <- lm(data = border_points, formula = maks ~ poly(time, 22) + poly(T, 2))




# Log-scale: + coord_trans(y = "log2")
t <- 2
rest <- analize_opt(S0 = S0, K = K, T = 2, delta_t = delta_t,
                    u = u, d = d, r = r, opt_type = "A", call_opt = FALSE)
border_pointst <- rest %>% 
  filter(if_executed == TRUE) %>% 
  group_by(time) %>% 
  summarise(maks = max(price))
border_pointst <- border_pointst %>% 
  mutate(T = t, predicted_maks = predict(model_barrier, newdata = data.frame(time = border_pointst$time, T = t)))


########
# Basic plot e-c
resec <- analize_opt(S0 = S0, K = K, T = 2, delta_t = delta_t,
                     u = u, d = d, r = r, opt_type = "E", call_opt = TRUE)
ggplot(resec) +
  theme_bw() +
  geom_point(aes(x = time, y = price, color = if_executed)) +
  geom_hline(yintercept = K) +
  geom_text(aes(0.03, K,label = paste("K=", K), vjust = 2)) +
  coord_trans(y = "log2") +
  labs(title = "Opcja europejska call", color = "Czy wykonano", x = "Czas", y="Cena aktywa") +
  scale_color_manual(labels=c("Nie", "Tak"), values=c("darkblue","cyan")) +
  theme(plot.title = element_text(hjust = 0.5), legend.position = 'none')

# Basic plot e-p
resep <- analize_opt(S0 = S0, K = K, T = 2, delta_t = delta_t,
                     u = u, d = d, r = r, opt_type = "E", call_opt = FALSE)
ggplot(resep) +
  theme_bw() +
  geom_point(aes(x = time, y = price, color = if_executed)) +
  geom_hline(yintercept = K) +
  geom_text(aes(0.03, K,label = paste("K=", K), vjust = 2)) +
  coord_trans(y = "log2") +
  labs(title = "Opcja europejska put", color = "Czy wykonano", x = "Czas", y="Cena aktywa") +
  scale_color_manual(labels=c("Nie", "Tak"), values=c("darkblue","cyan")) +
  theme(plot.title = element_text(hjust = 0.5))

# Basic plot a-p
resap <- analize_opt(S0 = S0, K = K, T = 2, delta_t = delta_t,
                    u = u, d = d, r = r, opt_type = "A", call_opt = FALSE)
ggplot(resap) +
  theme_bw() +
  geom_point(aes(x = time, y = price, color = if_executed)) +
  geom_hline(yintercept = K) +
  geom_text(aes(0.03, K,label = paste("K=", K), vjust = 2)) +
  coord_trans(y = "log2") +
  labs(title = "Opcja amerykańska put", color = "Czy wykonano", x = "Czas", y="Cena aktywa") +
  scale_color_manual(labels=c("Nie", "Tak"), values=c("darkblue","cyan")) +
  theme(plot.title = element_text(hjust = 0.5), legend.position = 'none')

# A-P PLot with predicted
border_pointst <- resap %>% 
  filter(if_executed == TRUE) %>% 
  group_by(time) %>% 
  summarise(maks = max(price))
border_pointst <- border_pointst %>% 
  mutate(T = 2, predicted_maks = predict(model_barrier, newdata = data.frame(time = border_pointst$time, T = 2)))

ggplot() +
  theme_bw() +
  geom_point(data = resap, aes(x = time, y = price, color = if_executed)) +
  geom_hline(yintercept = K) +
  geom_text(aes(0.03, K,label = paste("K=", K), vjust = 2)) +
  geom_line(data = border_pointst, aes(x = time, y = maks, linetype = 'Prawdziwy'), color = 'red', size = 1) +
  geom_line(data = border_pointst, aes(x = time, y = predicted_maks, linetype = 'Przewidziany'), size = 1, color = 'red') +
  coord_trans(y = "log2") +
  labs(title = "Opcja amerykańska put z predykcją", color = "Czy wykonano", x = "Czas", y = "Cena aktywa", linetype = "Podział\nmomentów\nwykonania") +
  scale_color_manual(labels=c("Nie", "Tak"), values=c("darkblue","cyan")) +
  theme(plot.title = element_text(hjust = 0.5))





##################################################################################
#               ANALIZA WRAZLIWOSCI
##################################################################################


##################################################################################
#     STRIKE  K
##################################################################################

(S0, K, T, delta_t, u, d, r, call_opt = FALSE, opt_type = "E")
strike <- seq(40,2000,10)

strike <- cbind(strike, rep(c(0),length(strike)), rep(c(0),length(strike)),rep(c(0),length(strike)),
rep(c(0),length(strike)))

for(i in 1:length(strike[,1]))
{
  w <- analize_opt(50,strike[i,1],T,delta_t,u,d,r)
  strike[i,2] <- w$costs[length(w$costs)]
  w <- analize_opt(50,strike[i,1],T,delta_t,u,d,r,FALSE)
  strike[i,3] <- w$costs[length(w$costs)]
  w <- analize_opt(50,strike[i,1],T,delta_t,u,d,r,TRUE,'A')
  strike[i,4] <- w$costs[length(w$costs)]
  w <- analize_opt(50,strike[i,1],T,delta_t,u,d,r,FALSE,'A')
  strike[i,5] <- w$costs[length(w$costs)]
}
#write.csv(strike,"C:\\Users\\weron\\Documents\\studia\\stopien_II\\semestr2_letni\\WDIF\\strike.csv", row.names = FALSE)
strike <- data.frame(strike)
ggplot(strike,aes(x = strike))+
  geom_line(aes(y= V2,colour = 'europejski call'))+
  geom_line(aes(y=V3,colour='europejski put'))+
  geom_line(aes(y=V4,colour='amerykañski call'))+geom_line(aes(y=V5,colour='amerykañski put'))+
  scale_color_manual(name="opcja",breaks = c("europejski call","europejski put","amerykañski call","amerykañski put"),
                     values=c("darkolivegreen","#66cc66","#003366","#33ccff"))+
  ylab("cena")+
  ggtitle("Zale¿noœæ ceny opcji od ceny wykonania")+theme_bw()+theme(plot.title = element_text(hjust = 0.5))

# strike blizej naszej sytuacji

strike <- seq(10,100,1)

strike <- cbind(strike, rep(c(0),length(strike)), rep(c(0),length(strike)),rep(c(0),length(strike)),
                rep(c(0),length(strike)))

for(i in 1:length(strike[,1]))
{
  w <- analize_opt(50,strike[i,1],T,delta_t,u,d,r)
  strike[i,2] <- w$costs[length(w$costs)]
  w <- analize_opt(50,strike[i,1],T,delta_t,u,d,r,FALSE)
  strike[i,3] <- w$costs[length(w$costs)]
  w <- analize_opt(50,strike[i,1],T,delta_t,u,d,r,TRUE,'A')
  strike[i,4] <- w$costs[length(w$costs)]
  w <- analize_opt(50,strike[i,1],T,delta_t,u,d,r,FALSE,'A')
  strike[i,5] <- w$costs[length(w$costs)]
}
#write.csv(strike,"C:\\Users\\weron\\Documents\\studia\\stopien_II\\semestr2_letni\\WDIF\\strike.csv", row.names = FALSE)
strike <- data.frame(strike)
ggplot(strike,aes(x = strike))+
  geom_line(aes(y= V2,colour = 'europejski call'))+
  geom_line(aes(y=V3,colour='europejski put'))+
  geom_line(aes(y=V4,colour='amerykañski call'))+geom_line(aes(y=V5,colour='amerykañski put'))+
  scale_color_manual(name="opcja",breaks = c("europejski call","europejski put","amerykañski call","amerykañski put"),
                     values=c("darkolivegreen","#66cc66","#003366","#33ccff"))+
  ylab("cena")+
  ggtitle("Zale¿noœæ ceny opcji od ceny wykonania")+theme_bw()+theme(plot.title = element_text(hjust = 0.5))




##################################################################################
#     CENA SPOT
##################################################################################

s0 <- seq(30,100,1)
n <- length(s0)
s0 <- cbind(s0,rep(c(0),n), rep(c(0),n),rep(c(0),n),rep(c(0),n))
View(s0)
#analize_opt(s0,K,T,delta_t,u,d,r)

for(i in 1:n)
{
  w <- analize_opt(s0[i,1],K,T,delta_t,u,d,r)
  s0[i,2] <- w$costs[length(w$costs)]
  w <- analize_opt(s0[i,1],K,T,delta_t,u,d,r,FALSE)
  s0[i,3] <- w$costs[length(w$costs)]
  w <- analize_opt(s0[i,1],K,T,delta_t,u,d,r,TRUE,'A')
  s0[i,4] <- w$costs[length(w$costs)]
  w <- analize_opt(s0[i,1],K,T,delta_t,u,d,r,FALSE,'A')
  s0[i,5] <- w$costs[length(w$costs)]
}

s0 <- data.frame(s0)
ggplot(s0,aes(x = s0))+
  geom_line(aes(y= V2,colour = 'europejski call'))+
  geom_line(aes(y=V3,colour='europejski put'))+
  geom_line(aes(y=V4,colour='amerykañski call'))+geom_line(aes(y=V5,colour='amerykañski put'))+
  scale_color_manual(name="opcja",breaks = c("europejski call","europejski put","amerykañski call","amerykañski put"),
                     values=c("darkolivegreen","#66cc66","#003366","#33ccff"))+
  ylab("cena")+
  xlab("cena spot")+ggtitle("Zale¿noœæ ceny opcji od ceny spot")+theme_bw()+theme(plot.title = element_text(hjust = 0.5))


# bardziej 'granicznie'

s0 <- seq(30,2000,10)
n <- length(s0)
s0 <- cbind(s0,rep(c(0),n), rep(c(0),n),rep(c(0),n),rep(c(0),n))
View(s0)
#analize_opt(s0,K,T,delta_t,u,d,r)

for(i in 1:n)
{
  w <- analize_opt(s0[i,1],K,T,delta_t,u,d,r)
  s0[i,2] <- w$costs[length(w$costs)]
  w <- analize_opt(s0[i,1],K,T,delta_t,u,d,r,FALSE)
  s0[i,3] <- w$costs[length(w$costs)]
  w <- analize_opt(s0[i,1],K,T,delta_t,u,d,r,TRUE,'A')
  s0[i,4] <- w$costs[length(w$costs)]
  w <- analize_opt(s0[i,1],K,T,delta_t,u,d,r,FALSE,'A')
  s0[i,5] <- w$costs[length(w$costs)]
}

s0 <- data.frame(s0)
ggplot(s0,aes(x = s0))+
  geom_line(aes(y= V2,colour = 'europejski call'))+
  geom_line(aes(y=V3,colour='europejski put'))+
  geom_line(aes(y=V4,colour='amerykañski call'))+geom_line(aes(y=V5,colour='amerykañski put'))+
  scale_color_manual(name="opcja",breaks = c("europejski call","europejski put","amerykañski call","amerykañski put"),
                     values=c("darkolivegreen","#66cc66","#003366","#33ccff"))+
  ylab("cena")+
  xlab("cena spot")+ggtitle("Zale¿noœæ ceny opcji od ceny spot")+theme_bw()+theme(plot.title = element_text(hjust = 0.5))


##################################################################################
#     SIGMA
##################################################################################

sig <- seq(0.15,0.5,0.05)
n <- length(sig)
sig <- cbind(sig, rep(c(0),n), rep(c(0),n), rep(c(0),n),rep(c(0),n))
View(sig)
#analize_opt(S0,K,T,delta_t,u,d,r)

for(i in (seq(1,n,1)))
{
  w <- analize_opt(S0,K,T,delta_t,exp(sig[i,1]*sqrt(delta_t)),exp(-sig[i,1]*sqrt(delta_t)),r)
  sig[i,2] <- w$costs[length(w$costs)]
  w <- analize_opt(S0,K,T,delta_t,exp(sig[i,1]*sqrt(delta_t)),exp(-sig[i,1]*sqrt(delta_t)),r,FALSE)
  sig[i,3] <- w$costs[length(w$costs)]
  w <- analize_opt(S0,K,T,delta_t,exp(sig[i,1]*sqrt(delta_t)),exp(-sig[i,1]*sqrt(delta_t)),r,TRUE,'A')
  sig[i,4] <- w$costs[length(w$costs)]
  w <- analize_opt(S0,K,T,delta_t,exp(sig[i,1]*sqrt(delta_t)),exp(-sig[i,1]*sqrt(delta_t)),r,FALSE,'A')
  sig[i,5] <- w$costs[length(w$costs)]
}
sig <- data.frame(sig)
library(latex2exp)
ggplot(sig,aes(x = sig))+
  geom_line(aes(y= V2,colour = 'europejski call'))+
  geom_line(aes(y=V3,colour='europejski put'))+
  geom_line(aes(y=V4,colour='amerykañski call'))+geom_line(aes(y=V5,colour='amerykañski put'))+
  scale_color_manual(name="opcja",breaks = c("europejski call","europejski put","amerykañski call","amerykañski put"),
                     values=c("darkolivegreen","#66cc66","#003366","#33ccff"))+
  ylab("cena")+
  xlab(TeX("$\\sigma$"))+ggtitle(TeX("Zaleznosc ceny opcji od $\\sigma$"))+theme_bw()+theme(plot.title = element_text(hjust = 0.5))


# 'granicznie'

sig <- seq(0,1,0.1)
n <- length(sig)
sig <- cbind(sig, rep(c(0),n), rep(c(0),n), rep(c(0),n),rep(c(0),n))
View(sig)
#analize_opt(S0,K,T,delta_t,u,d,r)

for(i in (seq(1,n,1)))
{
  w <- analize_opt(S0,K,T,delta_t,exp(sig[i,1]*sqrt(delta_t)),exp(-sig[i,1]*sqrt(delta_t)),r)
  sig[i,2] <- w$costs[length(w$costs)]
  w <- analize_opt(S0,K,T,delta_t,exp(sig[i,1]*sqrt(delta_t)),exp(-sig[i,1]*sqrt(delta_t)),r,FALSE)
  sig[i,3] <- w$costs[length(w$costs)]
  w <- analize_opt(S0,K,T,delta_t,exp(sig[i,1]*sqrt(delta_t)),exp(-sig[i,1]*sqrt(delta_t)),r,TRUE,'A')
  sig[i,4] <- w$costs[length(w$costs)]
  w <- analize_opt(S0,K,T,delta_t,exp(sig[i,1]*sqrt(delta_t)),exp(-sig[i,1]*sqrt(delta_t)),r,FALSE,'A')
  sig[i,5] <- w$costs[length(w$costs)]
}
sig <- data.frame(sig)
library(latex2exp)
ggplot(sig,aes(x = sig))+
  geom_line(aes(y= V2,colour = 'europejski call'))+
  geom_line(aes(y=V3,colour='europejski put'))+
  geom_line(aes(y=V4,colour='amerykañski call'))+geom_line(aes(y=V5,colour='amerykañski put'))+
  scale_color_manual(name="opcja",breaks = c("europejski call","europejski put","amerykañski call","amerykañski put"),
                     values=c("darkolivegreen","#66cc66","#003366","#33ccff"))+
  ylab("cena")+
  xlab(TeX("$\\sigma$"))+ggtitle(TeX("Zaleznosc ceny opcji od $\\sigma$"))+theme_bw()+theme(plot.title = element_text(hjust = 0.5))


##################################################################################
#     ZAPADALNOSC
##################################################################################
t <- 1:50
n <- length(t)
t <- cbind(t,rep(c(0),n), rep(c(0),n),rep(c(0),n),rep(c(0),n))

for(i in 1:50)
{
  w <- analize_opt(S0,K,i,delta_t,u,d,r)
  t[i,2] <- w$costs[length(w$costs)]
  w <- analize_opt(S0,K,i,delta_t,u,d,r,FALSE)
  t[i,3] <- w$costs[length(w$costs)]
  w <- analize_opt(S0,K,i,delta_t,u,d,r,TRUE,'A')
  t[i,4] <- w$costs[length(w$costs)]
  w <- analize_opt(S0,K,i,delta_t,u,d,r,FALSE,'A')
  t[i,5] <- w$costs[length(w$costs)]
}
t <- data.frame(t)

ggplot(t,aes(x = t))+
  geom_line(aes(y= V2,colour = 'europejski call'))+
  geom_line(aes(y=V3,colour='europejski put'))+
  geom_line(aes(y=V4,colour='amerykañski call'))+geom_line(aes(y=V5,colour='amerykañski put'))+
  scale_color_manual(name="opcja",breaks = c("europejski call","europejski put","amerykañski call","amerykañski put"),
                     values=c("darkolivegreen","#66cc66","#003366","#33ccff"))+
  ylab("cena")+
  xlab("zapadalnoœæ")+ggtitle("Zale¿noœæ ceny opcji od zapadalnoœci")+theme_bw()+theme(plot.title = element_text(hjust = 0.5))


t <- 1:150
n <- length(t)
t <- cbind(t,rep(c(0),n), rep(c(0),n),rep(c(0),n),rep(c(0),n))

for(i in 1:150)
{
  w <- analize_opt(S0,K,i,delta_t,u,d,r)
  t[i,2] <- w$costs[length(w$costs)]
  w <- analize_opt(S0,K,i,delta_t,u,d,r,FALSE)
  t[i,3] <- w$costs[length(w$costs)]
  w <- analize_opt(S0,K,i,delta_t,u,d,r,TRUE,'A')
  t[i,4] <- w$costs[length(w$costs)]
  w <- analize_opt(S0,K,i,delta_t,u,d,r,FALSE,'A')
  t[i,5] <- w$costs[length(w$costs)]
}
t <- data.frame(t)
t <- t[1:108,]

ggplot(t,aes(x = t))+
  geom_line(aes(y= V2,colour = 'europejski call'))+
  geom_line(aes(y=V3,colour='europejski put'))+
  geom_line(aes(y=V4,colour='amerykañski call'))+geom_line(aes(y=V5,colour='amerykañski put'))+
  scale_color_manual(name="opcja",breaks = c("europejski call","europejski put","amerykañski call","amerykañski put"),
                     values=c("darkolivegreen","#66cc66","#003366","#33ccff"))+
  ylab("cena")+
  xlab("zapadalnoœæ")+ggtitle("Zale¿noœæ ceny opcji od zapadalnoœci")+theme_bw()+theme(plot.title = element_text(hjust = 0.5))

##################################################################################
#     STOPA PROCENTOWA R
##################################################################################
# 'granicznie'
er <- seq(0,1,0.02)
n <- length(er)
er <- cbind(er, rep(c(0),n), rep(c(0),n), rep(c(0),n),rep(c(0),n))
View(er)
#analize_opt(S0,K,T,delta_t,u,d,r)

for(i in (seq(1,n,1)))
{
  w <- analize_opt(S0,K,T,delta_t,u,d,er[i])
  er[i,2] <- w$costs[length(w$costs)]
  w <- analize_opt(S0,K,T,delta_t,u,d,er[i],FALSE)
  er[i,3] <- w$costs[length(w$costs)]
  w <- analize_opt(S0,K,T,delta_t,u,d,er[i],TRUE,'A')
  er[i,4] <- w$costs[length(w$costs)]
  w <- analize_opt(S0,K,T,delta_t,u,d,er[i],FALSE,'A')
  er[i,5] <- w$costs[length(w$costs)]
}
er <- data.frame(er)

ggplot(er,aes(x = er))+
  geom_line(aes(y= V2,colour = 'europejski call'))+
  geom_line(aes(y=V3,colour='europejski put'))+
  geom_line(aes(y=V4,colour='amerykañski call'))+geom_line(aes(y=V5,colour='amerykañski put'))+
  scale_color_manual(name="opcja",breaks = c("europejski call","europejski put","amerykañski call","amerykañski put"),
                     values=c("darkolivegreen","#66cc66","#003366","#33ccff"))+
  ylab("cena")+
  xlab("r")+ggtitle("Zale¿noœæ ceny opcji od stopy procentowej")+theme_bw()+theme(plot.title = element_text(hjust = 0.5))


#nasz scenariusz


er <- seq(0,0.05,0.001)
n <- length(er)
er <- cbind(er, rep(c(0),n), rep(c(0),n), rep(c(0),n),rep(c(0),n))
View(er)
#analize_opt(S0,K,T,delta_t,u,d,r)

for(i in (seq(1,n,1)))
{
  w <- analize_opt(S0,K,T,delta_t,u,d,er[i])
  er[i,2] <- w$costs[length(w$costs)]
  w <- analize_opt(S0,K,T,delta_t,u,d,er[i],FALSE)
  er[i,3] <- w$costs[length(w$costs)]
  w <- analize_opt(S0,K,T,delta_t,u,d,er[i],TRUE,'A')
  er[i,4] <- w$costs[length(w$costs)]
  w <- analize_opt(S0,K,T,delta_t,u,d,er[i],FALSE,'A')
  er[i,5] <- w$costs[length(w$costs)]
}
er <- data.frame(er)

ggplot(er,aes(x = er))+
  geom_line(aes(y= V2,colour = 'europejski call'))+
  geom_line(aes(y=V3,colour='europejski put'))+
  geom_line(aes(y=V4,colour='amerykañski call'))+geom_line(aes(y=V5,colour='amerykañski put'))+
  scale_color_manual(name="opcja",breaks = c("europejski call","europejski put","amerykañski call","amerykañski put"),
                     values=c("darkolivegreen","#66cc66","#003366","#33ccff"))+
  ylab("cena")+
  xlab("r")+ggtitle("Zale¿noœæ ceny opcji od stopy procentowej")+theme_bw()+theme(plot.title = element_text(hjust = 0.5))

##################################################################################
# DELTA T (LICZBA KROKÓW)
##################################################################################
Delta_t<-c(T/(1:150))
put_e_price<-c()
put_a_price<-c()
call_e_price<-c()
call_a_price<-c()
for ( i in Delta_t){
  u=exp(sigma*sqrt(i))
  d=exp(-sigma*sqrt(i))
  a<-dim(analize_opt(S0, K, T,i,u,d, r, call_opt = F, opt_type = "E"))[1]
  put_e_price[which(Delta_t==i)]<-analize_opt(S0, K, T,i,u,d,r, call_opt = FALSE, opt_type = "E")[a,2]
  put_a_price[which(Delta_t==i)]<-analize_opt(S0, K, T,i,u,d,r, call_opt = FALSE, opt_type = "A")[a,2]
  call_e_price[which(Delta_t==i)]<-analize_opt(S0, K, T,i,u,d,r, call_opt = TRUE, opt_type = "E")[a,2]
  call_a_price[which(Delta_t==i)]<-analize_opt(S0, K, T,i,u,d,r, call_opt = TRUE, opt_type = "A")[a,2]
}
library(ggplot2)
pricing_time2<-data.frame(liczba_kroków=rep(c(1:150),4),typ=rep(c("europejski put","amerykański put","europejski call","amerykański call"),each=150),cena=c(put_e_price,put_a_price,call_e_price,call_a_price))
ggplot(pricing_time2,aes(x=liczba_kroków,y=cena,color=typ))+geom_line(size=1)+theme_bw()+ggtitle("Wrażliwość ceny opcji na liczbę kroków")+theme(plot.title=element_text(hjust=0.5))+xlab("liczba kroków")+scale_color_manual(name="typ",breaks = c("europejski call","europejski put","amerykański call","amerykański put"),values=c("darkolivegreen","#66cc66","#003366","#33ccff"))


##################################################################################
#     PORTFEL ZABEZPIECZAJACY
##################################################################################

##################################################################################
#     EUROPEJSKI PUT
##################################################################################

euro_call <- analize_opt(S0, K, T, delta_t, u, d, r, call_opt = FALSE, opt_type = "E")
euro_call2 <- analize_opt(S0, K, T, delta_t, u, d, r, call_opt = FALSE, opt_type = "E")
names(euro_call)[5] <- "akcje"
names(euro_call2)[6] <- "gotówka"
ggplot() +
geom_point(data=euro_call, aes(x=time, y=price, color=akcje), shape=17, size=4) +
coord_trans(y = 'log2') +
scale_fill_gradient(low="#cc99cc", high="#990099") +
geom_point(data= euro_call2, aes(x=time, y=price, fill=gotówka), shape=25, size=2) +
scale_fill_gradient(low="gray90", high="blue")+xlab("czas")+ylab("St")+ggtitle("Sk³ad portfela zabezpieczaj¹cego - europejski put")+
  theme_bw()


# gotówka

ggplot() +
  geom_point(data=euro_call, aes(x=time, y=price, color=akcje), shape=17, size=4) +
  coord_trans(y = 'log2')+ylab("St")+xlab("czas")+ggtitle("Portfel zabezpieczaj¹cy-akcja")+theme_bw()

ggplot() +
  geom_point(data=euro_call2, aes(x=time, y=price, color=gotówka), shape=17, size=4) +
  coord_trans(y = 'log2')+ylab("St")+xlab("czas")+ggtitle("Portfel zabezpieczaj¹cy-gotówka")+theme_bw()


##################################################################################
#     EUROPEJSKI CALL
##################################################################################
euro_call <- analize_opt(S0, K, T, delta_t, u, d, r, call_opt = TRUE, opt_type = "E")
euro_call2 <- analize_opt(S0, K, T, delta_t, u, d, r, call_opt = TRUE, opt_type = "E")
names(euro_call)[5] <- "akcje"
names(euro_call2)[6] <- "gotówka"
ggplot() +
  geom_point(data=euro_call, aes(x=time, y=price, color=akcje), shape=17, size=4) +
  coord_trans(y = 'log2') +
  scale_fill_gradient(low="#cc99cc", high="#990099") +
  geom_point(data= euro_call2, aes(x=time, y=price, fill=gotówka), shape=25, size=2) +
  scale_fill_gradient(low="gray90", high="blue")+xlab("czas")+ylab("St")+ggtitle("Sk³ad portfela zabezpieczaj¹cego - europejski call")+
  theme_bw()

ggplot() +
  geom_point(data=euro_call, aes(x=time, y=price, color=akcje), shape=17, size=4) +
  coord_trans(y = 'log2')+ylab("St")+xlab("czas")+ggtitle("Portfel zabezpieczaj¹cy-akcja")+theme_bw()

ggplot() +
  geom_point(data=euro_call2, aes(x=time, y=price, color=gotówka), shape=17, size=4) +
  coord_trans(y = 'log2')+ylab("St")+xlab("czas")+ggtitle("Portfel zabezpieczaj¹cy-gotówka")+theme_bw()


##################################################################################
#     AMERYKANSKI CALL
##################################################################################

euro_call <- analize_opt(S0, K, T, delta_t, u, d, r, call_opt = TRUE, opt_type = "A")
euro_call2 <- analize_opt(S0, K, T, delta_t, u, d, r, call_opt = TRUE, opt_type = "A")
names(euro_call)[5] <- "akcje"
names(euro_call2)[6] <- "gotówka"
ggplot() +
  geom_point(data=euro_call, aes(x=time, y=price, color=akcje), shape=17, size=4) +
  coord_trans(y = 'log2') +
  scale_fill_gradient(low="#cc99cc", high="#990099") +
  geom_point(data= euro_call2, aes(x=time, y=price, fill=gotówka), shape=25, size=2) +
  scale_fill_gradient(low="gray90", high="blue")+xlab("czas")+ylab("St")+ggtitle("Sk³ad portfela zabezpieczaj¹cego - amerykañski call")+
  theme_bw()

ggplot() +
  geom_point(data=euro_call, aes(x=time, y=price, color=akcje), shape=17, size=4) +
  coord_trans(y = 'log2')+ylab("St")+xlab("czas")+ggtitle("Portfel zabezpieczaj¹cy-akcja")+theme_bw()

ggplot() +
  geom_point(data=euro_call2, aes(x=time, y=price, color=gotówka), shape=17, size=4) +
  coord_trans(y = 'log2')+ylab("St")+xlab("czas")+ggtitle("Portfel zabezpieczaj¹cy-gotówka")+theme_bw()




##################################################################################
#     AMERYKANSKI PUT
##################################################################################


euro_call <- analize_opt(S0, K, T, delta_t, u, d, r, call_opt = FALSE, opt_type = "A")
euro_call2 <- analize_opt(S0, K, T, delta_t, u, d, r, call_opt = FALSE, opt_type = "A")
names(euro_call)[5] <- "akcje"
names(euro_call2)[6] <- "gotówka"
ggplot() +
  geom_point(data=euro_call, aes(x=time, y=price, color=akcje), shape=17, size=4) +
  coord_trans(y = 'log2') +
  scale_fill_gradient(low="#cc99cc", high="#990099") +
  geom_point(data= euro_call2, aes(x=time, y=price, fill=gotówka), shape=25, size=2) +
  scale_fill_gradient(low="gray90", high="blue")+xlab("czas")+ylab("St")+ggtitle("Sk³ad portfela zabezpieczaj¹cego - amerykañski put")+
  theme_bw()

ggplot() +
  geom_point(data=euro_call, aes(x=time, y=price, color=akcje), shape=17, size=4) +
  coord_trans(y = 'log2')+ylab("St")+xlab("czas")+ggtitle("Portfel zabezpieczaj¹cy-akcja")+theme_bw()

ggplot() +
  geom_point(data=euro_call2, aes(x=time, y=price, color=gotówka), shape=17, size=4) +
  coord_trans(y = 'log2')+ylab("St")+xlab("czas")+ggtitle("Portfel zabezpieczaj¹cy-gotówka")+theme_bw()

