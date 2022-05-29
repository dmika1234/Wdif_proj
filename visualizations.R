source('funs.R')
library(ggplot2)
library(MASS)
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




# ============= Analiza momentów wykonania
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
rest <- analize_opt(S0 = S0, K = K, T = t, delta_t = delta_t,
                    u = u, d = d, r = r, opt_type = "A", call_opt = FALSE)
border_pointst <- rest %>% 
  filter(if_executed == TRUE) %>% 
  group_by(time) %>% 
  summarise(maks = max(price))
border_pointst <- border_pointst %>% 
  mutate(T = t, predicted_maks = predict(model_barrier, newdata = data.frame(time = border_pointst$time, T = t)))


# Basic plot
ggplot() +
  theme_bw() +
  geom_point(data = rest, aes(x = time, y = price, color = if_executed)) +
  geom_hline(yintercept = K) +
  geom_line(data = border_pointst, aes(x = time, y = maks), color = 'green', size = 1) + 
  coord_trans(y = "log2")

# PLot with predicted
ggplot() +
  theme_bw() +
  geom_point(data = rest, aes(x = time, y = price, color = if_executed)) +
  geom_hline(yintercept = K) +
  geom_line(data = border_pointst, aes(x = time, y = maks), color = 'green', size = 1) +
  geom_line(data = border_pointst, aes(x = time, y = predicted_maks), color = 'red', linetype = 'dashed') +
  coord_trans(y = "log2")
#===========================


#Wrażliwosć ceny na delta_t
Delta_t<-c(T/(1:30)) 
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

pricing_time1<-data.frame(steps=rep(c(1:30),4),type=rep(c("european put","american put","european call","american call"),each=30),price=c(put_e_price,put_a_price,call_e_price,call_a_price))
write.csv(pricing_time1,"deltat.csv", row.names = FALSE)
pricing_time1 # tabelka do wykresu 
ggplot(pricing_time1,aes(x=steps,y=price,color=type,alpha=0.3))+geom_line(size=1,position=position_jitter(w=0.05, h=0.04))+theme_bw()+ggtitle("Wrażliwość ceny opcji na liczbę kroków")

