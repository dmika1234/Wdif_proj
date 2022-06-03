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
  geom_hline(yintercept = K, color = "blue") +
  geom_text(aes(0.03, K,label = paste("K=", K), vjust = -1.5), color = "blue") +
  coord_trans(y = "log2") +
  labs(title = "Opcja europejska call", color = "Czy wykonano", x = "Czas", y="Cena aktywa") +
  scale_color_manual(labels=c("Nie", "Tak"), values=c("orange", "cyan")) +
  theme(plot.title = element_text(hjust = 0.5))

# Basic plot e-p
resep <- analize_opt(S0 = S0, K = K, T = 2, delta_t = delta_t,
                     u = u, d = d, r = r, opt_type = "E", call_opt = FALSE)
ggplot(resep) +
  theme_bw() +
  geom_point(aes(x = time, y = price, color = if_executed)) +
  geom_hline(yintercept = K, color = "blue") +
  geom_text(aes(0.03, K,label = paste("K=", K), vjust = -1.5), color = "blue") +
  coord_trans(y = "log2") +
  labs(title = "Opcja europejska put", color = "Czy wykonano", x = "Czas", y="Cena aktywa") +
  scale_color_manual(labels=c("Nie", "Tak"), values=c("orange", "cyan")) +
  theme(plot.title = element_text(hjust = 0.5))

# Basic plot a-p
resap <- analize_opt(S0 = S0, K = K, T = 2, delta_t = delta_t,
                    u = u, d = d, r = r, opt_type = "A", call_opt = FALSE)
ggplot(resap) +
  theme_bw() +
  geom_point(aes(x = time, y = price, color = if_executed)) +
  geom_hline(yintercept = K, color = "blue") +
  geom_text(aes(0.03, K,label = paste("K=", K), vjust = -1.5), color = "blue") +
  coord_trans(y = "log2") +
  labs(title = "Opcja amerykańska put", color = "Czy wykonano", x = "Czas", y="Cena aktywa") +
  scale_color_manual(labels=c("Nie", "Tak"), values=c("orange", "cyan")) +
  theme(plot.title = element_text(hjust = 0.5))

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
  geom_line(data = border_pointst, aes(x = time, y = maks, linetype = 'Prawdziwy'), color = 'green', size = 1) +
  geom_line(data = border_pointst, aes(x = time, y = predicted_maks, linetype = 'Przewidziany'), color = 'red') +
  coord_trans(y = "log2") +
  labs(color = "Czy wykonano", x = "Czas", y = "Cena aktywa", linetype = "Podział\nmomentów\nwykonania") +
  scale_color_manual(labels=c("Nie", "Tak"), values=c("orange", "cyan")) +
  theme(plot.title = element_text(hjust = 0.5))
#===========================
#Wrażliwosć ceny na delta_t
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
pricing_time2<-data.frame(liczba_kroków=rep(c(1:150),4),typ=rep(c("europejska put","amerykańska put","europejska call","amerykańska call"),each=150),cena=c(put_e_price,put_a_price,call_e_price,call_a_price))
pricing_time2 #tabelka do wykresu
ggplot(pricing_time2,aes(x=liczba_kroków,y=cena,color=typ,alpha=0.3))+geom_line(size=1)+theme_bw()+ggtitle("Wrażliwość ceny opcji na liczbę kroków")+theme(plot.title=element_text(hjust=0.5))
# analiza wrazliwosci 
ggplot(strike,aes(x = strike))+
  geom_line(aes(y= V2,colour = 'european call'))+
  geom_line(aes(y=V3,colour='european put'))+
  geom_line(aes(y=V4,colour='asian call'))+geom_line(aes(y=V5,colour='asian put'))+
  scale_color_manual(name="option",breaks = c("european call","european put","asian call","asian put"),
                     values=c("green","orange","yellow","blue"))+
  ylab("cost")


ggplot(s0,aes(x = s0))+
  geom_line(aes(y= V2,colour = 'european call'))+
  geom_line(aes(y=V3,colour='european put'))+
  geom_line(aes(y=V4,colour='asian call'))+geom_line(aes(y=V5,colour='asian put'))+
  scale_color_manual(name="option",breaks = c("european call","european put","asian call","asian put"),
                     values=c("green","orange","yellow","blue"))+
  ylab("cost")+
  xlab("S0")


ggplot(sig,aes(x = sig))+
  geom_line(aes(y= V2,colour = 'european call'))+
  geom_line(aes(y=V3,colour='european put'))+
  geom_line(aes(y=V4,colour='asian call'))+geom_line(aes(y=V5,colour='asian put'))+
  scale_color_manual(name="option",breaks = c("european call","european put","asian call","asian put"),
                     values=c("green","orange","yellow","blue"))+
  ylab("cost")+
  xlab("sigma")


ggplot(t,aes(x = t))+
  geom_line(aes(y= V2,colour = 'european call'))+
  geom_line(aes(y=V3,colour='european put'))+
  geom_line(aes(y=V4,colour='asian call'))+geom_line(aes(y=V5,colour='asian put'))+
  scale_color_manual(name="option",breaks = c("european call","european put","asian call","asian put"),
                     values=c("green","orange","yellow","blue"))+
  ylab("cost")+
  xlab("zapadalnosc")


ggplot(er,aes(x = er))+
  geom_line(aes(y= V2,colour = 'european call'))+
  geom_line(aes(y=V3,colour='european put'))+
  geom_line(aes(y=V4,colour='asian call'))+geom_line(aes(y=V5,colour='asian put'))+
  scale_color_manual(name="option",breaks = c("european call","european put","asian call","asian put"),
                     values=c("green","orange","yellow","blue"))+
  ylab("cost")+
  xlab("r")
