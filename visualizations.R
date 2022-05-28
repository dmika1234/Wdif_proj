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
pricing_time1 # tabelka do wykresu 
ggplot(pricing_time1,aes(x=steps,y=price,color=type,alpha=0.3))+geom_line(size=1,position=position_jitter(w=0.05, h=0.04))+theme_bw()+ggtitle("Wrażliwość ceny opcji na liczbę kroków")

