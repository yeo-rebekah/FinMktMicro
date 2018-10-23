setwd("d:/NUS/HighFreqData")
hf <- read.csv(file="d:/NUS/HighFreqData/HighFrequency.csv", header=TRUE, sep=",")
library(dplyr)
library(ggplot2)

roll <- function(inp){
  a <- acf(as.matrix(inp),plot=FALSE,lag=1,type="covariance")
  r<-0
  scov <- a$acf[2]
  print(scov)
  if (scov<0){
    return(2*sqrt(-scov))
  }else{
    return(0)
  }
}


#summary statistics
summary(hf)

#TODO: Share Volume
#share volume taken to be Q.trade
#correlation of share volume and share price --> positively correlated means it moves together
#number of shares traded hf[,3]
#price of shares traded hf[,2]

cor_sharevol_px <- lm(hf[,2]~hf[,3], data=hf)
print(cor_sharevol_px)
summary(cor_sharevol_px)
scatter.smooth(x=hf$Q.Trade., y=hf$P.Trade., main="Share volume ~ Share price")

total_shares_traded <- sum(hf[,3])
print(total_shares_traded)

#TODO: Dollar volume
hf<-dplyr::mutate(hf,dolvol=hf[,2]*hf[,3])
dol_vol<-0
len_hf <-nrow(hf)
for (i in 1:len_hf){
  dol_vol<- dol_vol+hf[i,2]*hf[i,3]
}
print(dol_vol)

#TODO: Turnover
turnover <- total_shares_traded/6000000
print(turnover)

#TODO: Quoted Spread 
#primary buy (ask) price hf[,4] --> you buy at the ask price that someone needs to receive in order to let go of the stock
#primary sell (bid) price hf[,9] --> you sell at the bid price that someone is willing to pay for the stock
hf <-dplyr::mutate(hf,quo_spr=hf[,4]-hf[,9])
av_quo_spr <- 0
for (i in 1:len_hf){
  a=hf[i,4]-hf[i,9]
  if (av_quo_spr>0){
    av_quo_spr<- mean(av_quo_spr, a)
  }else {
    av_quo_spr<-a
  }
}
print(av_quo_spr)

#TODO: Relative Spread
hf <-dplyr::mutate(hf,rel_spr=(hf[,4]-hf[,9])/hf[,2])

av_rel_spr <- 0
for (i in 1:len_hf){
  a=(hf[i,4]-hf[i,9])/hf[i,2]
  if (av_rel_spr>0){
    av_rel_spr<- mean(av_rel_spr, a)
  }else {
    av_rel_spr<-a
  }
}
print(av_rel_spr)

#TODO: percentage spread
hf <- dplyr::mutate(hf,midquo=(hf[,4]+hf[,9])/2)
hf <- dplyr::mutate(hf,per_spr=quo_spr/midquo)

wide_hf <- ncol(hf)
av_per_spr <- mean(hf[,28])
print(av_per_spr)

#TODO: effective spread
hf <- dplyr::mutate(hf,eff_spr=2*abs(hf[,2]-midquo))
av_eff_spr <- mean(hf[,29])
print(av_eff_spr)

#TODO: roll effective spread
roll_spr <- roll(hf[,2])
print(roll_spr)


#TODO: depth at ask
#columns 19-23 --> sell quantities
#columns 14-18 --> buy quantities
hf<- dplyr::mutate(hf,depth_ask=hf[,19]+hf[,20]+hf[,21]+hf[,22]+hf[,23])
av_ask_depth <- mean(hf[,30])
print(av_ask_depth)

#TODO:depth at bid
hf<- dplyr::mutate(hf,depth_bid=hf[,14]+hf[,15]+hf[,16]+hf[,17]+hf[,18])
av_bid_depth <- mean(hf[,31])
print(av_bid_depth)

#TODO: quality index
hf <- dplyr::mutate(hf,qi=((depth_bid+depth_ask)/2)/per_spr)
av_qi<- mean(hf[,32])
print(av_qi)

#TODO: short-term volatility
#find mean r with simple return
ret <- 0
for (i in 2:len_hf){
  a=i-1
  ret=ret+(hf[i,2]/hf[a,2]-1)
}

mean_ret=ret/(len_hf-1)

volat<-0
for (i in 2:len_hf){
  a=i-1
  volat=volat+(hf[i,2]/hf[a,2]-1)^2
  #print(sum_ret_diff)
}
print(volat)

summary(hf)
#write.csv(hf,"hf_output.csv")
