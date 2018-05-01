setwd("~/Stat480/group-project")

# Using big matirx for analysis
library(biganalytics)
x <- attach.big.matrix("air0205.desc")

# data
x_02 <- x[which(x[ , "year"] ==2002), ]
x_05 <- x[which(x[ , "year"] ==2005), ]
x_cancel <- x[which(x[ , "cancelled"] ==1), ]


# by dayofweek - all data
dayind_02 = split(1:nrow(x_02), x_02[,"dayofweek"])
day_cancel_02 <- foreach(inds = dayind_02, .combine = cbind) %do% {
  mean(x_02[inds, "cancelled"], na.rm=TRUE)
}
day_cancel_02 = t(day_cancel_02)

dayind_05 = split(1:nrow(x_05), x_05[,"dayofweek"])
day_cancel_05 <- foreach(inds = dayind_05, .combine = cbind) %do% {
  mean(x_05[inds, "cancelled"], na.rm=TRUE)
}
day_cancel_05 = t(day_cancel_05)

library(ggplot2)
day = as.data.frame(cbind(day_cancel_02, day_cancel_05, days = c(1:7)))
ggplot(data=day) +
  geom_line(aes(x = days, y = V1, colour = "2002")) + 
  geom_line(aes(x = days, y = V2, colour = "2005")) +
  ylab("Cancellation Rate") +
  xlab("Day of Week") +
  ggtitle("Flight Cancellation Rate by Day of Week") +
  scale_x_continuous(breaks=seq(1, 7, 1), labels = c("Mon","Tue","Wed","Thu","Fri","Sat","Sun"))
  

# by month - all data
monthind_02 = split(1:nrow(x_02), x_02[,"month"])
month_cancel_02 <- foreach(inds = monthind_02, .combine = cbind) %do% {
  mean(x_02[inds, "cancelled"], na.rm=TRUE)
}
month_cancel_02 = t(month_cancel_02)

monthind_05 = split(1:nrow(x_05), x_05[,"month"])
month_cancel_05 <- foreach(inds = monthind_05, .combine = cbind) %do% {
  mean(x_05[inds, "cancelled"], na.rm=TRUE)
}
month_cancel_05 = t(month_cancel_05)

month = as.data.frame(cbind(month_cancel_02, month_cancel_05, Month = c(1:12)))
ggplot(data=month) +
  geom_line(aes(x = Month, y = V1, colour = "2002")) + 
  geom_line(aes(x = Month, y = V2, colour = "2005")) +
  ylab("Cancellation Rate") +
  xlab("Month")+
  ggtitle("Flight Cancellation Rate by Month") +
  scale_x_continuous(breaks=seq(1, 12, 1), labels = c("Jan","Feb","Mar","Apr","May","Jun","Jul","Aug","Sep","Oct","Nov","Dec"))


# by origin airport
airportind_02 = split(1:nrow(x_02), x_02[,"origin"])
airname_02 = names(airportind_02)
airport_cancel_02 <- foreach(inds = airportind_02, .combine = cbind) %do% {
  mean(x_02[inds, "cancelled"], na.rm=TRUE)
}
ac_02 = t(as.data.frame(airport_cancel_02))
ac_02 = cbind(ac_02,as.data.frame(airname_02))
ac_top5_02 = ac_02[order(-ac_02[,1], ac_02[,2]), ][1:5,]

airportind_05 = split(1:nrow(x_05), x_05[,"origin"])
airname_05 = names(airportind_05)
airport_cancel_05 <- foreach(inds = airportind_05, .combine = cbind) %do% {
  mean(x_05[inds, "cancelled"], na.rm=TRUE)
}
ac_05 = t(as.data.frame(airport_cancel_05))
ac_05 = cbind(ac_05,as.data.frame(airname_05))
ac_top5_05 = ac_05[order(-ac_05[,1], ac_05[,2]), ][1:5,]

ac = rbind(ac_top5_02$ac_02,ac_top5_05$ac_05)
colnames(ac) = c("AGS/PVU","DUT/ACK","GST/CSG","ORH/VIS","BIL/FLO")
barplot(ac,col=c("darkblue","lightblue"), beside=TRUE,
        xlab = "Airport",
        ylab = "Cancellation Rate",
        legend = c("2002", "2005"),
        main = "Top 5 Airport with Highest Flight Cancellation Rate")


# by origin state
stateind_02 = split(1:nrow(x_02), x_02[,"originstate"])
statename_02 = names(stateind_02)
state_cancel_02 <- foreach(inds = stateind_02, .combine = cbind) %do% {
  mean(x_02[inds, "cancelled"], na.rm=TRUE)
}
sc_02 = t(as.data.frame(state_cancel_02))
sc_02 = cbind(sc_02,as.data.frame(statename_02))
sc_top5_02 = sc_02[order(-sc_02[,1], sc_02[,2]), ][1:5,]

stateind_05 = split(1:nrow(x_05), x_05[,"originstate"])
statename_05 = names(stateind_05)
state_cancel_05 <- foreach(inds = stateind_05, .combine = cbind) %do% {
  mean(x_05[inds, "cancelled"], na.rm=TRUE)
}
sc_05 = t(as.data.frame(state_cancel_05))
sc_05 = cbind(sc_05,as.data.frame(statename_05))
sc_top5_05 = sc_05[order(-sc_05[,1], sc_05[,2]), ][1:5,]

sc = rbind(sc_top5_02$sc_02,sc_top5_05$sc_05)
colnames(sc) = c("AK/MS","ME/ME","WI/LA","IA/MA","MA/GA")
barplot(sc,col=c("darkblue","lightblue"), beside=TRUE,
        xlab = "State",
        ylab = "Cancellation Rate",
        legend = c("2002", "2005"),
        main = "Top 5 State with Highest Flight Cancellation Rate")


# by carrier - uniquecarrier
carrind_02 = split(1:nrow(x_02), x_02[,"uniquecarrier"])
carrname_02 = names(carrind_02)
carr_cancel_02 <- foreach(inds = carrind_02, .combine = cbind) %do% {
  mean(x_02[inds, "cancelled"], na.rm=TRUE)
}
cc_02 = t(as.data.frame(carr_cancel_02))
cc_02 = cbind(cc_02,as.data.frame(carrname_02))
cc_top5_02 = cc_02[order(-cc_02[,1], cc_02[,2]), ][1:5,]

carrind_05 = split(1:nrow(x_05), x_05[,"uniquecarrier"])
carrname_05 = names(carrind_05)
carr_cancel_05 <- foreach(inds = carrind_05, .combine = cbind) %do% {
  mean(x_05[inds, "cancelled"], na.rm=TRUE)
}
cc_05 = t(as.data.frame(carr_cancel_05))
cc_05 = cbind(cc_05,as.data.frame(carrname_05))
cc_top5_05 = cc_05[order(-cc_05[,1], cc_05[,2]), ][1:5,]

cc = rbind(cc_top5_02$cc_02,cc_top5_05$cc_05)
colnames(cc) = c("MQ/EV","AS/MQ","NW/OH","AA/DL","DL/XE")
barplot(cc,col=c("darkblue","lightblue"), beside=TRUE,
        xlab = "Carrier",
        ylab = "Cancellation Rate",
        legend = c("2002", "2005"),
        main = "Top 5 Carrier with Highest Flight Cancellation Rate")

# by cancellation code
x_cancel <- x[which(x[ , "cancelled"] ==1), ]
x_cancel_02 <- x_cancel[which(x_cancel[ , "year"] ==2002), ]
x_cancel_05 <- x_cancel[which(x_cancel[ , "year"] ==2005), ]

cancdind_05 = split(1:nrow(x_cancel_05), x_cancel_05[,"cancellationcode"])
cd_cancel_05 <- foreach(inds = cancdind_05, .combine = cbind) %do% {
  sum(x_cancel_05[inds, "cancelled"], na.rm = TRUE)/dim(x_cancel_05)[1]
}
cd_cancel_05 = t(cd_cancel_05)

cd = as.data.frame(cbind(cd_cancel_05,cd= c(1:4)))
barplot(cd$V1,col="lightblue",
        xlab = "State",
        ylab = "Cancellation Rate",
        main = "Cancellation Rate vs. Reasons in 2005",
        names.arg=c("Carrier","Weather","National Air System","Security"))

# logistic regression
lg_02<- bigglm.big.matrix( cancelled ~ dayofweek + month + origin + originstate, data = x_02, family = binomial())
summary(lg_02)
summary(lg_02)$rsq

lg_05<- bigglm.big.matrix( cancelled ~ dayofweek + month + origin + originstate, data = x_05, family = binomial())
summary(lg_05)
summary(lg_05)$rsq
