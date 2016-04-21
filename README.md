# EZ-Rxample
# Easy R example for demonstration

# Creates Box-plots comparing confidence interval ranges for full vs. reduced models
# Run in R

x0<-rep(1,22)
weight<-c(1.95,2.9,.72,.81,1.09,1.22,1.02,1.93,.64,2.08,1.98,1.9,8.56,4.49,8.49,6.17,7.54,6.36,7.63,7.78,10.15,6.88)
beakLength<-c(1.31,1.55,.99,.99,1.05,1.09,1.08,1.27,.99,1.34,1.3,1.33,1.86,1.58,1.97,1.8,1.75,1.72,1.68,1.75,2.19,1.73)
wingLength<-c(1.07,1.49,.84,.83,.9,.93,.9,1.08,.85,1.13,1.1,1.1,1.47,1.34,1.59,1.56,1.58,1.43,1.57,1.59,1.86,1.67)
beakToNotch<-c(.44,.53,.34,.34,.36,.42,.4,.44,.36,.45,.45,.48,.6,.52,.67,.66,.63,.64,.72,.68,.75,.64)
notchToWing<-c(.75,.9,.57,.54,.64,.61,.51,.77,.56,.77,.76,.77,1.01,.95,1.2,1.02,1.09,1.02,.96,1.08,1.24,1.14)
width<-c(.35,.47,.32,.27,.3,.31,.31,.34,.29,.37,.38,.38,.65,.5,.59,.59,.59,.63,.68,.62,.72,.55)

squid<-cbind(weight, x0,beakLength,wingLength,beakToNotch,notchToWing,width)
FullX<-squid[,2:7]
RedX<-squid[, c(2,5:7)]
y<-squid[,1]
# View(squid)
R_g1_g2<-t(y)%*%(FullX%*%solve(t(FullX)%*%FullX)%*%t(FullX) - RedX%*%solve(t(RedX)%*%RedX)%*%t(RedX))%*%y
SSE<- t(y)%*%(diag(22) - FullX%*%solve(t(FullX)%*%FullX)%*%t(FullX))%*%y
Fstat<-(R_g1_g2/2)/(SSE/16)
# Fstat
# [1,] 1.047543
# 1-pf(Fstat, df1=2, df2=16)
# [1,] 0.3736583

m1<-lm(y~FullX)
m2<-lm(y~RedX)
# anova(m1,m2)

CIfull<-predict(m1,interval="confidence")
CIred<-predict(m2,interval="confidence")
squid<-cbind(squid,CIfull,CIred)
colnames(squid)[8:13] <- c("Full_yHat", "Full_yHat_lower", "Full_yHat_upper","Red_yHat","Red_yHat_lower","Red_yHat_upper")
FullCI<-(squid[,10]-squid[,9])
RedCI<-(squid[,13]-squid[,12])
par(mfrow=c(1,2))
boxplot(FullCI, ylim=c(.5,2.5), main="95% Confidence interval ranges \nof y-hat in full model", col="pink")
boxplot(RedCI, ylim=c(.5,2.5), main="95% Confidence interval ranges \nof y-hat in reduced model")
par(mfrow=c(1,1))
