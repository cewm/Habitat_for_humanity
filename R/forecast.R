rm(list=ls())
setwd('C:/Users/white/Documents/Practicum/R/ZIP')

####load data
data=read.csv("30363.csv",header=T)
head(data)
#create time series
temp = ts(data, start = 2007)
#plot time series
ts.plot(temp, ylab = 'Total Assessment', main = '30363')
#####plot acf and pacf
acf(temp)
pacf(temp)

#Order selection -- AIC
n = length(temp)
norder = 2
p = c(1:norder)-1; q = c(1:norder)-1
aic = matrix(0,norder,norder)
for(i in 1:norder){
  for(j in 1:norder){
    modij = arima(temp, order = c(p[i],1,q[j]), method='ML')
    aic[i,j] = modij$aic-2*(p[i]+q[j]+1)+2*(p[i]+q[j]+1)*n/(n-p[i]-q[j]-2)
  }  
}


aicv = as.vector(aic)  
plot(aicv,ylab="AIC values")
indexp = rep(c(1:norder),norder)
indexq = rep(c(1:norder),each=norder)
indexaic = which(aicv == min(aicv))
porder = indexp[indexaic]-1
qorder = indexq[indexaic]-1
porder
qorder



#####model creation

final_model = arima(temp, order = c(1,1,1), method = "ML")


## GOF: residual analysis
plot(resid(final_model), ylab='Residuals',type='o',main="Residual Plot")
abline(h=0)
acf(resid(final_model),main="ACF: Residuals")
pacf(resid(final_model),main="PACF: Residuals")
hist(resid(final_model),xlab='Residuals',main='Histogram: Residuals')
qqnorm(resid(final_model),ylab="Sample Q",xlab="Theoretical Q")
qqline(resid(final_model))

Box.test(final_model$resid, lag = (porder+qorder+1), type = "Box-Pierce", fitdf = (porder+qorder))
Box.test(final_model$resid, lag = (porder+qorder+1), type = "Ljung-Box", fitdf = (porder+qorder))

summary(final_model)

#####forecasting
####6 quarters ahead
n = length(temp)
nfit = n
outsales = arima(temp[1:nfit], order = c(1,1,1), method = 'ML')
outpred = predict(outsales, n.ahead = 5)

temp
outpred$pred[1:5]

hola <- c(as.vector(temp),  as.vector(outpred$pred[1:5]))
hola

sink("output.txt")
print(out)
sink()


#accuracy measures
obssales= temp[(nfit+1):n]
predsales = (outpred$pred)^2 ### Mean Squared Prediction Error (MSPE)
mean((predsales-obssales)^2) ### Mean Absolute Prediction Error (MAE)
mean(abs(predsales-obssales)) ### Mean Absolute Percentage Error (MAPE)
mean(abs(predsales-obssales)/obssales) ### Precision Measure (PM)
sum((predsales-obssales)^2)/sum((obssales-mean(obssales))^2)

sink("output.txt")
print(out)
sink()




