data <- read.csv(file = 'CommViolPredUnnormalizedData.csv', stringsAsFactors=FALSE)

data$Community.name <- NULL
data$state <- NULL

for(c in colnames(data)){
  col <- data[[c]]
  
  for(i in 1:length(col)) {
    col[i] <- as.numeric(as.character(col[i]))
  }
  
  col_d <- as.double(as.character(col))
  col_withot_na <- na.omit(col_d)
  m <- mean(col_withot_na)
  
  for(i in 1:length(col)) {
    if(is.na(col[i])){
      col[i] <- m
    }
  }
  
  col <- as.double(as.character(col))
  
  data[[c]] <- col
}
data

dataFiltered <- data[,c("medIncome","PctPopUnderPov","PctUnemployed","ViolentCrimesPerPop","nonViolPerPop")]
dataFiltered

attach(dataFiltered)

#Violent Crimes

multi.fit = lm(ViolentCrimesPerPop~medIncome+PctPopUnderPov+PctUnemployed)
summary(multi.fit)

multi.fit = lm(ViolentCrimesPerPop~PctPopUnderPov+PctUnemployed)
summary(multi.fit)

multi.fit = lm(ViolentCrimesPerPop~PctUnemployed)
summary(multi.fit)

multi.fit = lm(ViolentCrimesPerPop~PctPopUnderPov)
summary(multi.fit)

res<-scale( multi.fit$residuals )

mean(multi.fit$residuals)
sum(multi.fit$residuals)

dwtest(multi.fit)

bptest(multi.fit)

layout(matrix(c(1,2,3,4),2,2,byrow=T))
plot(multi.fit$fitted.values,rstandard(multi.fit),
     main = "Multi Fit Standarized Residuals",
     xlab="Predictions",
     ylab="Standarized Residuals",
     ylim = c(-2.5,2.5))
abline(h=0,lty=2)
plot(ViolentCrimesPerPop,res)
abline(h=0,lty=2)
hist(res,main="Histograma de Residuos")
qqnorm(res)
qqline(res)


#Non-Violent Crimes

multi.fit = lm(nonViolPerPop~medIncome+PctPopUnderPov+PctUnemployed)
summary(multi.fit)

multi.fit = lm(nonViolPerPop~PctPopUnderPov+PctUnemployed)
summary(multi.fit)

multi.fit = lm(nonViolPerPop~PctUnemployed)
summary(multi.fit)

multi.fit = lm(nonViolPerPop~PctPopUnderPov)
summary(multi.fit)

res<-scale( multi.fit$residuals )

mean(multi.fit$residuals)
sum(multi.fit$residuals)

dwtest(multi.fit)

bptest(multi.fit)

layout(matrix(c(1,2,3,4),2,2,byrow=T))
plot(multi.fit$fitted.values,rstandard(multi.fit),
     main = "Multi Fit Standarized Residuals",
     xlab="Predictions",
     ylab="Standarized Residuals",
     ylim = c(-2.5,2.5))
abline(h=0,lty=2)
plot(nonViolPerPop,res)
abline(h=0,lty=2)
hist(res,main="Histograma de Residuos")
qqnorm(res)
qqline(res)
