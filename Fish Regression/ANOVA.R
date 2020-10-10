df <- read.csv('Fish.csv')
print(df)

perch_df <- df[df[,1]=="Perch",]
attach(perch_df)

plot(Width~Height)


model = lm(Width~Height,data = perch_df)

plot(model)

sumMod <- summary(model)

sumMod$coefficients

#95% confidence intervals
confint(model, level=0.95)

#So b0 is between -0.2778046 and 0.239670
#   b1 is between 0.5751260 ans 0.6370005

#Predict Yh(new) with 95% prediction interval
predict.lm(model, data.frame(Height=10),interval="prediction", level = 0.95)


#Predict Yhath with 95% confidence interval
predict.lm(model, data.frame(Height=10),interval="confidence", level = 0.95)



library(onewaytests)
attach(perch_df)
detach(perch_df)
bf.test(Width ~ Height,data = perch_df, alpha = 0.05)



bfTest <- function(linearModel, alpha){
  df <- data.frame(model$residuals)
  df <- data.frame(X,Y)
  df <- df[order(X),]
  row.names(df) <- NULL
  lowerDf <- df[row.names(df)<=length(df),]
  upperDf <- df[row.names(df)>length(df),]
  
  
}

predict.lm(model)
bfTest(perch_df$Width,perch_df$Weight , 0.2)
