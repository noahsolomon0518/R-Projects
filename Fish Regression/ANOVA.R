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

bf.test(Width~Height,data = perch_df, alpha = 0.05)


