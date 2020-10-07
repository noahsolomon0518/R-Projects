mydata = read.csv("co2_emission.csv")
print(mydata)



dataGroupBy <- function(country){
  data <- mydata[mydata[,"Entity"]==country,]
  data <- data[data[,4]!=0,]
  return(reindex(data))
}


AfricaC02 <- dataGroupBy("Africa")
attach(AfricaC02)

test <- AfricaC02[,3]^(1/2)
plot(AfricaC02[,4]~test)


mod = lm(AfricaC02[,4]~AfricaC02[,3])

summary(mod)

plot(mod)
