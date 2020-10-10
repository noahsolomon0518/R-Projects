twoSamplePropConf <- function(x,n, conf){
  z = qnorm(conf+(1-conf)/2)
  m = z*sqrt((x[1]*(n[1]-x[1]))/(n[1]**3)   +     (x[2]*(n[2]-x[2]))/(n[2]**3) )
  diff = abs( (x[1]/n[1])-(x[2]/n[2]) )
  
  cat("Confidence Interval: (", diff-m,",",diff+m,")\n" )
}


twoSamplePropTest <- function(x,n, p=-1){
  
  props = getProps(x,n)
  
  if(p==-1){
    
    pooledP = (x[1]+x[2])/(n[1]+n[2])
    SE = (pooledP)*(1-pooledP)*(1/n[1]+1/n[2])
    z = abs(props[1] - props[2])/SE
    cat("pooledP = ", pooledP, "Z = ", z, "\n")
  }
  else{
    SE = (p)*(1-p)*(1/n[1]+1/n[2])
    z = abs(props[1] - props[2])/SE
    cat( "Z = ", z, "p = ", pnorm(z),"\n")
  }
}

getProps <- function(x,n){
  props = c()
  for (i in 1:length(x)) {
    p = x[i]/n[i]
    props <- c(props,p)
  }
  cat("Raw numbers: ", x, n, "\n")
  cat("Proportions: " ,props,"\n")
  
  return(props)
}



propSummary <- function(x, n, conf, p=-1){
  twoSamplePropTest(x,n, p)
  twoSamplePropConf(x,n, conf)
}



propSummary(c(574,947),c(1063,1064), 0.95)



