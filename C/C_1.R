##### C8 #####

###FunciOn de masa  y de distribuciOn de probabilidad CONOCIDAS

f_X<-function(x){
  if (x>=0){
    x_f <- 1/4*exp((-1/4)*x)
  }else{
    x_f <- 0
  }
  return(x_f)
}

F_X<-function(x){
  if(x>=0){
    x_f <- 1-exp((-1/4)*x)
  }else{
    x_f <- 0
  }
  return(x_f)
}

###Gr?ficos

dens<-NULL
Dist<-NULL
for (i in seq(0,20,by=0.01)){
  dens<-c(dens,f_X(i))
  Dist<-c(Dist,F_X(i))
}

plot(seq(0,20,by=0.01),dens,type="l")


plot(seq(0,20,by=0.01),Dist,type="l")

###Algoritmo para simular

sim<-function(x){
  if (x>=0){
    x_f <- -4*log(1-x)
  } else{
    x_f <- 0
  }
  return(x_f)
}

###Simulaci?n de 10000 realizaciones
vector_u<-runif(10000)
vector_x<-NULL

for (j in 1:10000){
  vector_x[j]<-sim(vector_u[j])
}

hist(vector_x,prob=TRUE)
lines(seq(0,20,by=0.01),dens,type="l",col="red")



