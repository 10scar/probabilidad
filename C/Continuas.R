##### EJEMPLO 1: SIMULACION DE UNA DENSIDAD#####

###Funci?n de masa  y de distribuci?n de probabilidad

f_X<-function(x){
  if (x>=-1 & x<=1){
    return(1.5*x^2)
  } else {
    0
  }
}

F_X<-function(x){
  if (x< -1){
    0
  } else {
    if (x>=-1 & x<=1){
      return(1.5*(x^3/3 + 1/3))
    } else {
      1
    }
  }
}

###Gr?ficos

dens<-NULL
Dist<-NULL
for (i in seq(-2,2,by=0.01)){
  dens<-c(dens,f_X(i))
  Dist<-c(Dist,F_X(i))
}

plot(seq(-2,2,by=0.01),dens,type="l")


plot(seq(-2,2,by=0.01),Dist,type="l")

###Algoritmo para simular

sim<-function(u){
  if (u>=0.5){
    x<-(2*u-1)^(1/3)
  } else {
    x<- -(1-2*u)^(1/3)
  }
  return(x)
}

###Simulaci?n de 10000 realizaciones
vector_u<-runif(10000)
vector_x<-NULL

for (j in 1:10000){
  vector_x[j]<-sim(vector_u[j])
}

hist(vector_x,prob=TRUE)
lines(seq(-2,2,by=0.01),dens,type="l",col="red")

##### EJEMPLO 2: SIMULACION DE UNA UNIFORME (a=150, b=210)#####

vector_unif<-runif(n=10000,min=150,max=210)

hist(vector_unif,prob=TRUE)
lines(seq(150,210,by=0.01),dunif(x=seq(150,210,by=0.01),min=150,max=210),type="l",col="red")

##### EJEMPLO 3: SIMULACION DE UNA NORMAL (mu=10, sigma=5)#####

vector_norm<-rnorm(n=10000,mean=10,sd=5)

hist(vector_norm,prob=TRUE)
lines(seq(-10,30,by=0.01),dnorm(x=seq(-10,30,by=0.01),mean=10,sd=5),type="l",col="red")
