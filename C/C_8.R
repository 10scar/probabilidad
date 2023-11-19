##### C8 #####

###FunciOn de masa  y de distribuciOn de probabilidad

f_X<-function(x){
  if (x>=0 & x<10){
    x_f <- (1/150*x)
  } else if(x>=10 & x<=30) {
    x_f <- 1/10-(1/300*x)
  }else{
    x_f <- 0
  }
  return(x_f)
}

F_X<-function(x){
  if(x<0){
    x_f <- 0
  }
  else if (x>=0 & x<10){
    x_f <- (x^2/300)
  } else if(x>=10 & x<=30) {
    x_f <- -(x^2/600)+(x/10)-(1/2)
  }else if (x>30){
    x_f <- 1
  }
  return(x_f)
}

###Gr?ficos

dens<-NULL
Dist<-NULL
for (i in seq(-10,40,by=0.01)){
  dens<-c(dens,f_X(i))
  Dist<-c(Dist,F_X(i))
}

plot(seq(-10,40,by=0.01),dens,type="l")


plot(seq(-10,40,by=0.01),Dist,type="l")

###Algoritmo para simular

sim<-function(x){
  if (x>=0 & x<1/3){
    x_f <- (sqrt(300*x))
  } else if(x>=1/3 & x<=1) {
    x_f <- 30-(10*sqrt(6-6*x))
  return(x_f)
  }
}

###Simulaci?n de 10000 realizaciones
vector_u<-runif(10000)
vector_x<-NULL

for (j in 1:10000){
  vector_x[j]<-sim(vector_u[j])
}

hist(vector_x,prob=TRUE)
lines(seq(-10,40,by=0.01),dens,type="l",col="red")

##### EJEMPLO 2: SIMULACION DE UNA UNIFORME (a=150, b=210)#####

vector_unif<-runif(n=10000,min=150,max=210)

hist(vector_unif,prob=TRUE)
lines(seq(150,210,by=0.01),dunif(x=seq(150,210,by=0.01),min=150,max=210),type="l",col="red")

