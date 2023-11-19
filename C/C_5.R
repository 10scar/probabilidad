##### Parte C#5: SIMULACION DE UNA DENSIDAD#####

###Función de masa  y de distribución de probabilidad


f_X<-function(x){
  if (x>=0 & x<=1){
    return(3/2*(1-x^2))
  } else {
    0
  }
}

F_X<-function(x){
  if (x< 0){
    0
  } else {
    if (x>=0 & x<=1){
      return(0.5*(3*x- x^3))
    } else {
      1
    }
  }
}

###Graficos

dens<-NULL
Dist<-NULL
for (i in seq(-2,2,by=0.01)){
  dens<-c(dens,f_X(i))
  Dist<-c(Dist,F_X(i))
}

plot(seq(-2,2,by=0.01),dens,type="l")


plot(seq(-2,2,by=0.01),Dist,type="l")

###Algoritmo para simular INVERSA DE LA FUNCIÓN DE DISTRIBUCIÓN
## SE TOMA LA PARTE REAL DE LA SOLUCIÓN DE TERCER GRADO EN EL INTERVALO 0,1

sim <- function(x){
  y <- ((1 + sqrt(3)*1i)*(x + sqrt(-(x^2) + 1)*1i)^(2/3) + (1 - sqrt(3)*1i))/(2*(x + sqrt(-(x^2) + 1)*1i)^(1/3))
  return(Re(y))
}



###SimulaciÓn de 10000 realizaciones
vector_u<-runif(10000)
vector_x<-NULL

for (j in 1:10000){
  vector_x[j]<-sim(vector_u[j])
}


hist(vector_x,prob=TRUE)
lines(seq(-2,2,by=0.01),dens,type="l",col="red")



