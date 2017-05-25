# Modelos de Inferencia Estadística
# José Luis Baroja
# Enero 12-23, 2015



# Regresión Lineal Simple
# Este código implementa una versión Bayesiana del modelo de regresión lineal simple.
# Aparte, muestra cómo utilizar la función 'polygon()' para presentar distribuciones posteriores predictivas.



# Regresión Lineal Simple + rectas creíbles (SIN distribución posterior predictiva)
rm(list=ls())
library('R2jags')
# a) Datos
setwd("~/Documents/Luis/Courses and Workshops/Winter 2014-2015 Proba Camp/Jan2015/Data")
datos<-read.csv('Jan2015_RegresionLineal_1.csv')
x<-datos$x
y<-datos$y
n_obs<-length(x)
datos_jags<-list('x','y','n_obs')
# ¡Siempre hay que graficar los datos antes de hacer cualquier análisis!
plot(x,y)

# b) Modelo
write('model{
      # Priors
      beta1~dunif(-20,20)
      beta0~dunif(-20,20)
      sd~dunif(0,100)
      tau<-1/pow(sd,2)
      
      # Datos
      for(i in 1:n_obs){
      y[i]~dnorm(mu[i],tau)
      mu[i]<-beta1*x[i]+beta0
      }
      
      }','RegresionLineal.bug')
# c) Parámetros
parametros<-c('beta1','beta0','mu','sd')
# d) Valores Iniciales
iniciales<-list(
  list(beta1=runif(1,-10,10),beta0=runif(1,-10,10),sd=runif(1,0,100)),
  list(beta1=runif(1,-10,10),beta0=runif(1,-10,10),sd=runif(1,0,100)),
  list(beta1=runif(1,-10,10),beta0=runif(1,-10,10),sd=runif(1,0,100)))  

# e) Sampleo
posteriores_RegresionLineal<-jags( 
  data=datos_jags, 
  inits=iniciales, 
  parameters.to.save=parametros, 
  model.file='RegresionLineal.bug',
  n.chains=3,
  n.iter=20000, 
  n.burnin=10000, 
  n.thin=10, 
  DIC=T) 
# ¡Hay que verificar convergencia con el estadístico Rhat y analizando visualmente las cadenas!
# f) Resultados
summary(posteriores_RegresionLineal$BUGSoutput$summary)
names(posteriores_RegresionLineal$BUGSoutput$sims.list)
beta1<-posteriores_RegresionLineal$BUGSoutput$sims.list$beta1
beta0<-posteriores_RegresionLineal$BUGSoutput$sims.list$beta0
sd<-posteriores_RegresionLineal$BUGSoutput$sims.list$sd

# Graficando las observaciones + rectas creíbles
layout(matrix(c(rep(1,3),2:4),ncol=2),widths=c(1,.5))
plot(0,type='n',
     ylim=c(40,70),
     xlim=c(15,25))
indices<-sample(1:length(beta1),40)
for(s in 1:length(indices)){
  abline(beta0[s],beta1[s],
         col=rgb(160,160,160,maxColor=255,alpha=50))
}
points(x,y,
       pch=16)
hist(beta1,breaks=100,col='black',xlim=c(0,5))
hist(beta0,breaks=100,col='black',xlim=c(0,20))
hist(sd,breaks=100,col='black',xlim=c(0,5))



# Regresión Lineal Simple + distribución posterior predictiva
rm(list=ls())
library('R2jags')
# a) Datos
setwd("~/Documents/Luis/Courses and Workshops/Winter 2014-2015 Proba Camp/Jan2015/Data")
datos<-read.csv('Jan2015_RegresionLineal_1.csv')
x<-datos$x
y<-datos$y
n_obs<-length(x)
# 'x_base' and 'tamano_base' are used to compute posterior prediction
x_base<-seq(10,30,.1)
tamano_base<-length(x_base)
data2jags<-list('x','y','n_obs','x_base','tamano_base')
# b) Modelo
write('model{
      # Priors
      beta1~dunif(-20,20)
      beta0~dunif(-20,20)
      sd~dunif(0,100)
      tau<-1/pow(sd,2)

      # Datos
      for(i in 1:n_obs){
      y[i]~dnorm(mu[i],tau)
      mu[i]<-beta1*x[i]+beta0
      }

      # Posterior Predictiva + base
      for(j in 1:tamano_base){
      y_pred[j]~dnorm(mu_pred[j],tau)
      mu_pred[j]<-beta1*x_base[j]+beta0
      }

      }','LinearRegression_M1.bug')
# c) Parámetros
parameters2monitor<-c('beta1','beta0','mu','sd','y_pred')
# d) Valores iniciales
start.values<-list(list(beta1=runif(1,-10,10),beta0=runif(1,-10,10),sd=runif(1,0,100)),
                   list(beta1=runif(1,-10,10),beta0=runif(1,-10,10),sd=runif(1,0,100)),
                   list(beta1=runif(1,-10,10),beta0=runif(1,-10,10),sd=runif(1,0,100))) 
# e) Sampleo
LinearRegression_M1<-jags(data2jags,
                        inits=start.values,
                        parameters2monitor,
                        model.file='LinearRegression_M1.bug',
                        n.chains=3,
                        n.iter=20000,
                        n.burnin=10000,
                        n.thin=10,
                        DIC=T)
unlink('LinearRegression_M1.bug')
# f) Resultados
summary(LinearRegression_M1$BUGSoutput$summary)
names(LinearRegression_M1$BUGSoutput$sims.list)
beta1<-LinearRegression_M1$BUGSoutput$sims.list$beta1
beta0<-LinearRegression_M1$BUGSoutput$sims.list$beta0
sd<-LinearRegression_M1$BUGSoutput$sims.list$sd
y_pred<-LinearRegression_M1$BUGSoutput$sims.list$y_pred

# Resumiendo la distribución posterior predictiva:
# Definiendo intervalos de mayor densidad posterior (predictiva)
lower_HDI<-NA
higher_HDI<-NA
# dim(y_pred)[2]
for(i in 1:dim(y_pred)[2]){
  lower_HDI[i]<-quantile(y_pred[,i],probs=0.025)
  higher_HDI[i]<-quantile(y_pred[,i],probs=0.975)
}

# Graficando la distribución posterior predictiva: 
# Posterior predictiva con líneas
layout(matrix(c(rep(1,3),2:4),ncol=2),widths=c(1,.5))
plot(0,type='n',xlim=c(5,35),ylim=c(20,90))
segments(x_base,lower_HDI,x_base,higher_HDI,
         col=rgb(170,170,170,maxColor=255,alpha=100))
points(x,y,pch=16)
hist(beta1,breaks=100,col='black',xlim=c(0,5))
hist(beta0,breaks=100,col='black',xlim=c(0,20))
hist(sd,breaks=100,col='black',xlim=c(0,5))

# Posterior predictiva con polígono (+ distribuciones posteriores marginales)
layout(matrix(c(rep(1,3),2:4),ncol=2),widths=c(1,.5))
plot(0,type='n',xlim=c(5,35),ylim=c(20,90))
polygon(c(x_base,x_base[length(x_base):1]),
        c(higher_HDI,lower_HDI[length(x_base):1]),
        col=rgb(180,180,180,maxColor=255,alpha=100),
        border=NA)
points(x,y,pch=16)
hist(beta1,breaks=100,col='black',xlim=c(0,5))
hist(beta0,breaks=100,col='black',xlim=c(-5,15))
hist(sd,breaks=100,col='black',xlim=c(0,5))
