# Modelos de Inferencia Estadística
# José Luis Baroja
# Enero 12-23, 2015

# Inferencia Bayesiana con JAGS y R en modelos Normales
# Este código muestra cómo aproximar distribuciones posteriores sobre parámetros Normales




# Modelo 1: Infiriendo parámetros normales
rm(list=ls())
library('R2jags')
# a) Datos
setwd("~/Documents/Luis/Courses and Workshops/Winter 2014-2015 Proba Camp/Jan2015/Data")
datos<-read.csv('Jan2015_DatosNormal.csv')
edad<-datos$edad
n_obs<-length(edad)
# ¡Siempre hay que graficar los datos antes de hacer cualquier análisis!
hist(edad)
datos_jags<-list('edad','n_obs') 

# b) Modelo
write('model{
      
      # Distribuciones Prior:
      mu~dunif(-100,100)
      sd~dunif(0,10)
      precision<-1/pow(sd,2)
      
      # Distribución de los Datos:
      for(i in 1:n_obs){
      edad[i]~dnorm(mu,precision) 
      # Nota: La distribución Normal se parametriza EN JAGS en términos de PRECISION (no de DESV STD)
      # Sin embargo, es posible inferir los valores de DESV STD a partir de los de PRECISION (Ver líneas 28 y 29.)
      }
      
      }','ejemploNormal.bug')

# c) Parámetros
parametros<-c('mu','sd')

# d) Valores Iniciales
iniciales<-list( 
  list(mu=-20,sd=1),
  list(mu=80,sd=4),
  list(mu=1,sd=2)
)

# e) Sampleo
posteriores_Normal<-jags( 
  data=datos_jags, 
  inits=iniciales, 
  parameters.to.save=parametros, 
  model.file='ejemploNormal.bug',
  n.chains=3,
  n.iter=2500, 
  n.burnin=500, 
  n.thin=2, 
  DIC=T) 

# f) Resultados
#traceplot(posteriores_Normal)
summary(posteriores_Normal$BUGSoutput$summary)

# Extracción de sampleos posteriores de cada parámetro:
mu<-posteriores_Normal$BUGSoutput$sims.list$mu
sd<-posteriores_Normal$BUGSoutput$sims.list$sd

# Distribuciones posteriores marginales:
# Distribución marginal de la media (mu)
hist(mu,breaks=100,col='gray',border='gray',xlim=c(21.5,23.5))
# La función 'quantile' devuelve el valor del soporte que corresponde con 'probs' valor de probabilidad acumulada
quantiles_mu<-quantile(mu,probs=c(0.025,0.975))
# La función 'lines' agrega líneas sobre un gráfico previamente especificado.
# En este ejemplo, la utilizamos para dibujar una línea que denota el intervalo de mayor densidad (IMD) posterior
lines(quantiles_mu,c(0,0),lwd=8)

hist(sd,breaks=100,col='gray',border='gray',xlim=c(1.5,3))
quantiles_sd<-quantile(sd,probs=c(0.025,0.975))
lines(quantiles_sd,c(0,0),lwd=8)
