# Modelos de Inferencia Estad??stica
# Jos?? Luis Baroja
# Enero 12-23, 2015

# Inferencia Bayesiana con JAGS y R en modelos Normales
# Este c??digo muestra c??mo aproximar distribuciones posteriores sobre par??metros Normales




# Modelo 1: Infiriendo par??metros normales
rm(list=ls())
library('R2jags')
# a) Datos
# setwd("~/Documents/Luis/Courses and Workshops/Winter 2014-2015 Proba Camp/Jan2015/Data")
datos<-read.csv('Jan2015_DatosNormal.csv')
edad<-datos$edad
n_obs<-length(edad)
# ??Siempre hay que graficar los datos antes de hacer cualquier an??lisis!
layout(1:2)
hist(edad)
datos_jags<-list('edad','n_obs') 

# b) Modelo
write('model{
      
      # Distribuciones Prior:
      mu~dunif(-100,100)
      sd~dunif(0,10)
      precision<-1/pow(sd,2)
      
      # Distribuci??n de los Datos:
      for(i in 1:n_obs){
      edad[i]~dnorm(mu,precision) 
      # Nota: La distribuci??n Normal se parametriza EN JAGS en t??rminos de PRECISION (no de DESV STD)
      # Sin embargo, es posible inferir los valores de DESV STD a partir de los de PRECISION (Ver l??neas 28 y 29.)
      }
      
      }','ejemploNormal.bug')

# c) Par??metros
parametros<-c('mu','sd','precision')

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

# Extracci??n de sampleos posteriores de cada par??metro:
mu<-posteriores_Normal$BUGSoutput$sims.list$mu
sd<-posteriores_Normal$BUGSoutput$sims.list$sd
precision<-posteriores_Normal$BUGSoutput$sims.list$precision

# Distribuciones posteriores marginales:
# Distribuci??n marginal de la media (mu)
hist(mu,breaks=100,col='gray',border='gray',xlim=c(16,28))
# La funci??n 'quantile' devuelve el valor del soporte que corresponde con 'probs' valor de probabilidad acumulada
quantiles_mu<-quantile(mu,probs=c(0.025,0.975))
# La funci??n 'lines' agrega l??neas sobre un gr??fico previamente especificado.
# En este ejemplo, la utilizamos para dibujar una l??nea que denota el intervalo de mayor densidad (IMD) posterior
lines(quantiles_mu,c(0,0),lwd=8)

hist(sd,breaks=100,col='gray',border='gray',xlim=c(1.5,3))
# hist(precision,breaks=100,col='gray',border='gray')


quantiles_sd<-quantile(sd,probs=c(0.025,0.975))
lines(quantiles_sd,c(0,0),lwd=8)
