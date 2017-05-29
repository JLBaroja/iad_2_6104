# Modelos de Inferencia Estad??stica
# Jos?? Luis Baroja
# Enero 12-23, 2015

# Inferencia Bayesiana con JAGS y R.
# Este c??digo presenta varios ejemplos que muestran c??mo aproximar distribuciones posteriores utilizando R y JAGS.
# Para que el c??digo funcione es necesario instalar 'JAGS' [http://mcmc-jags.sourceforge.net/] y el paquete 'R2jags' en R [install.packages('R2jags')].

# EJEMPLO 1: UNA observaci??n binomial, UN par??metro desconocido
rm(list=ls()) # Esta instrucci??n borra todos los objetos en el espacio de trabajo de R (limpia memoria antes de empezar).
library('R2jags') # 'Activa' el paquete que permite a R interactuar con JAGS. 
# (Es necesario instalar 'JAGS' y el paquete de R 'R2jags' para que el resto del c??digo funcione; ver l??nea 7.)

# a) Datos
# En esta secci??n 'vaciamos' nuestras observaciones en objetos interpretables por R:
n<-15 # N??mero de ensayos Bernoulli observados.
x<-4 # N??mero de ??xitos observados en los 'n' ensayos.
datos_jags<-list('n','x') # 'datos_jags' es una lista que indica en qu?? objetos est??n los datos observados.

# b) Modelo
# En esta parte del c??digo traducimos el modelo gr??fico que relaciona nuestros datos y nuestros par??metros
# en c??digo interpretable por JAGS:
write('model{
      
      # Distribuci??n Prior (sobre theta, par??metro desconocido):
      theta~dunif(0,1)
      
      # Distribuci??n de los Datos (en funci??n de par??metros conocidos y desconocidos): 
      x~dbinom(theta,n)
      
      }','ejemplo1.bug')

# c) Par??metros
parametros<-c('theta') # En esta l??nea especificamos qu?? par??metros nos interesa rastrear.

# d) Valores Iniciales
iniciales<-list( # Este objeto contiene los valores iniciales para cada cadena (3 en este ejemplo).
  list(theta=.2),
  list(theta=.8),
  list(theta=.5)
)

# e) Sampleo
# En esta secci??n inclu??mos toda la informaci??n necesaria para que jags calcule la(s) distribuci??n(es) posterior(es) sobre el(los) par??metro(s) de inter??s:
posteriores_ej1<-jags( 
  data=datos_jags, # Especificamos la lista con los nombres de los objetos que contienen las observaciones (ver l??nea 18)
  inits=iniciales, # Especificamos los valores iniciales de cada cadena (ver l??neas 37-41)
  parameters.to.save=parametros, # Especificamos los nombres de los par??metros que queremos rastrear (ver l??nea 34)
  model.file='ejemplo1.bug', # Especificamos cu??l es el archivo del modelo gr??fico (no olvidar comillas!; ver l??neas 21-31)
  n.chains=3, # Especificamos cu??ntas cadenas queremos correr (debe coincidir con el n??mero de sublistas de 'iniciales'; ver l??neas 37-41 otra vez)
  n.iter=5000, # Especificamos el n??mero total de sampleos por cadena
  n.burnin=1000, # Especificamos cu??ntos sampleos queremos desechar al principio del proceso de sampleo
  n.thin=4, # Especificamos cada cu??ntos sampleos queremos conservar del total restante
  DIC=T) 

# f) Resultados
# En esta secci??n examinamos los resultados que JAGS regresa despu??s de observar nuestros datos, de acuerdo con nuestro modelo y supuestos iniciales:

# i) Siempre es conveniente examinar las cadenas visualmente para asegurarnos de que todas 
# se 'mueven' en el mismo rango de valores, lo cual indica que la distribuci??n posterior es confiable.
traceplot(posteriores_ej1)
# Nota: Cuando las cadenas convergen en la misma distribuci??n posterior, el 'traceplot' se ve como una
# 'oruga gorda y peluda' (Lee y Wagenmakers, 2013, p. 80).

# ii) En general, la mejor manera de examinar una distribuci??n es grafic??ndola.
# Para graficar la distribuci??n posterior sobre theta (nuestro par??metro desconocido)
# es necesario extraer los sampleos correspondientes a dicha distribuci??n del objeto 'posteriores':
theta<-posteriores_ej1$BUGSoutput$sims.list$theta
# Nota: La sint??xis en R '$' es una forma de examinar subcomponentes de un objeto complejo; como veremos
# m??s adelante, el objeto que jags regresa, 'posteriores_ej1', no s??lo contiene los sampleos de las distribuciones
# posteriores que nos interesan, sino mucha informaci??n adicional organizada en diferentes secciones.
# La l??nea 68 'extrae' los sampleos de la distribuci??n posterior sobre theta y los vac??a en un objeto nuevo
# que llamamos 'theta'. La ventaja de extraer los sampleos de esta manera es que ahora es mucho m??s f??cil y 
# c??modo trabajar con ellos y manipularlos. Por ejemplo, para presentar la distribuci??n posterior en
# forma de histograma:
hist(theta)
# Argumentos adicionales cambian la apariencia de la gr??fica:
hist(theta,breaks=100,col='black',xlim=c(0,1))

# iii) Tambi??n podemos analizar directamente algunas medidas que brindan informaci??n adicional sobre la distribuci??n posterior:
posteriores_ej1





# EJEMPLO 2: UNA observaci??n binomial, DOS par??metros desconocidos
rm(list=ls())
library('R2jags')

# a) Datos
x<-24 # N??mero de ??xitos observados (en qui??n sabe cu??ntos ensayos Bernoulli).
n_max<-100 # N??mero m??ximo posible de ensayos (supuesto)
datos_jags<-list('x','n_max') 

# b) Modelo
write('model{
      
      # Distribuciones Prior (sobre theta y sobre n):
      theta~dunif(0,1)
      n~dcat(pi[])
      
      # Esta secci??n especifica un prior uniforme DISCRETO sobre n:
      for(i in 1:n_max){
      pi[i]<-1/n_max
      }
      
      # Distribuci??n de los Datos:
      x~dbinom(theta,n)
      
      }','ejemplo2.bug')

# c) Par??metros
parametros<-c('theta','n') # A diferencia del ejemplo anterior, en este queremos rastrear DOS par??metros desconocidos

# d) Valores Iniciales
iniciales<-list( 
  list(theta=.2,n=30), # Cada sublista debe incluir un valor inicial PARA CADA par??metro desconocido
  list(theta=.8,n=50),
  list(theta=.5,n=80)
)

# e) Sampleo
# En esta secci??n inclu??mos toda la informaci??n necesaria para que jags calcule la(s) distribuci??n(es) posterior(es) sobre el(los) par??metro(s) de inter??s:
posteriores_ej2<-jags( 
  data=datos_jags, 
  inits=iniciales, 
  parameters.to.save=parametros, 
  model.file='ejemplo2.bug',
  n.chains=3,
  n.iter=5000, 
  n.burnin=1000, 
  n.thin=4, 
  DIC=T) 

# f) Resultados
traceplot(posteriores_ej2)

# * Aparte de examinar visualmente las cadenas, el estad??stico ^R ayuda a evaluar la convergencia de las cadenas.
# En general, valores de ^R mayores que 1.05 indican convergencia deficiente y sugieren cambiar los par??metros
# del sampleo para corregir la no-convergencia de las cadenas. Valores ^R<1.05 indican convergencia adecuada y
# que los sampleos conservados provienen de una distribuci??n posterior confiable.
summary(posteriores_ej2$BUGSoutput$summary)

# Extracci??n de sampleos posteriores de cada par??metro:
theta<-posteriores_ej2$BUGSoutput$sims.list$theta
n<-posteriores_ej2$BUGSoutput$sims.list$n

# Distribuciones posteriores marginales:
hist(theta,breaks=100,col='black',xlim=c(0,1))
hist(n,breaks=100,col='black',xlim=c(0,100))

# Distribuci??n posterior conjunta:
# Cuando existen dos o m??s par??metros desconocidos es recomendable examinar no s??lo qu?? valores de cada par??metros
# son m??s probables (distribuciones posteriores marginales), sino tambi??n qu?? COMBINACIONES de ambos par??metros son
# m??s probables (distribuciones posteriores conjuntas):
plot(theta,n)
plot(theta,n,pch=16,xlim=c(0,1),ylim=c(0,100),main='Distribuci??n Posterior Conjunta')





# EJEMPLO 3: MUCHAS observaciones Bernoulli
rm(list=ls())
library('R2jags')

# a) Datos
x<-c(0,1,0,0,0,1,1,0,1,1,1,0,1,0,1,1) # Esta vez 'x' es un vector que contiene muchas observaciones
n_obs<-length(x) # El objeto 'n_obs' indica cu??ntas observaciones tenemos
datos_jags<-list('x','n_obs') 

# b) Modelo
write('model{
      
      # Distribuci??n Prior:
      theta~dunif(0,1)
      
      # Distribuci??n de los Datos:
      # El ciclo FOR indica que todas las observaciones almacenadas en x
      # provienen del mismo proceso Bernoulli (i.e., el par??metro theta es constante y ??nico para todas las observaciones)
      for(i in 1:n_obs){
      x[i]~dbern(theta) 
      }
      
      }','ejemplo3.bug')

# c) Par??metros
parametros<-c('theta')

# d) Valores Iniciales
iniciales<-list( 
  list(theta=.2),
  list(theta=.8),
  list(theta=.5)
)

# e) Sampleo
# En esta secci??n inclu??mos toda la informaci??n necesaria para que jags calcule la(s) distribuci??n(es) posteriores sobre los par??metros de inter??s:
posteriores_ej3<-jags( 
  data=datos_jags, 
  inits=iniciales, 
  parameters.to.save=parametros, 
  model.file='ejemplo3.bug',
  n.chains=3,
  n.iter=2500, 
  n.burnin=500, 
  n.thin=2, 
  DIC=T) 

# f) Resultados
traceplot(posteriores_ej3)
summary(posteriores_ej3$BUGSoutput$summary)

# Extracci??n de sampleos posteriores de cada par??metro:
theta<-posteriores_ej3$BUGSoutput$sims.list$theta

# Distribuciones posteriores marginales:
hist(theta,breaks=100,col='black',xlim=c(0,1))
# Nota: ??C??mo se modifica la distribuci??n posterior sobre theta cuando recolectamos m??s datos?





# EJEMPLO 4: MUCHAS observaciones Bernoulli + Distribuci??n Posterior Predictiva
rm(list=ls())
library('R2jags')

# a) Datos
x<-c(0,0,0,1,1,1,0,0,0,0,0,0,1,0,1,0,0,0,0,0,0,0,0,0,0,
     1,0,0,0,0,0,0,1,0,1,0,0,0,0,1,1,1,1,0,0,0,0,0,0,1,
     0,1,0,0,0,1,1,0,0,1,0,1,1,0,0,0,0,0,1,1,0,0,0,0,0,
     1,0,0,0,0,0,0,1,0,1,0,0,0,0,1,1,1,1,0,0,0,0,0,0,1,
     0,1,0,0,0,1,1,0,0,1,0,1,1,0,0,0,0,0,1,1,0,0,0,0,0,
     1,0,0,0,0,0,0,1,0,1,0,0,0,0,1,1,1,1,0,0,0,0,0,0,1,
     0,1,0,0,0,1,1,0,0,1,0,1,1,0,0,0,0,0,1,1,0,0,0,0,0,
     1,0,1,0,1,1,1,0,0,0,1,0,1,0,0,0,0,0,0,1,1,0,0,1,0) 
n_obs<-length(x) # El objeto 'n_obs' indica cu??ntas observaciones tenemos
datos_jags<-list('x','n_obs') 

# b) Modelo
write('model{
      
      # Distribuci??n Prior:
      theta~dunif(0,1)
      
      # Distribuci??n de los Datos:
      for(i in 1:n_obs){
      x[i]~dbern(theta) 
      }
      
      # Distribuci??n Predictiva:
      # La distribuci??n predictiva se calcula creando una nueva variable
      # que depende de los par??metros que vamos a inferir (y que se distribuye igual
      # que las observaciones recolectadas; ver l??nea 250):
      for(i in 1:n_obs){
      x_predictiva[i]~dbern(theta)
      }
      
      }','ejemplo4.bug')

# c) Par??metros
parametros<-c('theta','x_predictiva') # Aunque 'x_predictiva' no es un par??metro, hay que incluirlo en esta lista
# para que jags devuelva sampleos sobre ??l.

# d) Valores Iniciales
iniciales<-list( 
  list(theta=.2),
  list(theta=.8),
  list(theta=.5)
)

# e) Sampleo
# En esta secci??n inclu??mos toda la informaci??n necesaria para que jags calcule la(s) distribuci??n(es) posteriores sobre los par??metros de inter??s:
posteriores_ej4<-jags( 
  data=datos_jags, 
  inits=iniciales, 
  parameters.to.save=parametros, 
  model.file='ejemplo4.bug',
  n.chains=3,
  n.iter=2500, 
  n.burnin=500, 
  n.thin=2, 
  DIC=T) 

# f) Resultados
#traceplot(posteriores_ej4)
summary(posteriores_ej4$BUGSoutput$summary)
posteriores_ej4

# Extracci??n de sampleos posteriores de cada par??metro (y distribuciones predictivas):
theta<-posteriores_ej4$BUGSoutput$sims.list$theta
x_predictiva<-posteriores_ej4$BUGSoutput$sims.list$x_predictiva
# El objeto 'x_predictiva' tiene varias secuencias de 0 y 1:
dim(x_predictiva)
# En concreto, tiene 3000 secuencias (total de sampleos posteriores) de 100 elementos (total de observaciones) cada una
x_predictiva[1,]
x_predictiva[2,]

# Distribuciones posteriores marginales:
hist(theta,breaks=100,col='black',xlim=c(0,1))

# ??El modelo es adecuado?
plot(0,xlim=c(1,n_obs),ylim=c(1,n_obs),type='n')
for(k in 1:dim(x_predictiva)[1]){
  lines(cumsum(x_predictiva[k,]),col=rgb(190,190,190,maxColor=255,alpha=20))
}
lines(cumsum(x),lwd=2)





# EJEMPLO 5: MUCHAS observaciones Bernoulli de dos procesos diferentes + diferencia entre tasas
rm(list=ls())
library('R2jags')

# a) Datos
v<-c(0,0,0,1,1,1,0,0,0,0,0,0,1,0,1,0,0,0,0,0,0,0,0,0,0) 
d<-c(0,1,0,1,0,1,0,1,1,1,0,1,0,0,1,1,1,0,0,1,1,0,1,0,0) 
n_obs<-length(d) # El objeto 'n_obs' indica cu??ntas observaciones tenemos
datos_jags<-list('v','d','n_obs') 

# b) Modelo
write('model{
      
      # Distribuci??n Prior:
      theta_v~dunif(0,1)
      theta_d~dunif(0,1)
      
      # Diferencia entre tasas:
      dif<-theta_v-theta_d
      
      # Distribuci??n de los Datos:
      for(i in 1:n_obs){
      v[i]~dbern(theta_v)
      d[i]~dbern(theta_d)
      }
      
      }','ejemplo5.bug')

# c) Par??metros
parametros<-c('theta_v','theta_d','dif') 

# d) Valores Iniciales
iniciales<-list( 
  list(theta_v=0.2,theta_d=0.3),
  list(theta_v=0.8,theta_d=0.8),
  list(theta_v=0.5,theta_d=0.6)
)

# e) Sampleo
# En esta secci??n inclu??mos toda la informaci??n necesaria para que jags calcule la(s) distribuci??n(es) posteriores sobre los par??metros de inter??s:
posteriores_ej5<-jags( 
  data=datos_jags, 
  inits=iniciales, 
  parameters.to.save=parametros, 
  model.file='ejemplo5.bug',
  n.chains=3,
  n.iter=2500, 
  n.burnin=500, 
  n.thin=2, 
  DIC=T) 

# f) Resultados
#traceplot(posteriores_ej5)
summary(posteriores_ej5$BUGSoutput$summary)
posteriores_ej5

# Extracci??n de sampleos posteriores de cada par??metro (y distribuciones predictivas):
theta_v<-posteriores_ej5$BUGSoutput$sims.list$theta_v
theta_d<-posteriores_ej5$BUGSoutput$sims.list$theta_d
dif<-posteriores_ej5$BUGSoutput$sims.list$dif

# Distribuciones posteriores marginales:
hist(theta_v,breaks=100,col='black',xlim=c(0,1))
hist(theta_d,breaks=100,col='black',xlim=c(0,1))

# Distribuci??n posterior de la diferencia entre tasas:
hist(dif,breaks=100,col='black',xlim=c(-1,1))





# EJEMPLO 6: MUCHAS observaciones Bernoulli con cambio de tasa
rm(list=ls())
library('R2jags')

# a) Datos
# setwd("~/Documents/Luis/Courses and Workshops/Winter 2014-2015 Proba Camp/Jan2015/Data") # Esta instrucci??n especifica en qu?? directorio estamos trabajando
datos<-read.csv("Jan2015_DataSetChangePoint.csv") # El archivo 'Participante_2.csv' debe estar en el directorio de trabajo
elecciones<-datos$puerta_elegida # Extrae la columna 'puerta_elegida' del objeto 'datos' y la guarda en 'elecciones'
ensayos<-1:nrow(datos) # Crea un vector con n??meros enteros desde 1 hasta el n??mero de renglones de 'datos'
n_obs<-length(elecciones) # El objeto 'n_obs' indica cu??ntas observaciones contiene el objeto 'elecciones'
datos_jags<-list('elecciones','n_obs','ensayos') 

# b) Modelo 6a: Sin cambio de tasa
write('model{
      
      # Distribuci??n Prior:
      theta~dunif(0,1)
      
      # Distribuci??n de los Datos:
      for(i in 1:n_obs){
      elecciones[i]~dbern(theta)
      }
      
      # Distribuci??n Predictiva
      for(i in 1:n_obs){
      x_predictiva[i]~dbern(theta)
      }
      
      }','ejemplo6.bug')

# c) Par??metros
parametros<-c('theta','x_predictiva') 

# d) Valores Iniciales
iniciales<-list( 
  list(theta=0.2),
  list(theta=0.8),
  list(theta=0.5)
)

# e) Sampleo
# En esta secci??n inclu??mos toda la informaci??n necesaria para que jags calcule la(s) distribuci??n(es) posteriores sobre los par??metros de inter??s:
posteriores_ej6<-jags( 
  data=datos_jags, 
  inits=iniciales, 
  parameters.to.save=parametros, 
  model.file='ejemplo6.bug',
  n.chains=3,
  n.iter=2500, 
  n.burnin=500, 
  n.thin=2, 
  DIC=T) 

# f) Resultados
#traceplot(posteriores_ej6)
summary(posteriores_ej6$BUGSoutput$summary)
posteriores_ej6

# Extracci??n de sampleos posteriores de cada par??metro (y distribuciones predictivas):
theta<-posteriores_ej6$BUGSoutput$sims.list$theta
x_predictiva<-posteriores_ej6$BUGSoutput$sims.list$x_predictiva

# Distribuciones posteriores marginales:
hist(theta,breaks=100,col='black',xlim=c(0,1))

# ??El modelo es adecuado?
plot(0,xlim=c(1,n_obs),ylim=c(1,n_obs),type='n')
for(k in 1:dim(x_predictiva)[1]){
  lines(cumsum(x_predictiva[k,]),col=rgb(190,190,190,maxColor=255,alpha=20))
}
lines(cumsum(elecciones),lwd=2)


# b) Modelo 6b: Con cambio de tasa
write('model{
      
      # Distribuci??n Prior:
      theta[1]~dunif(0,1)
      theta[2]~dunif(0,1)
      
      # Prior sobre punto de cambio
      pto_cambio~dcat(pi[])
      for(i in 1:n_obs){
      pi[i]<-1/n_obs
      }
      
      # Indice del punto de cambio:
      for(i in 1:n_obs){
      z[i]<-step(ensayos[i]-pto_cambio)+1
      }
      
      # Distribuci??n de los Datos:
      for(i in 1:n_obs){
      elecciones[i]~dbern(theta[z[i]])
      }
      
      # Distribuci??n Predictiva
      for(i in 1:n_obs){
      x_predictiva[i]~dbern(theta[z[i]])
      }
      
      }','ejemplo6b.bug')

# c) Par??metros
parametros<-c('theta','x_predictiva','pto_cambio') 

# d) Valores Iniciales
iniciales<-list( 
  list(theta=c(0.2,0.2)),
  list(theta=c(0.7,0.7)),
  list(theta=c(0.9,0.9))
)

# e) Sampleo
# En esta secci??n inclu??mos toda la informaci??n necesaria para que jags calcule la(s) distribuci??n(es) posteriores sobre los par??metros de inter??s:
posteriores_ej6b<-jags( 
  data=datos_jags, 
  inits=iniciales, 
  parameters.to.save=parametros, 
  model.file='ejemplo6b.bug',
  n.chains=3,
  n.iter=2500, 
  n.burnin=500, 
  n.thin=2, 
  DIC=T) 

# f) Resultados
#traceplot(posteriores_ej6b)
summary(posteriores_ej6b$BUGSoutput$summary)
posteriores_ej6b

# Extracci??n de sampleos posteriores de cada par??metro (y distribuciones predictivas):
theta<-posteriores_ej6b$BUGSoutput$sims.list$theta
x_predictiva<-posteriores_ej6b$BUGSoutput$sims.list$x_predictiva
pto_cambio<-posteriores_ej6b$BUGSoutput$sims.list$pto_cambio

# Distribuciones posteriores marginales:
hist(theta,breaks=100,col='black',xlim=c(0,1))
hist(theta[,1],breaks=100,col='black',xlim=c(0,1))
hist(theta[,2],breaks=100,col='black',xlim=c(0,1))
hist(pto_cambio,breaks=100,col='black',xlim=c(1,100))


# ??El modelo es adecuado?
plot(0,xlim=c(1,n_obs),ylim=c(1,n_obs),type='n')
for(k in 1:dim(x_predictiva)[1]){
  lines(cumsum(x_predictiva[k,]),col=rgb(190,190,190,maxColor=255,alpha=20))
}
lines(cumsum(elecciones),lwd=2)

