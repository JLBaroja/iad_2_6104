# Investigación y Análisis de Datos II
# Prof. José Luis Baroja
# Febrero 2017

# Inferencia Bayesiana con JAGS y R.
# Este código presenta varios ejemplos que muestran como aproximar distribuciones posteriores utilizando R y JAGS.
# Para que el código funcione es necesario instalar 'JAGS' en la compu [http://mcmc-jags.sourceforge.net/] y también el paquete 'R2jags' en R [install.packages('R2jags')].

# EJEMPLO 1: UNA observación binomial, UN parámetro desconocido
rm(list=ls()) # Esta instrucción borra todos los objetos en el espacio de trabajo de R (limpia memoria antes de empezar).
library('R2jags') # 'Activa' el paquete que permite a R interactuar con JAGS. 
                  # (Es necesario instalar 'JAGS' Y el paquete de R 'R2jags' para que el resto del código funcione; ver línea 7.)

# a) Datos
# En esta sección 'vaciamos' nuestras observaciones en objetos interpretables por R:
n<-15 # Número de ensayos Bernoulli observados.
x<-4 # Número de éxitos observados.
datos_jags<-list('n','x') # 'datos_jags' es una lista que indica en qué objetos están los datos observados.

# b) Modelo
# En esta parte del código traducimos el modelo gráfico que relaciona nuestros datos y nuestros parámetros
# en código interpretable por JAGS:
write('model{

      # Distribución Prior (sobre theta, parámetro desconocido):
      theta~dunif(0,1)

      # Distribución de los Datos (en función de parámetros conocidos y desconocidos): 
      x~dbinom(theta,n)

}','ejemplo1.bug')

# c) Parámetros
parametros<-c('theta') # En esta línea especificamos qué parámetros nos interesa rastrear.

# d) Valores Iniciales
iniciales<-list( # Este objeto contiene los valores iniciales para cada cadena (3 en este ejemplo).
  list(theta=.2),
  list(theta=.8),
  list(theta=.5)
  )

# e) Sampleo
# En esta sección incluímos toda la información necesaria para que jags calcule la(s) distribución(es) posterior(es) sobre el(los) parámetro(s) de interés:
posteriores_ej1<-jags( 
  data=datos_jags, # Especificamos la lista con los nombres de los objetos que contienen las observaciones (ver línea 18)
  inits=iniciales, # Especificamos los valores iniciales de cada cadena (ver líneas 37-41)
  parameters.to.save=parametros, # Especificamos los nombres de los parámetros que queremos rastrear (ver línea 34)
  model.file='ejemplo1.bug', # Especificamos cuál es el archivo del modelo gráfico (no olvidar comillas!; ver líneas 21-31)
  n.chains=3, # Especificamos cuántas cadenas queremos correr (debe coincidir con el número de sublistas de 'iniciales'; ver líneas 37-41 otra vez)
  n.iter=500, # Especificamos el número total de sampleos por cadena
  n.burnin=100, # Especificamos cuántos sampleos queremos desechar al principio del proceso de sampleo
  n.thin=1, # Especificamos cada cuántos sampleos queremos conservar del total restante
  DIC=T) 
