
# rm(list=ls())

K <- 7 # Sabemos que hay 7 exitos en la segunda baraja
n <- 7 # Sabemos que vamos a sacar "de a 7"

sup_X <- 0:7 # No sabemos cuántos éxitos vamos a observar 
sup_N <- 0:30 # No sabemos cuántas cartas quedaron en la baraja reducida

vector_datos <- c(3,2,2,3,1,2)

## Antes de la primera obs (diferentes possible priors)
# Prior "Luis"
prior_N <- rep(1/length(sup_N),length(sup_N))

# Prior "Eduardo"
prior_N <- dhyper(7,
                  m=10,
                  n=30-10,
                  k=0:30)
prior_N <- prior_N/sum(prior_N)

# Prior "Al"
prior_N <- dgeom(0:30,prob=.5)

plot(sup_N,prior_N)
# plot(sup_N,prior_N)

for(pos in 1:length(vector_datos)){
  
  likelihood <- array(dim=c(length(sup_N),length(sup_X)))
  joint <- array(dim=c(length(sup_N),length(sup_X)))
  for(NN in 1:length(sup_N)){
    for(XX in 1:length(sup_X)){
      likelihood[NN,XX] <- dhyper(sup_X[XX],
                                  m=K,
                                  n=sup_N[NN]-K,
                                  k=n)
      joint[NN,XX] <- likelihood[NN,XX]*prior_N[NN]
      #     print(c(sup_N[NN],sup_X[XX]))
    }
  }
  
  layout(1:2)
  par(mar=rep(1,4))
  persp(sup_N,sup_X,likelihood,col='#33dddd44',ticktype = 'detailed',theta=50)
  persp(sup_N,sup_X,joint,col='#33dd3344',ticktype = 'detailed',theta=50)
  
  
  # pos <- 1
  dato <- vector_datos[pos]
  slice_x <- which(sup_X==dato)
  sum(joint[,slice_x]/sum(joint[,slice_x],na.rm=T),na.rm=T)
  posterior_N <- joint[,slice_x]/sum(joint[,slice_x],na.rm=T)
  plot(sup_N,posterior_N,col='red')
  points(sup_N,prior_N,col='blue')
  prior_N <- posterior_N
  
}


# posterior_lalo <- posterior_N
# posterior_mia <- posterior_N
posterior_Al <- posterior_N

plot(sup_N,posterior_lalo)
points(sup_N,posterior_mia,col='green')
points(sup_N,posterior_Al,col='red')
