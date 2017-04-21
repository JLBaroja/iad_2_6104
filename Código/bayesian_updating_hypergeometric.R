rm(list=ls())

# Moldelando el primer saquito
M=18
H=8
h=0:8
m=8
pmf_s1 <- dhyper(h,
                 m=H,
                 n=M-H,
                 k=m)

# Modelando el segundo saco
K=0:8

## Primera extracción:
prior_K=rep(NA,9)
for(i in 1:length(K)){
  prior_K[i] <- pmf_s1[(10-i)]
}

par(mar=c(4,1,1,1))
layout(1:2)
plot(h,pmf_s1)
plot(K,prior_K)

sup_X <- 0:4
joint_K_X <- array(dim=c(length(sup_X),length(K)))

for(kk in 1:length(K)){
  for(xx in 1:length(sup_X)){
    joint_K_X[xx,kk] <- dhyper(sup_X[xx],
                               m=K[kk],
                               n=10-K[kk],
                               k=4)*prior_K[kk]
  }
}

layout(1)
persp(sup_X,K,joint_K_X,col='#88662255',
      ticktype='detailed',theta=120,phi=30)

dato_X <- 2
posterior_K <- joint_K_X[which(sup_X==dato_X),]/sum(joint_K_X[which(sup_X==dato_X),])

plot(K,posterior_K,type='o',pch=16,col='blue')
points(K,prior_K,type='o',col='red')


## Segunda extracción:
prior_K=posterior_K
sup_X <- 0:4
joint_K_X <- array(dim=c(length(sup_X),length(K)))

for(kk in 1:length(K)){
  for(xx in 1:length(sup_X)){
    joint_K_X[xx,kk] <- dhyper(sup_X[xx],
                               m=K[kk],
                               n=10-K[kk],
                               k=4)*prior_K[kk]
  }
}

layout(1)
persp(sup_X,K,joint_K_X,col='#88662255',
      ticktype='detailed',theta=120,phi=30)

dato_X <- 1
posterior_K <- joint_K_X[which(sup_X==dato_X),]/sum(joint_K_X[which(sup_X==dato_X),])

plot(K,posterior_K,type='o',pch=16,col='blue')
points(K,prior_K,type='o',col='red')


## Siguientes extracciones:
vector_datos <- c(2,1,2)
for(dd in 1:length(vector_datos)){
  prior_K=posterior_K
  sup_X <- 0:4
  joint_K_X <- array(dim=c(length(sup_X),length(K)))
  
  for(kk in 1:length(K)){
    for(xx in 1:length(sup_X)){
      joint_K_X[xx,kk] <- dhyper(sup_X[xx],
                                 m=K[kk],
                                 n=10-K[kk],
                                 k=4)*prior_K[kk]
    }
  }
  
  layout(1)
  persp(sup_X,K,joint_K_X,col='#88662255',
        ticktype='detailed',theta=120,phi=30)
  
  dato_X <- vector_datos[dd]
  posterior_K <- joint_K_X[which(sup_X==dato_X),]/sum(joint_K_X[which(sup_X==dato_X),])
  
  plot(K,posterior_K,type='o',pch=16,col='blue')
  points(K,prior_K,type='o',col='red')
}