rm(list=ls())

sup_theta <- c(0,.2,.4,.6,.8,1)
prior_theta <- c(6/36,10/36,8/36,6/36,4/36,2/36)

obs_x_a <- c(1,0,1,0,0,1,0,0,1,1,0,0,0,0,0,1,1,1,1,0,0,0,1,1)
sup_x_a <- 0:1

obs_x_b <- c(sum(obs_x_a[1:12]),sum(obs_x_a[13:24]))
sup_x_b <- 0:12

obs_x_c <- sum(obs_x_a)
sup_x_c <- 0:24

obs_x_d <- c(sum(obs_x_a[1:4]),sum(obs_x_a[5:8]),
             sum(obs_x_a[9:12]),sum(obs_x_a[13:16]),
             sum(obs_x_a[17:20]),sum(obs_x_a[21:24]))
sup_x_d <- 0:4






bayesian_update <- function(obs_x,
                            sup_x){  
  #   obs_x <- obs_x_b
  #   sup_x <- sup_x_b
  bin_size <- max(sup_x)    
  joint <- array(dim=c(length(sup_x),length(sup_theta),length(obs_x)))
  likelihood <- array(dim=dim(joint))
  priors_theta <- array(dim=c(length(sup_theta),length(obs_x)))
  priors_theta[,1] <- prior_theta
  posteriors_theta <- array(dim=c(length(sup_theta),length(obs_x)))
  for(ii in 1:length(obs_x)){
    # ii <- 1
    for(xx in 1:length(sup_x)){
      for(tt in 1:length(sup_theta)){
        likelihood[xx,tt,ii] <- dbinom(sup_x[xx],size=bin_size,prob=sup_theta[tt])
        joint[xx,tt,ii] <- likelihood[xx,tt,ii]*priors_theta[tt,ii]
      }
    }
    data_index <- which(sup_x==obs_x[ii])
    posteriors_theta[,ii] <- joint[data_index,,ii]/sum(joint[data_index,,ii])
    if(ii!=length(obs_x)){
      priors_theta[,(ii+1)] <- posteriors_theta[,ii]
    }
  }
  
  return(list(joint=joint,
              priors=priors_theta,
              posteriors=posteriors_theta,
              sup_x=sup_x))
  
}

bu <- bayesian_update(obs_x_d,sup_x_d)

layout(1)
plot(0,type='n',xlim=c(1,length(sup_theta)),ylim=c(0,1),axes=F)
axis(1,at=1:length(sup_theta),labels=sup_theta)
axis(2)
for(xx in 1:dim(bu$posteriors)[2]){
  polygon(x=c(1,6,6,1),y=c(0,0,1,1),col='#ffffff35',border=F)
  lines(bu$posteriors[,xx],col='#0066ee',lwd=5)
}
lines(bu$priors[,1],lty='dashed',col='#ff5500',lwd=3)


layout(matrix(1:6,ncol=2,byrow=F))
par(mar=rep(2,4))
for(xx in 1:dim(bu$posteriors)[2]){
  persp(x=bu$sup_x,y=sup_theta,z=bu$joint[,,xx],
        col='#00ee3344',
        theta=115,phi=40,
        ticktype='detailed',
        main=paste('Obs',xx))
}







