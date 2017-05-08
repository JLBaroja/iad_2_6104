# Updating a joint prior over hypergeometric parameters
# d: die {1,2,...,6} (not shown)
# n = 16-d (arbitrary way to decide extraction size; influences num. of successes, too)
# K ~ Binomial(theta = n/20, size = 8) 
# X ~ Hypergeometric(total = 20, successes = K, extraction_size = n)

# We observed some X and inferred both n and K:

rm(list=ls())

# Variables
sup_X <- 0:8
sup_n <- 10:15
sup_K <- 0:8


# Functions
build_joint_nKX <- function(joint_nK){
  
  joint_n_K_X <- array(dim=c(length(sup_n),length(sup_K),length(sup_X)))
  mar_n_K <- array(dim=c(length(sup_n),length(sup_K)))
  mar_K_X <- array(dim=c(length(sup_K),length(sup_X)))
  mar_n_X <- array(dim=c(length(sup_n),length(sup_X)))
  mar_n <- apply(joint_n_K_X,MARGIN = 1,FUN=sum)
  mar_K <- apply(joint_n_K_X,MARGIN = 2,FUN=sum)
  mar_X <- apply(joint_n_K_X,MARGIN = 3,FUN=sum)
  
  for(nn in 1:length(sup_n)){
    for(KK in 1:length(sup_K)){
      for(XX in 1:length(sup_X)){
        joint_n_K_X[nn,KK,XX] <- joint_nK[nn,KK]*dhyper(x=sup_X[XX], # Support
                                                        m=sup_K[KK], # No. Successes
                                                        n=20-sup_K[KK], # No. Failures
                                                        k=sup_n[nn]) # Extraction Size
      }
    }
  }
  
  for(nn in 1:length(sup_n)){
    for(KK in 1:length(sup_K)){
      mar_n_K[nn,KK] <- sum(joint_n_K_X[nn,KK,])
    }
  }
  
  for(nn in 1:length(sup_n)){
    for(XX in 1:length(sup_X)){
      mar_n_X[nn,XX] <- sum(joint_n_K_X[nn,,XX])
    }
  }
  
  for(KK in 1:length(sup_K)){
    for(XX in 1:length(sup_X)){
      mar_K_X[KK,XX] <- sum(joint_n_K_X[,KK,XX])
    }
  }
  
  return(list(p_nKX=joint_n_K_X,
              p_n=mar_n,
              p_K=mar_K,
              p_X=mar_X,
              p_nK=mar_n_K,
              p_nX=mar_n_X,
              p_KX=mar_K_X))
}


bayesian_updater <- function(observed_X,prior_nK){
  
  posteriors_nK <- vector('list',length(observed_X))
  mar_pos_n <- vector('list',length(observed_X))
  mar_pos_K <- vector('list',length(observed_X))
  priors_nK <- vector('list',length(observed_X))
  
  for(ii in 1:length(observed_X)){
    
    if(ii==1){
      joint_nK <- prior_nK
    }
    else{
      joint_nK <- posterior_nK
    }
    
    jnt_nKX <- build_joint_nKX(joint_nK)
    slice_x <- which(sup_X==observed_X[ii])
    posterior_nK <- jnt_nKX$p_nKX[,,slice_x]/sum(jnt_nKX$p_nKX[,,slice_x])
    posteriors_nK[[ii]] <- posterior_nK
    mar_pos_n[[ii]] <- apply(posterior_nK,MARGIN=1,FUN=sum)
    mar_pos_K[[ii]] <- apply(posterior_nK,MARGIN=2,FUN=sum)
    priors_nK[[ii]] <- joint_nK
  }
  
  return(list(post_nK=posteriors_nK,
              mar_pos_n=mar_pos_n,
              mar_pos_K=mar_pos_K,
              prior_nK=priors_nK))
}



# Prior_nK_a (this closely resembles problem definition)
pr_n <- rep(1/6,6)
prior_nK_a <- array(dim=c(length(sup_n),
                          length(sup_K)))
for(nn in 1:length(sup_n)){
  for(KK in 1:length(sup_K)){
    prior_nK_a[nn,KK] <- dbinom(x=sup_K[KK],
                                size=8,
                                prob=sup_n[nn]/20)*pr_n[nn]
  }
}


# Prior_nK_b (this changes the assumption of dependence between n and K)
load('just_another_joint.Rdata')
prior_nK_b <- another_joint_n_K


# Data
datos_X <- c(3,2,2,3,2,4)


# Put everything to work!
inference_a <- bayesian_updater(datos_X,prior_nK = prior_nK_a)
inference_b <- bayesian_updater(datos_X,prior_nK = prior_nK_b)
color_a <- '#5588ee'
color_b <- '#55dd99'


# Plot joint posteriors_nK
layout(matrix(1:14,ncol=7,byrow=F))
par(mar=rep(.5,4))
for(i in 1:(length(inference_a$post_nK)+1)){
  th <- 130
  ph <- 30
  
  if(i==1){ 
    persp(x=sup_n,y=sup_K,z=inference_a$prior_nK[[1]],
          col=paste(color_a,'66',sep=''),theta=th,phi=ph,
          ticktype ='detailed',zlab='',
          main='P ( n,K )')
    persp(x=sup_n,y=sup_K,z=inference_b$prior_nK[[1]],
          col=paste(color_b,'66',sep=''),theta=th,phi=ph,
          ticktype ='detailed',zlab='',
          main='P ( n,K )')
  }
  else{
    j <- i-1
    persp(x=sup_n,y=sup_K,z=inference_a$post_nK[[j]],
          col=paste(color_a,'66',sep=''),theta=th,phi=ph,
          ticktype ='detailed',zlab='',
          main=bquote(bold(paste('P ( n,K | ',X[1:.(j)],' )',sep=''))))
    persp(x=sup_n,y=sup_K,z=inference_b$post_nK[[j]],
          col=paste(color_b,'66',sep=''),theta=th,phi=ph,
          ticktype ='detailed',zlab='',
          main=bquote(bold(paste('P ( n,K | ',X[1:.(j)],' )',sep=''))))
  }
}

# Plot marginal posteriors
layout(matrix(1:2,ncol=2))

plot(0,type='n',
     xlim=c(min(sup_n),max(sup_n)),ylim=c(0,.3),
     ylab='Posterior Density',xlab='sup_n')
for(i in 1:length(inference_a$post_nK)){
  polygon(x=rep(c(min(sup_n),max(sup_n)),each=2),
          y=c(0,.3,.3,0),col='#ffffff66',border=F)
  lines(sup_n,inference_a$mar_pos_n[[i]],col=color_a,lwd=5)
  lines(sup_n,inference_b$mar_pos_n[[i]],col=color_b,lwd=5)
}

plot(0,type='n',
     xlim=c(min(sup_K),max(sup_K)),ylim=c(0,.6),
     ylab='Posterior Density',xlab='sup_K')
for(i in 1:length(inference_a$post_nK)){
  polygon(x=rep(c(min(sup_K),max(sup_K)),each=2),
          y=c(0,.6,.6,0),col='#ffffff66',border=F)
  lines(sup_K,inference_a$mar_pos_K[[i]],col=color_a,lwd=5)
  lines(sup_K,inference_b$mar_pos_K[[i]],col=color_b,lwd=5)
}









