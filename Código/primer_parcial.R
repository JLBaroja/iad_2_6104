# Primer Parcial
# IAD 

rm(list=ls())

sup_f <- 0:20
sup_s <- 0:12
sup_b <- 0:18
sup_t <- 0:15

p_f <- dhyper(sup_f,
              m=31,
              n=57-31,
              k=20)

p_fs <- array(dim=c(length(sup_f),length(sup_s)))
p_fsb <- array(dim=c(length(sup_f),length(sup_s),length(sup_b)))
p_fst <- array(dim=c(length(sup_f),length(sup_s),length(sup_t)))


for(ff in 1:length(sup_f)){
  for(ss in 1:length(sup_s)){
    p_fs[ff,ss] <- p_f[ff]*dhyper(sup_s[ss],
                                  m=sup_f[ff],
                                  n=20-sup_f[ff],
                                  k=12)
  }
}


p_s <- apply(p_fs,MARGIN=2,FUN=sum)
layout(1:2)
plot(sup_s,p_s,col='blue')

plot(sup_s,p_s,col='red')

for(ff in 1:length(sup_f)){
  for(ss in 1:length(sup_s)){
    p_fs[ff,ss] <- p_f[ff]*dhyper(sup_s[ss],
                                  m=sup_f[ff],
                                  n=20-sup_f[ff],
                                  k=12)
    for(bb in 1:length(sup_b)){
      p_fsb[ff,ss,bb] <- p_fs[ff,ss]*dbinom(sup_b[bb],
                                            prob=sup_s[ss]/12,
                                            size=18)
    }
    for(tt in 1:length(sup_t)){
      p_fst[ff,ss,tt] <- p_fs[ff,ss]*dgeom(sup_t[tt],
                                           prob=sup_s[ss]/12)
    }
  }
}

p_s <- apply(p_fs,MARGIN=2,FUN=sum)
p_b <- apply(p_fsb,MARGIN=3,FUN=sum)
p_t <- apply(p_fst,MARGIN=3,FUN=sum,na.rm=T)

p_fb <- array(dim=c(length(sup_f),length(sup_b)))
for(ff in 1:length(sup_f)){
  for(bb in 1:length(sup_b)){
    p_fb[ff,bb] <- sum(p_fsb[ff,,bb])
  }
}

p_ft <- array(dim=c(length(sup_f),length(sup_t)))
for(ff in 1:length(sup_f)){
  for(tt in 1:length(sup_t)){
    p_ft[ff,tt] <- sum(p_fst[ff,,tt],na.rm=T)
  }
}


layout(1)
par(mar=rep(5,4))
persp(x=sup_f,y=sup_b,z=p_fb,
      phi=30,theta=90,
      ticktype = 'detailed',col='#22998844')

par(mar=rep(5,4))
persp(x=sup_f,y=sup_t,z=p_ft,
      phi=30,theta=90,
      ticktype = 'detailed',col='#6633ee44')



# Inference
obs_b <- 14
slice_b <- which(sup_b==obs_b) 
pos_fs <- p_fsb[,,slice_b]/sum(p_fsb[,,slice_b])
pos_f <- apply(pos_fs,MARGIN=1,FUN=sum)
pos_s <- apply(pos_fs,MARGIN=2,FUN=sum)

layout(matrix(1:4,ncol=2))
par(mar=rep(5,4))
persp(x=sup_f,y=sup_s,z=p_fs,
      phi=30,theta=90,
      ticktype = 'detailed',col='#22998844')
persp(x=sup_f,y=sup_s,z=pos_fs,
      phi=30,theta=90,
      ticktype = 'detailed',col='#22998844')
plot(sup_f,pos_f,pch=16,type='o');points(sup_f,p_f,lty='dashed',type='o')
plot(sup_s,pos_s,pch=16,type='o');points(sup_s,p_s,lty='dashed',type='o')


obs_t <- 7
slice_t <- which(sup_t==obs_t) 
pos_fs <- p_fst[,,slice_t]/sum(p_fst[,,slice_t],na.rm=T)
pos_f <- apply(pos_fs,MARGIN=1,FUN=sum,na.rm=T)
pos_s <- apply(pos_fs,MARGIN=2,FUN=sum,na.rm=T)

layout(matrix(1:4,ncol=2))
par(mar=rep(5,4))
persp(x=sup_f,y=sup_s,z=p_fs,
      phi=30,theta=90,
      ticktype = 'detailed',col='#6633ee44')
persp(x=sup_f,y=sup_s,z=pos_fs,
      phi=30,theta=90,
      ticktype = 'detailed',col='#6633ee44')
plot(sup_f,pos_f,pch=16,type='o');points(sup_f,p_f,lty='dashed',type='o')
plot(sup_s,pos_s,pch=16,type='o');points(sup_s,p_s,lty='dashed',type='o')














layout(matrix(1:6,ncol=3))
persp(x=sup_f,y=sup_s,z=p_fs,
      phi=40,theta=70,
      ticktype = 'detailed',col='#22dd8844')
plot(sup_f,p_f,pch=16,type='o')
plot(sup_s,p_s,pch=16,type='o')
plot(sup_b,p_b,pch=16,type='o')
plot(sup_t,p_t,pch=16,type='o')

layout(matrix(1:15,ncol=5))
par(mar=rep(1,4))
for(ss in 1:length(sup_s)){
  persp(x=sup_f,y=sup_b,z=p_fsb[,ss,],
        phi=40,theta=90,
        ticktype = 'detailed',col='#22998844')
}

