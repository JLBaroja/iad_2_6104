
rm(list=ls())
library('R2jags')

# Single Bernoulli observation (Wagenmakers et al., 'Another Statistical Paradox')
observation <- 1 
data_list <- list('observation')

# First (incorrect) solution: model selection as interval estimation
write('
      model{
      theta_prior~dbeta(0.05,0.05)
      theta~dbeta(0.05,0.05)
      observation~dbern(theta)
      }','model_selection.bug')
parameters <- c('theta','theta_prior')
posteriors <- jags(
  data=data_list,
  parameters.to.save=parameters,
  model.file='model_selection.bug',
  n.chains=4,
  n.iter=5000,
  n.burnin=2000,
  n.thin=3)
unlink('model_selection.bug')
theta_prior <- posteriors$BUGSoutput$sims.list$theta_prior
theta <- posteriors$BUGSoutput$sims.list$theta
layout(1:2)
hist(theta_prior,
     main=expression(Prior(theta)),
     ylim=c(0,3750),xlim=c(0,1),breaks=seq(0,1,length.out=50),
     col='#ee880044')
abline(v=0.5,lty='dashed')
lines(quantile(theta_prior,prob=c(.05,.975)),c(3500,3500),lwd=15)
lines(quantile(theta_prior,prob=c(.05,.975)),c(3500,3500),lwd=6,col='#ee8800')
hist(theta,
     main=expression(Posterior(theta)),
     ylim=c(0,3750),xlim=c(0,1),breaks=seq(0,1,length.out=50),
     col='#ee000077')
abline(v=0.5,lty='dashed')
lines(quantile(theta,prob=c(.05,.975)),c(3500,3500),lwd=15)
lines(quantile(theta,prob=c(.05,.975)),c(3500,3500),lwd=6,col='#ee0000')


# Second (closer to correct?) solution: model selection as a-very-special-kind-of-parameter estimation
write('
      model{
      the_alternative_prior~dbeta(0.05,0.05)
      the_alternative~dbeta(0.05,0.05)
      the_null <- 1/2 
      selector_node_prior~dbern(1/2) # Same probability to the null than to alternative
      selector_node~dbern(1/2) # Same probability to the null than to alternative
      
      observation_parent <- the_null*equals(selector_node,0)+ # If selector_node is 0, the null is the case
                            the_alternative*equals(selector_node,1) # If selector_node is 1, the alternative is the case
      observation~dbern(observation_parent)
      }','model_selection_proper.bug')
parameters <- c('the_alternative','the_alternative_prior',
  'selector_node','selector_node_prior',
                'observation_parent')
posteriors <- jags(
  data=data_list,
  parameters.to.save=parameters,
  model.file='model_selection_proper.bug',
  n.chains=4,
  n.iter=5000,
  n.burnin=2000,
  n.thin=3)
unlink('model_selection_proper.bug')
the_alternative_prior <- posteriors$BUGSoutput$sims.list$the_alternative_prior
the_alternative <- posteriors$BUGSoutput$sims.list$the_alternative
selector_node_prior <- posteriors$BUGSoutput$sims.list$selector_node_prior
selector_node <- posteriors$BUGSoutput$sims.list$selector_node
observation_parent <- posteriors$BUGSoutput$sims.list$observation_parent

layout(1:2)
hist(selector_node_prior,
     xlim=c(0,1),breaks=seq(0,1,length.out=50))
hist(selector_node,
     xlim=c(0,1),breaks=seq(0,1,length.out=50))

layout(1:2)
hist(the_alternative_prior,
     main=expression(Prior(theta)),
     ylim=c(0,3750),xlim=c(0,1),breaks=seq(0,1,length.out=50),
     col='#ee880044')
abline(v=0.5,lty='dashed')
lines(quantile(the_alternative_prior,prob=c(.05,.975)),c(3500,3500),lwd=15)
lines(quantile(the_alternative_prior,prob=c(.05,.975)),c(3500,3500),lwd=6,col='#ee8800')
hist(the_alternative,
     main=expression(Posterior(theta)),
     ylim=c(0,3750),xlim=c(0,1),breaks=seq(0,1,length.out=50),
     col='#ee000077')
abline(v=0.5,lty='dashed')
lines(quantile(the_alternative,prob=c(.05,.975)),c(3500,3500),lwd=15)
lines(quantile(the_alternative,prob=c(.05,.975)),c(3500,3500),lwd=6,col='#ee0000')

summary(selector_node_prior)



