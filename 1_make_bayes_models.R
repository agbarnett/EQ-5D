# 1_make_bayes_models.R
# make the Bayesian models ready for WinBUGS
# August 2020

# a) single outcome
bfile = 'EQ5D.bugs.single.outcome.txt'
bugs = file(bfile, 'w')
cat('model{
    for (i in 1:N){
      Y[i] ~ dnorm(mu[i], tau)
      mu[i] <- beta[1] + beta[2]*sex[i]', file=bugs)
if(add_age==TRUE){cat('+ beta[3]*age[i]', file=bugs)}
  cat('
  }
    beta[1] ~ dbeta(1, 1)
    beta[2] ~ dnorm(0, 0.0001)
    ', file=bugs)
  if(add_age==TRUE){cat('beta[3] ~ dnorm(0, 0.0001)
    ', file=bugs)}
   cat('tau ~ dgamma(1, 1)
    sigma <- 1/sqrt(tau) # standard deviation
    pred[1] <- beta[1]
    pred[2] <- beta[1] + beta[2]
    }', file=bugs)
close(bugs)

# c) multiple outcomes, alternative parameterisation for means
bfile = 'EQ5D.bugs.multiple.outcomes.txt'
bugs = file(bfile, 'w')
cat('model{
    for (i in 1:N){
      Y[i, 1:2] ~ dmnorm(mu[i,1:2], Omega[1:2, 1:2])
      mu[i,1] <- beta[1] + beta[3]*sex[i]', file=bugs)
if(add_age==TRUE){cat('+ beta[4]*age[i]', file=bugs)}
cat('
      mu[i,2] <- beta[1] + beta[2] + beta[3]*sex[i]', file=bugs)
if(add_age==TRUE){cat('+ beta[4]*age[i]', file=bugs)}
cat('
    }
    beta[1] ~ dbeta(1, 1)
    beta[2] ~ dbeta(1, 1)
    beta[3] ~ dnorm(0, 0.0001)', file=bugs)
if(add_age==TRUE){cat('
    beta[4] ~ dnorm(0, 0.0001)', file=bugs)}
cat('
    Omega[1:2, 1:2] ~ dwish(R[1:2, 1:2], 2)
    sigma[1:2, 1:2] <- inverse(Omega[1:2, 1:2])
    pred[1] <- beta[1]
    pred[2] <- beta[1] + beta[3] # plus sex
}', file=bugs)
close(bugs)

# b) multiple outcomes - original parameterisation
bfile = 'EQ5D.bugs.multiple.outcomes.txt'
bugs = file(bfile, 'w')
cat('model{
    for (i in 1:N){
      Y[i, 1:2] ~ dmnorm(mu[i,1:2], Omega[1:2, 1:2])
      mu[i,1] <- beta[1] + beta[3]*sex[i]', file=bugs)
if(add_age==TRUE){cat('+ beta[4]*age[i]', file=bugs)}
cat('
      mu[i,2] <- beta[2] + beta[3]*sex[i]', file=bugs)
if(add_age==TRUE){cat('+ beta[4]*age[i]', file=bugs)}
cat('
    }
    beta[1] ~ dbeta(1, 1)
    beta[2] ~ dbeta(1, 1)
    beta[3] ~ dnorm(0, 0.0001)', file=bugs)
if(add_age==TRUE){cat('
    beta[4] ~ dnorm(0, 0.0001)', file=bugs)}
cat('
    Omega[1:2, 1:2] ~ dwish(R[1:2, 1:2], 2)
    sigma[1:2, 1:2] <- inverse(Omega[1:2, 1:2])
    pred[1] <- beta[1]
    pred[2] <- beta[1] + beta[3] # plus sex
}', file=bugs)
close(bugs)