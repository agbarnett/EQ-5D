# 1_run_bayes_models.R
# run the Bayesian models of EQ-5D
# August 2020
library(R2WinBUGS)
library(dplyr)
bugs_location = "c:/Program Files/WinBUGS14" # location of the file WinBUGS14.exe

# get the Sri Lanka and Wounds data
load('data/EQ5D.RData') # from 0_read_data.R
load('data/simulated_eq5d.RData') # from 0_simulate_data.R

## chain variables
MCMC = 5000; thin=3; n.chains=2; # number of chains, samples and thinning

# which data set?
which_data = 'simulated'
if(which_data == 'sri_lanka'){
  data = sri_lanka
  add_age = TRUE
}
if(which_data == 'wounds'){
  data = filter(wounds, !is.na( EQ5D_VAS)) # remove 4 missing
  add_age = FALSE
}
if(which_data == 'simulated'){
  data = sim_data # 
  add_age = FALSE
}

## make Bayes models as external txt files depending on data
source('1_make_bayes_models.R')

## prepare the data for winbugs
N = nrow(data) # sample size
# set up the data
bdata.qs = list(N = N, Y = data$EQ5D, sex=data$sex) 
bdata.vas = list(N = N, Y = data$EQ5D_VAS, sex=data$sex) # 
# prepare multivariate data for joint model
Y = as.matrix(dplyr::select(data, EQ5D, EQ5D_VAS)) # Questions as 1, VAS as 2
R = matrix(data=c(1,0,0,1), nrow=2)
bdata.both = list(N = N, Y = Y, R=R, sex=data$sex) #
# add age depending on data
if(add_age == TRUE){
  bdata.qs[['age']] = data$age
  bdata.vas[['age']] = data$age
  bdata.both[['age']] = data$age
}

# initial values for single variable outcome (use mean for intercept)
inits = list(beta=c(mean(data$EQ5D),0), tau=1) 
if(add_age == TRUE){inits$beta = append(inits$beta, 0)} # add zero for age
inits = rep(list(inits), n.chains) # repeat initial values per chain

## run WinBUGS once for each of the three models
# a) VAS single
parms = c('beta','pred','mu')
bugs.only.VAS = bugs(data=bdata.vas, inits=inits, parameters=parms, model.file='EQ5D.bugs.single.outcome.txt', DIC=FALSE,
                     n.chains=n.chains, n.iter=MCMC*thin*2, n.thin=thin, bugs.seed=87947, debug=FALSE,
                     bugs.directory=bugs_location)
# b) questions single
bugs.only.Qs = bugs(data=bdata.qs, inits=inits, parameters=parms, model.file='EQ5D.bugs.single.outcome.txt', DIC=FALSE,
                    n.chains=n.chains, n.iter=MCMC*thin*2, n.thin=thin, bugs.seed=87947, debug=FALSE,
                    bugs.directory=bugs_location)
# c) VAS and questions together
# update initial values for multivariate model
inits = list(beta=c(mean(data$EQ5D),mean(data$EQ5D_VAS),0), Omega=R) # 
if(add_age == TRUE){inits$beta = append(inits$beta, 0)} # add zero for age
inits = rep(list(inits), n.chains) # repeat initial values per chain
parms = c('beta','sigma','pred','mu')
bugs.both = bugs(data=bdata.both, inits=inits, parameters=parms, model.file='EQ5D.bugs.multiple.outcomes.txt', DIC=FALSE,
                 n.chains=n.chains, n.iter=MCMC*thin*2, n.thin=thin, bugs.seed=87947, debug=FALSE,
                 bugs.directory=bugs_location)


## save
outfile = paste('results/bugs_models_', which_data,'.RData', sep='')
save(bugs.only.VAS, bugs.only.Qs, bugs.both, file=outfile)
