# 1_run_bayes_models_simulations.R
# run the Bayesian models of EQ-5D
# versions for the simulated data
# August 2020
library(R2WinBUGS)
library(dplyr)
source('0_simulate_data.R')
bugs_location = "c:/Program Files/WinBUGS14" # location of the file WinBUGS14.exe

## chain variables
MCMC = 3000; thin=3; n.chains=2; # number of chains, samples and thinning

## parameters for simulations
add_age = FALSE
parms = list(N.sim=100, 
             N = 250, # sample size per gender
             thresholds = c(-1.75,1), # thresholds for determining numbers of good, fair, poor answers
             noise.SD = 1, # standard deviation of the noise on the logit scale
             shape1_men=3, shape2_men=1, # wide spread, but lean towards higher score
             shape1_women=3, shape2_women=1)

## big loop
simulation_results = NULL
for (i in 1:parms$N.sim){
  data = simulate_data(N=parms$N, noise.SD=parms$noise.SD, 
                       thresholds = parms$thresholds, shape1_men=parms$shape1_men,
                       shape2_men=parms$shape2_men, shape1_women=parms$shape1_women,
                       shape2_women=parms$shape2_women)
  
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
  parameters = c('pred')
  # a) questions single
  bugs.only.Qs = bugs(data=bdata.qs, inits=inits, parameters=parameters, model.file='EQ5D.bugs.single.outcome.txt', DIC=FALSE,
                      n.chains=n.chains, n.iter=MCMC*thin*2, n.thin=thin, bugs.seed=87947, debug=FALSE,
                      bugs.directory=bugs_location)
  # b) VAS and questions together
  # update initial values for multivariate model
  inits = list(beta=c(mean(data$EQ5D),mean(data$EQ5D_VAS),0), Omega=R) # 
  if(add_age == TRUE){inits$beta = append(inits$beta, 0)} # add zero for age
  inits = rep(list(inits), n.chains) # repeat initial values per chain
  bugs.both = bugs(data=bdata.both, inits=inits, parameters=parameters, model.file='EQ5D.bugs.multiple.outcomes.txt', DIC=FALSE,
                   n.chains=n.chains, n.iter=MCMC*thin*2, n.thin=thin, bugs.seed=87947, debug=FALSE,
                   bugs.directory=bugs_location)
  
  # store results
  r1 = bugs.only.Qs$summary[, c(1,2,3,7)]
  r1 = data.frame(r1) %>%
    mutate(sim=i, model = 'single', sex=1:2)
  r2 = bugs.both$summary[, c(1,2,3,7)] 
  r2 = data.frame(r2) %>%
    mutate(sim=i, model = 'combined', sex=1:2)
  
  simulation_results = bind_rows(simulation_results, r1, r2)
  
}

# minor edits
simulation_results = rename(simulation_results, 'lower' = 'X2.5.',
                              'upper' = 'X97.5.') %>%
  mutate(width = upper - lower)

# save
save(parms, simulation_results, file='results/simulation_results_4.RData')

