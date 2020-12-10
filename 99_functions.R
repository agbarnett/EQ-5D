# 99_functions.R
# handy functions
# august 2020
library(eq5d)

# function for rounding numbers with zeros kept
roundz = function(x, digits){
  dformat = paste('%.', digits, 'f', sep='')
  x = sprintf(dformat, round(x, digits))
  return(x)
}


## Add noise to latent QoL to make VAS
# for simulating data
# input = latent QoL
# noise standard deviation is on logit scale
create_VAS = function(input, noise.SD=1){
  N = length(input)
  logit = log(input/ (1-input)) # transform QOL to logit
  noise = rnorm(N, mean=0, sd=noise.SD)
  logit = logit + noise
  output = exp(logit) / (1+exp(logit)) # back-transform to [0,1]
  return(output)
}

## Add noise to latent QoL to make five questions, using three-level response
# five dimensions; mobility, self-care, usual activities, pain/discomfort, and anxiety/depression
# input = latent QoL
# thresholds = are on logit scale
# for simulating data
create_Qs = function(input, thresholds = c(-1.75,1), noise.SD=1){
  Q = 5 # number of questions
  N = length(input)
  logit = log(input/ (1-input)) # transform QOL to logit
  logit = logit * (-1) # reverse, as 1 is high QoL
  noise = matrix(rnorm(N*Q, mean=0, sd=noise.SD), ncol=5)
  logit = matrix(rep.int(logit, 5), ncol=5) # repeat latent QoL over all questions
  logit = logit + noise
  # responses are 1 = no problems, 2 = some problems, 3 = big problems
  questions = matrix(1, ncol=Q, nrow=N) + (logit>thresholds[1]) + (logit>thresholds[2])
  q = as.numeric(apply(questions, 1, function(x) paste(x, collapse = ""))) # collapse into 5 digit string
  score = eq5d(scores=q, type = "TTO", country = "Australia", version='3L')
  return(score)
}

