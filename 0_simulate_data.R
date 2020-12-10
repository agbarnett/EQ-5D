# 0_simulate_data.R
# simulate EQ-5D data
# August 2020
library(readxl)
library(dplyr)
source('99_functions.R')

## key variables
simulate_data = function(
N = 250, # sample size per gender
thresholds = c(-1.75,1), # thresholds for determining numbers of good, fair, poor answers
noise.SD = 1, # standard deviation of the noise on the logit scale
# latent QoL:
shape1_men=3, shape2_men=1, # wide spread, but lean towards higher score
shape1_women=2, shape2_women=1){ 

# estimate mean sex difference
mean_men = shape1_men / (shape1_men + shape2_men)
mean_women = shape1_women / (shape1_women + shape2_women)
sex_effect = mean_women - mean_men

# 1) create latent QoL that depends on sex
latent_qol_men = rbeta(N, shape1=shape1_men, shape2=shape2_men) # 
latent_qol_women = rbeta(N, shape1=shape1_women, shape2=shape2_women) # 

# 2) create score from five questions dependent on latent QoL
score_men = create_Qs(input=latent_qol_men, thresholds=thresholds, noise.SD=noise.SD)
score_women = create_Qs(input=latent_qol_women, thresholds=thresholds, noise.SD=noise.SD)

# 3) create VAS dependent on latent QoL
VAS_men = create_VAS(input=latent_qol_men, noise.SD=noise.SD)
VAS_women = create_VAS(input=latent_qol_women, noise.SD=noise.SD)

# 4) combine
men = data.frame(sex=0, EQ5D_VAS=VAS_men, EQ5D=score_men) # match names from real data
women = data.frame(sex=1, EQ5D_VAS=VAS_women, EQ5D=score_women)
sim_data = bind_rows(men, women)

return(sim_data)

} # end of function

## save ##
#save(sim_data, sex_effect, file='data/simulated_eq5d.RData')
