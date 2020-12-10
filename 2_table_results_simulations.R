# 2_table_results_simulations.R
# tabulate the simulation results
# August 2020
library(ggplot2)
library(dplyr)
library(stringr)
library(tidyr)
source('99_functions.R')

# get the results 
to_load = dir('results', pattern='simulation') # get the results files
to_table = NULL
for (f in to_load){
  infile = paste('results/', f, sep='')
  load(infile)
  
  # estimate mean group difference
  mean_men = parms$shape1_men / (parms$shape1_men + parms$shape2_men)
  mean_women = parms$shape1_women / (parms$shape1_women + parms$shape2_women)
  group_effect = mean_men - mean_women
  
  # generate estimated group difference
  difference = select(simulation_results, sim, model, sex, mean) %>%
    spread(sex, mean) %>%
    mutate(diff = `1` - `2`) %>%
    group_by(model) %>%
    summarise(est = mean(diff),
              sd = sd(diff)) %>%
    ungroup() %>%
    mutate(cell = paste(roundz(est,4), ' (', roundz(sd,4), ')', sep='')) %>%
    select(model, cell) %>%
    spread(model, cell)
  
  # generate CI width reduction
  width = select(simulation_results, sim, model, sex, width) %>%
    group_by(sim, sex) %>%
    spread(model, width) %>%
    mutate(perc = 100*(combined - single) / single) %>%
    ungroup() %>%
    summarise(est = roundz(mean(perc),1)) 
  
  # concatenate
  frame = bind_cols(group_effect, difference, width)
  to_table = bind_rows(to_table, frame)
  
}

to_table = rename(to_table, 
                  'Difference' = '...1',
                  'Percent reduction' = 'est') %>%
  arrange(Difference)
to_table
