# 2_plot_results_simulations.R
# plot the estimates from the Bayesian models
# version for the simulations
# August 2020
library(ggplot2)
library(dplyr)
library(stringr)
library(tidyr)

# get the results 
to_load = dir('results', pattern='simulation') # get the results files
to_plot = NULL
for (f in to_load){
  infile = paste('results/', f, sep='')
  load(infile)
  
  # estimate mean group difference
  mean_men = parms$shape1_men / (parms$shape1_men + parms$shape2_men)
  mean_women = parms$shape1_women / (parms$shape1_women + parms$shape2_women)
  group_effect = mean_men - mean_women
  simulation_results = mutate(simulation_results,
                              group_effect=group_effect,
            group_effect_nice = paste('Difference = ', roundz(group_effect,3), sep=''))
  
  # concatenate
  to_plot = bind_rows(to_plot, simulation_results)
  
}

# prepare for plot
to_plot = mutate(to_plot, 
         sex_jitter = ifelse(model=='single', sex-0.05, sex+0.05)) # jitter

# get difference
wide = select(to_plot, group_effect, group_effect_nice, sim, model, sex, mean) %>%
  spread(sex, mean) %>%
  mutate(diff = `1` - `2`)
# for horizontal line
hline_dat = select(wide, group_effect_nice, group_effect) %>%
  unique() %>%
  mutate(model='combined', diff=0)

# estimated group (sex) difference
group_plot = ggplot(wide, aes(x=model, y=diff, col=factor(model)))+
  geom_boxplot()+
  geom_hline(data=hline_dat, aes(yintercept=group_effect), lty=2)+ # to update
  scale_x_discrete(breaks=1:2, labels=c('Questions + VAS','Questions only'))+ # 
  scale_colour_manual(NULL, values=c("#3A5FCD", "#CD2626"), labels=c('Questions + VAS','Questions only'))+ # 
  theme_bw()+
  ylab('Difference in EQ-5D')+
  xlab('')+
  coord_flip()+
  facet_wrap(~group_effect_nice)+
  theme(legend.position = 'top',
        legend.key.width = grid::unit(4, "lines")) # wider lines in legend
group_plot

# plot means
mean_plot = ggplot(to_plot, aes(x=factor(sex), y=mean, col=factor(model)))+
  geom_boxplot()+
  scale_x_discrete(breaks=1:2, labels=c('Group 1','Group 2'))+ # 
  scale_colour_manual(NULL, values=c("#3A5FCD", "#CD2626"), labels=c('Questions + VAS','Questions only'))+ # 
  theme_bw()+
  ylab('Model EQ-5D estimate')+
  xlab('')+
  coord_flip()+
  facet_wrap(~group_effect_nice)+
  theme(legend.position = 'top',
        legend.key.width = grid::unit(4, "lines")) # wider lines in legend
mean_plot

# plot CI width
width_plot = ggplot(to_plot, aes(x=factor(sex), y=width, col=factor(model)))+
  geom_boxplot()+
  scale_x_discrete(breaks=1:2, labels=c('Group 1','Group 2'))+ # 
  scale_colour_manual(NULL, values=c("#3A5FCD", "#CD2626"), labels=c('Questions + VAS','Questions only'))+ # 
  theme_bw()+
  ylab('Width of 95% credible interval')+
  xlab('')+
  coord_flip()+
  facet_wrap(~group_effect_nice)+
  theme(legend.position = 'top',
        legend.key.width = grid::unit(4, "lines")) # wider lines in legend
width_plot
  
# export figures
outfile = paste('figures/group.bias.simulated_combined.jpg', sep='')
jpeg(outfile, width=6, height=5, units='in', res=500)
print(group_plot)
dev.off()
outfile = paste('figures/group.means.simulated_combined.jpg', sep='')
jpeg(outfile, width=6, height=5, units='in', res=500)
print(mean_plot)
dev.off()
outfile = paste('figures/group.width.simulated_combined.jpg', sep='')
jpeg(outfile, width=6, height=5, units='in', res=500)
print(width_plot)
dev.off()

