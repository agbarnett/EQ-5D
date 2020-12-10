# 2_plot_results.R
# plot the estimates from the Bayesian models
# August 2020
library(ggplot2)
library(dplyr)
library(stringr)
library(tidyr)

# get the results for both data sets
results = NULL
correlations = list()
for (which_data in c('sri_lanka','wounds')){
  infile = paste('results/bugs_models_', which_data,'.RData', sep='')
  load(infile) # from 1_run_bayes_models.R
  
  # examine the predictions for gender
  res1 = data.frame(bugs.only.Qs$summary[grep('pred',rownames(bugs.only.Qs$summary)), c(1,2,3,7)])
  res1$var = rownames(res1)
  res2 = data.frame(bugs.only.VAS$summary[grep('pred',rownames(bugs.only.VAS$summary)), c(1,2,3,7)]) 
  res2$var = rownames(res2)
  res3 = data.frame(bugs.both$summary[grep('pred', rownames(bugs.both$summary)),c(1,2,3,7)]) 
  res3$var = rownames(res3)
  
  # concatenate
  concat = bind_rows(res1, res3, .id = 'type') %>% # just questions and combined, not vAS
    mutate(study = which_data)
  results = bind_rows(results, concat)
  
  # examine the correlation in the multi-variate normal
  cor = data.frame(bugs.both$summary[grep('sigma', rownames(bugs.both$summary)),c(1,2,3,7)]) 
  V = matrix(cor$mean, ncol=2) # variance-covariance matrix...
  C = cov2cor(V) #... correlation matrix
 
  correlations[[which_data]] = C
}

# prepare for plot
to_plot = mutate(results, 
         sex = as.numeric(str_remove_all(string=var, pattern='[^0-9]')),
         type = as.numeric(type),
         sex_jitter = ifelse(type==1, sex-0.05, sex+0.05), # jitter
         study_nice = ifelse(study=='wounds', 'Wounds', "Sri Lanka")) %>% 
  rename('lower' = 'X2.5.',
         'upper' = 'X97.5.') %>%
  mutate(width = upper - lower)

# plot
compare_plot = ggplot(to_plot, aes(x=sex_jitter, y=mean, ymin=lower, ymax=upper, col=factor(type)))+
  geom_point(size=4)+
  geom_errorbar(width=0, size=1.1)+
  scale_x_continuous(breaks=1:2, labels=c('Male','Female'))+ # 
  scale_colour_manual(NULL, values=c("#3A5FCD", "#CD2626"), labels=c('Questions only','Questions + VAS'))+ # 
  theme_bw()+
  ylab('Model EQ-5D estimate')+
  xlab('')+
  facet_wrap(~study_nice, scale='free_x')+
  coord_flip()+
  theme(legend.position = 'top',
        legend.key.width = grid::unit(4, "lines")) # wider lines in legend
compare_plot
  
# export
outfile = paste('figures/sex.both.results.jpg', sep='')
jpeg(outfile, width=6, height=5, units='in', res=500)
print(compare_plot)
dev.off()

# widths
wide = select(to_plot, study_nice, sex, type, width) %>%
  spread(type, width) %>%
  mutate(reduction = 100 * (`2` - `1`)/`1`,
         reduction = round(reduction, 1)) %>%
  select(study_nice, sex, reduction)
wide

# correlations in multi-variate Normal
correlations
