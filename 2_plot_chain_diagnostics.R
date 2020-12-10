# 2_plot_chain_diagnostics.R
# plot the chains from the Bayesian models
# August 2020
library(ggplot2)
library(dplyr)
library(gridExtra)
library(stringr)

# results
which_data = 'sri_lanka'
which_data = 'wounds'
infile = paste('results/bugs_models_', which_data,'.RData', sep='')
load(infile) # from 1_run_bayes_models.R

# function
plot_chain = function(inres, title, var_to_plot='beta'){
  to.use1 = as.matrix(inres$sims.array[,1,]) # first chain
  to.use2 = as.matrix(inres$sims.array[,2,]) # second chain
  # switch from wide to long
  long1 = data.frame(to.use1) %>%
    mutate(iter = 1:n(), chain=1) %>%
    tidyr::gather(key='var', value='value', -iter, -chain)
  long2 = data.frame(to.use2) %>%
    mutate(iter = 1:n(), chain=2) %>%
    tidyr::gather(key='var', value='value', -iter, -chain)
  to.plot = bind_rows(long1, long2) %>%
    filter(str_detect(var, var_to_plot))  # remove predictions
  cplot.hrec = ggplot(data=to.plot, aes(x=iter, y=value, col=factor(chain)))+
    geom_line(lty=2)+
    theme_bw()+
    xlab('Iteration')+
    ylab('Estimate')+
    scale_color_manual('Chain', values=c('dark red','skyblue'))+
    ggtitle(title)+
    facet_wrap(~var, scales = 'free_y')
}

#
plot_vas = plot_chain(inres = bugs.only.VAS, title = 'VAS only')
plot_qs = plot_chain(inres = bugs.only.Qs, title = 'Questions only')
plot_both = plot_chain(inres = bugs.both, title = 'Combined')

# plot all three
outfile = paste('figures/check_chains_', which_data, '.jpg', sep='')
jpeg(outfile, width=5, height=7, units='in', res=500)
grid.arrange(grobs = list(plot_vas, plot_qs, plot_both), heights=c(1,1,1.7), ncol=1)
dev.off()
