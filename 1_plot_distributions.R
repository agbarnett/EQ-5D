# 1_plot_distributions.R
# plot the distributions of eq5d and VAS in the two studies
# august 2020
library(dplyr)
library(tidyr)
library(ggplot2)
library(gridExtra)

# get the results for both data sets
load('data/EQ5D.RData') # from 0_read_data.R
all_data = bind_rows(sri_lanka, wounds, .id='study') %>%
  select(study, EQ5D, EQ5D_VAS) %>%
  gather(key='measure', value='res', -`study`) %>%
  mutate(measure = ifelse(measure=='EQ5D', 'EQ-5D', 'VAS'))

# now split data again!
hplot1 = ggplot(data=filter(all_data, study==1), aes(x=res))+
  geom_histogram(breaks=seq(0,1,0.1), fill='darkseagreen2')+
  ggtitle('Sri Lanka')+
  xlab('Quality of life')+
  theme_bw()+
  theme(panel.grid.minor = element_blank())+
  facet_wrap(~measure)
hplot1
hplot2 = ggplot(data=filter(all_data, study==2), aes(x=res))+
  geom_histogram(breaks=seq(0,1,0.1), fill='skyblue')+
  ggtitle('Wounds')+
  xlab('Quality of life')+
  theme_bw()+
  theme(panel.grid.minor = element_blank())+
  facet_wrap(~measure)
hplot2

jpeg('figures/distributions.jpg', width=5, height=5, units='in', res=400)
gridExtra::grid.arrange(hplot1, hplot2, ncol=1)
dev.off()
