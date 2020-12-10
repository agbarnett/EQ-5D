# 2_plot_means_by_sex.R
# plot the observed results/means across eq-5d by sex
# August 2020
library(dplyr)
library(tidyr)
library(ggplot2)

# get the Sri Lanka and Wounds data
load('data/EQ5D.RData') # from 0_read_data.R

# which data set? only available for Sri Lanka
which_data = 'sri_lanka'
if(which_data == 'sri_lanka'){
  data = sri_lanka
  add_age = TRUE
}

# get means by sex
to_plot = select(data, -age) %>%
  gather(value='res', key='var', -`sex`) %>%
  group_by(sex, var) %>%
  summarise(mean = mean(res)) 
  # for ordering

#
splot = ggplot(to_plot, aes(x=var, y=mean, col=factor(sex), shape=factor(sex)))+
  geom_point(size=5)+
  scale_color_manual('Sex', values=c("#EE2C2C", "#528B8B"), labels=c('Male','Female'))+
  scale_shape_manual(NULL, values=c(2,19))+
  theme_bw()+
  xlab('')
splot

## plot questions by sex
# get proportions
to_plot_bar = select(data, -age) %>%
  gather(value='res', key='var', -`sex`) %>%
  filter(substr(var,1,4) == 'Eq5D') %>%
  group_by(sex, var, res) %>%
  summarise(n = n()) %>%
  group_by(sex, var) %>%
  mutate(
    var = str_remove(var, 'Eq5D'),
    N = sum(n), prop = n/N) 
  #
bar_plot = ggplot(data=to_plot_bar, aes(x=sex, y=prop, fill=factor(res)))+
  geom_bar(stat='identity', position='stack')+
  theme_bw()+
  scale_fill_manual('Response', values=c("#009ACD", "#E066FF", "#CD0000"), labels=c('Perfect','Okay','Poor'))+
  xlab('')+
  ylab('Proportion')+
  scale_x_continuous(breaks=0:1, labels=c('Male','Female'))+
  facet_wrap(~var)+
  theme(legend.position=c(0.85,0.15))

outfile  = paste('figures/bar_plot_sex_',which_data,'.jpg',sep='')
jpeg(outfile, width=5, height=4, units='in', res=500)
print(bar_plot)
dev.off()
