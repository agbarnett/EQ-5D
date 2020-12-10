# 0_read_data.R
# read the EQ-5D data
# August 2020
library(readxl)
library(dplyr)

## load the data from Excel
# a) Sri Lanka
sri_lanka = read_excel('data/EQ 5D 3L Data CKD Sri Lanka.xlsx') %>%
  filter(!is.na(EQ5D_VAS)) %>% # remove rows missing the VAS
  mutate(EQ5D_VAS = EQ5D_VAS/100, # scale the VAS to [0,1]
         EQ5D = ifelse(EQ5D<0, 0, EQ5D), # scale the questions to [0,1]
         age = (age_corrected-60)/10) %>% # scale age per 10 year increase
  select(-age_corrected, -No) 

# b) wounds
wounds = read_excel('data/EQ-5D datasets for wounds projects.xlsx') %>%
  rename('sex' = 'Sex',
          'EQ5D_VAS' = 'VAS',
         'EQ5D' ='Utility') %>%
  filter(!is.na(EQ5D)) %>% # remove one missing row
  mutate(
    sex = ifelse(sex=='F', 1, 0), # female = 1
    EQ5D_VAS = EQ5D_VAS/100, # scale the VAS to [0,1]
    EQ5D = ifelse(EQ5D<0, 0, EQ5D)) # scale age per 10 year increase

#
save(sri_lanka, wounds, file='data/EQ5D.RData')