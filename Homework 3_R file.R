## Homework 3 - Quinnlan Smith & Tyler Butts ## 
library(tidyverse) 
library(lubridate)

dat = read_csv('BSB_tagging_data.csv')
dat

# Q1.1: Plot a probability density function (probability density vs. proportions from 0 to 
# 1) for the proportion of female black sea bass that changed sex out of all those which 
# were recaptured after the end of the spawning season (i.e., after July). 
# Relevant functions: dbeta()

dat_filter = dat %>% 
  filter(Sex_at_capture == 'F') %>% 
  mutate(capture = mdy(Date_at_capture)) %>% 
  mutate(recapture = mdy(Date_at_recapture)) %>% 
  select(capture, recapture, Sex_at_capture, Sex_at_recapture) %>%
  mutate(change = if_else(Sex_at_recapture == 'F', 0, 1)) %>%
  mutate(length_btwn_capture = interval(capture, recapture)/ days(1)) %>%
  filter(length_btwn_capture > 30) # shortest time to sex change occurred within a month, filter by month 
dat_filter # 58 females at capture 

# n = number of bernoulli trials = 58
# k = successes = 14 
# shape1 = 15 
# shape2 = 43


prob = seq(0,1, by=0.01)
plot(prob, dbeta(prob, shape1 = 15, shape2 = 43))

pdf_1.1 = dbeta(prob, shape1 = 15, shape2 = 43)
#============================================================================================================#

# Q1.2: Give the 95% CI for the probability of sex change for these individuals in Q1.1
# Relevant functions: qbeta()
prob = seq(0.025, 0.975, by=0.01)
prob2 = c(0.025,0.975)
qbeta(prob2, shape1= 15, shape2 = 43, lower.tail = TRUE)

## Confidence Interval ## 
## 15.5% - 37.8% # 

#=============================================================================================================# 

