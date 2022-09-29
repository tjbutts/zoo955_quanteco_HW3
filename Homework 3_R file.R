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

# Q1.3a: Does the length of a female influence its probability of sex change given that it 
#was recaptured after the end of the spawning season?  Give a p value to support your answer.

#Buildin a GLM, we should be able to grab some stats to determine what contributes to the model 
#Note we need Length_at_capture back in the dataframe 
#Also note the link function will be Binomial, meaning the link is: 'η=log(µ/(1–µ))' and the variance fun is: 'µ(1–µ)' 
dat_q3 = dat %>% 
  filter(Sex_at_capture == 'F') %>% 
  mutate(capture = mdy(Date_at_capture)) %>% 
  mutate(recapture = mdy(Date_at_recapture)) %>% 
  select(capture, recapture, Sex_at_capture, Sex_at_recapture, Length_at_capture) %>% #Just added length at capture back in here
  mutate(change = if_else(Sex_at_recapture == 'F', 0, 1)) %>%
  mutate(length_btwn_capture = interval(capture, recapture)/ days(1)) %>%
  filter(length_btwn_capture > 30) # shortest time to sex change occurred within a month, filter by month 

plot(change ~ Length_at_capture, dat_q3) #Explore some trends 

fish_length<-dat_q3$Length_at_capture
hist(fish)
#Ehhhhh this is not normally distributed, but this is alright?

glm <- glm(cbind(change,1-change) ~ Length_at_capture,family=binomial, dat_q3) #Change, 1-change should not be necessary but should give same values
summary(glm)
##Double check this##
#Not significant p-value 0.240

# Q1.3b: By how much is the log odds of sex change predicted to change for every 
#millimeter increase in length?
#Intercept: -5.53588
#Length at capture 0.01429 - this should be the log odds sex change predicted per mm length increase 
##Also double check this value##

#Predicting some values for the hell of it 
ilogit(-5.53588-0.01429*300)

# Q1.4: Plot the relationship between the probability of sex change for these individuals 
#and length.  Overlay the model estimated relationship on the data.  Label axes 
#appropriately and provide a figure caption

#ggplot things 
predict(glm, type = c("response"), se.fit = FALSE, dispersion = NULL, terms = NULL)

#ADD LABS TO THIS FRIDAY AM QUINN
ggplot(data=dat_q3, aes(Length_at_capture, change)) + 
geom_point() +
  geom_smooth(method="glm", method.args=list(family="binomial"), 
              fullrange=TRUE, se=FALSE)