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

plot(change ~ Length_at_capture, dat_q3)#Explore some trends 


fish_length<-dat_q3$Length_at_capture
hist(fish)
#Ehhhhh this is not normally distributed, but this is alright?

glm <- glm(change ~ Length_at_capture,family=binomial(link=logit), dat_q3) #Logit is default link 
summary(glm)
##Double check this##
#Not significant p-value 0.240

# Q1.3b: By how much is the log odds of sex change predicted to change for every 
#millimeter increase in length?
#Intercept: -5.53588
#Length at capture 0.01429 - this should be the log odds sex change predicted per mm length increase 
##Also double check this value##

#Predicting some probability values to check out values 
ilogit(-5.53588-0.01429*500)

# Q1.4: Plot the relationship between the probability of sex change for these individuals 
#and length.  Overlay the model estimated relationship on the data.  Label axes 
#appropriately and provide a figure caption

#Figure 1: Black sea bass (Centropristis striata) odds of transitioning from female to male based on length at capture. Fish were captured,
#tagged, measured, sexed, and released. Individuals recaptured at least a month after initial tagging were sexed again. A value of 1 
#indicates a female that was previously marked and had transitioned or was in the process of transitioning to a male. A value of 0 
#indicates a female that was previously marked and had not transitioned. 

predict(glm, type = c("response"), se.fit = FALSE, dispersion = NULL, terms = NULL)

ggplot(data=dat_q3, aes(Length_at_capture, change)) + 
geom_point() +
  geom_smooth(method="glm", method.args=list(family="binomial"), 
              fullrange=TRUE, se=FALSE) +
  xlab('Length at capture')+
  ylab('Odds of transition')

# Q1.5: Provide a 95% prediction interval for the probability of sex change for a female of length 300 mm.

#Not normal on the scale of change, however errors around the predictor are 
#Type will be link, se.fit set to TRUE to get SE's (+/- 1.96 SE returns 95% CI)
#The linkinv function gets these back on proportions 

#Relevant functions: predict.glm(), model$family$linkinv()

pred_se <-predict.glm(glm, type="link", se.fit=TRUE)

val <- 1.96 #Used for 95% CI's

upper <- pred_se$fit + (val * pred_se$se.fit)
lower <- pred_se$fit - (val * pred_se$se.fit)
fit <- pred_se$fit

upper2 <- glm$family$linkinv(upper)
lower2 <- glm$family$linkinv(lower)

glm$upper <- upper2
glm$lower <- lower2

ggplot(data=dat_q3, mapping=aes(x=Length_at_capture,y=change)) + geom_point() +         
  stat_smooth(method="glm", method.args=list(family=binomial)) 
  
#ahh hell how do we call valeus for 300
