###Rule of 3
# Suppose you encounter a zero numerator in a study, e.g. in a trial of a drug that reports its side effects.
# How do you estimate the p? It is impossible in the absence of a probability, but what you can estimate
# is the upper CI for this, that is the maximum theoretical value that would be obtained in the long run
#, i.. after repeated samplings within the 95% of the sample. 

library(tidyverse)

n <-  c(10,20, 30, 40, 50,  80, 100, 120, 150, 200, 250,500,750, 1000)

p_max_risk <- 0.05

# (n choose k)* p^k*(1-p)^n-k is the Bernoulli function for binary events , but since k = 0 it simplifies to

# (1-p)^n    This is the equation from the simplified Bernoulli function

# (1-p)^n = p_max_risk This is what needs to be satisfied to find the p at max risk, e.g. 0.05

# 1-p = p_max_risk^(1/n) which can be re-written as this

# p = 1- p_max_risk^(1/n)  and ends up like this

p = 1- p_max_risk^(1/n) # this is the one to get

p



# you can verify the values in this vector in the following way: for this rate of events, i.e. zero at each 
# of the sample sizes and for each p-value contained in the vector, the output should be 0.05 when 
# plugged into the binomial formula. Here is the test.
pbinom(0, n,p) # where n is the sample sizes above, and p the values for max risk.


### now calculate the above using the rule of three

rule_three  <- 3/n # this is the rule of three
rule_three 

### now plot all this

# first create teh dataframe
risk_sample_size <- data.frame( max_risk = c(p, rule_three), sample_size = c(n, n), estimation = factor(
                                  rep(c("analytical", "rule_of_three" ), each = length(n))))


# plot
risk_sample_size %>% 
  ggplot(aes(x = sample_size, y = max_risk))  +
  geom_jitter(aes(colour = estimation), alpha = 0.5)+
  geom_line(aes(colour = estimation), position=position_dodge(width=20.0))+
  xlab("Sample Sizes")+
  ylab("Maximum Tolerable Risk")+
  ggtitle(paste0("The upper limit of risk when encountering a zero numerator \nfor a ", 100*(1-p_max_risk), 
                 "% ", "Confidence Interval"))

  
  




