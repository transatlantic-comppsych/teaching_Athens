library(tidyverse)

### PART A 
#This code allows you to do two things:
# 1. Show importance of sample size by demonstrating its effects on a fictional distribution of parties
# 2. Show impact of bias

# let there be two parties A and B in a population of voters
pop_A = 52000
pop_B = 48000
criterion_for_win = 0.5


samp_prob_A = 1.0 # here you could specify a different sampling probability, e.g. 0.8
samp_prob_B = 1.0

# for this you need to first first create a dataframe with probabilities attached to the two groups
population_probs <- data.frame(population = c(rep("0",pop_A), rep("1", pop_B) ), 
                               probability = c(rep(samp_prob_A,pop_A), rep(samp_prob_B, pop_B)))

# then sample from it with  probs for sampling
pop_sampling_diff_probs <- function(population, n){
  a_sample <- population_probs[sample(nrow(population_probs), n, prob = population_probs$probability, replace = TRUE), ]$population  # Draw sample of data frame
  a_percentage <- sum(a_sample==0)/length(a_sample) 
  return(a_percentage) 
}


n<- c(10, 30, 50, 100, 150, 300, 500, 1000, 5000)
percent_per_sample <- list()
for(i in 1:length(n)){
  
  percent_per_sample [[i]]<-   replicate(5000, pop_sampling_diff_probs(population, n[i]))
}
percent_per_sample 


avg<-0
std<-0
percentage_opposite_result<-0
for(i in 1:length(n)){
  avg[i] <- mean(percent_per_sample [[i]])
  std[i] <- sd(percent_per_sample [[i]])
  percentage_opposite_result[i] <- sum(percent_per_sample [[i]]<=criterion_for_win)/length(percent_per_sample [i])
}

avg
std
percentage_opposite_result


#is_it_df <- data.frame(test)

is_it_df<- as.data.frame(do.call(cbind, percent_per_sample))
is_it_df
colnames(is_it_df) <- paste0("n_",n)


#turn to long for plotting
long_is_it_df <- is_it_df %>%
  pivot_longer(cols = starts_with("n") , names_to = "sample_sizes", values_to = "percentages")
dim(long_is_it_df)



long_is_it_df %>% 
  ggplot(aes(x = factor(sample_sizes, level= c("n_10", "n_30", "n_50", "n_100", 
                                               "n_150", "n_300", "n_500", "n_1000", "n_5000")),  y = percentages)) +
  geom_count()+
  geom_hline(yintercept =pop_A/(pop_A+pop_B), linetype = "dashed", colour = "red") +
  geom_hline(yintercept = criterion_for_win, linetype = "dashed", colour = "blue") + 
  theme(legend.position = "none")  +
  ggtitle("Imprecision and sign errors when sampling voting preferences 
          n = 100 simulated samplings per sample size 
          population size ν = 10^5, sampling prob = 0.95"  )+
  xlab("sample sizes") +
  ylab("percentages from each sampling event")



### PART B
# here I created a function to make it easier to show the effects of different bias probabilities. It is
# basically all of the above (except graphs) wrapped into a function, ran for different probs and then plotted.
# could clea it up and merge with all the above. 
testing_sampling <- function(probs_for_sampling){
  pop_A = 52000
  pop_B = 48000
  criterion_for_win = 0.5
  # pop_A = 5000
  # pop_B = 95000
  # criterion_for_win = 0.045
  samp_prob_A = probs_for_sampling
  samp_prob_B = 1.0
  
  # for this you need to first first create a dataframe with probabilities attached to the two groups
  population_probs <- data.frame(population = c(rep("0",pop_A), rep("1", pop_B) ), 
                                 probability = c(rep(samp_prob_A,pop_A), rep(samp_prob_B, pop_B)))
  
  # then sample from it with different probs in the updated 
  pop_sampling_diff_probs <- function(population, n){
    a_sample <- population_probs[sample(nrow(population_probs), n, prob = population_probs$probability, replace = TRUE), ]$population  # Draw sample of data frame
    a_percentage <- sum(a_sample==0)/length(a_sample) 
    return(a_percentage) 
  }
  
  
  n<- c(10, 30, 50, 100, 150, 300, 500, 1000, 5000)
  percent_per_sample <- list()
  for(i in 1:length(n)){
    
    percent_per_sample [[i]]<-   replicate(5000, pop_sampling_diff_probs(population, n[i]))
  }
  
  
  avg<-0
  std<-0
  percentage_opposite_result<-0
  for(i in 1:length(n)){
    avg[i] <- mean(percent_per_sample [[i]])
    std[i] <- sd(percent_per_sample [[i]])
    percentage_opposite_result[i] <- sum(percent_per_sample [[i]]<=criterion_for_win)/length(percent_per_sample [i])
  }
  
  return (percentage_opposite_result)
  
}


# the below is the code to plot it all 
list_percentage_opposite <- list ()
probs <- c(1, 0.95, 0.9, 0.85, 0.8, 0.75, 0.7)
for  (i in 1: length(probs)){
  list_percentage_opposite[[i]] <- testing_sampling(probs[i])
  
}


list_percentage_opposite

list_percentage_opposite_df<- as.data.frame(do.call(cbind, list_percentage_opposite))
list_percentage_opposite_df
colnames(list_percentage_opposite_df) <- paste0("sampling_prob_",probs)


#turn to long for plotting
long_list_percentage_opposite_df <- list_percentage_opposite_df %>%
  pivot_longer(cols = starts_with("sampling_prob_") , names_to = "probs", values_to = "percentages")
dim(long_list_percentage_opposite_df)

long_list_percentage_opposite_df <- data.frame(long_list_percentage_opposite_df, samp_size = rep(c("n_10", "n_30", "n_50", "n_100", 
                                                                                                   "n_150", "n_300", "n_500", "n_1000", "n_5000"),each = 7))


long_list_percentage_opposite_df %>% 
  ggplot(aes(x = factor(samp_size, level= c("n_10", "n_30", "n_50", "n_100", 
                                            "n_150", "n_300", "n_500", "n_1000", "n_5000")),  y = percentages, 
             group = factor(probs))) +
  geom_line(aes(colour=probs))+
  geom_point() +
  geom_hline(yintercept =pop_A/(pop_A+pop_B), linetype = "dashed", colour = "red") +
  geom_hline(yintercept = criterion_for_win, linetype = "dashed", colour = "blue") + 
  #theme(legend.position = "none")  +
  ggtitle("proportion opposite result by sample size and bias magnitude 
          n_sims = 5000"  )+
  xlab("sample sizes") +
  ylab("percentages from each sampling event")


###PART C: simulating building collapses
# the code below simulates the probability of a building falling in an earthquake under the 
# assumption of constant forces same probability for each house.

# number of "flips"
num_buildings <- 1000

# collapse simulation
probability_collapse <- c(0.3, 0.7)
building_collapse <- c('yes', 'no')
buildings <- sample(building_collapse, size = num_buildings, replace = TRUE, prob = probability_collapse)


# number of heads and tails
freqs <- table(buildings)
freqs


# get the relative frequencies of building collapse by taking the cumulative sum
collapse_freq <- cumsum(buildings == 'yes') / 1:num_buildings

# create data frame with collapse frequencies and num of buildings to be able to plot
df_building_collapse <- data.frame(collapse_freq = collapse_freq , num_buildings = 1:num_buildings)

# plot relative frequencies of against number of buildings
df_building_collapse %>% 
ggplot(aes(x = num_buildings, y = collapse_freq))+
  geom_line() +
  geom_point()+
  ggtitle("Long Run Probabilities of Building Collapse  
          cumulative sum of collapsed buildings")+
  geom_hline(yintercept = probability_collapse[1], linetype = "dashed", colour = "red")+
  xlab("number of buildings")+
  ylab("relative frequency of collapse")




###PART D: Demonstrating effects of prevalence on positive predictive value and the trouble with screening
population_size <- 1000000 # any ppopulation that could vary in some characteristic, binary in this case.
prevalence <- c(0.5, 0.4, 0.3, 0.2, 0.1, 0.05, 0.025, 0.01, 0.005, 0.0025) # these are ranges of prevalences.
sensitivity <- 0.9 # the sensitivity of your diagnostic/screening instrument
specificity <- 0.9 # the specificity

n_with_disorder <- 0
n_without_disorder<- 0
n_true_positive<- 0
n_false_negative<- 0
n_true_negative<- 0
n_false_positive<- 0
  
# the loop below will show the problem for various prevalences. You can run the code chunk outside the loop to understand
# things for each prevalence separately. simply remove the for line and the curly brackets at the very end and 
# replace i with indices for prevalence positions in the prevalence vector.
for(i in 1:length(prevalence)){
n_with_disorder[i] <- population_size*prevalence[i]

n_without_disorder[i] <- population_size*(1-prevalence[i])

n_true_positive[i] <- n_with_disorder[i]*sensitivity

n_false_negative[i] <- n_with_disorder[i]*(1-sensitivity)

n_true_negative[i] <- n_without_disorder[i]*specificity

n_false_positive[i] <- n_without_disorder[i]*(1-specificity)
}


n_with_disorder
n_without_disorder
n_true_positive
n_false_negative
n_true_negative
n_false_positive


false_vs_tru_pos <- data.frame(cbind (n_false_positive, n_true_positive))
false_vs_tru_pos_long <- false_vs_tru_pos %>% 
  pivot_longer(cols = starts_with("n_") , names_to = "type_of_positivity", values_to = "numbers")
false_vs_tru_pos_long <- cbind (false_vs_tru_pos_long, prevalence= rep(prevalence,each = 2))
 

dim(false_vs_tru_pos_long)

# stacked bars showing proportions of false over true positives
false_vs_tru_pos_long %>% 
ggplot(aes(x = factor(100*prevalence), y = numbers)) +
  geom_col (aes(fill = factor(type_of_positivity)), width = 0.7) +
  ggtitle("Influence of Prevalence on False and True Positives
          at sensitivity = 0.9 and specificity = 0.9 ") +
  xlab("Prevalence in %") +
  ylab("Absolute Numbers of False or True Positives") 
  

ratio_true_overall <- data.frame(ratio_true_pos_total_pos = n_true_positive/(n_false_positive + n_true_positive), prevalence = prevalence)

# line showing true positives over total n of positives (true and false)
ratio_true_overall %>% 
  ggplot(aes(y = ratio_true_pos_total_pos, x = factor(prevalence), group = 1)) +
  geom_line()+
  geom_point() +
  ylab("ratio of true positives over all positives") +
  xlab("prevalence")+
  ggtitle("Effect of Prevalence on ratio of 
          true positives to overall positives at 
          sensitivity = .9 and  specificity = .9")
  

           
