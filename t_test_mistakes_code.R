# 
# 
# test_df <- data.frame(test_var = rnorm(100, 50, 10), test_gender = rep(c("male", "female"),50))
# t.test(test_df$test_var ~ test_df$test_gender)
# 
# t_test_func <- function(sample_size, mean_a, mean_b, sd_a, sd_b){
# 
# test_var_boys = rnorm(sample_size, mean_a, sd_a)
# 
# test_var_girls = rnorm(sample_size, mean_a, sd_b)
# 
# test_df <- data.frame(test_var = c(test_var_boys, test_var_girls), test_gender = rep(c("male", "female"),each = sample_size/2))
# 
# p_values <- t.test(test_df$test_var ~ test_df$test_gender)$p.value
# 
# diffs <- t.test(test_df$test_var ~ test_df$test_gender)$estimate[[1]]- 
#                     t.test(test_df$test_var ~ test_df$test_gender)$estimate[[2]]
# 
# result_list <- list(p_values, diffs)
# 
# return(result_list)
# }
# 
# 
# n_sims <- 500
# sims_t_test <- replicate(n_sims,t_test_func(sample_size = 1000, mean_a =50, mean_b = 50, sd_a = 10, sd_b = 10), 
#                                     simplify = F)
# 
# test_list_to_df <- do.call(rbind.data.frame, sims_t_test)
# test_list_to_df
# colnames(test_list_to_df) <-c("p_values", "differences")
# test_list_to_df$n_sims <- 1:n_sims
# test_list_to_df
# 
# mean(test_list_to_df$p_values)
# mean(test_list_to_df$differences)
# hist((test_list_to_df$p_values))
# hist((test_list_to_df$differences))
# min(test_list_to_df$p_values)
# sum(test_list_to_df$p_values<0.05)/length(test_list_to_df$p_values)
# 



# simulate brain areas
# using the below to show what happens when you do multiple testing. 
# doing a simulation

# let there be a situation where you want to test the differneces between boys and girls in some 
# brain structure/function
# the definition of brain structure/function can vary between 1 (e.g. a hypothesis driven area) to
# as many as you like to consider (e.g. all Brodmann areas that sum up to about 100
# across both hemispheres)

#starting with writing a simple function where you can vary num of participants 
# and num of brain areas
brain_ares_mult_test_func <- function (num_participants, number_of_brain_areas){

brain_areas_df <- data.frame(matrix(data = NA, nrow = number_of_participants, ncol = number_of_brain_areas))

colnames(brain_areas_df) <- c(paste0("broadman_area_", 1:number_of_brain_areas))


for(i in 1:ncol(brain_areas_df)){
  
  brain_areas_df[,i] <- rnorm(number_of_participants, mean = 50, sd = 10)
}                            
brain_areas_df$sex <- rep(c("boy", "girl"), 
                          each = number_of_participants/2) 


p_values_brain_areas <-0 
for(i in 1:(ncol(brain_areas_df)-1)){
  
  p_values_brain_areas[i]  <- t.test(brain_areas_df[,i]~ brain_areas_df$sex)$p.value
}                            

return(p_values_brain_areas)
}
p_values_brain_areas

n_sims <- 10

p_vals_for_brain_areas <- replicate(100,brain_ares_mult_test_func(100,100))
p_vals_for_brain_areas


criterion <- 0.05 # this is your p_value
#if you are using only one brain area, run this
sum(p_vals_for_brain_areas < criterion)/length(p_vals_for_brain_areas)

# if you are using more than one brain area, you will end up with a list, so use the following.
proportion_below_criterion <- 0
number_below_criterion <- 0
for(i in 1: ncol(p_vals_for_brain_areas)){
  proportion_below_criterion[i] <- sum(p_vals_for_brain_areas[,i]<criterion)/length(p_vals_for_brain_areas[,i])
number_below_criterion[i] <-sum(p_vals_for_brain_areas[,i]<criterion)
  }
proportion_below_criterion
mean(proportion_below_criterion)

number_below_criterion
mean(number_below_criterion)






