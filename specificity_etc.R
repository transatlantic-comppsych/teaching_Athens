### Understanding tests and using the magic of contingency tables 

## the motivation for this is to show how much you can do with simple contingency tables and how wrong one can be 

## when screening and looking at the reuslts of tests frequently lused for screening, from which people derive

## sensitivities, specificities etc. 

## The best way to illustrate this is through a tree/branch diagram--see diagram

## here I create a function to derive some of the basic metrics

## the motivation is the following: 

##

library(tidyverse)

prediction_metrics <- function(population, prevalence, sensitivity, specificity){

have_depression <- population*prevalence # that is how many will have the disease out of the population
have_no_depression <- population*(1-prevalence) # the rest of the people

have_depression_pos_test <- have_depression*sensitivity # these are the true positives
have_depression_neg_test <- have_depression*(1-sensitivity) # these are the false negatives

have_no_depression_pos_test <- have_no_depression*(1-specificity) # these are the false positives 
have_no_depression_neg_test <- have_no_depression*specificity # these are the true negatives


pos_pred_value <- have_depression_pos_test/(have_depression_pos_test+have_no_depression_pos_test) # the ratio of true positives over all positives
pos_pred_value  # this is also called the precision of the prediction. It is quite low in this case.

neg_pred_value <- have_no_depression_neg_test/(have_depression_neg_test+have_no_depression_neg_test) # the ratio of true negatives over all negatives
neg_pred_value 

having_diag_when_neg_test <- have_depression_neg_test/(have_depression_neg_test+have_no_depression_neg_test) # if I have a negative test, how likely am I to have the disorder?
having_diag_when_neg_test # this is P(D+|Test-)

accuracy <- (have_depression_pos_test + have_no_depression_neg_test)/(have_depression_pos_test + have_depression_neg_test+ 
                                                                     have_no_depression_pos_test + have_no_depression_neg_test)

accuracy# this is the accuracy of the prediction, which is high, as one might expect given the sensitivity and specificity

metrics_df <- data.frame(prevalence = prevalence, specificity = specificity, sensitivity= sensitivity, # now create a dataframe with all of these metrics
                 have_depression = have_depression, have_depression_pos_test = have_depression_pos_test, 
                 have_depression_neg_test = have_depression_neg_test,
                     have_no_depression = have_no_depression, have_no_depression_neg_test = have_no_depression_neg_test, 
                 have_no_depression_pos_test = have_no_depression_pos_test,
                     pos_pred_value= pos_pred_value, neg_pred_value = neg_pred_value, accuracy = accuracy, 
                 having_diag_when_neg_test = having_diag_when_neg_test)

return(metrics_df)

}



effects_of_prevalence <- prediction_metrics(population = c(10000), prevalence = c(0.05, 0.1, 0.15, 0.2, 0.25, 0.3, 0.35, 0.4), 
                                            sensitivity = 0.9, specificity = 0.9)

effects_of_prevalence

effects_of_prevalence %>% ggplot(aes(as.factor(x = 100*prevalence), y = 100*pos_pred_value))+
  geom_bar(stat="identity")+
  ylim(0,100)+
  ggtitle("Effects of prevalence on positive predictive value (i.e. accuracy)") +
  ylab("positive predictive value %")+
  xlab("prevalence %")



effects_on_ppv <- effects_of_prevalence %>% ggplot(aes(x = 100*prevalence, y = 100*pos_pred_value))+
  geom_line ()+
  geom_point()+
  ylim(0,100)+
  ggtitle("Effects of prevalence on positive predictive value (i.e. accuracy)") +
  labs(subtitle = "sensitivity = 0.9; specificity = 0.9")+
  ylab("positive predictive value %")+
  xlab("prevalence %")
effects_on_ppv



effects_on_false_positives <- effects_of_prevalence %>% ggplot(aes(x = 100*prevalence, y = have_no_depression_pos_test))+
  geom_line ()+
  geom_point()+
  ggtitle("Effects of prevalence on false positives ") +
  labs(subtitle = "sensitivity = 0.9; specificity = 0.9")+
  ylab("false positives")+
  xlab("prevalence %")
effects_on_false_positives

effects_on_npv <- effects_of_prevalence %>% ggplot(aes(x = 100*prevalence, y = 100*neg_pred_value))+
  geom_line ()+
  geom_point()+
  ylim(0,100)+
  ggtitle("Effects of prevalence on negative predictive value ") +
  labs(subtitle = "sensitivity = 0.9; specificity = 0.9")+
  ylab("negative predictive value %")+
  xlab("prevalence %")

effects_on_npv



effects_on_having_diagnosis_with_neg_test <- effects_of_prevalence %>% ggplot(aes(x = 100*prevalence, y = 100*having_diag_when_neg_test))+
  geom_line ()+
  geom_point()+
  ylim(0,10)+
  ggtitle("Effects of prevalence on having a disease when test negative") +
  labs(subtitle = "the question of P(D-|T-) at sensitivity = 0.9; specificity = 0.9")+
  ylab("having disease when test negative %")+
  xlab("prevalence %")

effects_on_having_diagnosis_with_neg_test # also compliment on npv obviuosly



## now assume you don't have a branch diagram, and are only given the above numbers. This is the way 
## to put them in a table and work with them. 

# create contingency table using data of choice from the function above (could write another function to produce multple tests, 
# but didn't have time)

cont_table = data.frame(matrix(
  c(effects_of_prevalence[1,]$have_depression_pos_test, 
    effects_of_prevalence[1,]$have_no_depression_pos_test, 
    effects_of_prevalence[1,]$have_depression_neg_test, 
    effects_of_prevalence[1,]$have_no_depression_neg_test),
  nrow = 2,
  ncol = 2
)
)

colnames(cont_table) <- c("test_positive", "test_negative")
rownames(cont_table) <- c("depression", "no_depression")
cont_table 



## but now if you want to ask whether there is an association between two variables
## it would seem strange to ask whether depression and a test for it are associated
## but it is not completely far fetched. 
## more commonly you would ask this question of other variables, e.g. depression and anxiety relations.

# marginal totals
cont_table_row_marg_depression <- rowSums(cont_table)[[1]] # first row
cont_table_row_marg_no_depression<- rowSums(cont_table)[[2]] # second row

cont_table_col_marg_test_positive <- colSums(cont_table)[[1]] # first column
cont_table_col_marg_test_negative<- colSums(cont_table)[[2]] # second column

cont_table_marg_total <- sum(rowSums(cont_table,1)) # the total, i.e. the population

# get expected values
# these are nothing but a rule of three results
# the marginal of the row is found in the total: row1_marginal/total, then (under the null hypothesis) 
# what should we expect of the first cell (position row = 1, column = 1); obviously that it be 
# the equivalent for whatever the column total is, threfore cell_1 = row1_maringal/total; and 
# the equivalent for all cells

ev_cell_11 <- (cont_table_row_marg_depression/cont_table_marg_total)*cont_table_col_marg_test_positive
ev_cell_12 <-(cont_table_row_marg_depression/cont_table_marg_total)*cont_table_col_marg_test_negative

ev_cell_21 <- (cont_table_row_marg_no_depression/cont_table_marg_total)*cont_table_col_marg_test_positive
ev_cell_22 <- (cont_table_row_marg_no_depression/cont_table_marg_total)*cont_table_col_marg_test_negative

# prepare the values for the chi-square test which is Sum(O-E)^2/expected
value_1 <- ((cont_table[1,1] - ev_cell_11)^2)/ev_cell_11
value_2 <- ((cont_table[1,2] - ev_cell_12)^2)/ev_cell_12
value_3 <- (cont_table[2,1] - ev_cell_21)^2/ev_cell_21
value_4 <- (cont_table[2,2] - ev_cell_22)^2/ev_cell_22

chi_square <- sum(value_1, value_2, value_3, value_4)
chi_square

# which is what you also get from R's inbuilt chi-square

chi_square == chisq.test (matrix(
  c(have_depression_pos_test, have_no_depression_pos_test, have_depression_neg_test, 
    have_no_depression_neg_test),
  nrow = 2,
  ncol = 2
),correct=F
)$statistic



############ Now a simulated example with sensitivity and specificity etc.



library(Bolstad2)

library(tidyverse)

library(pROC)

library(ggthemes)

library(patchwork)

library(cowplot)


#### I didn't have time to write generalisable code, but here is a copy paste version of two 
#### cases, good and bad

### the good rating scale
lower_threshold <- 0

upper_threshold <- 26



n_healthy <- 100

avg_healthy <- 4

sd_halthy <- 2





n_disorder <- 100

avg_disorder <- 11

sd_disorder <- 4







values_healthy <- round(truncnorm::rtruncnorm(n_healthy, lower_threshold, upper_threshold, avg_healthy, sd_halthy),0)



values_disorder <- round(truncnorm::rtruncnorm(n_disorder, lower_threshold, upper_threshold, avg_disorder, sd_disorder),0)



values <- c(values_healthy, values_disorder)



labels <- c(rep("healthy",n_healthy), rep("depressed",n_disorder))

labels



df_psychometrics <- data.frame(cbind(values = values, labels = labels))

df_psychometrics$labels <- as.factor(df_psychometrics$labels)

df_psychometrics$values <- as.numeric(df_psychometrics$values)


plot_psychometrics <- df_psychometrics %>%
  
  ggplot(aes(x = labels, y = values, colour = labels))+
  
  geom_point(position = "jitter", alpha = 0.5) +
  
  geom_hline(yintercept = 6.5,show.legend = "sens =  spec = ") +
  
  geom_hline(yintercept = 2.5, linetype = "dashed") +
  
  geom_hline(yintercept = 10.5, linetype = "dotted") +
  
  
  annotate("text", x=2.4, y=7.5, label="Sens = 0.92\nSpec = 0.91") +
  
  annotate ("text", x = 2.4, y = 11.5, label = "Sens = 1.00\nSpec = 0.55") +
  
  annotate ("text", x = 2.4, y = 1.5, label = "Sens = 0.18\nSpec = 0.99") +
  
  ggtitle("Values of a rating scale with good discrimination") +
  
  xlab("disease status") +
  ylab("depression score") 
 # theme(legend.position="bottom",legend.justification="right")



plot_psychometrics <- plot_psychometrics +
  
  theme_tufte(base_size = 14)+
  
  theme(legend.position = c(.64, .84))



plot_psychometrics



roc_obj <- roc(response = df_psychometrics$labels, predictor = df_psychometrics$values)

auc(roc_obj)


extract_thresholds <- data.frame(coords(roc=roc_obj, x = "all", transpose = FALSE))

extract_thresholds



the_curve <- ggroc(roc_obj)+
  
  geom_abline(intercept = 1, linetype = "dashed") +
  
  ggtitle("ROC curve of a good test: AUC = 0.96")



the_curve <- the_curve + theme_tufte(base_size = 14)

the_curve 

# joint_plot <- plot_psychometrics / the_curve
# 
# joint_plot




### the poor rating scale

lower_threshold <- 0

upper_threshold <- 26



n_healthy <- 100

avg_healthy <- 4

sd_halthy <- 7





n_disorder <- 100

avg_disorder <- 11

sd_disorder <- 8







values_healthy <- round(truncnorm::rtruncnorm(n_healthy, lower_threshold, upper_threshold, avg_healthy, sd_halthy),0)



values_disorder <- round(truncnorm::rtruncnorm(n_disorder, lower_threshold, upper_threshold, avg_disorder, sd_disorder),0)



values <- c(values_healthy, values_disorder)



labels <- c(rep("healthy",n_healthy), rep("depressed",n_disorder))

labels



df_psychometrics <- data.frame(cbind(values = values, labels = labels))

df_psychometrics$labels <- as.factor(df_psychometrics$labels)

df_psychometrics$values <- as.numeric(df_psychometrics$values)


plot_psychometrics <- df_psychometrics %>%
  
  ggplot(aes(x = labels, y = values, colour = labels))+
  
  geom_point(position = "jitter", alpha = 0.5) +
  
  geom_hline(yintercept = 14.5,show.legend = "sens =  spec = ") +
  
  geom_hline(yintercept = 2.5, linetype = "dashed") +
  
  geom_hline(yintercept = 10.5, linetype = "dotted") +
  
  
  annotate("text", x=2.4, y=11.5, label="Sens = 0.70\nSpec = 0.74") +
  
  annotate ("text", x = 2.4, y = 15.5, label = "Sens = 0.91\nSpec = 0.32") +
  
  annotate ("text", x = 2.4, y = 2.5, label = "Sens = 0.50\nSpec = 0.80") +
  
  ggtitle("Values of a rating scale with good discrimination") +
  
  xlab("disease status") +
  ylab("depression score") 
# theme(legend.position="bottom",legend.justification="right")



plot_psychometrics <- plot_psychometrics +
  
  theme_tufte(base_size = 14)+
  
  theme(legend.position = c(.64, .84))



plot_psychometrics


roc_obj <- roc(response = df_psychometrics$labels, predictor = df_psychometrics$values)

auc(roc_obj)


extract_thresholds <- data.frame(coords(roc=roc_obj, x = "all", transpose = FALSE))

extract_thresholds



the_curve <- ggroc(roc_obj)+
  
  geom_abline(intercept = 1, linetype = "dashed") +
  
  ggtitle("ROC curve of a poor test: AUC = 0.73")



the_curve <- the_curve + theme_tufte(base_size = 14)

the_curve

# joint_plot <- plot_psychometrics / the_curve
# 
# joint_plot














