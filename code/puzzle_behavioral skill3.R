#### Boldness shapes skill refinement in a novel foraging task in wild squirrels ####

## Sonja Wild, Lucy M. Todd, Lupin M. Teles, Andrew Sih, Jennifer E. Smith ##


# 1) Load libraries -------------------------------------------------------


# load libraries
library(brms)
library(dplyr)
library(psych)
library(ggpubr)
library(ggplot2)
library(emmeans)
library(performance)
# library(stringdist)
# library(RColorBrewer)
# library(readxl)
library(tidyr)
# library(posterior)
# library(tibble)
library(tidyverse)
library(broom.mixed)
library(flextable)
library(officer)


# 2) Read and prep data ------------------------------------------------------------
setwd("C:/Users/sonja/Desktop/Ground Squirrels/Behavioral skill/git/squirrels_behavioral_skill")

# each row is one solve
solving_data <- read.csv("Data2/combined.data.csv")
# to repeat the analysis with a threshold of 5 solves (instead of 10), uncomment the respective lines that read in the data. The rest remains the same
#solving_data <- read.csv("Data2/combined.data_5.csv")


# each row is one visit
visit_data <- read.csv("Data2/visits.data.csv")
#visit_data <- read.csv("Data2/visits.data_5.csv")

# this contains the individual level data - each row is one individual
subject_data <- read.csv("Data2/Subject_data.csv")
#subject_data <- read.csv("Data2/Subject_data_5.csv")


# we want to calculate a boldness score consisting of individuals' trappability and their propensity to show any fear responses in the trap (chatter, struggle, call)

# run pca on trap behavior and trappabilty

bh_data_scaled <- scale(subject_data[,c("any_beh_prop", "trap_rate_per_day")])

colnames(bh_data_scaled) <- c("trap behav.", "trappability")

pca_bh <- principal(bh_data_scaled, nfactors=2, rotate = "none")
summary(pca_bh)

# Factor analysis with Call: principal(r = bh_data_scaled, nfactors = 2, rotate = "none")
# 
# Test of the hypothesis that 2 factors are sufficient.
# The degrees of freedom for the model is -2  and the objective function was  0 
# The number of observations was  26  with Chi Square =  0  with prob <  NA 
# 
# The root mean square of the residuals (RMSA) is  0

pca_bh$loadings

# Loadings:
#   PC1    PC2   
# trap behav.  -0.810  0.586
# trappability  0.810  0.586
# 
# PC1   PC2
# SS loadings    1.313 0.687
# Proportion Var 0.657 0.343
# Cumulative Var 0.657 1.000

# we can see that trap behavior and trappability load in opposite directions and account for 65.7% of the variance


# Increase margins to give labels room, especially on the right side
# Extract scores and loadings
scores <- pca_bh$scores
loadings <- pca_bh$loadings[,1:2]


# add the scores to our subject data as boldness
subject_data$beh_type <- pca_bh$scores[,1]

cor(bh_data_scaled)

# trap behav. trappability
# trap behav.    1.0000000   -0.3130662
# trappability  -0.3130662    1.0000000

range(subject_data$beh_type)
# -2.673573  2.064101

mean(subject_data$beh_type)
# subject_data$beh_type

sd(subject_data$beh_type)
# 1


# add the individual level data to the solving data
solving_data <- left_join(solving_data, subject_data, by="Subject")

# and to the visit data
visit_data <- left_join(
  visit_data,
  subject_data %>% select(Subject, beh_type, log_training_solves),
  by = "Subject"
)


# Make a combined figure with the distribution of boldness scores and the biplot

tiff("Figures output/Boldness_PCA_combined2.tiff", units = "in", width = 8, height = 4, res = 300, compression = 'lzw')

layout(matrix(c(1, 2), nrow = 1))

## Panel A: Histogram
par(mar = c(5, 4, 4, 2) + 0.1)

percentiles <- quantile(subject_data$beh_type, probs = c(0.10, 0.50, 0.90), na.rm = TRUE)
hist(subject_data$beh_type, xlab = "Boldness score (PCA)", ylim = c(0, 12), breaks = 8, main = "")
abline(v = percentiles, col = "red", lty = 2, lwd = 1.5)
text(x = percentiles, y = 12, labels = c("10th", "50th", "90th"), pos = 4, col = "red", cex = 0.8)
mtext("a", side = 3, line = 1, at = par("usr")[1] - diff(par("usr")[1:2]) * 0.17, cex = 1.4, font = 2)

## Panel B: Biplot
par(mar = c(5, 4, 4, 6) + 0.1)

plot(scores[,1], scores[,2],
     xlab = "PC1 (65.7% variance)",
     ylab = "PC2 (34.3% variance)",
     main = "",
     pch = 19, col = "black",
     xlim = range(scores[,1]) * 1.2,
     ylim = range(scores[,2]) * 1.2)

scale_factor <- max(abs(scores)) * 0.8

arrows(0, 0, loadings[,1]*scale_factor, loadings[,2]*scale_factor,
       col = "red", length = 0.1)

text(loadings[,1]*scale_factor*1.15, loadings[,2]*scale_factor*1.15,
     labels = rownames(loadings), col = "red", cex = 0.9, xpd = TRUE)

mtext("b", side = 3, line = 1, at = par("usr")[1] - diff(par("usr")[1:2]) * 0.22, cex = 1.4, font = 2)

dev.off()



# 3) Extract some numbers -------------------------------------------------


# how often did the use which paw
table(solving_data$body_part)

# both paws  left paw right paw 
# 760       590       558 

# how many unique squirrels
length(unique(subject_data$Subject))
# 30 squirrels that have either visited the puzzle box 10 times or solved 10 times

table(cbind.data.frame(subject_data$age, subject_data$sex))
# subject_data$sex
# subject_data$age F M
# A 10 10
# P 9 1

# get the number of visits of each of those individuals
max.visits <- visit_data %>%
  group_by(Subject) %>%
  summarise(max_visits = max(cumulative_visits, na.rm = TRUE))

# add the number of visits
subject_data <- subject_data %>%
  left_join(max.visits, by = "Subject")

summary_table <- subject_data %>%
  select(Subject, n_solves, max_visits)

summary_table

# get a list of those with at least 10 visits
min_10_visits <- max.visits[max.visits$max_visits>=10,]
#min_10_visits <- max.visits[max.visits$max_visits>=5,]


# add in which analysis they were included:
summary_table <- summary_table %>%
  mutate(
    min_10_solves = as.integer(Subject %in% solving_data$Subject),
    min_10_visits = as.integer(Subject %in% min_10_visits$Subject)
  )

# let's add age and sex to the table 
summary_table <- left_join(summary_table, select(subject_data, c("Subject", "age", "sex")), by="Subject")

# how many individuals in total
table(select(summary_table, c( "age", "sex")))
# sex
# age  F  M
# A  10 10
# P  9  1

summary_table %>%
  filter(min_10_visits == 1) %>%
  select(age, sex) %>%
  table()
# sex
# age  F  M
# A 10 10
# P  9  1


summary_table %>%
  filter(min_10_solves == 1) %>%
  select(age, sex) %>%
  table()
# sex
# age F M
# A 6 6
# P 5 1

# how many are in both data sets, versus one versus the other

summary_table %>%
  summarise(
    both = sum(min_10_visits == 1 & min_10_solves == 1),
    only_visits = sum(min_10_visits == 1 & min_10_solves == 0),
    only_solves = sum(min_10_visits == 0 & min_10_solves == 1),
    neither = sum(min_10_visits == 0 & min_10_solves == 0)
  )

# both only_visits only_solves neither
# 1   18          12           0       0


mean(table(solving_data$Subject))
106
median(table(solving_data$Subject))
67

range(table(solving_data$Subject))
10
526

# standard deviation
sd(as.numeric(table(solving_data$Subject)))
151.61

# how many visits
mean(table(visit_data$Subject))
81.56667

median(table(visit_data$Subject))
30

sd(table(visit_data$Subject))
95.53793

range(table(visit_data$Subject))
#12 353

sum(summary_table$max_visits)
# 2447 visits

sum(summary_table$n_solves)
# 1908




length(solving_data$lever_side)
1908

table(solving_data$lever_side)
#left lever right lever 
#1227         681

# how many solving events where more than one squirrel was present

table(solving_data$con_pres)
# 1647 alone, 261 in presence of a conspecific
261/1908

# in 13.7% of solves, a conspecific was present


# 4) Skill improvement  with increasing experience  -------------------------------------------------

# 4.1) Does behavioral type influence success rate over time? -------------------------------------------------------------------

# in a previous study, we have demonstrated that bolder squirrels tend to be faster at learning to solve (latency until first solve). But are they also more successful? We look at whether or not they solved  and consumed per visit over time.
# we control for time present in front of the box, number of training solves, age and sex

# let's remove those with fewer than 10 visits
visit_data_success <- visit_data[visit_data$Subject %in% min_10_visits$Subject,]


# we center the cumulaitve number of visits around 0
visit_data_success$log_cumulative_visits <- visit_data_success$log_cumulative_visits-mean(visit_data_success$log_cumulative_visits)
visit_data_success$log_training_solves <- visit_data_success$log_training_solves-mean(visit_data_success$log_training_solves)
# we also create a binary variable to see whether they solve during a visit or not (rather than the number of solves)
visit_data_success$solved <- visit_data_success$solves_per_visit
visit_data_success$solved[visit_data_success$solved>0] <- 1

# we create a new composite variables consisting of whether or not they solved, and whether or not they consumed
visit_data_success <- visit_data_success %>%
  mutate(success = case_when(
    solved == 1 & consumption == 1 ~ "yes_yes",
    solved == 1 & consumption == 0 ~ "yes_no",
    solved == 0 & consumption == 1 ~ "no_yes",
    solved == 0 & consumption == 0 ~ "no_no"
  ))


unique(visit_data_success$success)
# no_no = no solve, no consumption
# yes_no = solve, but not consumption
# no_yes = no solve, but consumption (scrounging)
# yes_yes = solve and consumption

# make conspecific presence a factor
visit_data_success$con_pres <- as.factor(visit_data_success$con_pres)

# check for VIFs
check_collinearity(
  lm(rep(1, nrow(visit_data_success)) ~  log_cumulative_visits + beh_type + age + sex + log_training_solves + scale(total_time_present) + con_pres,
     data = visit_data_success)
)


# Low Correlation
# 
# Term  VIF   VIF 95% CI adj. VIF Tolerance Tolerance 95% CI
# log_cumulative_visits 1.17 [1.13, 1.23]     1.08      0.85     [0.81, 0.89]
# beh_type 2.02 [1.90, 2.14]     1.42      0.50     [0.47, 0.53]
# age 2.11 [1.99, 2.25]     1.45      0.47     [0.45, 0.50]
# sex 2.01 [1.90, 2.14]     1.42      0.50     [0.47, 0.53]
# log_training_solves 1.94 [1.83, 2.06]     1.39      0.52     [0.49, 0.55]
# scale(total_time_present) 1.06 [1.03, 1.12]     1.03      0.94     [0.89, 0.97]
# con_pres 1.04 [1.02, 1.12]     1.02      0.96     [0.90, 0.98]


model_success <- brm(  
  success ~ log_cumulative_visits*beh_type + age + sex + log_training_solves + scale(total_time_present) + con_pres + (1 | Subject), 
  data = visit_data_success, 
  family = categorical(),
  cores = 4,
  iter = 4000,
  chains = 4,
  seed = 3,
  control = list(adapt_delta = 0.95)
)

#save(model_success, file="model output/model_success2.RDA")
load("model output/model_success2.RDA")
#load("model output/model_success_5.RDA") # load this object for the threshold of 5 visits or 5 solves


# check for model fit
pp_msuccess <- pp_check(model_success)
pp_msuccess

# check for stationarity and mixing
plot(model_success)

summary(model_success)

# Family: categorical 
# Links: munoyes = logit; muyesno = logit; muyesyes = logit 
# Formula: success ~ log_cumulative_visits_c * beh_type + age + sex + log_training_solves + scale(total_time_present) + con_pres + (1 | Subject) 
# Data: visit_data_success (Number of observations: 2447) 
# Draws: 4 chains, each with iter = 4000; warmup = 2000; thin = 1;
# total post-warmup draws = 8000
# 
# Multilevel Hyperparameters:
#   ~Subject (Number of levels: 30) 
# Estimate Est.Error l-95% CI u-95% CI Rhat Bulk_ESS Tail_ESS
# sd(munoyes_Intercept)      0.51      0.15     0.25     0.85 1.00     2750     4454
# sd(muyesno_Intercept)      0.78      0.23     0.40     1.30 1.00     2631     4511
# sd(muyesyes_Intercept)     1.16      0.27     0.74     1.78 1.00     2971     4434
# 
# Regression Coefficients:
#   Estimate Est.Error l-95% CI u-95% CI Rhat Bulk_ESS Tail_ESS
# munoyes_Intercept                            -0.54      0.26    -1.07    -0.04 1.00     4150     4676
# muyesno_Intercept                            -1.71      0.39    -2.51    -0.98 1.00     3509     4077
# muyesyes_Intercept                           -0.10      0.49    -1.05     0.87 1.00     3104     4102
# munoyes_log_cumulative_visits_c               0.33      0.07     0.20     0.46 1.00     7010     6584
# munoyes_beh_type                              0.21      0.14    -0.06     0.49 1.00     4850     5269
# munoyes_ageP                                  0.25      0.39    -0.55     1.04 1.00     4061     4511
# munoyes_sexM                                 -0.03      0.31    -0.64     0.61 1.00     4521     5200
# munoyes_log_training_solves                  -0.02      0.07    -0.17     0.11 1.00     4058     4949
# munoyes_scaletotal_time_present               1.81      0.13     1.55     2.08 1.00     6250     5784
# munoyes_con_pres1                             0.05      0.12    -0.19     0.28 1.00    11695     6699
# munoyes_log_cumulative_visits_c:beh_type     -0.13      0.05    -0.23    -0.03 1.00     9504     6703
# muyesno_log_cumulative_visits_c               0.11      0.08    -0.06     0.27 1.00     8607     6275
# muyesno_beh_type                              0.08      0.21    -0.37     0.48 1.00     3571     4349
# muyesno_ageP                                  0.58      0.56    -0.50     1.75 1.00     3245     3919
# muyesno_sexM                                  0.23      0.45    -0.61     1.14 1.00     3588     4580
# muyesno_log_training_solves                   0.01      0.11    -0.20     0.22 1.00     3489     4024
# muyesno_scaletotal_time_present              -0.34      0.26    -0.86     0.15 1.00    10887     6150
# muyesno_con_pres1                            -0.69      0.17    -1.02    -0.35 1.00    12680     6279
# muyesno_log_cumulative_visits_c:beh_type     -0.15      0.07    -0.28    -0.02 1.00     9960     6308
# muyesyes_log_cumulative_visits_c              0.87      0.09     0.70     1.04 1.00     9398     6265
# muyesyes_beh_type                             1.13      0.27     0.61     1.70 1.00     3376     4447
# muyesyes_ageP                                -0.46      0.77    -2.02     1.02 1.00     2910     3784
# muyesyes_sexM                                -0.13      0.61    -1.37     1.08 1.00     3420     4193
# muyesyes_log_training_solves                 -0.05      0.14    -0.33     0.23 1.00     2983     4158
# muyesyes_scaletotal_time_present              2.18      0.14     1.91     2.46 1.00     6081     5401
# muyesyes_con_pres1                           -0.55      0.15    -0.84    -0.26 1.00    12580     6804
# muyesyes_log_cumulative_visits_c:beh_type    -0.13      0.06    -0.26    -0.01 1.00     9390     5831
# 
# Draws were sampled using sampling(NUTS). For each parameter, Bulk_ESS
# and Tail_ESS are effective sample size measures, and Rhat is the potential
# scale reduction factor on split chains (at convergence, Rhat = 1).

plot.success <- plot(conditional_effects(model_success, categorical = T))

# get odds by exponteiationg
exp(fixef(model_success))


# munoyes_Intercept                         0.5802885  1.296671 0.34421163  0.9627952
# muyesno_Intercept                         0.1808215  1.474627 0.08149168  0.3745628
# muyesyes_Intercept                        0.9010209  1.628787 0.34932656  2.3766656
# munoyes_log_cumulative_visits_c           1.3883757  1.068188 1.21992441  1.5848014
# munoyes_beh_type                          1.2313049  1.149622 0.93820939  1.6267058
# munoyes_ageP                              1.2786022  1.483849 0.57684247  2.8249240
# munoyes_sexM                              0.9718385  1.368591 0.52701509  1.8441953
# munoyes_log_training_solves               0.9765585  1.074205 0.84199208  1.1195322
# munoyes_scaletotal_time_present           6.0973201  1.142469 4.72132738  7.9690185
# munoyes_con_pres1                         1.0473261  1.132584 0.82404559  1.3283975
# munoyes_log_cumulative_visits_c:beh_type  0.8793612  1.052679 0.79493992  0.9701812
# muyesno_log_cumulative_visits_c           1.1113402  1.085863 0.94604187  1.3108631
# muyesno_beh_type                          1.0866866  1.235915 0.69265089  1.6172624
# muyesno_ageP                              1.7818281  1.757025 0.60440570  5.7829090
# muyesno_sexM                              1.2528469  1.569182 0.54219814  3.1162725
# muyesno_log_training_solves               1.0055815  1.111814 0.82159297  1.2514941
# muyesno_scaletotal_time_present           0.7100485  1.296088 0.42138783  1.1644969
# muyesno_con_pres1                         0.5024574  1.185288 0.36112780  0.7016653
# muyesno_log_cumulative_visits_c:beh_type  0.8637158  1.068673 0.75589697  0.9796196
# muyesyes_log_cumulative_visits_c          2.3803859  1.090996 2.01043559  2.8295045
# muyesyes_beh_type                         3.1093435  1.314194 1.83530103  5.4787021
# muyesyes_ageP                             0.6330642  2.156004 0.13278335  2.7702426
# muyesyes_sexM                             0.8745125  1.847215 0.25389351  2.9362469
# muyesyes_log_training_solves              0.9476888  1.149725 0.72200103  1.2566384
# muyesyes_scaletotal_time_present          8.8196364  1.149317 6.77472554 11.7035225
# muyesyes_con_pres1                        0.5767238  1.159215 0.42966299  0.7709858
# muyesyes_log_cumulative_visits_c:beh_type 0.8763805  1.065457 0.77300411  0.9907792


d  <- as_draws_df(model_success)
b0 <- d$b_muyesyes_beh_type
b1 <- d$`b_muyesyes_log_cumulative_visits_c:beh_type`
sapply(c(1, 10, 50, 100, 353), function(v) {
  x <- exp(b0 + b1 * (log(v) - cp))
  c(visits = v, median = median(x), lo = quantile(x, .025), hi = quantile(x, .975))
})

# [,1]      [,2]      [,3]       [,4]       [,5]
# visits    1.000000 10.000000 50.000000 100.000000 353.000000
# median    5.229398  3.850193  3.106873   2.836153   2.400951
# lo.2.5%   2.554678  2.207452  1.850815   1.682998   1.344848
# hi.97.5% 10.814523  7.092700  5.511547   5.098350   4.448890


# 4.2) Latency to first solve ---------------------------------------

# for the latency model, we actually use the threshol of 10 solves rather than 10 visits. 

# and then subset it to individuals who are in the solving data
visit_data_latency <- visit_data[visit_data$Subject %in% solving_data$Subject,]

visit_data_latency$log_latency <- log(visit_data_latency$latency_to_first_solve)

# for the min 5 threshold, add a small constant as there is one entry with 0 latency
#visit_data_latency$log_latency <- log(visit_data_latency$latency_to_first_solve+0.00001)



# check for VIFs
check_collinearity(
  lm(rep(1, nrow(visit_data_latency[!is.na(visit_data_latency$latency_to_first_solve),])) ~  log_cumulative_visits + beh_type + age + sex + log_training_solves + con_pres,
     data = visit_data_latency[!is.na(visit_data_latency$latency_to_first_solve),])
)

# # Check for Multicollinearity
# 
# Low Correlation
# 
# Term  VIF   VIF 95% CI adj. VIF Tolerance Tolerance 95% CI
# log_cumulative_visits 1.34 [1.23, 1.48]     1.16      0.75     [0.68, 0.81]
# beh_type 2.18 [1.97, 2.45]     1.48      0.46     [0.41, 0.51]
# age 2.21 [1.99, 2.49]     1.49      0.45     [0.40, 0.50]
# sex 2.12 [1.91, 2.37]     1.45      0.47     [0.42, 0.52]
# log_training_solves 2.10 [1.89, 2.36]     1.45      0.48     [0.42, 0.53]
# con_pres 1.11 [1.05, 1.24]     1.05      0.90     [0.80, 0.95]


# this only considers visits where solves have actually occurred
model_latency <- brm(  
  log_latency ~ log_cumulative_visits*beh_type + age + sex + log_training_solves  + con_pres + (1 | Subject), 
  data = visit_data_latency, 
  cores = 4,
  iter = 4000,
  chains = 4,
  seed = 3,
  control = list(adapt_delta = 0.95)
)

#save(model_latency, file="model output/model_latency2.RDA")
load("model output/model_latency2.RDA")
#load("model output/model_latency_5.RDA") # load this model object for the threshold of 5


pp_lat <- pp_check(model_latency)
pp_lat
plot(model_latency)

r2_bayes(model_latency)

# # Bayesian R2 with Compatibility Interval
# 
# Conditional R2: 0.184 (95% CI [0.136, 0.229])
# Marginal R2: 0.186 (95% CI [0.077, 0.318])

summary(model_latency)

# Family: gaussian 
# Links: mu = identity 
# Formula: log_latency ~ log_cumulative_visits * beh_type + age + sex + log_training_solves + con_pres + (1 | Subject) 
# Data: visit_data_latency (Number of observations: 726) 
# Draws: 4 chains, each with iter = 4000; warmup = 2000; thin = 1;
# total post-warmup draws = 8000
# 
# Multilevel Hyperparameters:
#   ~Subject (Number of levels: 18) 
# Estimate Est.Error l-95% CI u-95% CI Rhat Bulk_ESS Tail_ESS
# sd(Intercept)     0.56      0.16     0.32     0.94 1.00     2433     3909
# 
# Regression Coefficients:
#   Estimate Est.Error l-95% CI u-95% CI Rhat Bulk_ESS Tail_ESS
# Intercept                          2.99      0.51     1.97     3.99 1.00     3726     4788
# log_cumulative_visits             -0.13      0.06    -0.24    -0.01 1.00     6561     6112
# beh_type                           0.35      0.24    -0.11     0.84 1.00     3917     5085
# ageP                              -0.08      0.48    -1.06     0.86 1.00     3606     4091
# sexM                               0.28      0.38    -0.47     1.05 1.00     4238     4511
# log_training_solves               -0.01      0.08    -0.18     0.16 1.00     3567     3960
# con_pres                          -0.35      0.11    -0.56    -0.14 1.00    10988     6083
# log_cumulative_visits:beh_type    -0.13      0.04    -0.22    -0.04 1.00     4853     4868
# 
# Further Distributional Parameters:
#   Estimate Est.Error l-95% CI u-95% CI Rhat Bulk_ESS Tail_ESS
# sigma     1.14      0.03     1.08     1.20 1.00    10775     5412
# 
# Draws were sampled using sampling(NUTS). For each parameter, Bulk_ESS
# and Tail_ESS are effective sample size measures, and Rhat is the potential
# scale reduction factor on split chains (at convergence, Rhat = 1).

plot.latency <- plot(conditional_effects(model_latency))

# overall, latency to first solve decreased with increasing experience
# shier individuals took overall longer but uncertain effect
# no effect of age, sex or training solves
# bolder individuals decreased their latency to first solve faster than shy individuals

d <- as_draws_df(model_latency)
b_v <- d$b_log_cumulative_visits
b_i <- d$`b_log_cumulative_visits:beh_type`
sapply(c(risk_averse = -1, medium = 0, bold = 1), function(z) {
  s <- b_v + b_i * z
  c(median = median(s), lo = quantile(s, .025), hi = quantile(s, .975))
})

# risk_averse     medium       bold
# median    0.0004321226 -0.1287323 -0.2588549
# lo.2.5%  -0.1599411690 -0.2419271 -0.3765875
# hi.97.5%  0.1674659556 -0.0140602 -0.1421233

b_b <- d$b_beh_type
cp <- mean(log(visit_data_latency$cumulative_visits))   # raw log visits (centering point)
sapply(c(1, 10, 50), function(v) {
  e <- b_b + b_i * (log(v) - cp)
  c(visits = v, median = median(e), lo = quantile(e, .025), hi = quantile(e, .975))
})

# [,1]         [,2]        [,3]
# visits   1.0000000 10.000000000 50.00000000
# median   0.8842403  0.582205885  0.37515315
# lo.2.5%  0.1175196  0.001493053 -0.09342937
# hi.97.5% 1.7236167  1.228010838  0.89154106

# 4.3) Motor flexibility - paw choice ----------------------

# this now takes the solving data - each data point is a solve rather than a visit

# For better interpretability, we also scale the experience (log_cumulative_count) and training exposure

solving_data$log_cumulative_count_sc <- scale(solving_data$log_cumulative_count)
solving_data$log_training_solves_sc <- scale(solving_data$log_training_solves)

# we make the reaction a binary variable (exit or stay)
solving_data$exit <- solving_data$reaction
solving_data$exit[solving_data$exit!="exit"] <- "stay"

# we make 'exit' the reference level
solving_data$exit <- as.factor(solving_data$exit)
solving_data$exit <- relevel(solving_data$exit, ref = "exit")

# make conspecific presence a factor
solving_data$con_pres <- as.factor(solving_data$con_pres)


# VIFs - these are the same for the next three models as they all have the same predictors
check_collinearity(
  lm( rep(1, nrow(solving_data)) ~  log_cumulative_count_sc + beh_type + age + sex + log_training_solves_sc + con_pres,
     data = solving_data)
)

# # Check for Multicollinearity
# 
# Low Correlation
# 
# Term  VIF   VIF 95% CI adj. VIF Tolerance Tolerance 95% CI
# log_cumulative_count_sc 1.41 [1.34, 1.50]     1.19      0.71     [0.67, 0.75]
# beh_type 2.24 [2.09, 2.41]     1.50      0.45     [0.42, 0.48]
# age 2.29 [2.14, 2.46]     1.51      0.44     [0.41, 0.47]
# sex 3.15 [2.92, 3.40]     1.77      0.32     [0.29, 0.34]
# log_training_solves_sc 2.96 [2.75, 3.19]     1.72      0.34     [0.31, 0.36]
# con_pres 1.08 [1.04, 1.15]     1.04      0.93     [0.87, 0.96]



# for vs ipsi vs contralateral paw
model_eff_paw <- brm(  
  paw_comb ~ log_cumulative_count_sc*beh_type + age + sex + log_training_solves_sc + con_pres + (1 | Subject), 
  data = solving_data, # we exclude both paws from this data set
  family = categorical(),
  cores = 4,
  iter = 4000,
  chains = 4,
  seed = 3,
  control = list(adapt_delta = 0.95)
)

#save(model_eff_paw, file="model output/model_eff_paw2.RDA")
load("model output/model_eff_paw2.RDA")
#load("model output/model_eff_paw_min5.RDA") # load this for the results of threshold of 5


pp_eff <- pp_check(model_eff_paw)
pp_eff

plot(model_eff_paw)

summary(model_eff_paw)

# Family: categorical 
# Links: mucontra = logit; muipsi = logit 
# Formula: paw_comb ~ log_cumulative_count_sc * beh_type + age + sex + log_training_solves_sc + con_pres + (1 | Subject) 
# Data: solving_data (Number of observations: 1908) 
# Draws: 4 chains, each with iter = 4000; warmup = 2000; thin = 1;
# total post-warmup draws = 8000
# 
# Multilevel Hyperparameters:
#   ~Subject (Number of levels: 18) 
# Estimate Est.Error l-95% CI u-95% CI Rhat Bulk_ESS Tail_ESS
# sd(mucontra_Intercept)     1.62      0.46     0.93     2.73 1.00     3801     5193
# sd(muipsi_Intercept)       1.25      0.32     0.77     2.02 1.00     3201     4743
# 
# Regression Coefficients:
#   Estimate Est.Error l-95% CI u-95% CI Rhat Bulk_ESS Tail_ESS
# mucontra_Intercept                           -1.26      0.79    -2.89     0.23 1.00     5232     5705
# muipsi_Intercept                              0.52      0.61    -0.71     1.69 1.00     5034     5158
# mucontra_log_cumulative_count_sc             -0.84      0.15    -1.14    -0.55 1.00     7063     6653
# mucontra_beh_type                            -0.50      0.45    -1.36     0.40 1.00     5593     5052
# mucontra_ageP                                -1.49      1.42    -4.47     1.22 1.00     4680     5069
# mucontra_sexM                                 0.94      1.04    -1.09     3.02 1.00     5716     5425
# mucontra_log_training_solves_sc               0.07      0.79    -1.58     1.62 1.00     4927     4632
# mucontra_con_pres1                           -0.83      0.29    -1.43    -0.28 1.00    11880     6278
# mucontra_log_cumulative_count_sc:beh_type     0.48      0.12     0.24     0.73 1.00     6603     6043
# muipsi_log_cumulative_count_sc               -0.86      0.14    -1.13    -0.59 1.00     5247     5945
# muipsi_beh_type                              -0.51      0.34    -1.17     0.15 1.00     5589     5239
# muipsi_ageP                                  -1.85      1.08    -4.01     0.28 1.00     4835     4450
# muipsi_sexM                                   0.27      0.82    -1.35     1.94 1.00     5247     5059
# muipsi_log_training_solves_sc                -0.15      0.60    -1.37     1.08 1.00     4954     4756
# muipsi_con_pres1                             -0.35      0.17    -0.68    -0.03 1.00    11957     5835
# muipsi_log_cumulative_count_sc:beh_type       0.14      0.09    -0.05     0.33 1.00     5807     5814
# 
# Draws were sampled using sampling(NUTS). For each parameter, Bulk_ESS
# and Tail_ESS are effective sample size measures, and Rhat is the potential
# scale reduction factor on split chains (at convergence, Rhat = 1).

plot.paw <- plot(conditional_effects(model_eff_paw, categorical =T))


plogis(fixef(model_eff_paw))

# Estimate Est.Error       Q2.5     Q97.5
# mucontra_Intercept                        0.2206443 0.6877222 0.05260478 0.5574230
# muipsi_Intercept                          0.6268848 0.6475531 0.32995499 0.8438021
# mucontra_log_cumulative_count_sc          0.3005446 0.5374932 0.24194861 0.3647824
# mucontra_beh_type                         0.3784702 0.6100715 0.20400785 0.5994214
# mucontra_ageP                             0.1833949 0.8057779 0.01126864 0.7725462
# mucontra_sexM                             0.7198579 0.7390987 0.25191313 0.9534225
# mucontra_log_training_solves_sc           0.5180204 0.6888434 0.17071284 0.8353942
# mucontra_con_pres1                        0.3031926 0.5727035 0.19235520 0.4304796
# mucontra_log_cumulative_count_sc:beh_type 0.6185112 0.5307273 0.56025961 0.6745133
# muipsi_log_cumulative_count_sc            0.2982217 0.5339117 0.24480157 0.3563584
# muipsi_beh_type                           0.3746617 0.5832316 0.23738700 0.5372547
# muipsi_ageP                               0.1357253 0.7471216 0.01777644 0.5689064
# muipsi_sexM                               0.5677363 0.6941111 0.20555707 0.8739921
# muipsi_log_training_solves_sc             0.4636346 0.6467531 0.20313114 0.7472162
# muipsi_con_pres1                          0.4126263 0.5423577 0.33532767 0.4919914
# muipsi_log_cumulative_count_sc:beh_type   0.5353048 0.5234546 0.48867227 0.5807362

exp(fixef(model_eff_paw))

# Estimate Est.Error       Q2.5      Q97.5
# mucontra_Intercept                        0.2831112  2.202277 0.05552570  1.2594940
# muipsi_Intercept                          1.6801374  1.837307 0.49243705  5.4021357
# mucontra_log_cumulative_count_sc          0.4296837  1.162130 0.31917178  0.5742637
# mucontra_beh_type                         0.6089333  1.564573 0.25629380  1.4963892
# mucontra_ageP                             0.2245822  4.148744 0.01139707  3.3964968
# mucontra_sexM                             2.5696171  2.832867 0.33674314 20.4695822
# mucontra_log_training_solves_sc           1.0747767  2.213816 0.20585492  5.0751200
# mucontra_con_pres1                        0.4351167  1.340296 0.23816807  0.7558633
# mucontra_log_cumulative_count_sc:beh_type 1.6213092  1.130957 1.27406903  2.0723221
# muipsi_log_cumulative_count_sc            0.4249514  1.145516 0.32415530  0.5536596
# muipsi_beh_type                           0.5991345  1.399414 0.31128108  1.1610160
# muipsi_ageP                               0.1570395  2.954470 0.01809816  1.3196818
# muipsi_sexM                               1.3134027  2.269161 0.25874366  6.9360092
# muipsi_log_training_solves_sc             0.8644006  1.830881 0.25491163  2.9559498
# muipsi_con_pres1                          0.7024936  1.185113 0.50450073  0.9684706
# muipsi_log_cumulative_count_sc:beh_type   1.1519481  1.098436 0.95569289  1.3851333

# Average or fixed values for other predictors
newdata <- data.frame(
  age = c("A", "P"),                 # adult, juvenile
  log_cumulative_count_sc = 0,       # centered
  beh_type = 0,                       # centered
  sex = "F",                           # reference sex
  log_training_solves_sc = 0,# centered
  con_pres=0, # no conspecific present
  Subject = NA                         # random effects set to population-level
)

# Get posterior predicted probabilities for all draws
pp <- posterior_epred(model_eff_paw, newdata = newdata, re_formula = NA)


# Predicted probabilities of "both" for each age class (already extracted)
both_adult    <- pp[,1,"both"]
both_juvenile <- pp[,2,"both"]

# Convert probabilities to odds
odds_adult    <- both_adult / (1 - both_adult)
odds_juvenile <- both_juvenile / (1 - both_juvenile)

# Odds ratio: juvenile relative to adult
OR_juv_vs_adult <- odds_juvenile / odds_adult

# Posterior summary
mean(OR_juv_vs_adult)
#  8.723502

median(OR_juv_vs_adult)
#[1] 5.51327
quantile(OR_juv_vs_adult, c(0.025, 0.975))
# 2.5%     97.5% 
#   0.852672 35.417501

# 4.4) Making mistakes -----------------------------------------------

# how many unsuccessful box interactions occurred between successes (solves)
model_box_int <- brm(  
  box_between_solves ~ log_cumulative_count_sc*beh_type + age + sex + log_training_solves_sc + con_pres + (1 | Subject), 
  data = solving_data, 
  family = negbinomial(),
  cores = 4,
  iter = 4000,
  chains = 4,
  seed = 3,
  control = list(adapt_delta = 0.95)
)

#save(model_box_int, file="model output/model_box_int2.RDA")
load("model output/model_box_int2.RDA")
#load("model output/model_box_int_5.RDA") # load this for threshold of 5

r2_bayes(model_box_int)

# # Bayesian R2 with Compatibility Interval
# 
# Conditional R2: 0.171 (95% CI [0.120, 0.226])
# Marginal R2: 0.119 (95% CI [0.031, 0.257])

pp_box <- pp_check(model_box_int)
pp_box

plot(model_box_int)

summary(model_box_int)

# Family: negbinomial 
# Links: mu = log 
# Formula: box_between_solves ~ log_cumulative_count_sc * beh_type + age + sex + log_training_solves_sc + con_pres + (1 | Subject) 
# Data: solving_data (Number of observations: 1908) 
# Draws: 4 chains, each with iter = 4000; warmup = 2000; thin = 1;
# total post-warmup draws = 8000
# 
# Multilevel Hyperparameters:
#   ~Subject (Number of levels: 18) 
# Estimate Est.Error l-95% CI u-95% CI Rhat Bulk_ESS Tail_ESS
# sd(Intercept)     0.53      0.14     0.31     0.86 1.00     2423     4404
# 
# Regression Coefficients:
#   Estimate Est.Error l-95% CI u-95% CI Rhat Bulk_ESS Tail_ESS
# Intercept                            0.67      0.26     0.14     1.18 1.00     3584     4168
# log_cumulative_count_sc             -0.28      0.04    -0.36    -0.21 1.00    10300     5949
# beh_type                            -0.18      0.14    -0.46     0.09 1.00     3774     4333
# ageP                                -0.40      0.46    -1.31     0.50 1.00     3314     3939
# sexM                                -0.59      0.35    -1.29     0.11 1.00     3794     4164
# log_training_solves_sc              -0.09      0.25    -0.59     0.39 1.00     3437     3803
# con_pres1                           -0.28      0.09    -0.46    -0.11 1.00     9737     4959
# log_cumulative_count_sc:beh_type     0.10      0.03     0.04     0.15 1.00    10734     5551
# 
# Further Distributional Parameters:
#   Estimate Est.Error l-95% CI u-95% CI Rhat Bulk_ESS Tail_ESS
# shape     1.47      0.10     1.29     1.68 1.00    11318     5917
# 
# Draws were sampled using sampling(NUTS). For each parameter, Bulk_ESS
# and Tail_ESS are effective sample size measures, and Rhat is the potential
# scale reduction factor on split chains (at convergence, Rhat = 1).

exp(fixef(model_box_int))

# Estimate Est.Error      Q2.5     Q97.5
# Intercept                        1.9460963  1.295214 1.1480936 3.2540399
# log_cumulative_count_sc          0.7553938  1.038650 0.7003411 0.8131538
# beh_type                         0.8325208  1.145986 0.6330650 1.0951892
# ageP                             0.6713735  1.578417 0.2692780 1.6413211
# sexM                             0.5561044  1.422363 0.2748573 1.1196859
# log_training_solves_sc           0.9155800  1.280989 0.5551846 1.4837501
# con_pres1                        0.7531977  1.094438 0.6315961 0.8988917
# log_cumulative_count_sc:beh_type 1.1018812  1.028897 1.0427920 1.1651588


plot.mistakes <- plot(conditional_effects(model_box_int))

# failure rate decreases with increasing experience
# no overall effect of boldness, age, sex, training solves
# shy individuals make more mistakes initially compared to bolder individuals, but improve over time, while bold individuals are consistent in their mistake rate


d <- as_draws_df(model_box_int)
b0 <- d$b_beh_type
b1 <- d$`b_log_cumulative_count_sc:beh_type`
cp <- mean(log(solving_data$cumulative_count))
s  <- sd(log(solving_data$cumulative_count))
sapply(c(1, 10, 100, 500), function(v) {
  x <- exp(b0 + b1 * (log(v) - cp) / s)
  c(solves = v, median = median(x), lo = quantile(x, .025), hi = quantile(x, .975))
})

# 
# [,1]       [,2]        [,3]        [,4]
# solves   1.0000000 10.0000000 100.0000000 500.0000000
# median   0.6146575  0.7210718   0.8440580   0.9440876
# lo.2.5%  0.4573270  0.5494481   0.6439328   0.7088645
# hi.97.5% 0.8374168  0.9551448   1.1175567   1.2697872

# 4.5)  reaction after solve  -------------------------------------------------------------------


model_stay <- brm(  
  exit ~ log_cumulative_count_sc*beh_type + age + sex + log_training_solves_sc + con_pres + (1 | Subject), 
  data = solving_data, 
  family = bernoulli(),
  cores = 4,
  iter = 4000,
  chains = 4,
  seed = 3,
  control = list(adapt_delta = 0.95)
)

#save(model_stay, file="model output/model_stay2.RDA")
load("model output/model_stay2.RDA")
#load("model output/model_stay_min5.RDA") # load this model output for a treshold of 5

r2_bayes(model_stay)
# # Bayesian R2 with Compatibility Interval
# 
# Conditional R2: 0.686 (95% CI [0.670, 0.700])
# Marginal R2: 0.592 (95% CI [0.480, 0.644])

pp_stay <- pp_check(model_stay)
pp_stay

summary(model_stay)
# Family: bernoulli 
# Links: mu = logit 
# Formula: exit ~ log_cumulative_count_sc * beh_type + age + sex + log_training_solves_sc + con_pres + (1 | Subject) 
# Data: solving_data (Number of observations: 1908) 
# Draws: 4 chains, each with iter = 4000; warmup = 2000; thin = 1;
# total post-warmup draws = 8000
# 
# Multilevel Hyperparameters:
#   ~Subject (Number of levels: 18) 
# Estimate Est.Error l-95% CI u-95% CI Rhat Bulk_ESS Tail_ESS
# sd(Intercept)     2.45      0.71     1.43     4.17 1.00     3045     4690
# 
# Regression Coefficients:
#   Estimate Est.Error l-95% CI u-95% CI Rhat Bulk_ESS Tail_ESS
# Intercept                            3.02      1.23     0.49     5.44 1.00     4331     4084
# log_cumulative_count_sc              2.76      0.22     2.35     3.19 1.00    10037     6105
# beh_type                             1.61      0.74     0.18     3.14 1.00     4310     4299
# ageP                                -4.11      2.33    -9.07     0.28 1.00     4077     3765
# sexM                                -0.63      1.71    -4.03     2.80 1.00     4162     4130
# log_training_solves_sc              -1.17      1.16    -3.56     1.05 1.00     4188     4292
# con_pres1                           -0.13      0.36    -0.84     0.59 1.00    11605     5930
# log_cumulative_count_sc:beh_type    -1.51      0.26    -2.03    -1.03 1.00     8210     5644
# 
# Draws were sampled using sampling(NUTS). For each parameter, Bulk_ESS
# and Tail_ESS are effective sample size measures, and Rhat is the potential
# scale reduction factor on split chains (at convergence, Rhat = 1).

exp(fixef(model_stay))

# Estimate Est.Error         Q2.5       Q97.5
# Intercept                        20.54165931  3.436049 1.631185e+00 229.8586951
# log_cumulative_count_sc          15.82588056  1.240910 1.052516e+01  24.3716679
# beh_type                          4.98414335  2.101855 1.198401e+00  23.1636290
# ageP                              0.01647614 10.295327 1.156074e-04   1.3247669
# sexM                              0.53209339  5.538675 1.776874e-02  16.4346267
# log_training_solves_sc            0.30890760  3.192185 2.834623e-02   2.8692880
# con_pres1                         0.87501248  1.437057 4.302601e-01   1.7952471
# log_cumulative_count_sc:beh_type  0.22139832  1.296728 1.309295e-01   0.3583747


plot.stay <- plot(conditional_effects(model_stay))

# overall more likely to stay with increaseing experience. 
# boldness positively correlated with staying, but uncertainty
# juveniles less likely to stay
# no effect of sex or training solves
# bolder individuals stay from the start, shier individuals imporove with increasing experience

d <- as_draws_df(model_stay)   # use your model object name
b0 <- d$b_beh_type
b1 <- d$`b_log_cumulative_count_sc:beh_type`
cp <- mean(log(solving_data$cumulative_count))
s  <- sd(log(solving_data$cumulative_count))
sapply(c(1, 10, 100, 500), function(v) {
  x <- exp(b0 + b1 * (log(v) - cp) / s)
  c(solves = v, median = median(x), lo = quantile(x, .025), hi = quantile(x, .975))
})

# [,1]      [,2]        [,3]        [,4]
# solves      1.00000  10.00000 100.0000000 500.0000000
# median    515.60800  43.65238   3.7198004   0.6690742
# lo.2.5%    73.53532   9.97896   0.8961566   0.1290175
# hi.97.5% 5338.02046 262.99137  17.6380066   3.3212920

# 5) Tables ---------------------------------------------------------------


### SUCCESS (SOLVING)
tidy_success <- tidy(
  model_success,
  effects = "fixed",
  conf.int = TRUE,
  conf.level = 0.95
) %>%
  select(-std.error) %>%
  mutate(
    estimate = exp(estimate),
    conf.low = exp(conf.low),
    conf.high = exp(conf.high)
  )

tidy_success <- tidy_success %>%
  mutate(
    term = recode(term,
                  "munoyes_(Intercept)" = "Intercept [consume]",
                  "muyesno_(Intercept)" = "Intercept [solve]",
                  "muyesyes_(Intercept)" = "Intercept [solve, consume]",
                  
                  "munoyes_log_cumulative_visits_c" = "Cumulative visits (log) [consume]",
                  "munoyes_beh_type" = "Boldness [consume]",
                  "munoyes_ageP"  = "Age (J:A) [consume]",
                  "munoyes_sexM"  = "Sex (M:F) [consume]",
                  "munoyes_log_training_solves" = "# training solves (log) [consume]",
                  "munoyes_scaletotal_time_present" = "Visit duration [consume]",
                  "munoyes_con_pres1" = "Conspecific presence at arrival [consume]",
                  "munoyes_log_cumulative_visits_c:beh_type" = "Cumulative visits (log) × Boldness [consume]",
                  
                  "muyesno_log_cumulative_visits_c" = "Cumulative visits (log) [solve]",
                  "muyesno_beh_type" = "Boldness [solve]",
                  "muyesno_ageP"  = "Age (J:A) [solve]",
                  "muyesno_sexM"  = "Sex (M:F) [solve]",
                  "muyesno_log_training_solves" = "# training solves (log) [solve]",
                  "muyesno_scaletotal_time_present" = "Visit duration [solve]",
                  "muyesno_con_pres1" = "Conspecific presence at arrival [solve]",
                  "muyesno_log_cumulative_visits_c:beh_type" = "Cumulative visits (log) × Boldness [solve]",
                  
                  "muyesyes_log_cumulative_visits_c" = "Cumulative visits (log) [solve, consume]",
                  "muyesyes_beh_type" = "Boldness [solve, consume]",
                  "muyesyes_ageP"  = "Age (J:A) [solve, consume]",
                  "muyesyes_sexM"  = "Sex (M:F) [solve, consume]",
                  "muyesyes_log_training_solves" = "# training solves (log) [solve, consume]",
                  "muyesyes_scaletotal_time_present" = "Visit duration [solve, consume]",
                  "muyesyes_con_pres1" = "Conspecific presence at arrival [solve, consume]",
                  "muyesyes_log_cumulative_visits_c:beh_type" = "Cumulative visits (log) × Boldness [solve, consume]"
    )
  )


table_success <- tidy_success %>%
  select(
    Predictor = term,
    Estimate = estimate,
    `Lower 95% CI` = conf.low,
    `Upper 95% CI` = conf.high
  ) %>%
  mutate(across(where(is.numeric), round, 2))

ft_success <- flextable(table_success) %>%
  autofit()

doc <- read_docx() %>%
  body_add_flextable(ft_success)

print(doc, target = "Tables output/model_success_5.docx")



# LATENCY

# LATENCY MODEL
tidy_latency <- tidy(
  model_latency,
  effects = "fixed",
  conf.int = TRUE,
  conf.level = 0.95
) %>%
  select(-std.error)

tidy_latency <- tidy_latency %>%
  mutate(
    term = recode(term,
                  "Intercept" = "Intercept",
                  "log_cumulative_visits" = "Cumulative visits (log)",
                  "beh_type" = "Boldness",
                  "ageP" = "Age (J:A)",
                  "sexM" = "Sex (M:F)",
                  "log_training_solves" = "# training solves (log)",
                  "con_pres1" = "Conspecific presence",
                  "log_cumulative_visits:beh_type" =
                    "Cumulative visits (log) × Boldness"
    )
  )

table_latency <- tidy_latency %>%
  select(
    Predictor = term,
    Estimate = estimate,
    `Lower 95% CI` = conf.low,
    `Upper 95% CI` = conf.high
  ) %>%
  mutate(across(where(is.numeric), round, 2))

ft_latency <- flextable(table_latency) %>%
  autofit()

doc <- read_docx() %>%
  body_add_flextable(ft_latency)

print(doc, target = "Tables output/model_latency.docx")


# PAW USE


tidy_eff_or <- tidy(
  model_eff_paw,
  effects = "fixed",
  conf.int = TRUE,
  conf.level = 0.95
) %>%
  mutate(
    OR = exp(estimate),
    CI_low = exp(conf.low),
    CI_high = exp(conf.high)
  ) %>%
  select(-estimate, -std.error, -conf.low, -conf.high)

tidy_eff_or <- tidy_eff_or %>%
  mutate(
    term = recode(term,
                  "mucontra_(Intercept)" = "Intercept [contra]",
                  "muipsi_(Intercept)" = "Intercept [ipsi]",
                  "mucontra_log_cumulative_count_sc" = "Cumulative solves (log) [contra]",
                  "muipsi_log_cumulative_count_sc" = "Cumulative solves (log) [ipsi]",
                  "mucontra_beh_type" = "Boldness [contra]",
                  "mucontra_ageP" = "Age (J:A) [contra]",
                  "mucontra_sexM" = "Sex (M:F) [contra]",
                  "mucontra_log_training_solves_sc" = "# training solves (log) [contra]",
                  "mucontra_con_pres1" = "Conspecific presence [contra]",
                  
                  "mucontra_log_cumulative_count_sc:beh_type" =
                    "Cumulative solves (log) × Boldness [contra]",
                  "muipsi_beh_type" = "Boldness [ipsi]",
                  "muipsi_ageP" = "Age (J:A) [ipsi]",
                  "muipsi_sexM" = "Sex (M:F) [ipsi]",
                  "muipsi_log_training_solves_sc" = "# training solves (log) [ipsi]",
                  "muipsi_con_pres1" = "Conspecific presence [ipsi]",
                  "muipsi_log_cumulative_count_sc:beh_type" =
                    "Cumulative solves (log) × Boldness [ipsi]"
    )
  )

table_eff <- tidy_eff_or %>%
  filter(term != "Intercept") %>%   # recommended
  select(
    Predictor = term,
    `Odds ratio (OR)` = OR,
    `Lower 95% CI` = CI_low,
    `Upper 95% CI` = CI_high
  ) %>%
  mutate(across(where(is.numeric), round, 2))


ft_eff <- flextable(table_eff) %>%
  autofit()

doc <- read_docx() %>%
  body_add_flextable(ft_eff)

print(doc, target = "Tables output/model_eff_paw_OR.docx")


# STAY VS EXIT

tidy_stay_or <- tidy(
  model_stay,
  effects = "fixed",
  conf.int = TRUE,
  conf.level = 0.95
) %>%
  mutate(
    OR = exp(estimate),
    CI_low = exp(conf.low),
    CI_high = exp(conf.high)
  ) %>%
  select(-estimate, -std.error, -conf.low, -conf.high)

tidy_stay_or <- tidy_stay_or %>%
  mutate(
    term = recode(term,
                  "Intercept" = "Intercept",
                  "log_cumulative_count_sc" = "Cumulative solves (log)",
                  "beh_type" = "Boldness",
                  "ageP" = "Age (J:A)",
                  "sexM" = "Sex (M:F)",
                  "log_training_solves_sc" = "# training solves (log)",
                  "con_pres1" = "Conspecific presence",
                  "log_cumulative_count_sc:beh_type" =
                    "Cumulative solves (log) × Boldness"
    )
  )

table_eff <- tidy_stay_or %>%
  filter(term != "Intercept") %>%   # recommended
  select(
    Predictor = term,
    `Odds ratio (OR)` = OR,
    `Lower 95% CI` = CI_low,
    `Upper 95% CI` = CI_high
  ) %>%
  mutate(across(where(is.numeric), round, 2))


ft_eff <- flextable(table_eff) %>%
  autofit()

doc <- read_docx() %>%
  body_add_flextable(ft_eff)

print(doc, target = "Tables output/model_stay_OR.docx")

# MANIPULATION ERRORS (negative binomial, log link) -> incidence rate ratios
tidy_box <- tidy(
  model_box_int,
  effects = "fixed",
  conf.int = TRUE,
  conf.level = 0.95
) %>%
  mutate(
    IRR      = exp(estimate),
    CI_low   = exp(conf.low),
    CI_high  = exp(conf.high)
  ) %>%
  select(-estimate, -std.error, -conf.low, -conf.high)

tidy_box <- tidy_box %>%
  mutate(
    term = dplyr::recode(term,
                         "Intercept" = "Intercept",
                         "log_cumulative_count_sc" = "Cumulative solves (log)",
                         "beh_type" = "Boldness",
                         "ageP" = "Age (J:A)",
                         "sexM" = "Sex (M:F)",
                         "log_training_solves_sc" = "# training solves (log)",
                         "con_pres1" = "Conspecific presence",
                         "log_cumulative_count_sc:beh_type" =
                           "Cumulative solves (log) × Boldness"
    )
  )

table_box <- tidy_box %>%
  filter(term != "Intercept") %>%
  select(
    Predictor = term,
    `Incidence rate ratio (IRR)` = IRR,
    `Lower 95% CI` = CI_low,
    `Upper 95% CI` = CI_high
  ) %>%
  mutate(across(where(is.numeric), ~ round(.x, 2)))

ft_box <- flextable(table_box) %>% autofit()

doc <- read_docx() %>% body_add_flextable(ft_box)
print(doc, target = "Tables output/model_box_int_IRR.docx")



# 6) Figures ---------------------------------------------------------------


# 6.1) Solving efficiency figures -----------------------------------------


# Panel a: change in success given cumulative visits 


plot.success <- plot(conditional_effects(model_success, categorical=T))

p1.panel.a <- plot.success$`log_cumulative_visits_c:cats__` +
  geom_line(linewidth = 1.2) +
  geom_ribbon(aes(ymin = lower__, ymax = upper__), alpha = 0.1, color = NA) +
  theme_bw() +
  ylim(c(0,1))+
  scale_x_continuous(
    breaks = log(c(1, 10, 100, 350)) - mean(log(visit_data_success$cumulative_visits)),
    labels = c(1, 10, 100, 350)
  )+
#  labs(x = "Cumulative visits (log scale)", ...)+
  labs(
    x = "Cumulative visits\n(log scale)",
    y = "Predicted probability"
  ) +
  scale_color_manual(values = c("yes_yes"= "#cb5357", "yes_no"= "#a361c7", "no_yes" = "#4aac8d", "no_no"= "#bd893d"),labels=c("none", "consume", "solve", "solve & consume"), name = "Behavior") +
  scale_fill_manual(values = c("yes_yes"= "#cb5357", "yes_no"= "#a361c7", "no_yes" = "#4aac8d", "no_no"= "#bd893d"), labels=c("none", "consume", "solve", "solve & consume"), name = "Behavior")+
  theme(legend.position.inside = c(0.7, 0.81), legend.position = "inside")

# panel b: change in success given boldness

p1.panel.b <- plot.success$`beh_type:cats__`+
  geom_line(linewidth = 1.2) +
  geom_ribbon(aes(ymin = lower__, ymax = upper__), alpha = 0.1, color = NA) +
  theme_bw() +
  labs(
    x = "Boldness\n ",
    y = ""
  ) +
  scale_color_manual(values = c("yes_yes"= "#cb5357", "yes_no"= "#a361c7", "no_yes" = "#4aac8d", "no_no"= "#bd893d"),labels=c("none", "consume", "solve", "solve & consume"), name = "Behavior") +
  scale_fill_manual(values = c("yes_yes"= "#cb5357", "yes_no"= "#a361c7", "no_yes" = "#4aac8d", "no_no"= "#bd893d"), labels=c("none", "consume", "solve", "solve & consume"), name = "Behavior")+
  theme(legend.position.inside = c(0.7, 0.81), legend.position = "inside")+
  theme(
    axis.text.y = element_blank()
  )




# panel c: time spent

p1.panel.c <- plot.success$`total_time_present:cats__`+
  geom_line(linewidth = 1.2) +
  geom_ribbon(aes(ymin = lower__, ymax = upper__), alpha = 0.1, color = NA) +
  theme_bw() +
  labs(
    x = "Visit duration (s)\n ",
    y = ""
  ) +
  scale_color_manual(values = c("yes_yes"= "#cb5357", "yes_no"= "#a361c7", "no_yes" = "#4aac8d", "no_no"= "#bd893d"),labels=c("none", "consume", "solve", "solve & consume"), name = "Behavior") +
  scale_fill_manual(values = c("yes_yes"= "#cb5357", "yes_no"= "#a361c7", "no_yes" = "#4aac8d", "no_no"= "#bd893d"), labels=c("none", "consume", "solve", "solve & consume"), name = "Behavior")+
  theme(legend.position.inside = c(0.7, 0.81), legend.position = "inside")+
  theme(
    axis.text.y = element_blank()
  )




# for interactions, we have to define which values we would like visualized.
# we take the 10th, 50th and 90th percentile of boldness scores

# Define the percentiles of beh_type you want to visualize
beh.type.q <- as.numeric(quantile(subject_data$beh_type, c(0.1, 0.5, 0.9)))

get_preds <- function(model, beh_values) {
  pred_list <- lapply(beh_values, function(x) {
    ce <- conditional_effects(
      model,
      effects = "log_cumulative_visits_c",
      conditions = data.frame(beh_type = x),
      categorical = TRUE
    )
    ce[[1]] %>%
      rename(success_level = cats__) %>%
      mutate(beh_type_val = x)
  })
  do.call(rbind, pred_list)
}

beh.type.q <- as.numeric(quantile(subject_data$beh_type, c(0.1, 0.5, 0.9)))
df_success <- get_preds(model_success, beh.type.q)

p1.row2 <- ggplot(df_success,
                  aes(x = log_cumulative_visits_c,
                      y = estimate__,
                      color = factor(beh_type_val),
                      fill = factor(beh_type_val),
                      group = factor(beh_type_val))) +
  geom_line(linewidth = 1.2) +
  geom_ribbon(aes(ymin = lower__, ymax = upper__),
              alpha = 0.2, color = NA) +
  scale_x_continuous(
    breaks = log(c(1, 10, 100, 350)) - mean(log(visit_data_success$cumulative_visits)),
    labels = c(1, 10, 100, 350)
  ) +
  facet_wrap(~ success_level,
             labeller = labeller(
               success_level = c(
                 no_no = "None",
                 no_yes = "Consume",
                 yes_no = "Solve",
                 yes_yes = "Solve & consume"
               )
             ), nrow = 1) +
  theme_bw() +
  labs(x = "Cumulative visits (log scale)",
       y = "Predicted probability") +
  scale_color_manual(values = c("#fdd0a2", "#fd8d3c", "#a63603"),
                     labels = c("risk-averse", "medium", "bold"), name = "Boldness") +
  scale_fill_manual(values = c("#fdd0a2", "#fd8d3c", "#a63603"),
                    labels = c("risk-averse", "medium", "bold"), name = "Boldness") +
  theme(
    legend.position = "inside",
    legend.position.inside = c(0.32, 0.85),
    legend.background = element_rect(fill = "white", color = NA),
    legend.key = element_rect(fill = "white", color = NA),
    legend.title = element_text(size = 10),
    legend.text = element_text(size = 9)
  )

# panel d: latency
conditions = list(beh_type = beh.type.q)
ce <- plot(conditional_effects(
  model_latency,
  effects = "log_cumulative_visits",
  conditions = list(beh_type = beh.type.q),
  prob = 0.95,    # 95% credible interval
  spaghetti = FALSE
))

df_latency <- ce$log_cumulative_visits$data


df_latency <- df_latency %>%
  mutate(beh_type_label = factor(beh_type,
                                 levels = beh.type.q,
                                 labels = c("risk-averse", "medium", "bold")))



range(df_latency$log_cumulative_visits)
range(log(visit_data_latency$cumulative_visits))   # what the model actually saw

p1.panel.d <- ggplot(df_latency,
       aes(x = log_cumulative_visits,
           y = estimate__,
           color = beh_type_label,
           fill = beh_type_label,
           group = beh_type_label)) +
  geom_line(linewidth = 1.2) +
  scale_x_continuous(
    breaks = log(c(1, 10, 100, 350)),
    labels = c(1, 10, 100, 350)
  ) +
  geom_ribbon(aes(ymin = lower__, ymax = upper__),
              alpha = 0.1, color = NA) +
  theme_bw()+
  labs(
    x = "Cumulative visits\n(log scale)",
    y = "Latency to first solve"
  )  +scale_color_manual(values = c(
    "#fdd0a2",
    "#fd8d3c",
    "#a63603"
  ), labels = c("risk-averse", "medium", "bold"), name="Boldness") +
  scale_fill_manual(values = c(
    "#fdd0a2",
    "#fd8d3c",
    "#a63603"
  ), labels = c("risk-averse", "medium", "bold"), name="Boldness")+
  theme(legend.position = "none")




# First row: panels a–c share a legend
row1_part <- ggarrange(
  p1.panel.a, p1.panel.b, p1.panel.c,
  ncol = 3,
  labels = c("a", "b", "c"),
  common.legend = TRUE,
  legend = "bottom"
)

p1.panel.d <- p1.panel.d +
  theme(
    plot.margin = unit(c(5.5, 5.5, 35, 5.5), "pt")  
    # order: top, right, bottom, left
  )

row1_part2 <- ggarrange(
  p1.panel.d,
  labels = "d",
  common.legend = F,  # no legend will appear
  legend = "none"
)

# Then, combine this sub-row with the fourth plot (which keeps its own legend)
full_row <- ggarrange(
  row1_part, row1_part2,
  ncol = 2,
  widths = c(3, 1)  # the first “column” (3-panel sub-row) is 3× wider than the 2nd column
)


# Second row: panels d–f share a legend
row2 <- 
  ggarrange(p1.row2, ncol = 1, labels = c("e"), common.legend = T, legend="bottom")


ggarrange(full_row, row2, nrow = 2)

ggsave("Figures output/Solving efficiency.tiff", units='in', width=8, height=5.5, bg="white")


# 6.2) Behavioral skill figures -------------------------------------------

# panel a: paw use with experience

plot.eff.paw <- plot(conditional_effects(model_eff_paw, categorical=T))

p2.panel.a <- plot.eff.paw$`log_cumulative_count_sc:cats__`+
  geom_line(linewidth = 1.2) +
  geom_ribbon(aes(ymin = lower__, ymax = upper__), alpha = 0.05, color = NA) +
  theme_bw() +
  scale_x_continuous(
    breaks = (log(c(1, 10, 50, 100, 500)) - cp) / s,
    labels = c(1, 10, 50, 100, 500)
  ) +
  labs(
    x = "Cumulative solves (log scale)",
    y = "Probability of paw choice"
  ) +
  scale_color_manual(values = c( "#00441b","#a0da94", "#5fb95f"), labels=c("both", "contralateral", "ipsilateral"), name = "Paw choice") +
  scale_fill_manual(values = c( "#00441b","#a0da94", "#5fb95f"), labels=c("both", "contralateral", "ipsilateral"), name = "Paw choice")+
  theme(legend.position.inside = c(0.7, 0.81), legend.position = "inside")


p2.panel.b <- plot.eff.paw$`age:cats__`+
  theme_bw() +
  labs(
    x = "Age category",
    y = "Probability"
  ) +
  ylim(c(0,1))+
  scale_color_manual(values = c( "#00441b","#a0da94", "#5fb95f"), labels=c("both", "contralateral", "ipsilateral"), name = "Paw use") +
  scale_fill_manual(values = c( "#00441b","#a0da94", "#5fb95f"), labels=c("both", "contralateral", "ipsilateral"), name = "Paw use")+
  scale_x_discrete(labels = c("A" = "Adult", "P" = "Juvenile"))


# get interaction plot for boldness x paw choice

beh.type.q <- as.numeric(quantile(subject_data$beh_type, c(0.1, 0.5, 0.9)))

get_preds <- function(model, beh_values) {
  pred_list <- lapply(beh_values, function(x) {
    ce <- conditional_effects(
      model,
      effects = "log_cumulative_count_sc",
      conditions = list(beh_type = x),
      categorical = TRUE
    )
    
    df <- ce[[1]] %>%
      rename(success_level = cats__) %>%   # outcome category
      mutate(beh_type_val = x)
    
    df
  })
  
  do.call(rbind, pred_list)
}

df_paw_choice <- get_preds(model_eff_paw, beh.type.q)



# panel c:

# Generate conditional effects for each value
pred_list <- lapply(beh.type.q, function(x) {
  ce <- conditional_effects(
    model_box_int,
    effects = "log_cumulative_count_sc",
    conditions = list(beh_type = x)
  )
  # Extract the data for 'solved' outcome (or whichever outcome you want)
  df <- ce$log_cumulative_count_sc
  df$beh_type_val <- x
  df
})

# Combine into one dataframe
df_combined <- bind_rows(pred_list)

# Optionally, label the percentiles for the legend
df_combined <- df_combined %>%
  mutate(beh_type_label = factor(beh_type_val,
                                 levels = beh.type.q,
                                 labels = c("risk-averse", "medium", "bold")))

# Plot all curves in one panel
p2.panel.c <- ggplot(df_combined, aes(x = log_cumulative_count_sc, y = estimate__, color = beh_type_label, fill = beh_type_label)) +
  geom_line(linewidth = 1.2) +
  geom_ribbon(aes(ymin = lower__, ymax = upper__), alpha = 0.2, color = NA) +
  theme_bw() +
  scale_x_continuous(
    breaks = (log(c(1, 10, 50, 100, 500)) - cp) / s,
    labels = c(1, 10, 50, 100, 500)
  ) +
  labs(
    x = "Cumulative solves (log scale)",
    y = "Manipulation errors \n between solves",
    color = "Boldness",
    fill = "Boldness"
  ) +
  scale_color_manual(values = c("#fdd0a2","#fd8d3c", "#a63603" )) +
  scale_fill_manual(values = c("#fdd0a2","#fd8d3c", "#a63603" ))+
  theme(legend.position.inside = c(0.7, 0.81), legend.position = "inside")


# panel d:

# Generate conditional effects for each value
pred_list <- lapply(beh.type.q, function(x) {
  ce <- conditional_effects(
    model_stay,
    effects = "log_cumulative_count_sc",
    conditions = list(beh_type = x)
  )
  # Extract the data for 'solved' outcome (or whichever outcome you want)
  df <- ce$log_cumulative_count_sc
  df$beh_type_val <- x
  df
})

# Combine into one dataframe
df_combined <- bind_rows(pred_list)

# Optionally, label the percentiles for the legend
df_combined <- df_combined %>%
  mutate(beh_type_label = factor(beh_type_val,
                                 levels = beh.type.q,
                                 labels = c("Risk-averse", "Medium", "Bold")))

# Plot all curves in one panel
p2.panel.d <- ggplot(df_combined, aes(x = log_cumulative_count_sc, y = estimate__, color = beh_type_label, fill = beh_type_label)) +
  geom_line(linewidth = 1.2) +
  geom_ribbon(aes(ymin = lower__, ymax = upper__), alpha = 0.2, color = NA) +
  theme_bw() +
  scale_x_continuous(
    breaks = (log(c(1, 10, 50, 100, 500)) - cp) / s,
    labels = c(1, 10, 50, 100, 500)
  )+
  labs(
    x = "Cumulative solves (log scale)",
    y = "Probability of staying \n after solve",
    color = "Boldness",
    fill = "Boldness"
  ) +
  scale_color_manual(values = c("#fdd0a2","#fd8d3c", "#a63603" )) +
  scale_fill_manual(values = c("#fdd0a2","#fd8d3c", "#a63603" ))+
  theme(legend.position = "none")

# note that we have changed the order (moved panel a as d)
ggarrange( p2.panel.a, p2.panel.c, p2.panel.b, p2.panel.d, labels=c("a", "c", "b", "d"), common.legend=F)


ggsave("Figures output/behavioral skill.tiff", units='in', width=10, height=7)

# without age

ggarrange( p2.panel.a, p2.panel.c, p2.panel.d, labels=c("a", "b", "c"), common.legend=F, nrow = 1)


ggsave("Figures output/behavioral skill.tiff", units='in', width=9, height=3.5)




##### Supplementary figure

SIfig.row1 <- ggplot(df_paw_choice,
                  aes(x = log_cumulative_count_sc,
                      y = estimate__,
                      color = factor(beh_type_val),
                      fill = factor(beh_type_val),
                      group = factor(beh_type_val))) +
  geom_line(linewidth = 1.2) +
  geom_ribbon(aes(ymin = lower__, ymax = upper__),
              alpha = 0.2, color = NA) +
  facet_wrap(~ success_level, 
             labeller = labeller(
               success_level = c(
                 both = "both",
                 ipsi = "ipsi",
                 contra = "contra"
               )
             ), nrow=1)+
  theme_bw() +
  scale_x_continuous(
    breaks = (log(c(1, 10, 50, 100, 500)) - cp) / s,
    labels = c(1, 10, 50, 100, 500)
  ) +
  theme(
    legend.position = c(0.55, 0.75), # relative coordinates (x, y) inside the plot
    legend.background = element_blank(),
    legend.title = element_text(size = 10),
    legend.text = element_text(size = 9)
  )+
  labs(x = "Cumulative solves (log scale)",
       y = "Predicted probability",
       color = "Boldness",
       fill = "Boldness")+
  scale_color_manual(values = c(
    "#fdd0a2",
    "#fd8d3c",
    "#a63603"
  ), labels = c("risk-averse", "medium", "bold"), name="Boldness") +
  scale_fill_manual(values = c(
    "#fdd0a2",
    "#fd8d3c",
    "#a63603"
  ), labels = c("risk-averse", "medium", "bold"), name="Boldness")+
  theme(
    legend.background = element_rect(fill = "white", color = NA),
    legend.key = element_rect(fill = "white", color = NA)
  )

SIfig.row1

ggsave("Figures output/behavioral skill_supplementary figure2.tiff", units='in', width=9, height=3.5)






# 6.3) posterior predictive checks ----------------------------------------
library(patchwork)

library(cowplot)

# Extract the legend from one of your plots
legend <- get_legend(
  pp_lat + theme(legend.position = "right")
)

# Remove legends from all plots
pp_msuccess <- pp_msuccess + theme(legend.position = "none")
pp_lat      <- pp_lat      + theme(legend.position = "none")
pp_eff      <- pp_eff      + theme(legend.position = "none")
pp_box      <- pp_box      + theme(legend.position = "none")
pp_stay     <- pp_stay     + theme(legend.position = "none")

legend <- wrap_elements(legend, ignore_tag = TRUE)

fig_pp_check <-
  (pp_msuccess + pp_lat + legend) /
  (pp_eff + pp_box + pp_stay) +
  plot_annotation(tag_levels = "a")

fig_pp_check

ggsave("Figures output/pp_check_grid.png", fig_pp_check, width = 8, height = 4.5, dpi = 300)





