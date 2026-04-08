
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


# 2) Read data ------------------------------------------------------------
setwd("C:/Users/sonja/Desktop/Ground Squirrels/Behavioral skill/git/squirrels_behavioral_skill")

# each row is one solve
solving_data <- read.csv("Data/combined.data.csv")

# each row is one visit
visit_data <- read.csv("Data/visits.data.csv")

# this contains the individual level data - each row is one individual
subject_data <- read.csv("Data/Subject_data.csv")


# we want to calculate a boldness score consisting of individuals' trappability and their propensity to show any fear responses in the trap (chatter, struggle, call)

# run pca on behaviors

bh_data_scaled <- scale(subject_data[,c("any_beh_prop", "trap_rate_per_day")])

pca_bh <- principal(bh_data_scaled, nfactors = 1, rotate = "none")
summary(pca_bh)

# Factor analysis with Call: principal(r = bh_data_scaled, nfactors = 1, rotate = "none")
# 
# Test of the hypothesis that 1 factor is sufficient.
# The degrees of freedom for the model is -1  and the objective function was  0.31 
# The number of observations was  18  with Chi Square =  4.64  with prob <  NA 
# 
# The root mean square of the residuals (RMSA) is  0.19 

pca_bh$loadings

# Loadings:
#   PC1   
# any_beh_prop      -0.899
# trap_rate_per_day  0.899
# 
# PC1
# SS loadings    1.615
# Proportion Var 0.808

# we can see that trap behavior and trappability load in opposite directions and account for 80.8% of the variance


biplot(pca_bh, main="Biplot of PCA")

# add the scores to our subject data as boldness
subject_data$beh_type <- pca_bh$scores[,1]

cor(bh_data_scaled)

#                      any_beh_prop trap_rate_per_day
# any_beh_prop         1.0000000        -0.6152862
# trap_rate_per_day   -0.6152862         1.0000000



# add the individual level data to the solving data
solving_data <- left_join(solving_data, subject_data, by="Subject")

# and to the visit data
visit_data <- left_join(
  visit_data,
  subject_data %>% select(Subject, beh_type, log_training_solves),
  by = "Subject"
)

# make a histogram of boldness scores for the SI

tiff("Figures output/Boldness histogram.tiff", units="in", width=5, height=5, res=300, compression = 'lzw')


hist(subject_data$beh_type, xlab = "Boldness score (PCA)", main="")

dev.off()

# 3) Extract some numbers -------------------------------------------------


# how often did the use which paw
table(solving_data$body_part)

# both paws  left paw right paw 
# 760       590       558 

# how many unique squirrels
length(unique(solving_data$Subject))
# 18

table(subject_data$age)
# A  P 
# 12  6 

# how many times did they solve?
table(solving_data$Subject)


# squirrel_1 squirrel_10 squirrel_11 squirrel_12 squirrel_13 squirrel_14 squirrel_15 squirrel_16 squirrel_17 squirrel_18  squirrel_2 
# 86          66         134          15         526         119          10          68          97          10          40 
# squirrel_3  squirrel_4  squirrel_5  squirrel_6  squirrel_7  squirrel_8  squirrel_9 
# 33          82         492          73          16          13          28 

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

length(solving_data$lever_side)
1908

table(solving_data$lever_side)
#left lever right lever 
#1227         681

# 4) Skill improvement  with increasing experience  -------------------------------------------------

# 4.1) Does behavioral type influence success rate over time? -------------------------------------------------------------------

# in a previous study, we have demonstrated that bolder squirrels tend to be faster at learning to solve (latency until first solve). But are they also more successful? We look at whether or not they solved  and consumed per visit over time.
# we control for time present in front of the box, number of training solves, age and sex

# we center the cumulaitve number of visits around 0
visit_data$log_cumulative_visits <- visit_data$log_cumulative_visits-mean(visit_data$log_cumulative_visits)
visit_data$log_training_solves <- visit_data$log_training_solves-mean(visit_data$log_training_solves)
# we also create a binary variable to see whether they solve during a visit or not (rather than the number of solves)
visit_data$solved <- visit_data$solves_per_visit
visit_data$solved[visit_data$solved>0] <- 1

# we create a new composite variables consisting of whether or not they solved, and whether or not they consumed
visit_data <- visit_data %>%
  mutate(success = case_when(
    solved == 1 & consumption == 1 ~ "yes_yes",
    solved == 1 & consumption == 0 ~ "yes_no",
    solved == 0 & consumption == 1 ~ "no_yes",
    solved == 0 & consumption == 0 ~ "no_no"
  ))


unique(visit_data$success)
# no_no = no solve, no consumption
# yes_no = solve, but not consumption
# no_yes = no solve, but consumption (scrounging)
# yes_yes = solve and consumption

# check for VIFs
check_collinearity(
  lm(rep(1, nrow(visit_data)) ~  log_cumulative_visits + beh_type + age + sex + log_training_solves + scale(total_time_present),
     data = visit_data)
)


# # Check for Multicollinearity
# 
# Low Correlation
# 
# Term  VIF   VIF 95% CI adj. VIF Tolerance Tolerance 95% CI
# log_cumulative_visits 1.15 [1.11, 1.22]     1.07      0.87     [0.82, 0.90]
# beh_type 2.19 [2.05, 2.35]     1.48      0.46     [0.43, 0.49]
# age 2.12 [1.99, 2.27]     1.46      0.47     [0.44, 0.50]
# sex 2.07 [1.94, 2.22]     1.44      0.48     [0.45, 0.51]
# log_training_solves 1.99 [1.87, 2.13]     1.41      0.50     [0.47, 0.54]
# scale(total_time_present) 1.07 [1.03, 1.14]     1.03      0.94     [0.88, 0.97]

model_success <- brm(  
  success ~ log_cumulative_visits*beh_type + age + sex + log_training_solves + scale(total_time_present) + (1 | Subject), 
  data = visit_data, 
  family = categorical(),
  cores = 4,
  iter = 4000,
  chains = 4,
  seed = 3,
  control = list(adapt_delta = 0.95)
)

#save(model_success, file="model output/model_success.RDA")
load("model output/model_success.RDA")

# check for model fit
pp_check(model_success)

# check for stationarity and mixing
plot(model_success)

summary(model_success)
# Family: categorical 
# Links: munoyes = logit; muyesno = logit; muyesyes = logit 
# Formula: success ~ log_cumulative_visits * beh_type + age + sex + log_training_solves + scale(total_time_present) + (1 | Subject) 
# Data: visit_data (Number of observations: 2023) 
# Draws: 4 chains, each with iter = 4000; warmup = 2000; thin = 1;
# total post-warmup draws = 8000
# 
# Multilevel Hyperparameters:
#   ~Subject (Number of levels: 18) 
# Estimate Est.Error l-95% CI u-95% CI Rhat Bulk_ESS Tail_ESS
# sd(munoyes_Intercept)      0.46      0.19     0.17     0.91 1.00     2807     4323
# sd(muyesno_Intercept)      0.53      0.24     0.12     1.07 1.00     1937     2048
# sd(muyesyes_Intercept)     1.12      0.31     0.66     1.84 1.00     2760     4383
# 
# Regression Coefficients:
#   Estimate Est.Error l-95% CI u-95% CI Rhat Bulk_ESS Tail_ESS
# munoyes_Intercept                          -0.47      0.30    -1.08     0.11 1.00     4479     4482
# muyesno_Intercept                          -1.55      0.37    -2.36    -0.88 1.00     4699     4253
# muyesyes_Intercept                          0.24      0.62    -0.97     1.55 1.00     3125     4205
# munoyes_log_cumulative_visits               0.37      0.08     0.22     0.52 1.00     8069     6023
# munoyes_beh_type                            0.28      0.19    -0.11     0.64 1.00     5009     4101
# munoyes_ageP                                0.10      0.45    -0.80     1.02 1.00     4217     4551
# munoyes_sexM                                0.23      0.37    -0.50     0.96 1.00     4885     4853
# munoyes_log_training_solves                -0.07      0.08    -0.24     0.09 1.00     4575     4827
# munoyes_scaletotal_time_present             1.73      0.16     1.41     2.05 1.00     6311     5705
# munoyes_log_cumulative_visits:beh_type     -0.13      0.07    -0.27    -0.00 1.00     8726     6836
# muyesno_log_cumulative_visits               0.07      0.09    -0.10     0.25 1.00     9533     6492
# muyesno_beh_type                            0.30      0.22    -0.15     0.72 1.00     5338     5257
# muyesno_ageP                               -0.10      0.53    -1.11     1.04 1.00     4473     4462
# muyesno_sexM                                0.52      0.44    -0.28     1.44 1.00     5112     4845
# muyesno_log_training_solves                -0.08      0.10    -0.28     0.12 1.00     4330     4460
# muyesno_scaletotal_time_present             0.18      0.27    -0.39     0.68 1.00     9107     5777
# muyesno_log_cumulative_visits:beh_type     -0.24      0.08    -0.40    -0.09 1.00     9458     5925
# muyesyes_log_cumulative_visits              0.84      0.09     0.67     1.03 1.00     8999     6052
# muyesyes_beh_type                           1.47      0.36     0.75     2.20 1.00     3883     4854
# muyesyes_ageP                              -0.62      0.95    -2.55     1.29 1.00     3162     3398
# muyesyes_sexM                               0.22      0.73    -1.24     1.68 1.00     3564     4395
# muyesyes_log_training_solves               -0.03      0.17    -0.38     0.31 1.00     3404     3834
# muyesyes_scaletotal_time_present            2.08      0.17     1.75     2.41 1.00     6495     5848
# muyesyes_log_cumulative_visits:beh_type    -0.22      0.08    -0.38    -0.07 1.00     9470     6652
# 
# Draws were sampled using sampling(NUTS). For each parameter, Bulk_ESS
# and Tail_ESS are effective sample size measures, and Rhat is the potential
# scale reduction factor on split chains (at convergence, Rhat = 1).

plot.success <- plot(conditional_effects(model_success, categorical = T))

# get odds by exponteiationg
exp(fixef(model_success))

# Estimate Est.Error       Q2.5      Q97.5
# munoyes_Intercept                       0.6267155  1.345793 0.33964531  1.1216264
# muyesno_Intercept                       0.2122871  1.448654 0.09396230  0.4138250
# muyesyes_Intercept                      1.2733283  1.867199 0.37976119  4.7115112
# munoyes_log_cumulative_visits           1.4414271  1.080753 1.24041069  1.6813564
# munoyes_beh_type                        1.3170373  1.206984 0.89459594  1.9011747
# munoyes_ageP                            1.1012223  1.569569 0.44773665  2.7632235
# munoyes_sexM                            1.2635069  1.442913 0.60581946  2.6197063
# munoyes_log_training_solves             0.9288855  1.085904 0.78788256  1.0924961
# munoyes_scaletotal_time_present         5.6206793  1.177659 4.08551821  7.7704274
# munoyes_log_cumulative_visits:beh_type  0.8759980  1.068882 0.76654665  0.9965506
# muyesno_log_cumulative_visits           1.0696631  1.092186 0.90427624  1.2786414
# muyesno_beh_type                        1.3511425  1.245655 0.86244320  2.0498062
# muyesno_ageP                            0.9051189  1.703411 0.32878350  2.8296733
# muyesno_sexM                            1.6890877  1.548775 0.75482128  4.2176112
# muyesno_log_training_solves             0.9216251  1.104127 0.75368386  1.1272626
# muyesno_scaletotal_time_present         1.1943327  1.306259 0.68002033  1.9676186
# muyesno_log_cumulative_visits:beh_type  0.7881384  1.082044 0.67013114  0.9170321
# muyesyes_log_cumulative_visits          2.3257600  1.096867 1.95136585  2.7977292
# muyesyes_beh_type                       4.3475309  1.431161 2.11630618  9.0417740
# muyesyes_ageP                           0.5397578  2.591540 0.07793824  3.6421639
# muyesyes_sexM                           1.2443934  2.083093 0.28915889  5.3692237
# muyesyes_log_training_solves            0.9668979  1.189126 0.68140270  1.3667559
# muyesyes_scaletotal_time_present        7.9954000  1.182406 5.78108986 11.1461303
# muyesyes_log_cumulative_visits:beh_type 0.8028599  1.081503 0.68654539  0.9332063


# 4.2) Latency to first solve ---------------------------------------

visit_data$log_latency <- log(visit_data$latency_to_first_solve)

# check for VIFs
check_collinearity(
  lm(rep(1, nrow(visit_data[!is.na(visit_data$latency_to_first_solve),])) ~  log_cumulative_visits + beh_type + age + sex + log_training_solves ,
     data = visit_data[!is.na(visit_data$latency_to_first_solve),])
)

# # Check for Multicollinearity
# 
# Low Correlation
# 
# Term  VIF   VIF 95% CI Increased SE Tolerance Tolerance 95% CI
# log_cumulative_visits 1.35 [1.25, 1.50]         1.16      0.74     [0.67, 0.80]
# beh_type 2.21 [1.98, 2.48]         1.48      0.45     [0.40, 0.50]
# age 2.00 [1.80, 2.24]         1.41      0.50     [0.45, 0.55]
# sex 1.99 [1.79, 2.23]         1.41      0.50     [0.45, 0.56]
# log_training_solves 1.98 [1.79, 2.22]         1.41      0.50     [0.45, 0.56]


# this only considers visits where solves have actually occurred
model_latency <- brm(  
  log_latency ~ log_cumulative_visits*beh_type + age + sex + log_training_solves  + (1 | Subject), 
  data = visit_data, 
  cores = 4,
  iter = 4000,
  chains = 4,
  seed = 3,
  control = list(adapt_delta = 0.95)
)

#save(model_latency, file="model output/model_latency.RDA")
load("model output/model_latency.RDA")

pp_check(model_latency)
plot(model_latency)


summary(model_latency)
# Family: gaussian 
# Links: mu = identity 
# Formula: log_latency ~ log_cumulative_visits * beh_type + age + sex + log_training_solves + (1 | Subject) 
# Data: visit_data (Number of observations: 704) 
# Draws: 4 chains, each with iter = 4000; warmup = 2000; thin = 1;
# total post-warmup draws = 8000
# 
# Multilevel Hyperparameters:
#   ~Subject (Number of levels: 18) 
# Estimate Est.Error l-95% CI u-95% CI Rhat Bulk_ESS Tail_ESS
# sd(Intercept)     0.51      0.15     0.27     0.87 1.00     2603     4067
# 
# Regression Coefficients:
#   Estimate Est.Error l-95% CI u-95% CI Rhat Bulk_ESS Tail_ESS
# Intercept                          2.82      0.38     2.03     3.56 1.00     5202     4525
# log_cumulative_visits             -0.15      0.06    -0.28    -0.03 1.00     7927     6457
# beh_type                           0.47      0.30    -0.10     1.09 1.00     4834     4974
# ageP                               0.03      0.45    -0.87     0.94 1.00     3831     4281
# sexM                               0.42      0.34    -0.25     1.10 1.00     4511     4351
# log_training_solves                0.02      0.08    -0.14     0.18 1.00     3614     3929
# log_cumulative_visits:beh_type    -0.17      0.06    -0.28    -0.05 1.00     5313     4950
# 
# Further Distributional Parameters:
#   Estimate Est.Error l-95% CI u-95% CI Rhat Bulk_ESS Tail_ESS
# sigma     1.28      0.04     1.21     1.35 1.00    11587     5051
# 
# Draws were sampled using sampling(NUTS). For each parameter, Bulk_ESS
# and Tail_ESS are effective sample size measures, and Rhat is the potential
# scale reduction factor on split chains (at convergence, Rhat = 1).
plot.latency <- plot(conditional_effects(model_latency))

# overall, latency to first solve decreased with increasing experience
# shier individuals took overall longer but uncertain effect
# no effect of age, sex or training solves
# bolder individuals decreased their latency to first solve faster than shy individuals


# 4.3) Motor flexibility - paw use ----------------------

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

# VIFs - these are the same for the next three models as they all have the same predictors
check_collinearity(
  lm( rep(1, nrow(solving_data)) ~  log_cumulative_count_sc + beh_type + age + sex + log_training_solves_sc,
     data = solving_data)
)
# # Check for Multicollinearity
# 
# Low Correlation
# 
# Term  VIF   VIF 95% CI adj. VIF Tolerance Tolerance 95% CI
# log_cumulative_count_sc 1.39 [1.31, 1.47]     1.18      0.72     [0.68, 0.76]
# beh_type 2.21 [2.07, 2.38]     1.49      0.45     [0.42, 0.48]
# age 2.27 [2.12, 2.43]     1.51      0.44     [0.41, 0.47]
# sex 3.02 [2.80, 3.25]     1.74      0.33     [0.31, 0.36]
# log_training_solves_sc 2.90 [2.70, 3.13]     1.70      0.35     [0.32, 0.37]

# for vs ipsi vs contralateral paw
model_eff_paw <- brm(  
  paw_comb ~ log_cumulative_count_sc*beh_type + age + sex + log_training_solves_sc + (1 | Subject), 
  data = solving_data, # we exclude both paws from this data set
  family = categorical(),
  cores = 4,
  iter = 4000,
  chains = 4,
  seed = 3,
  control = list(adapt_delta = 0.95)
)

#save(model_eff_paw, file="model output/model_eff_paw.RDA")
load("model output/model_eff_paw.RDA")

pp_check(model_eff_paw)
plot(model_eff_paw)

summary(model_eff_paw)

# Family: categorical 
# Links: mucontra = logit; muipsi = logit 
# Formula: paw_comb ~ log_cumulative_count_sc * beh_type + age + sex + log_training_solves_sc + (1 | Subject) 
# Data: solving_data (Number of observations: 1908) 
# Draws: 4 chains, each with iter = 4000; warmup = 2000; thin = 1;
# total post-warmup draws = 8000
# 
# Multilevel Hyperparameters:
#   ~Subject (Number of levels: 18) 
# Estimate Est.Error l-95% CI u-95% CI Rhat Bulk_ESS Tail_ESS
# sd(mucontra_Intercept)     1.57      0.46     0.88     2.67 1.00     3366     5401
# sd(muipsi_Intercept)       1.23      0.31     0.76     1.95 1.00     3754     5027
# 
# Regression Coefficients:
#   Estimate Est.Error l-95% CI u-95% CI Rhat Bulk_ESS Tail_ESS
# mucontra_Intercept                           -1.43      0.77    -2.97     0.10 1.00     5104     4802
# muipsi_Intercept                              0.41      0.59    -0.78     1.56 1.00     5609     5123
# mucontra_log_cumulative_count_sc             -0.80      0.15    -1.09    -0.52 1.00     7071     6234
# mucontra_beh_type                            -0.73      0.54    -1.77     0.40 1.00     5723     5320
# mucontra_ageP                                -1.24      1.38    -4.04     1.47 1.00     4797     4371
# mucontra_sexM                                 1.05      1.05    -1.01     3.18 1.00     5694     4867
# mucontra_log_training_solves_sc               0.17      0.78    -1.39     1.71 1.00     5128     5397
# mucontra_log_cumulative_count_sc:beh_type     0.49      0.15     0.21     0.79 1.00     7026     6958
# muipsi_log_cumulative_count_sc               -0.82      0.13    -1.08    -0.57 1.00     6317     6412
# muipsi_beh_type                              -0.69      0.41    -1.50     0.13 1.00     5243     4956
# muipsi_ageP                                  -1.71      1.06    -3.83     0.43 1.00     4955     5121
# muipsi_sexM                                   0.33      0.79    -1.21     1.93 1.00     5408     4736
# muipsi_log_training_solves_sc                -0.10      0.60    -1.27     1.11 1.00     5127     4940
# muipsi_log_cumulative_count_sc:beh_type       0.11      0.11    -0.11     0.34 1.00     6271     5622
# 
# Draws were sampled using sampling(NUTS). For each parameter, Bulk_ESS
# and Tail_ESS are effective sample size measures, and Rhat is the potential
# scale reduction factor on split chains (at convergence, Rhat = 1).

# both ipsi and contralateral paw use decrease with successive task exposure, meaning that they increase the use of both paws
# slopes for ipsi and contra are similar - they do not shift with increasing experience


plot.paw <- plot(conditional_effects(model_eff_paw, categorical =T))


plogis(fixef(model_eff_paw))

# Average or fixed values for other predictors
newdata <- data.frame(
  age = c("A", "P"),                 # adult, juvenile
  log_cumulative_count_sc = 0,       # centered
  beh_type = 0,                       # centered
  sex = "F",                           # reference sex
  log_training_solves_sc = 0,         # centered
  Subject = NA                         # random effects set to population-level
)

# Get posterior predicted probabilities for all draws
pp <- posterior_epred(model_eff_paw, newdata = newdata, re_formula = NA)


both_adult <- pp[,1,"both"]
both_juvenile <- pp[,2,"both"]

diff_both <- both_adult - both_juvenile

# posterior mean
mean(diff_both)

# 95% credible interval
quantile(diff_both, c(0.025, 0.975))

# 4.4) Making mistakes -----------------------------------------------

# how many unsuccessful box interactions occurred between successes (solves)
model_box_int <- brm(  
  box_between_solves ~ log_cumulative_count_sc*beh_type + age + sex + log_training_solves_sc + (1 | Subject), 
  data = solving_data, 
  family = negbinomial(),
  cores = 4,
  iter = 4000,
  chains = 4,
  seed = 3,
  control = list(adapt_delta = 0.95)
)

#save(model_box_int, file="model output/model_box_int.RDA")
load("model output/model_box_int.RDA")


pp_check(model_box_int)
plot(model_box_int)

summary(model_box_int)

# Family: negbinomial 
# Links: mu = log 
# Formula: box_between_solves ~ log_cumulative_count_sc * beh_type + age + sex + log_training_solves_sc + (1 | Subject) 
# Data: solving_data (Number of observations: 1908) 
# Draws: 4 chains, each with iter = 4000; warmup = 2000; thin = 1;
# total post-warmup draws = 8000
# 
# Multilevel Hyperparameters:
#   ~Subject (Number of levels: 18) 
# Estimate Est.Error l-95% CI u-95% CI Rhat Bulk_ESS Tail_ESS
# sd(Intercept)     0.52      0.14     0.31     0.84 1.00     2260     3820
# 
# Regression Coefficients:
#   Estimate Est.Error l-95% CI u-95% CI Rhat Bulk_ESS Tail_ESS
# Intercept                            0.59      0.24     0.12     1.09 1.00     3749     4208
# log_cumulative_count_sc             -0.29      0.04    -0.37    -0.22 1.00    10245     5444
# beh_type                            -0.24      0.17    -0.57     0.09 1.00     3695     4224
# ageP                                -0.35      0.44    -1.23     0.50 1.00     3860     4219
# sexM                                -0.54      0.34    -1.22     0.10 1.00     3798     4376
# log_training_solves_sc              -0.06      0.24    -0.55     0.42 1.00     3692     4166
# log_cumulative_count_sc:beh_type     0.11      0.03     0.04     0.18 1.00     9948     5860
# 
# Further Distributional Parameters:
#   Estimate Est.Error l-95% CI u-95% CI Rhat Bulk_ESS Tail_ESS
# shape     1.52      0.10     1.33     1.73 1.00    10817     5198
# 
# Draws were sampled using sampling(NUTS). For each parameter, Bulk_ESS
# and Tail_ESS are effective sample size measures, and Rhat is the potential
# scale reduction factor on split chains (at convergence, Rhat = 1).

plot.mistakes <- plot(conditional_effects(model_box_int))

# failure rate decreases with increasing experience
# no overall effect of boldness, age, sex, training solves
# shy individuals make more mistakes initially compared to bolder individuals, but improve over time, while bold individuals are consistent in their mistake rate


# 4.5)  reaction after solve  -------------------------------------------------------------------


model_stay <- brm(  
  exit ~ log_cumulative_count_sc*beh_type + age + sex + log_training_solves_sc + (1 | Subject), 
  data = solving_data, 
  family = bernoulli(),
  cores = 4,
  iter = 4000,
  chains = 4,
  seed = 3,
  control = list(adapt_delta = 0.95)
)
#save(model_stay, file="model output/model_stay.RDA")
load("model output/model_stay.RDA")

summary(model_stay)
# Family: bernoulli 
# Links: mu = logit 
# Formula: exit ~ log_cumulative_count_sc * beh_type + age + sex + log_training_solves_sc + (1 | Subject) 
# Data: solving_data (Number of observations: 1908) 
# Draws: 4 chains, each with iter = 4000; warmup = 2000; thin = 1;
# total post-warmup draws = 8000
# 
# Multilevel Hyperparameters:
#   ~Subject (Number of levels: 18) 
# Estimate Est.Error l-95% CI u-95% CI Rhat Bulk_ESS Tail_ESS
# sd(Intercept)     2.40      0.68     1.42     4.04 1.00     2918     4437
# 
# Regression Coefficients:
#   Estimate Est.Error l-95% CI u-95% CI Rhat Bulk_ESS Tail_ESS
# Intercept                            3.13      1.19     0.67     5.51 1.00     4296     4484
# log_cumulative_count_sc              2.66      0.21     2.27     3.08 1.00    10523     5898
# beh_type                             1.96      0.87     0.31     3.78 1.00     4228     4101
# ageP                                -4.06      2.27    -8.80     0.15 1.00     3928     4617
# sexM                                -0.64      1.68    -4.05     2.63 1.00     4215     4371
# log_training_solves_sc              -1.16      1.14    -3.46     1.09 1.00     4093     4103
# log_cumulative_count_sc:beh_type    -1.82      0.32    -2.47    -1.24 1.00    10139     6002
# 
# Draws were sampled using sampling(NUTS). For each parameter, Bulk_ESS
# and Tail_ESS are effective sample size measures, and Rhat is the potential
# scale reduction factor on split chains (at convergence, Rhat = 1).



plot.stay <- plot(conditional_effects(model_stay))

# overall more likely to stay with increaseing experience. 
# boldness positively correlated with staying, but uncertainty
# juveniles less likely to stay
# no effect of sex or training solves
# bolder individuals stay from the start, shier individuals imporove with increasing experience



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
                  "munoyes_log_cumulative_visits" = "Log cumulative visits [consume]",
                  "munoyes_beh_type" = "Boldness [consume]",
                  "munoyes_ageP"  = "Age (J:A) [consume]",
                  "munoyes_sexM"  = "Sex (M:F) [consume]",
                  "munoyes_log_training_solves" = "Log # of training solve [consume]",
                  "munoyes_scaletotal_time_present" = "Visit duration [consume]",
                  "munoyes_log_cumulative_visits:beh_type" = "Cumulative visits (log) × Boldness [consume]",
                  
                  "muyesno_log_cumulative_visits" = "Log cumulative visits [solve]",
                  "muyesno_beh_type" = "Boldness [solve]",
                  "muyesno_ageP"  = "Age (J:A) [solve]",
                  "muyesno_sexM"  = "Sex (M:F) [solve]",
                  "muyesno_log_training_solves" = "Log # of training solve [solve]",
                  "muyesno_scaletotal_time_present" = "Visit duration [solve]",
                  "muyesno_log_cumulative_visits:beh_type" = "Cumulative visits (log) × Boldness [solve]",
                  
                  "muyesyes_log_cumulative_visits" = "Log cumulative visits [solve, consume]",
                  "muyesyes_beh_type" = "Boldness [solve, consume]",
                  "muyesyes_ageP"  = "Age (J:A) [solve, consume]",
                  "muyesyes_sexM"  = "Sex (M:F) [solve, consume]",
                  "muyesyes_log_training_solves" = "Log # of training solve, consume [solve, consume]",
                  "muyesyes_scaletotal_time_present" = "Visit duration [solve, consume]",
                  "muyesyes_log_cumulative_visits:beh_type" = "Cumulative visits (log) × Boldness [solve, consume]"
                  
                  

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

print(doc, target = "Tables output/model_success.docx")



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
                  "mucontra_log_cumulative_count_sc:beh_type" =
                    "Cumulative solves (log) × Boldness [contra]",
                  "muipsi_beh_type" = "Boldness [ipsi]",
                  "muipsi_ageP" = "Age (J:A) [ipsi]",
                  "muipsi_sexM" = "Sex (M:F) [ipsi]",
                  "muipsi_log_training_solves_sc" = "# training solves (log) [ipsi]",
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

# MISTAKES

tidy_box <- tidy(
  model_box_int,
  effects = "fixed",
  conf.int = TRUE,
  conf.level = 0.95
) %>%
  select(-std.error)

tidy_box <- tidy_box %>%
  mutate(
    term = dplyr::recode(term,
                  "Intercept" = "Intercept",
                  "log_cumulative_count_sc" = "Cumulative solves (log)",
                  "beh_type" = "Boldness",
                  "ageP" = "Age (J:A)",
                  "sexM" = "Sex (M:F)",
                  "log_training_solves_sc" = "# training solves (log)",
                  "log_cumulative_count_sc:beh_type" =
                    "Cumulative solves (log) × Boldness"
    )
  )


table_box <- tidy_box %>%
  filter(term != "Intercept") %>%   # recommended
  select(
    Predictor = term,
    `β` = estimate,
    `Lower 95% CI` = conf.low,
    `Upper 95% CI` = conf.high
  ) %>%
  mutate(across(where(is.numeric), round, 2))


ft_box <- flextable(table_box) %>%
  autofit()

doc <- read_docx() %>%
  body_add_flextable(ft_box)

print(doc, target = "Tables output/model_box_int.docx")



# 6) Figures ---------------------------------------------------------------


# 6.1) Solving efficiency figures -----------------------------------------


# Panel a: change in success given cumulative visits 




plot.success <- plot(conditional_effects(model_success, categorical=T))

p1.panel.a <- plot.success$`log_cumulative_visits:cats__`+
  geom_line(linewidth = 1.2) +
  geom_ribbon(aes(ymin = lower__, ymax = upper__), alpha = 0.1, color = NA) +
  theme_bw() +
  ylim(c(0,1))+
  labs(
    x = "Log # of cumulative visits",
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
    x = "Boldness",
    y = ""
  ) +
  scale_color_manual(values = c("yes_yes"= "#cb5357", "yes_no"= "#a361c7", "no_yes" = "#4aac8d", "no_no"= "#bd893d"),labels=c("none", "consume", "solve", "solve & consume"), name = "Behavior") +
  scale_fill_manual(values = c("yes_yes"= "#cb5357", "yes_no"= "#a361c7", "no_yes" = "#4aac8d", "no_no"= "#bd893d"), labels=c("none", "consume", "solve", "solve & consume"), name = "Behavior")+
  theme(legend.position.inside = c(0.7, 0.81), legend.position = "inside")




# panel c: time spent

p1.panel.c <- plot.success$`total_time_present:cats__`+
  geom_line(linewidth = 1.2) +
  geom_ribbon(aes(ymin = lower__, ymax = upper__), alpha = 0.1, color = NA) +
  theme_bw() +
  labs(
    x = "Visit duration",
    y = ""
  ) +
  scale_color_manual(values = c("yes_yes"= "#cb5357", "yes_no"= "#a361c7", "no_yes" = "#4aac8d", "no_no"= "#bd893d"),labels=c("none", "consume", "solve", "solve & consume"), name = "Behavior") +
  scale_fill_manual(values = c("yes_yes"= "#cb5357", "yes_no"= "#a361c7", "no_yes" = "#4aac8d", "no_no"= "#bd893d"), labels=c("none", "consume", "solve", "solve & consume"), name = "Behavior")+
  theme(legend.position.inside = c(0.7, 0.81), legend.position = "inside")




# for interactions, we have to define which values we would like visualized.
# we take the 10th, 50th and 90th percentile of boldness scores

# Define the percentiles of beh_type you want to visualize
beh.type.q <- as.numeric(quantile(subject_data$beh_type, c(0.1, 0.5, 0.9)))

get_preds <- function(model, beh_values) {
  pred_list <- lapply(beh_values, function(x) {
    ce <- conditional_effects(
      model,
      effects = "log_cumulative_visits",
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

df_success <- get_preds(model_success, beh.type.q)

p1.row2 <- ggplot(df_success,
       aes(x = log_cumulative_visits,
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
                 no_no = "None",
                 no_yes = "Consume",
                 yes_no = "Solve",
                 yes_yes = "Solve & consume"
               )
             ), nrow=1)+
  theme_bw() +
  theme(
    legend.position = c(0.32, 0.85), # relative coordinates (x, y) inside the plot
    legend.background = element_blank(),
    legend.title = element_text(size = 10),
    legend.text = element_text(size = 9)
  )+
  labs(x = "Log cumulative visits",
       y = "Predicted probability",
       color = "Behavior type",
       fill = "Behavior type")+
  scale_color_manual(values = c(
    "#fdd0a2",
     "#fd8d3c",
    "#a63603"
  ), labels = c("shy", "medium", "bold"), name="Boldness") +
  scale_fill_manual(values = c(
     "#fdd0a2",
     "#fd8d3c",
   "#a63603"
  ), labels = c("shy", "medium", "bold"), name="Boldness")+
  theme(
  legend.background = element_rect(fill = "white", color = NA),
  legend.key = element_rect(fill = "white", color = NA)
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
                                 labels = c("Shy", "Medium", "Bold")))



p1.panel.d <- ggplot(df_latency,
       aes(x = log_cumulative_visits,
           y = estimate__,
           color = beh_type_label,
           fill = beh_type_label,
           group = beh_type_label)) +
  geom_line(linewidth = 1.2) +
  geom_ribbon(aes(ymin = lower__, ymax = upper__),
              alpha = 0.1, color = NA) +
  theme_bw()+
  labs(
    x = "Log cumulative visits",
    y = "Latency to first solve"
  )  +scale_color_manual(values = c(
    "#fdd0a2",
    "#fd8d3c",
    "#a63603"
  ), labels = c("shy", "medium", "bold"), name="Boldness") +
  scale_fill_manual(values = c(
    "#fdd0a2",
    "#fd8d3c",
    "#a63603"
  ), labels = c("shy", "medium", "bold"), name="Boldness")+
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

ggsave("Figures output/Solving efficiency.tiff", units='in', width=10, height=7, bg="white")


# 6.2) Behavioral skill figures -------------------------------------------

# panel a: paw use with experience

plot.eff.paw <- plot(conditional_effects(model_eff_paw, categorical=T))

p2.panel.a <- plot.eff.paw$`log_cumulative_count_sc:cats__`+
  geom_line(linewidth = 1.2) +
  geom_ribbon(aes(ymin = lower__, ymax = upper__), alpha = 0.05, color = NA) +
  theme_bw() +
  labs(
    x = "Log # of cumulative solves",
    y = "Probability"
  ) +
  scale_color_manual(values = c( "#00441b","#c7e9c0", "#74c476"), labels=c("both", "contralateral", "ipsilateral"), name = "Paw use") +
  scale_fill_manual(values = c( "#00441b","#c7e9c0", "#74c476"), labels=c("both", "contralateral", "ipsilateral"), name = "Paw use")+
  theme(legend.position.inside = c(0.7, 0.81), legend.position = "inside")


p2.panel.b <- plot.eff.paw$`age:cats__`+
  theme_bw() +
  labs(
    x = "Age category",
    y = "Probability"
  ) +
  ylim(c(0,1))+
  scale_color_manual(values = c( "#00441b","#c7e9c0", "#74c476"), labels=c("both", "contralateral", "ipsilateral"), name = "Paw use") +
  scale_fill_manual(values = c( "#00441b","#c7e9c0", "#74c476"), labels=c("both", "contralateral", "ipsilateral"), name = "Paw use")+
  scale_x_discrete(labels = c("A" = "Adult", "P" = "Juvenile"))



# # Generate conditional effects for each value
# pred_list <- lapply(beh.type.q, function(x) {
#   ce <- conditional_effects(
#     model_eff_paw,
#     effects = "log_cumulative_count_sc",
#     conditions = list(beh_type = x), 
#     categorical=T
#   )
#   # Extract the data for 'solved' outcome (or whichever outcome you want)
#   df <- ce$log_cumulative_count_sc
#   df$beh_type_val <- x
#   df
# })
# 
# # Combine into one dataframe
# df_combined <- bind_rows(pred_list)
# 
# # Optionally, label the percentiles for the legend
# df_combined <- df_combined %>%
#   mutate(beh_type_label = factor(beh_type_val,
#                                  levels = beh.type.q,
#                                  labels = c("Shy", "Medium", "Bold")))
# 
# # Plot all curves in one panel
# p2.panel.a <- ggplot(df_combined, aes(x = log_cumulative_count_sc, y = estimate__, color = beh_type_label, fill = beh_type_label)) +
#   geom_line(linewidth = 1.2) +
#   geom_ribbon(aes(ymin = lower__, ymax = upper__), alpha = 0.2, color = NA) +
#   theme_bw() +
#   labs(
#     x = "# cumulative solves (log)",
#     y = "Probability of using \n ipsilateral paw",
#     color = "Boldness",
#     fill = "Boldness"
#   ) +
#   scale_color_manual(values = c("#fdd0a2","#fd8d3c", "#a63603" )) +
#   scale_fill_manual(values = c("#fdd0a2","#fd8d3c", "#a63603" ))+
#   theme(legend.position = "none")
# 
# 
# # panel b:
# 
# # Generate conditional effects for each value
# pred_list <- lapply(beh.type.q, function(x) {
#   ce <- conditional_effects(
#     model_eff_both,
#     effects = "log_cumulative_count_sc",
#     conditions = list(beh_type = x)
#   )
#   # Extract the data for 'solved' outcome (or whichever outcome you want)
#   df <- ce$log_cumulative_count_sc
#   df$beh_type_val <- x
#   df
# })
# 
# # Combine into one dataframe
# df_combined <- bind_rows(pred_list)
# 
# # Optionally, label the percentiles for the legend
# df_combined <- df_combined %>%
#   mutate(beh_type_label = factor(beh_type_val,
#                                  levels = beh.type.q,
#                                  labels = c("Shy", "Medium", "Bold")))
# 
# # Plot all curves in one panel
# p2.panel.b <- ggplot(df_combined, aes(x = log_cumulative_count_sc, y = estimate__, color = beh_type_label, fill = beh_type_label)) +
#   geom_line(linewidth = 1.2) +
#   geom_ribbon(aes(ymin = lower__, ymax = upper__), alpha = 0.2, color = NA) +
#   theme_bw() +
#   labs(
#     x = "# cumulative solves (log)",
#     y = "Probability of using \n both paws",
#     color = "Boldness",
#     fill = "Boldness"
#   ) +
#   scale_color_manual(values = c("#fdd0a2","#fd8d3c", "#a63603" )) +
#   scale_fill_manual(values = c("#fdd0a2","#fd8d3c", "#a63603" ))+
#   theme(legend.position = "none")

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
                                 labels = c("Shy", "Medium", "Bold")))

# Plot all curves in one panel
p2.panel.c <- ggplot(df_combined, aes(x = log_cumulative_count_sc, y = estimate__, color = beh_type_label, fill = beh_type_label)) +
  geom_line(linewidth = 1.2) +
  geom_ribbon(aes(ymin = lower__, ymax = upper__), alpha = 0.2, color = NA) +
  theme_bw() +
  labs(
    x = "Log # of cumulative solves",
    y = "# of manipulation errors \n between solves",
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
                                 labels = c("Shy", "Medium", "Bold")))

# Plot all curves in one panel
p2.panel.d <- ggplot(df_combined, aes(x = log_cumulative_count_sc, y = estimate__, color = beh_type_label, fill = beh_type_label)) +
  geom_line(linewidth = 1.2) +
  geom_ribbon(aes(ymin = lower__, ymax = upper__), alpha = 0.2, color = NA) +
  theme_bw() +
  labs(
    x = "Log # of cumulative solves",
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


ggsave("Figures output/behavioral skill.tiff", units='in', width=10, height=4)





