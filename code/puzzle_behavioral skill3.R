#### Boldness predicts skill refinement in a novel foraging task in wild squirrels ####

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
solving_data <- read.csv("Data/combined.data.csv")
# to repeat the analysis with a threshold of 5 solves (instead of 10), uncomment the respective lines that read in the data. The rest remains the same
#solving_data <- read.csv("Data/combined.data_min5.csv")


# each row is one visit
visit_data <- read.csv("Data/visits.data.csv")
#visit_data <- read.csv("Data/visits.data_min5.csv")

# this contains the individual level data - each row is one individual
subject_data <- read.csv("Data/Subject_data.csv")
#subject_data <- read.csv("Data/Subject_data_min5.csv")


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
# any_beh_prop      -0.838  0.546
# trap_rate_per_day  0.838  0.546
# 
# PC1   PC2
# SS loadings    1.404 0.596
# Proportion Var 0.702 0.298
# Cumulative Var 0.702 1.000

# we can see that trap behavior and trappability load in opposite directions and account for 70.2% of the variance


# Increase margins to give labels room, especially on the right side
# Extract scores and loadings
scores <- pca_bh$scores
loadings <- pca_bh$loadings[,1:2]


# add the scores to our subject data as boldness
subject_data$beh_type <- pca_bh$scores[,1]

cor(bh_data_scaled)

#                     any_beh_prop trap_rate_per_day
# any_beh_prop         1.0000000        -0.4039727
# trap_rate_per_day   -0.4039727         1.0000000

range(subject_data$beh_type)
# -2.484865  1.932444

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

tiff("Figures output/Boldness_PCA_combined.tiff", units = "in", width = 8, height = 4, res = 300, compression = 'lzw')

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
     xlab = "PC1 (70.2% variance)",
     ylab = "PC2 (29.8% variance)",
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
# 26 squirrels that have either visited the puzzle box 10 times or solved 10 times

table(cbind.data.frame(subject_data$age, subject_data$sex))
# subject_data$sex
# subject_data$age F M
# A 7 10
# P 8 1

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
# A  7 10
# P  8  1

summary_table %>%
  filter(min_10_visits == 1) %>%
  select(age, sex) %>%
  table()
# sex
# age  F  M
# A  5 10
# P  7  1


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
# 1   15           8           3       0


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
91.57692

median(table(visit_data$Subject))
41

sd(table(visit_data$Subject))
99

range(table(visit_data$Subject))
#13 353

sum(summary_table$max_visits)
# 621 visits

sum(summary_table$n_solves)
# 1908




length(solving_data$lever_side)
1908

table(solving_data$lever_side)
#left lever right lever 
#1227         681

# how many solving events where more than one squirrel was present

table(solving_data$con_pres)
# 1657 alone, 251 in presence of a conspecific
251/1657

# in 15% of solves, a conspecific was present


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


# # Check for Multicollinearity
# 
# Low Correlation
# 
# Term  VIF   VIF 95% CI adj. VIF Tolerance Tolerance 95% CI
# log_cumulative_visits 1.14 [1.10, 1.21]     1.07      0.88     [0.83, 0.91]
# beh_type 1.74 [1.65, 1.85]     1.32      0.57     [0.54, 0.61]
# age 2.08 [1.96, 2.22]     1.44      0.48     [0.45, 0.51]
# sex 2.08 [1.96, 2.21]     1.44      0.48     [0.45, 0.51]
# log_training_solves 1.69 [1.60, 1.79]     1.30      0.59     [0.56, 0.62]
# scale(total_time_present) 1.06 [1.03, 1.13]     1.03      0.94     [0.89, 0.97]
# con_pres 1.12 [1.08, 1.18]     1.06      0.89     [0.84, 0.93]

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

#save(model_success, file="model output/model_success.RDA")
load("model output/model_success.RDA")
#load("model output/model_success_min5.RDA") # load this object for the threshold of 5 visits or 5 solves


# check for model fit
pp_msuccess <- pp_check(model_success)
pp_msuccess

# check for stationarity and mixing
plot(model_success)

summary(model_success)

# Family: categorical 
# Links: munoyes = logit; muyesno = logit; muyesyes = logit 
# Formula: success ~ log_cumulative_visits * beh_type + age + sex + log_training_solves + scale(total_time_present) + con_pres + (1 | Subject) 
# Data: visit_data_success (Number of observations: 2313) 
# Draws: 4 chains, each with iter = 4000; warmup = 2000; thin = 1;
# total post-warmup draws = 8000
# 
# Multilevel Hyperparameters:
#   ~Subject (Number of levels: 23) 
# Estimate Est.Error l-95% CI u-95% CI Rhat Bulk_ESS Tail_ESS
# sd(munoyes_Intercept)      0.49      0.17     0.22     0.91 1.00     2523     4573
# sd(muyesno_Intercept)      0.78      0.26     0.36     1.39 1.00     2714     4563
# sd(muyesyes_Intercept)     1.00      0.26     0.61     1.61 1.00     4058     5080
# 
# Regression Coefficients:
#   Estimate Est.Error l-95% CI u-95% CI Rhat Bulk_ESS Tail_ESS
# munoyes_Intercept                          -0.74      0.28    -1.30    -0.21 1.00     4972     4834
# muyesno_Intercept                          -1.49      0.42    -2.37    -0.69 1.00     4292     4444
# muyesyes_Intercept                         -0.80      0.48    -1.75     0.18 1.00     4012     4568
# munoyes_log_cumulative_visits               0.13      0.07    -0.02     0.27 1.00     8983     7102
# munoyes_beh_type                            0.07      0.15    -0.22     0.38 1.00     5743     5003
# munoyes_ageP                                0.11      0.39    -0.66     0.89 1.00     5212     5445
# munoyes_sexM                                0.03      0.34    -0.65     0.68 1.00     4947     5385
# munoyes_log_training_solves                 0.00      0.07    -0.14     0.14 1.00     5082     5015
# munoyes_scaletotal_time_present             1.57      0.13     1.31     1.83 1.00     9542     6728
# munoyes_con_pres1                           0.06      0.13    -0.19     0.30 1.00    11953     6303
# munoyes_log_cumulative_visits:beh_type      0.00      0.05    -0.10     0.11 1.00    10570     6412
# muyesno_log_cumulative_visits               0.16      0.10    -0.03     0.35 1.00    10110     6001
# muyesno_beh_type                            0.03      0.23    -0.43     0.46 1.00     4827     4715
# muyesno_ageP                                0.33      0.59    -0.78     1.57 1.00     3911     4125
# muyesno_sexM                                0.12      0.50    -0.84     1.16 1.00     4385     4287
# muyesno_log_training_solves                 0.05      0.11    -0.15     0.29 1.00     4471     5073
# muyesno_scaletotal_time_present            -0.45      0.26    -0.99     0.05 1.00    14835     5936
# muyesno_con_pres1                          -0.76      0.18    -1.13    -0.41 1.00    14656     5158
# muyesno_log_cumulative_visits:beh_type     -0.04      0.07    -0.19     0.10 1.00    11008     6559
# muyesyes_log_cumulative_visits              0.38      0.07     0.23     0.52 1.00    11220     6941
# muyesyes_beh_type                           0.66      0.25     0.15     1.16 1.00     5027     5281
# muyesyes_ageP                              -0.01      0.70    -1.41     1.36 1.00     4134     4809
# muyesyes_sexM                               0.12      0.60    -1.10     1.27 1.00     4081     4852
# muyesyes_log_training_solves                0.11      0.13    -0.14     0.37 1.00     4428     4917
# muyesyes_scaletotal_time_present            1.92      0.14     1.66     2.19 1.00     9555     6807
# muyesyes_con_pres1                         -0.63      0.15    -0.93    -0.33 1.00    13864     6212
# muyesyes_log_cumulative_visits:beh_type    -0.10      0.06    -0.21     0.01 1.00    12823     6248
# 
# Draws were sampled using sampling(NUTS). For each parameter, Bulk_ESS
# and Tail_ESS are effective sample size measures, and Rhat is the potential
# scale reduction factor on split chains (at convergence, Rhat = 1).

plot.success <- plot(conditional_effects(model_success, categorical = T))

# get odds by exponteiationg
exp(fixef(model_success))


# munoyes_Intercept                       0.4770008  1.318951 0.2733845 0.8085090
# muyesno_Intercept                       0.2260418  1.516852 0.0938127 0.4999519
# muyesyes_Intercept                      0.4511360  1.621072 0.1737743 1.1912929
# munoyes_log_cumulative_visits           1.1353100  1.073434 0.9847879 1.3044353
# munoyes_beh_type                        1.0764644  1.163135 0.8039153 1.4694206
# munoyes_ageP                            1.1146607  1.475684 0.5145592 2.4460178
# munoyes_sexM                            1.0344404  1.399078 0.5202196 1.9672613
# munoyes_log_training_solves             1.0036476  1.073350 0.8653437 1.1482712
# munoyes_scaletotal_time_present         4.8060230  1.140984 3.7148124 6.2523595
# munoyes_con_pres1                       1.0573372  1.134706 0.8236954 1.3508489
# munoyes_log_cumulative_visits:beh_type  1.0034074  1.056112 0.9028356 1.1175733
# muyesno_log_cumulative_visits           1.1694921  1.100436 0.9661681 1.4162666
# muyesno_beh_type                        1.0319202  1.253884 0.6536332 1.5896565
# muyesno_ageP                            1.3923113  1.812364 0.4576248 4.7876513
# muyesno_sexM                            1.1319646  1.644876 0.4314722 3.1947061
# muyesno_log_training_solves             1.0561846  1.115389 0.8622559 1.3331478
# muyesno_scaletotal_time_present         0.6351516  1.300912 0.3734222 1.0510601
# muyesno_con_pres1                       0.4659120  1.198018 0.3241729 0.6652415
# muyesno_log_cumulative_visits:beh_type  0.9590255  1.077027 0.8273760 1.1106517
# muyesyes_log_cumulative_visits          1.4601000  1.077505 1.2590422 1.6859168
# muyesyes_beh_type                       1.9379316  1.288014 1.1628384 3.1842676
# muyesyes_ageP                           0.9880676  2.015512 0.2438690 3.9029168
# muyesyes_sexM                           1.1262097  1.814949 0.3313329 3.5574296
# muyesyes_log_training_solves            1.1144785  1.136277 0.8671040 1.4481357
# muyesyes_scaletotal_time_present        6.8231813  1.144934 5.2599543 8.9401782
# muyesyes_con_pres1                      0.5328252  1.163979 0.3948218 0.7158780
# muyesyes_log_cumulative_visits:beh_type 0.9081172  1.057423 0.8130097 1.0144784

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
# log_cumulative_visits 1.27 [1.18, 1.40]     1.13      0.79     [0.71, 0.85]
# beh_type 2.07 [1.87, 2.32]     1.44      0.48     [0.43, 0.53]
# age 2.16 [1.94, 2.42]     1.47      0.46     [0.41, 0.51]
# sex 2.11 [1.90, 2.37]     1.45      0.47     [0.42, 0.53]
# log_training_solves 2.08 [1.88, 2.34]     1.44      0.48     [0.43, 0.53]
# con_pres 1.24 [1.15, 1.37]     1.11      0.81     [0.73, 0.87]


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

#save(model_latency, file="model output/model_latency.RDA")
load("model output/model_latency.RDA")
#load("model output/model_latency_min5.RDA") # load this model object for the threshold of 5


pp_lat <- pp_check(model_latency)
pp_lat
plot(model_latency)

r2_bayes(model_latency)

# # Bayesian R2 with Compatibility Interval
# 
# Conditional R2: 0.182 (95% CI [0.139, 0.230])
# Marginal R2: 0.152 (95% CI [0.051, 0.270])

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
# sd(Intercept)     0.56      0.15     0.33     0.91 1.00     2462     4440
# 
# Regression Coefficients:
#   Estimate Est.Error l-95% CI u-95% CI Rhat Bulk_ESS Tail_ESS
# Intercept                          2.68      0.45     1.82     3.58 1.00     3145     4020
# log_cumulative_visits             -0.13      0.05    -0.22    -0.03 1.00     9184     6101
# beh_type                           0.06      0.17    -0.27     0.41 1.00     4053     4592
# ageP                               0.03      0.47    -0.89     0.95 1.00     3123     3845
# sexM                               0.36      0.38    -0.40     1.12 1.00     3848     4623
# log_training_solves               -0.01      0.09    -0.17     0.16 1.00     3073     3867
# con_pres                          -0.26      0.11    -0.48    -0.04 1.00     8660     6035
# log_cumulative_visits:beh_type    -0.13      0.03    -0.19    -0.06 1.00     8843     5681
# 
# Further Distributional Parameters:
#   Estimate Est.Error l-95% CI u-95% CI Rhat Bulk_ESS Tail_ESS
# sigma     1.14      0.03     1.08     1.20 1.00     9867     5866
# 
# Draws were sampled using sampling(NUTS). For each parameter, Bulk_ESS
# and Tail_ESS are effective sample size measures, and Rhat is the potential
# scale reduction factor on split chains (at convergence, Rhat = 1).

plot.latency <- plot(conditional_effects(model_latency))

# overall, latency to first solve decreased with increasing experience
# shier individuals took overall longer but uncertain effect
# no effect of age, sex or training solves
# bolder individuals decreased their latency to first solve faster than shy individuals


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
# log_cumulative_count_sc 1.41 [1.34, 1.50]     1.19      0.71     [0.66, 0.75]
# beh_type 2.24 [2.10, 2.41]     1.50      0.45     [0.42, 0.48]
# age 2.28 [2.13, 2.45]     1.51      0.44     [0.41, 0.47]
# sex 3.12 [2.90, 3.36]     1.77      0.32     [0.30, 0.35]
# log_training_solves_sc 2.95 [2.74, 3.18]     1.72      0.34     [0.31, 0.37]
# con_pres 1.07 [1.04, 1.15]     1.04      0.93     [0.87, 0.97]

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

#save(model_eff_paw, file="model output/model_eff_paw.RDA")
load("model output/model_eff_paw.RDA")
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
# sd(mucontra_Intercept)     1.56      0.44     0.88     2.60 1.00     3836     5236
# sd(muipsi_Intercept)       1.23      0.31     0.76     1.98 1.00     4036     4935
# 
# Regression Coefficients:
#   Estimate Est.Error l-95% CI u-95% CI Rhat Bulk_ESS Tail_ESS
# mucontra_Intercept                           -1.29      0.76    -2.86     0.19 1.00     4931     5018
# muipsi_Intercept                              0.50      0.59    -0.66     1.69 1.00     4936     5241
# mucontra_log_cumulative_count_sc             -0.77      0.15    -1.05    -0.48 1.00     7051     6762
# mucontra_beh_type                            -0.68      0.53    -1.71     0.42 1.00     6186     4566
# mucontra_ageP                                -1.39      1.33    -4.09     1.25 1.00     4477     4583
# mucontra_sexM                                 0.95      1.03    -1.09     3.00 1.00     4965     5210
# mucontra_log_training_solves_sc               0.11      0.76    -1.46     1.59 1.00     4859     4910
# mucontra_con_pres1                           -0.78      0.28    -1.36    -0.24 1.00    12213     6191
# mucontra_log_cumulative_count_sc:beh_type     0.54      0.15     0.25     0.84 1.00     7418     6602
# muipsi_log_cumulative_count_sc               -0.80      0.13    -1.06    -0.56 1.00     6355     6862
# muipsi_beh_type                              -0.66      0.41    -1.45     0.16 1.00     5339     4995
# muipsi_ageP                                  -1.77      1.04    -3.88     0.23 1.00     4609     4816
# muipsi_sexM                                   0.28      0.80    -1.31     1.84 1.00     5199     4299
# muipsi_log_training_solves_sc                -0.12      0.58    -1.28     1.03 1.00     4808     4999
# muipsi_con_pres1                             -0.40      0.17    -0.74    -0.07 1.00    13684     5215
# muipsi_log_cumulative_count_sc:beh_type       0.14      0.12    -0.08     0.37 1.00     6440     6764
# 
# Draws were sampled using sampling(NUTS). For each parameter, Bulk_ESS
# and Tail_ESS are effective sample size measures, and Rhat is the potential
# scale reduction factor on split chains (at convergence, Rhat = 1).


plot.paw <- plot(conditional_effects(model_eff_paw, categorical =T))


plogis(fixef(model_eff_paw))

# Estimate Est.Error       Q2.5     Q97.5
# mucontra_Intercept                        0.2152295 0.6812697 0.05436128 0.5466701
# muipsi_Intercept                          0.6227960 0.6434757 0.34036224 0.8446008
# mucontra_log_cumulative_count_sc          0.3172380 0.5367231 0.25841005 0.3819967
# mucontra_beh_type                         0.3364407 0.6290896 0.15282829 0.6045042
# mucontra_ageP                             0.1998040 0.7909423 0.01649451 0.7767910
# mucontra_sexM                             0.7207793 0.7365183 0.25255083 0.9525636
# mucontra_log_training_solves_sc           0.5269247 0.6816645 0.18872361 0.8303833
# mucontra_con_pres1                        0.3152547 0.5706160 0.20449817 0.4406033
# mucontra_log_cumulative_count_sc:beh_type 0.6308934 0.5372061 0.56232605 0.6983263
# muipsi_log_cumulative_count_sc            0.3090984 0.5321901 0.25701952 0.3645382
# muipsi_beh_type                           0.3409419 0.6000637 0.18933090 0.5409865
# muipsi_ageP                               0.1456399 0.7386336 0.02014479 0.5577530
# muipsi_sexM                               0.5687080 0.6893438 0.21284586 0.8632398
# muipsi_log_training_solves_sc             0.4690129 0.6416331 0.21720930 0.7368661
# muipsi_con_pres1                          0.4014766 0.5424246 0.32298975 0.4831475
# muipsi_log_cumulative_count_sc:beh_type   0.5345660 0.5288417 0.47923707 0.5903943



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
# 7.97
quantile(OR_juv_vs_adult, c(0.025, 0.975))
#      2.5%      97.5% 
#   0.8915073 32.5449541

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

#save(model_box_int, file="model output/model_box_int.RDA")
load("model output/model_box_int.RDA")
#load("model output/model_box_int_min5.RDA") # load this for threshold of 5

r2_bayes(model_box_int)
# # Bayesian R2 with Compatibility Interval
# 
# Conditional R2: 0.166 (95% CI [0.122, 0.220])
# Marginal R2: 0.099 (95% CI [0.029, 0.211])


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
# sd(Intercept)     0.52      0.14     0.31     0.85 1.00     2749     4363
# 
# Regression Coefficients:
#   Estimate Est.Error l-95% CI u-95% CI Rhat Bulk_ESS Tail_ESS
# Intercept                            0.64      0.25     0.15     1.13 1.00     3893     4390
# log_cumulative_count_sc             -0.28      0.04    -0.36    -0.21 1.00     8817     5422
# beh_type                            -0.23      0.17    -0.56     0.10 1.00     4070     4262
# ageP                                -0.39      0.44    -1.30     0.48 1.00     3563     4247
# sexM                                -0.58      0.35    -1.28     0.10 1.00     4188     4473
# log_training_solves_sc              -0.08      0.25    -0.56     0.41 1.00     3169     3860
# con_pres1                           -0.23      0.09    -0.41    -0.05 1.00    10774     5872
# log_cumulative_count_sc:beh_type     0.12      0.03     0.05     0.19 1.00     9403     5969
# 
# Further Distributional Parameters:
#   Estimate Est.Error l-95% CI u-95% CI Rhat Bulk_ESS Tail_ESS
# shape     1.53      0.10     1.34     1.74 1.00    11450     5683
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
  exit ~ log_cumulative_count_sc*beh_type + age + sex + log_training_solves_sc + con_pres + (1 | Subject), 
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
#load("model output/model_stay_min5.RDA") # load this model output for a treshold of 5

r2_bayes(model_stay)
# # Bayesian R2 with Compatibility Interval
# 
# Conditional R2: 0.688 (95% CI [0.673, 0.703])
# Marginal R2: 0.595 (95% CI [0.488, 0.647])


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
# sd(Intercept)     2.41      0.69     1.43     4.09 1.00     3233     4601
# 
# Regression Coefficients:
#   Estimate Est.Error l-95% CI u-95% CI Rhat Bulk_ESS Tail_ESS
# Intercept                            3.22      1.21     0.74     5.55 1.00     3465     4022
# log_cumulative_count_sc              2.68      0.21     2.30     3.10 1.00     9878     5856
# beh_type                             2.00      0.87     0.33     3.77 1.00     4161     4653
# ageP                                -4.08      2.29    -9.03     0.14 1.00     3767     3851
# sexM                                -0.70      1.70    -4.19     2.63 1.00     3558     3765
# log_training_solves_sc              -1.15      1.16    -3.47     1.12 1.00     3895     3795
# con_pres1                           -0.33      0.36    -1.02     0.40 1.00    11871     5763
# log_cumulative_count_sc:beh_type    -1.82      0.31    -2.47    -1.24 1.00    10091     6338
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
                  "munoyes_con_pres1" = "Conspecific presence [consume]",
                  "munoyes_log_cumulative_visits:beh_type" = "Cumulative visits (log) × Boldness [consume]",
                  
                  
                  "muyesno_log_cumulative_visits" = "Log cumulative visits [solve]",
                  "muyesno_beh_type" = "Boldness [solve]",
                  "muyesno_ageP"  = "Age (J:A) [solve]",
                  "muyesno_sexM"  = "Sex (M:F) [solve]",
                  "muyesno_log_training_solves" = "Log # of training solve [solve]",
                  "muyesno_scaletotal_time_present" = "Visit duration [solve]",
                  "muyesno_con_pres1" = "Conspecific presence [solve]",
                  "muyesno_log_cumulative_visits:beh_type" = "Cumulative visits (log) × Boldness [solve]",
                  
                  "muyesyes_log_cumulative_visits" = "Log cumulative visits [solve, consume]",
                  "muyesyes_beh_type" = "Boldness [solve, consume]",
                  "muyesyes_ageP"  = "Age (J:A) [solve, consume]",
                  "muyesyes_sexM"  = "Sex (M:F) [solve, consume]",
                  "muyesyes_log_training_solves" = "Log # of training solve, consume [solve, consume]",
                  "muyesyes_scaletotal_time_present" = "Visit duration [solve, consume]",
                  "muyesyes_con_pres1" = "Conspecific presence [solve, consume]",
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
                  "con_pres1" = "Conspecific presence",
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
    x = "Cumulative visits (log)",
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
    x = "Visit duration",
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
  labs(x = "Cumulative visits (log)",
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
    x = "Cumulative visits (log)",
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

ggsave("Figures output/Solving efficiency.tiff", units='in', width=8, height=5.5, bg="white")


# 6.2) Behavioral skill figures -------------------------------------------

# panel a: paw use with experience

plot.eff.paw <- plot(conditional_effects(model_eff_paw, categorical=T))

p2.panel.a <- plot.eff.paw$`log_cumulative_count_sc:cats__`+
  geom_line(linewidth = 1.2) +
  geom_ribbon(aes(ymin = lower__, ymax = upper__), alpha = 0.05, color = NA) +
  theme_bw() +
  labs(
    x = "Log # of cumulative solves",
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
  theme(
    legend.position = c(0.55, 0.75), # relative coordinates (x, y) inside the plot
    legend.background = element_blank(),
    legend.title = element_text(size = 10),
    legend.text = element_text(size = 9)
  )+
  labs(x = "Log # of cumulative solves",
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



ggsave("Figures output/behavioral skill_supplementary figure.tiff", units='in', width=9, height=3.5)






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





