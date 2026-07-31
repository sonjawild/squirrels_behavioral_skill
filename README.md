# Boldness shapes cognitive processes underlying skill refinement during anthropogenic resource exploitation

## Folder: Code
contains R script to fully reproduce analysis, create tables and figures

## Folder: Data

Note that for each file there are two versions - one with an inclusion criterion of 10 visits and/or 10 solves, respectively, which was used for the main analysis. 
The second set has an inclusion criterion of 5 visits and/or 5 solves (all files have the addition _min5) in the file name. Since colummn names in both are the same, we will list them here only once:

Visits data: each row represents one distinct to a puzzle box
- Subject: Individual identifier for each squirrel
- Cumulative_visits: # of cumulative visits to any puzzle box
- Solves_per_visit: How many solves did the squirrel do during the given visit?
- Latency_to_first_solve: Latency between arrival and the first solve during a vist (given a solve occurred). In s
- Consumption: Did the squirrel visibly consume seed during the visit
- Total_time_present: visit duration in s
- age: A for adult, P for juveniles
- sex: F for female, M for male
- con_pres: 0 for no other squirrel present upon arrival, 1 for conspecific(s) present at the box upon arrival
- log_cumulative_visits: log # of cumulative visits

Combined data: each row represents one solve
- Subject: Individual identifier for each squirrel
- lever_side: which lever was pushed for solving (right or left)
- reaction: behavioral reaction after solving (exit means leaving the antenna area; flinch, no reaction or alert mean they stayed in the antenna area). Is changed into exit or stay for modeling purposes
- cumulative_count = # of cumulative solves per individual
- colony = which population (C or P). Note that they are pooled for this analysis due to limited sample size
- body_part = which body part was used for solving (left_paw, right_paw, both_paws). Is modified into contralateral paw, ipsilateral paw and both paws for modeling purposes (last column)
- log_cuulative_count = log # of cumulative individual solves
- box_between_solves = how many times did the squirrel (unsuccessfully) interact with the puzzle box (sniffing, touching) in between solves
- paw_comb = which paw did they use in relation to the lever (ipsi = left paw on left lever, or right paw on right lever; contra = righ paw on left lever or left paw on right lever; both = both paws on same side of lever)
- con_pres: 0 for no other squirrel present during a solve, 1 for conspecific(s) present at the box during a solve

Subject_data: Individual co-variates
- Subject: Unqiue identifier for each squirrel
- n_solves: number of total solves produced during the trial phase
- n_locations: how many puzzle box locations did they visit (out of 6 possible)
- age: A for adult, P for juvenile
- sex: F for female, M for male
- trainig_solves: how many solves did they produce during the training phase (as a proxy for task exposure during training)
- logtrainig_solve: log number of the above
- num_trapped: total number of times trapped during summer
- trap_rate_per_day: average number of times an individual was trapped on a trapping day (used to calculate boldness)
- any_beh_prop: proportion of trapping occasions during which a squirrel showed a fear response to an approaching human observer when trapped (struggle, chatter, alarm call). Used to compute boldness score

## Folder: Tables output
Model summaries (as presented in SI)

## Folder: Figures output
Figures as presented in the MS and SI (fully reproducible with the provided R code)

## Folder: model output
contains R Data objects of all five models with the two inclusion criteria (10 and 5)
