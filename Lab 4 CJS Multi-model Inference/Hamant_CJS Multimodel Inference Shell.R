#### ---------------------------------------------------------------------------- ####
#### NTRES 4120/6120 -- Spring 2026. Multi-model Inference Lab 4
##   Part 1. Exploring the data
##   Part 2. Model Selection: Simple Example
##   Part 3. Model Averaging: Simple Example
##   Part 4. Model Selection: Advanced Example
##   Part 5. Model Averaging: Advanced Example
##   Part 6. Adding a New Covariate
#### ---------------------------------------------------------------------------- ####


#### Set Working directory
#setwd("C:/github/NTRES-6120/Lab 4 CJS Multi-model Inference")

#### Install and load necessary packages to make functions accessible
#install.packages("RMark") # This only needs to be done once on every computer you use.
library(RMark)
library(tidyverse)

#### Load data
data <- read_csv("CaptHist_Dippers.csv", col_types = list("character", "factor"))


#### -------------- Part 1. Exploring the data ----------------- ####

#### Step 1: Check to make sure the data read in correctly
head(data)
nrow(data) # number of individuals in dataset

#### Step 2: Create a MARK data frame and design matrix using the functions process.data and make.design.data
dipper.processed <- process.data(data = data, model = "CJS", begin.time = 1981,
                                 groups = "sex") # MARK dataframe with a start time and groups

dipper.ddl <- make.design.data(dipper.processed) # MARK design matrix

#### -------------- Part 2. Model Selection: Simple Example  ----------------- ####

#### Step 3: Create new objects that each contain one of the models for phi and one of the models for p
## For phi, include all possible models for the global model: cohort
Phi.cohort <- list(formula = ~ cohort)  # survival model with covariates
Phi.dot <- list(formula =~ 1) # survival null model

## For p, include all possible models for the global model: time
p.time <- list(formula =~ time) # capture model with covariates
p.dot <- list(formula =~ 1) # capture null model

#### Step 4: Create a model list of all possible model combinations given the formulas specified above using the function create.model.list()
model.list <- create.model.list("CJS") # creates all the possible model combinations

### NOTE: Create.model.list scans the environment and collects all models that have phi. or p. and combines them. If I have multiple lists named this, 
### and I don't want create.model.list to take them, I have to start them with something else. 


#### Step 5: Run MARK over the model list created in Step 4 using the function mark.wrapper()
results <- mark.wrapper(model.list, data = dipper.processed, ddl = dipper.ddl, delete = TRUE) # runs MARK over all the models in the model list

summary.mark(results$Phi.cohort.p.dot) # output of model specified

## Model Selection table
results # AIC 


#### -------------- Part 3. Model Averaging: Simple Example  ----------------- ####

#### Step 6: Compute model averaged estimates of phi from the results object generated in step 5 using the function model.average()
model.av.phi <- model.average(results, "Phi")

#### Step 7: Compute model averaged estimates of p from the results object generated in step 5 using the function model.average()
model.av.p <- model.average(results, "p")
  
#### Step 8: Compute summed weight of support for cohort effect on phi - I have done this for you here!
cohort_models <- results$model.table[grepl("cohort",results$model.table$model), ]
sum(cohort_models$weight)     # Sum of model support with cohort effect

no_cohort_models <- results$model.table[!grepl("cohort", results$model.table$model),]
sum(no_cohort_models$weight)  # Sum of model support without cohort effect


#### -------------- Part 4. Model Selection: Advanced Example  ----------------- ####

#### Step 9: Create new objects that each contain one of the models for phi and one of the models for p
## For phi, include all possible models for the global model: cohort + sex + time

Phi.cohort <- list(formula = ~ cohort)
Phi.sex <- list(formula = ~ sex) 
Phi.time <- list(formula = ~ time) 
Phi.cohort.time <- list(formula = ~ cohort + time) 
Phi.cohort.sex <- list(formula = ~ cohort + sex)
Phi.sex.time <- list(formula = ~ sex + time)
Phi.sex.cohort.time <- list(formula = ~ sex + cohort + time)

# ^ all covariate combinations, each in a separate list

Phi.dot <- list(formula =~ 1) # null model

## For p, include all possible models for the global model: sex + time

### needs to start with p. for create.model.list to work
p.time <- list(formula = ~ time)
p.sex <- list(formula = ~ sex)
p.time.sex <- list(formula = ~sex + time) # all covariate combinations

p.dot <- list(formula =~ 1) # capture null model

#### Step 10: Explore outputs from some of the model specifications that we have not yet discussed, including:
#### phi(cohort + time) p(sex + time) - fit a model specific to this parameterization! using the mark() function

model_1 <- mark(dipper.processed, dipper.ddl, model.parameters = list(Phi = Phi.cohort.time, p = p.time.sex)) # MARK model

####phi(cohort + sex + time) p(sex + time) - fit a model specific to this parameterization! using the mark() function

model_2 <- mark(dipper.processed, dipper.ddl, model.parameters = list(Phi = Phi.sex.cohort.time, p = p.time.sex)) # MARK model


#### Step 11: Create a model list of all possible model combinations given the formulas specified above using the function create.model.list()

model_list_11 <- create.model.list("CJS") # specify the type of mark model, combines all phi. and p. lists in environment

#### Step 12: Run MARK over the model list created in Step 9 using the function mark.wrapper()

results_11 <- mark.wrapper(model_list_11, data = dipper.processed, ddl = dipper.ddl, delete = TRUE) # MARK model for all model combinations in model_list_11

results_11 # AIC 

#### -------------- Part 5. Model Averaging: Advanced Example  ----------------- ####

#### Step 13: Compute model averaged estimates of phi from the results object generated in step 12 using the function model.average()

model.av.phi_11 <- model.average(results_11, "Phi")

#### Step 14: Compute model averaged estimates of p from the results object generated in step 12 using the function model.average()

model.av.p_11 <- model.average(results_11, "p")

#### -------------- Part 6. Adding in a New Covariate  ----------------- ####

## There may be other confounding factors that may also explain the relationships we were seeing in our "best-fit" model
## In this system, flooding occurred in 1982 and 1983. Let's look at the effect of flooding on apparent survival probability.

#### Step 15: Create a variable indicating whether the effect of flooding occurred in each year (I have done this for you in the next 2 lines!! Make sure you understand what the code is doing!) 
dipper.ddl$Phi$flood=0  # This will add another column to our design matrix for whether flooding occurred
dipper.ddl$Phi$flood[dipper.ddl$Phi$time==1982 | dipper.ddl$Phi$time==1983] = 1   # Flooding effect is indicated by a "1" when it occurred in years 2 OR 3 (1982, 1983)

#### Step 16: Take the set of models that you specified in step 9 and adapt the list to include:
## For phi, include all possible models for the global model: cohort + sex + time + flood

Phi.cohort <- list(formula = ~ cohort)
Phi.sex <- list(formula = ~ sex) 
Phi.time <- list(formula = ~ time) 
Phi.cohort.time <- list(formula = ~ cohort + time) 
Phi.cohort.sex <- list(formula = ~ cohort + sex)
Phi.sex.time <- list(formula = ~ sex + time)
Phi.sex.cohort.time <- list(formula = ~ sex + cohort + time)
Phi.flood <- list (formula = ~ flood)
Phi.cohort.flood <- list (formula = ~ cohort + flood)
Phi.sex.flood <- list (formula = ~ sex + flood)
Phi.time.flood <- list (formula = ~ time + flood)
Phi.cohort.sex.flood <- list (formula = ~ cohort + sex + flood)
Phi.cohort.time.flood <- list (formula = ~ cohort + time + flood)
Phi.sex.time.flood <- list (formula = ~ sex + time + flood)
Phi.cohort.sex.time.flood <- list (formula = ~ cohort + sex + time + flood)



## For p, include all possible models for the global model: sex + time

p.time <- list(formula = ~ time)
p.sex <- list(formula = ~ sex)
p.time.sex <- list(formula = ~sex + time)

#### Step 17: Create a model list of all possible model combinations given the formulas specified above using the function create.model.list()

model_list_17 <- create.model.list("CJS") # specify the type of mark model, combines all phi. and p. lists in environment. This includes the flood ones now

#### Step 18: Run MARK over the model list created in Step 17 using the function mark.wrapper()

results_17 <- mark.wrapper(model_list_17, data = dipper.processed, ddl = dipper.ddl, delete = TRUE) # includes flood 

results_17 # AIC table with flood included

#### Step 19: Compute summed weight of support for flood effect on phi

flood_models <- results_17$model.table[grepl("flood",results_17$model.table$model), ] # pulls out flood models 

sum(flood_models$weight)     # Sum of model support with flood effect

#cleanup(ask = FALSE)
