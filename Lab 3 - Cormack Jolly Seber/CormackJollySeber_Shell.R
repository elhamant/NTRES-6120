#### ---------------------------------------------------------------------------- ####
#### NTRES 4120 -- Spring 2026. CJS Lab. Shell Script
##   Part 1: Exploring and formatting the data
##   Part 2: Constant (time-invariant) survival and capture CJS model
##   Part 3: Time-varying survival and capture CJS model
##   Part 4: Adding a group-level covariate to survival and capture
##   Part 5: Model inference (plotting)
#### ---------------------------------------------------------------------------- ####


#### Set Working directory
setwd("C:/github/NTRES-6120/Lab 3 - Cormack Jolly Seber")

#### Install and load necessary packages to make functions accessible

library(tidyverse)
library(RMark)
library(ggplot2)


#### Load data

dippers <- read_csv("CaptHist_Dippers.csv", col_types = list("character", "factor")) #col types c = character, f = factor

 
#### -------------- Part 1. Exploring and formatting the data ----------------- ####

#### Step 1: Check to make sure the data are read in correctly

head(dippers) # first six rows

str(dippers) # structure of data 

nrow(dippers) # number of individuals seen and captured during the study 


#### Step 2: Create a MARK data frame and design matrix using the functions process.data and make.design.data

dipper.proccessed <- process.data( data = dippers, model = "CJS")

dipper.dm <- make.design.data(dipper.proccessed)

dipper.dm


#### -------------- Part 2. Constant (time-invariant) survival and capture CJS model ----------------- ####

# time - invariant = constant time

#### Step 3: Create new objects that contain a constant (dot) model for phi and a constant (dot) model for p

phi.dot <- list(formula = ~1) # model formulas. ~1 is an intercept only model. no variation across time or groups
 
p.dot <- list(formula = ~1) # model formula for p. ~1 is an intercept only model. no variation across time or groups


#### Step 4: Create the MARK model using the mark data frame, design matrix, and the model for phi and p specified above

# mark is how to run the model. This is the same regardless of model types, why you have to specify the type when you read data into RMark

model_1 <- mark(dipper.proccessed, dipper.dm, model.parameters = list(Phi = phi.dot, p = p.dot)) # always need to specify Phi and p model formulas for CJS models 

## Beta parameters are on the logit scale 

## Real Parameter Phi and p - unlogit, real probabilities 



#### Step 5: Pull out parameter estimates for a given model and interpret

# Phi - .56. An individiual has ~56% chance of surviving between any two time periods 

# p - .90. 90% chance of capturing a marked individual at any capture ocassion 

summary.mark(model_1) # to see the output from when it ran that model 

model_1$results$beta # parameter estimates on logit scale 

model_1$results$real # parameter estimates on probability scale

# g1 c1 a0 t1 <- group 1, cohort 1, age 0, time 1 

model_1$results$real[1,] # pull out estimates for Phi, not p. Phi is in the first row 

model_1$results$real[grepl("Phi", rownames(model_1$results$real)),] #pulls out rows that have Phi in them

#grepl search through text and pulls out anything equal to the text you told it to look for

# grepl(what you want it to look for, where you want it to look for (rownames, column, etc))




#### -------------- Part 3. Time-varying survival and capture CJS model ----------------- ####

#### Step 6: Create new objects that contain a time-varying model for phi and p

phi.time.dot <- list(formula = ~time)

p.time.dot <- list(formula = ~time)

#### Step 7: Create the MARK model using the mark data frame, design matrix, and the model for phi and p specified above

model_2 <- mark(dipper.proccessed, dipper.dm, model.parameters = list(Phi = phi.time.dot, p = p.time.dot))

# want B1? Need to take the intercept (B0) and B1

summary(model_2)


#### -------------- Part 4. Adding a group-level covariate to survival and capture ----------------- ####

#### Step 8: Create a MARK data frame and design matrix using the functions process.data and make.design.data
# These codes add additional information to the dataframe that mark uses. 


dipper.processed2 <- process.data(data = dippers, model = "CJS", groups = "sex") # groups groups the data by sex

dipper.dm2 <- make.design.data(dipper.processed2)

dipper.dm2

#### Step 9: Create new objects that contain a group-level covariate on survival for "cohort" and a group-level covariate on p for "sex"

phi.4.dot <- list(formula = ~cohort)

p.4.dot <- list(formula = ~sex)

#### Step 10: Create the MARK models using the mark data frame, design matrix, and the model for phi and p specified above


model_4 <- mark(dipper.processed2, dipper.dm2, model.parameters = list(Phi = phi.4.dot, p = p.4.dot))

#### Step 11: Pull out parameter estimates on the real/probability scale for a each model and interpret


model_4$results$real[grepl("Phi", rownames(model_4$results$real)),] #generally looks like as cohort increases, survival increases


model_4$results$real[grepl("p", rownames(model_4$results$real)),] # higher probability to capture males vs females 

#### -------------- Part 5. Model inference (plotting) ----------------- ####
#### Step 12: Plot the estimates of survival probability from the last model (phi(cohort)p(sex))


##   Step 12a: Pull out the derived estimates of survival probability using grepl and $results$real and assign them to an object

plotsurvival <- model_4$results$real[grepl("Phi", rownames(model_4$results$real)),]

## Step 12b: Create a new column in that object called "cohort" that specifies the cohort each estimate corresponds to

plotsurvival$cohort <- c(1:6)
  
## Step 12c: Create a plot of the survival probability for each cohort on the x-axis with 95% confidence intervals

y_min <- min(plotsurvival$lcl)
y_max <- max(plotsurvival$ucl)

plot(plotsurvival$cohort, plotsurvival$estimate, 
     xlab = "Cohort", ylab  = "Survival Probability", 
     ylim = c(y_min - 0.5, y_max + 0.5), pch = 19) +
  arrows(plotsurvival$cohort, plotsurvival$cohort, 
         y0 = plotsurvival$lcl, y1 = plotsurvival$ucl, col = "darkblue", lwd = 1, length = .15, angle = 90, code = 3)



#### Step 13: Plot the estimates of capture probability from the last model (phi(cohort)p(sex))
# Step 13a: Pull out the derived estimates of capture probability using grepl and $results$real and assign them to an object

plotcapture <- model_4$results$real[grepl("p", rownames(model_4$results$real)),]


## Step 13b: Create a new column called "sex" that specifies the sex each estimate corresponds to

plotcapture$sex <- c("Female", "Male")

## Step 13c: Create a plot of the capture probability for each sex on the x-axis with 95% confidence intervals
?barplot()
y_minc <- min(plotcapture$lcl)
y_maxc <- max(plotcapture$ucl)

p <- barplot(plotcapture$estimate, xlab = "Sex", ylab = "Capture Probability", names.arg = c("Female", "Male"),
             ylim = c(0, y_maxc + 0.2), pch = 19) +
arrows(x0 = p, x1 = p, y0 = plotcapture$lcl, y1 = plotcapture$ucl, code = 3, angle = 90)

#cleanup(ask = FALSE)