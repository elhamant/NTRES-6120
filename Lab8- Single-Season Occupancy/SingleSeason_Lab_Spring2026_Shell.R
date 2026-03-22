#### ---------------------------------------------------------------------------- ####
#### NTRES 4120/6120 -- Spring 2026. Occupancy Lab
##   Part 1: Real data with no covariates
##   Part 2: Real data with survey-level covariate on detection and categorical site-level covariate on occupancy
##   Part 3: Real data with survey-level covariate on detection and, categorical and continuous site-level covariates on occupancy
#### ---------------------------------------------------------------------------- ####

#### Set Working directory
#setwd("C:/github/NTRES-6120/Lab8- Single-Season Occupancy")

#### Install and load necessary packages to make functions accessible
#install.packages('unmarked')
library(unmarked)
library(tidyverse)

#### Load data
af <- read_csv("ALFL_Data.csv")
# Ssubset of the Alder Flycatcher dataset from Chandler et al. (2009)
# https://doi.org/10.1525/auk.2009.08083


#### ----------------- Part 1. REAL DATA WITH NO COVARIATES ----------------------- ####
# Class level Coding

#### Step 1: Explore the dataset

head(af)


#### Step 2: Index out just the detection/non-detection data

detect_af <- af |>
  select(Visit1:Visit3) |>
  

## What is the naive occupancy rate?

af |>
  summarize(count = sum(Visit1 + Visit2 + Visit3), 
            total_visit = n()*3, 
            naive_occupany = count/total_visit)

  
#### Step 3: Create an unmarked data frame that will format the data we need for this analysis
## Explore the function that creates the unmarked data
?unmarkedFrameOccu

## Create the unmarked data frame 


af_df <- unmarkedFrameOccu(as.matrix(detect_af))
                        
                           
                              #siteCovs = list(site = af$Site, af$woodveg, af$Habitat),
                         #  obsCovs = list(time1 = af$time.1, time2 = af$time.2, time3 = af$time.3))


#### Step 4: Fit the model and examine estimates

## Explore the function that fits single-season occupancy models
?occu

## Run the model

m_nocovariates <- occu(~1 ~1, data = af_df)


## Back-transform coefficent estimates (of psi and p) to the probability scale

m_nocovariates

# occupancy = 0.987

plogis(0.987) 

# detection = 0.384

plogis(0.384)

## What are the 95% confidence intervals on these estimates? 


confint(m_nocovariates, type = "state", level = 0.95)


confint(m_nocovariates, type = "det", level = 0.95)


#### -------------- Part 2. REAL DATA WITH SITE-LEVEL COVARIATE ON OCCUPANCY ----------------- ####
#Mixed- class led/guided by instructor
# We will continue working with the detection/nondetection object af.dat

#### Step 5: Isolate site-level covariate(s)

# unmarked needs the covariates as factors. 
sitecovs <- af |>
  select(Site, woodveg, Habitat)

habitat <- af |>
  select(Habitat) |>
  mutate(habitat = as.factor(Habitat)) |>
  select(habitat) # I hope this works as a factor for unmarked

table(habitat) # shows the number of records for each factor
  
#### Step 6: Isolate the survey-level covariate(s)

time <- af |>
  select(time.1, time.2, time.3)



dim(time) # shows amount of rows and amount of columns. Important because the amount of rows and columns must match
  

#### Step 7: Create an unmarked data frame with one site-level covariate, habitat, and one survey-level covariate, time

af_umf_twocovariates <- unmarkedFrameOccu(y =as.matrix(detect_af), siteCovs = data.frame(habitat = habitat), 
                                        obsCovs = list(time = time))

# list is making it go into an array format so that each site/visit combo has one time attached. 

# y matrix is the detection data. Papers use that terminolology

af_umf_twocovariates

#### Step 8: Standardize observation covariates to improve estimation

obsCovs(af_umf_twocovariates) <- scale(obsCovs(af_umf_twocovariates))

summary(af_umf_twocovariates)

#### Step 9: Fit the model and examine estimates

fm2 <- occu(formula = ~ time ~ habitat, data = af_umf_twocovariates) 

#IMPORTANT - First detection, then occupancy (~detection model ~occupancy model)

## Run the model
fm2

#### Step 10: Make predictions for occupancy as a function of our covariate
## First, create new data frame that has the range of covariate values we want to predict over

newdata.psi <- data.frame(habitat = c("A", "B", "C"))
  
## Now we can plug that into the predict() function

est.psi <- predict(object = fm2, type = "state", newdata = newdata.psi, appendData = TRUE)

head(est.psi) # shows the predicted occupancy for each habitat type 

mean(est.psi$Predicted) ## average occupancy across the 3 habitats

#### Step 11: Create a plot of the relationship between predicted occupancy and habitat type

af.barplot <- barplot(est.psi$Predicted ~est.psi$habitat, ylim = c(0,1), 
                      xlab = "Habitat Type", 
                      ylab = "Expected Occupancy probability")

# segments(af.barplot, est.psi$lower, af.barplot, est.psi$upper, lwd = 1.5) to get the line

arrows(af.barplot, est.psi$lower, af.barplot, est.psi$upper, lwd = 1.5, angle = 90, code = 3 , length = 0.1) 

#### Step 12: Plot relationship between p (detection probability) and time of day

newdata.p <- data.frame (time = as.numeric(seq(-2.08258, 1.85788, by = 0.1))) #obtain min value and max value from the summary of the model

est.p <- predict(fm2, type = "det", newdata = newdata.p, appendData = TRUE) # appendData makes the estimated data align to the predicted data

head(est.p) # shows first 6 values


plot(est.p$Predicted ~est.p$time, type = "l", ylim = c(0,1), col = "blue", 
     xlab = "Time of day (standardized)", 
     ylab = "Expected Occupancy probability")

lines(est.p$lower ~ est.p$time, type = "l", col = gray(0.5))
lines(est.p$upper ~ est.p$time, type = "l", col = gray(0.5))

#### -------------- Part 3. REAL DATA WITH SURVEY-LEVEL COVARIATE ON DETECTION AND A CONTINUOUS AND CATEGORICAL COVARIATE ON OCCUPANCY ----------------- ####
# Develop the code to fit a model and view the summary output for a model where:  time is the covariate for detection probability and habitat and woody vegetation are the covariates for occupancy probability. 

#### Step 13: Isolate and standardize covariate on percent woody vegetation

woodveg <- af |>
  select(woodveg) |>
  mutate(wood = as.numeric(scale(woodveg)))|>
  select(wood)
  

#### Step 14: Create an unmarked data frame that includes y, our site-level covariate (standardized woody veg.) and our survey-level covariate (time)
af_umf3 <- unmarkedFrameOccu(y =as.matrix(detect_af), siteCovs = data.frame(wood = woodveg), 
                                          obsCovs = list(time = time))

#### Step 15: Standardize observation-level covariate(s)

obsCovs(af_umf3) <- scale(obsCovs(af_umf3))

summary(af_umf3)

#### Step 16: Fit the model and examine estimates

fm3 <- occu(formula = ~ time ~wood, data = af_umf3)

fm3

#### Step 17: Plotting occupancy as a function of woody vegetation
# Create new data frame for predictions
newdata.psi2<-data.frame(wood=seq(-1.5967,2.3826, by=0.1), habitat="A") #We keep the habitat covariate constant by selecting the reference category

# Plug that new data frame into predict function

est2.psi <- predict(fm3, type = "state", newdata = newdata.psi2, appendData = TRUE)
                           
# Plot                          

plot(est2.psi$Predicted ~est2.psi$wood, type = "l", ylim = c(0,1), col = "blue", 
     xlab = "Woody Vegetation (Standardized)", 
     ylab = "Expected Occupancy probability")

lines(est2.psi$lower ~ est2.psi$wood, type = "l", col = gray(0.5))
lines(est2.psi$upper ~ est2.psi$wood, type = "l", col = gray(0.5))

