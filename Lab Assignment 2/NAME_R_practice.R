#### ------------------------------------------------------------------------- ####
#### [YOUR NAME]: R take-home assignment 
#### ------------------------------------------------------------------------- ####

#### Set working directory (we will set our own working directory when grading)


#### Load woodpecker home range data


#### Explore the data

## Q1


## Q2


## Q3


## Q4



#### Analyze the data

## Q5


# Which has a stronger correlation?
# Answer: 

## Q6


## Q7


# Coefficient for Dead_tree_BA:
# Coefficient for Elevation: 
# Answer:


#### Create your own plot
# Hint: use par(mfrow=c(2,1)) to make a 2-panel plot
# Hint: we will need to use predict() to generate model predictions to a sequence
#       of x values. Because the model had 2 covariates, we need to create a "newdata"
#       data frame for each covariate. When predicting to a sequence of x values for 
#       one covariate, we set the other covariate at its mean value. 
#       Use the code below to create "newdata" for the predict() function.
newdata.ba <- data.frame(Dead_tree_BA = seq(from = 1, to = 39, length.out = 100),
                         Elevation = rep(mean(hr$Elevation), times = 100))
newdata.elev <- data.frame(Dead_tree_BA = rep(mean(hr$Dead_tree_BA), times = 100),
                           Elevation = seq(from = 1233, to = 2583, length.out = 100))


