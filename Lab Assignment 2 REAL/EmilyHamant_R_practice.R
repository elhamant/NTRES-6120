#### ------------------------------------------------------------------------- ####
#### [Emily Hamant]: R take-home assignment 
#### ------------------------------------------------------------------------- ####

#### Set working directory (we will set our own working directory when grading)

setwd("C:/github/NTRES-6120")

#### Load woodpecker home range data
library(tidyverse) #install.packages("tidyverse")
library(ggplot2) # install.packages("ggplot2")

woodpecker <- read_csv("Lab Assignment 2/Woodpecker_data.csv")
#### Explore the data

head(woodpecker) # shows the first 6 lines of woodpecker

## Q1

woodpecker |> # selects dataset
  arrange(-HR_size)  # arranges the data by HR_size from highest to lowest

which.max(woodpecker$HR_size) 
  # selects the row that has the maximum HR size in the woodpecker dataset

woodpecker[9,] 
  # shows the information in row 9

## Q2

woodpecker |> # choose dataset
  group_by(Nat_forest)|> # group by the national forest
  summarize(median = median(Elevation)) # calculates the median elevation for each national forest

tapply(X = woodpecker$Elevation, INDEX = woodpecker$Nat_forest, FUN = median) 
  # chooses the elevation data in woodpecker, indexes by national forest, applies a median function to the chosen data

## Q3

woodpecker |> # choose dataset
  group_by(Nat_forest) |> # group by National Forest
  summarize(mean = mean(HR_size)) # calculates the mean HR size for each national forest

tapply(X = woodpecker$HR_size, INDEX = woodpecker$Nat_forest, FUN = mean) 
  # subsets the data to contain HR size, indexes by national forest, applies the mean function to all HR size data for each national forest



## Q4

woodpecker |> # chooses dataset
  group_by(Nat_forest) |> # groups by national forest
  summarize(count = n()) # summarizes the number of rows within each national forest

tapply(X = woodpecker, INDEX = woodpecker$Nat_forest, FUN = count)
  #selects the entirety of the woodpecker dataset, indexes by national forest, and counts the number of rows




#### Analyze the data

## Q5

cor(woodpecker$HR_size, woodpecker$Elevation)

cor(woodpecker$HR_size, woodpecker$Dead_tree_BA)
  
  
# Which has a stronger correlation?
# Answer: Home range size and dead tree basal area

## Q6

Q6model <- lm(HR_size ~ Dead_tree_BA + Elevation, data = woodpecker)

summary (Q6model)

## Q7


# Coefficient for Dead_tree_BA: -2.069
# Coefficient for Elevation: 2.651
# Answer:


#### Create your own plot
# Hint: use par(mfrow=c(2,1)) to make a 2-panel plot
# Hint: we will need to use predict() to generate model predictions to a sequence
#       of x values. Because the model had 2 covariates, we need to create a "newdata"
#       data frame for each covariate. When predicting to a sequence of x values for 
#       one covariate, we set the other covariate at its mean value. 
#       Use the code below to create "newdata" for the predict() function.

newdata.ba <- data.frame(Dead_tree_BA = seq(from = 1, to = 39, length.out = 100),
                         Elevation = rep(mean(woodpecker$Elevation), times = 100)) 
  # generating model predictions from the linear model for dead tree basal area and keeping elevation at the mean. 

newdata.elev <- data.frame(Dead_tree_BA = rep(mean(woodpecker$Dead_tree_BA), times = 100),
                           Elevation = seq(from = 1233, to = 2583, length.out = 100))
  # generating model predictions from the linear model for elevation and keeping dead tree basal area at the mean. 

new_ba <- as.data.frame(predict(Q6model, newdata =  newdata.ba, 
                                type = "response", interval = "confidence", level = 0.95))
  # creating a dataframe with new predictions from dead tree basal area, with a 95% confidence interval

new_elevation <- as.data.frame(predict(Q6model, newdata =  newdata.elev, 
                                type = "response", interval = "confidence", level = 0.95))
  # creating a dataframe with new predictions from elevation, with a 95% confidence interval


jpeg(filename = "HR_plot.jpg", width = 12, height = 20,
     units = "in", res = 300)
  # open a jpeg file

par(mfrow=c(2,1)) 
  # create a panel plot with 2 rows and 1 column

plot(x = woodpecker$Dead_tree_BA, y = woodpecker$HR_size, 
     xlab = "Dead Tree Basal Area (m^2/ha)", ylab = "Home Range Size (ha)", pch = 19, col = factor(woodpecker$Nat_forest), cex.lab = 1.5) 
  # base plot for HR vs dead tree basal area, with axis labels and color by national forest
lines(newdata.ba$Dead_tree_BA, new_ba$fit, col = "darkblue", lwd = 1)
  # add model fit line
lines(newdata.ba$Dead_tree_BA, new_ba$lwr, col = "darkblue", lty = 3)
  # add lower confidence interval
lines(newdata.ba$Dead_tree_BA, new_ba$upr, col = "darkblue", lty = 3)
  # add upper confidence interval
legend("topright",
       legend = levels(factor(woodpecker$Nat_forest)),
       pch = 19,
       col = factor(levels(factor(woodpecker$Nat_forest))), 
       cex = 1.1)
  # add a legend in the top right corner


plot(x = woodpecker$Elevation, y = woodpecker$HR_size, 
     xlab = "Elevation (m)", ylab = "Home Range Size (ha)", pch = 19, col = factor(woodpecker$Nat_forest), cex.lab = 1.5)
  # base plot for HR vs elevation, with axis labels and color by national forest
lines(newdata.elev$Elevation, new_elevation$fit, col = "darkblue", lwd = 1)
  #add model fit line
lines(newdata.elev$Elevation, new_elevation$lwr, col = "darkblue", lty = 3)
  #add lower confidence interval
lines(newdata.elev$Elevation, new_elevation$upr, col = "darkblue", lty = 3)
  #add upper confidence interval
legend("topright",
       legend = levels(factor(woodpecker$Nat_forest)),
       pch = 19,
       col = factor(levels(factor(woodpecker$Nat_forest))), 
       cex = 1.1)

  #add legend in top right corner

dev.off()
  #turn off jpeg dev
