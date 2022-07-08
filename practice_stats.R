# t-test ----

# If you don't have the penguins data package installed from previous exercises,
# uncomment the line below and run it to install
#install.packages("palmerpenguins")

# load the data
library(palmerpenguins) 


# Remember the variable names by printing the names
# Information on the variables: https://github.com/allisonhorst/palmerpenguins
names(penguins)


# Do different types of penguin species have different average bill depths?
# Compute the means for each group

tapply(penguins$bill_depth_mm,penguins$species,mean,na.rm=TRUE)
aggregate(bill_depth_mm~species,data=penguins,mean,na.rm=TRUE)


# Test if the differences
# for Chinstrap and Gentoo are statistically different with a t-test:
t.test(penguins$bill_depth_mm[penguins$species == "Chinstrap"],
       penguins$bill_depth_mm[penguins$species == "Gentoo"])

# What do you conclude from the test?




# Bonus question: Does Adelie penguin body mass differ by island?
aggregate(body_mass_g~island,data=penguins[penguins$species=="Adelie",],mean,na.rm=TRUE)
summary(aov(body_mass_g~island,data=penguins[penguins$species=="Adelie",]))
# Make a subset of the data with rows just for Adelie penguins:
adelie <- penguins[penguins$species=="Adelie", ]


# Using adelie, compute the mean body_mass_g per island
adelie$island
t.test(adelie$body_mass_g[adelie$island=="Biscoe"],adelie$body_mass_g[adelie$island=="Dream"])


# Does the mass of penguins on Biscoe and Torgersen islands differ?




# Regression ----

# If you don't have the penguins data package installed from previous exercises,
# uncomment the line below and run it to install
#install.packages("palmerpenguins")

# load the data
library(palmerpenguins) 


# Remember the variable names by printing the names
# Information on the variables: https://github.com/allisonhorst/palmerpenguins
names(penguins)


# We want to know: Can we predict/explain penguin body mass with the variables we have?

# Run a linear regression with body_mass_g as the dependent (y, outcome) variable and 
# species, bill_length_mm, bill_depth_mm, flipper_length_mm, and sex as the independent (x, predictor) variables
reg1 <- lm(body_mass_g ~ species+bill_length_mm+bill_depth_mm+flipper_length_mm+sex, data=penguins)


# Look at the results of the regression with the summary function
summary(reg1)

# Notice in the summary output that observations (rows) with missing data were automatically
# removed from the analysis

# Plot the regression object to see a series of diagnostic plots
plot(reg1)


# Fit overall is pretty good.  The model above made Adelie penguins the base case, 
# and generated other indicator variables for the other species and gender.  
# To make a separate intercept for each species instead, add a -1 to the 
# formula you ran before; summarize the results again
reg2 <- lm(body_mass_g ~ -1+species+bill_length_mm+bill_depth_mm+flipper_length_mm+sex, data=penguins)
summary(reg2)



# If we look at the coefficients for the two models, you can see the coefficients 
# for species were the only ones to change
reg1$coefficients
reg2$coefficients


# Now, maybe the combination of bill length and depth also matters for body mass.
# So let's include an interaction term between the two bill variables.
# Hint: multiple two variables together to include them independently in the model and 
# have an interaction term automatically generated
reg3 <- lm(body_mass_g~bill_length_mm*bill_depth_mm+sex+species, data=penguins)
plot(reg3)
summary(reg3)



# Looks like that is important.  

# Bonus: prediction ----

# Let's use our model to predict the body mass for a new penguin.
# To predict, we need a data frame with the same column names as our original data.
newdata <- data.frame(species="Adelie",
                      sex="female",
                      # set other variables to mean for Adelie penguins
                      flipper_length_mm=mean(penguins$flipper_length_mm[
                        penguins$species=="Adelie"], na.rm=TRUE),
                      bill_length_mm=mean(penguins$bill_length_mm[
                        penguins$species=="Adelie"], na.rm=TRUE),
                      bill_depth_mm=mean(penguins$bill_depth_mm[
                        penguins$species=="Adelie"], na.rm=TRUE))

# The predict function will automatically make the indicator and interaction variables for us.
# Hint: look at the help for predict.lm (what gets called since we have an lm model)
# to see the order of the arguments it expects
predict(reg3, newdata )

# Is the predicted body mass in the range we might expect for Adelie penguins?
mean(penguins$body_mass_g[penguins$sex == "female" & penguins$species=="Adelie"], na.rm=TRUE)


# What's the confidence interval on the prediction?
# We use the prediction interval type because we want to know the expected 
# range for the mass of individual penguins, not the variability in the 
# expected value of the mean mass of penguins with these characteristics.
predict(reg3, newdata, interval="prediction")

