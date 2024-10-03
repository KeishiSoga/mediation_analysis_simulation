rm(list = ls())
# Import Packages
library(lavaan)
library(semPlot)

# Dataset
# Generate random numbers
set.seed(1234)
ta = rnorm(20, 50, 10) # Normal distribution: mean 50, standard deviation 10
tb = rnorm(20, 65, 10) # Normal distribution: mean 65, standard deviation 10
tc = rnorm(20, 80, 10) # Normal distribution: mean 80, standard deviation 10

# Generate random numbers
ea = rnorm(20, 20, 5) # Normal distribution: mean 20, standard deviation 5
eb = rnorm(20, 40, 5) # Normal distribution: mean 40, standard deviation 5
ec = rnorm(20, 60, 5) # Normal distribution: mean 60, standard deviation 5

# Generate random numbers
ra = rnorm(20, 43, 2) # Normal distribution: mean 43, standard deviation 2
rb = rnorm(20, 38, 2) # Normal distribution: mean 38, standard deviation 2
rc = rnorm(20, 22, 2) # Normal distribution: mean 22, standard deviation 2

# Create data frames
data1 = data.frame(ta, ea, ra)
data2 = data.frame(tb, eb, rb)
data3 = data.frame(tc, ec, rc)

# Rename data frame columns
names(data1) <- c("SC", "PA", "BMI")
names(data2) <- c("SC", "PA", "BMI")
names(data3) <- c("SC", "PA", "BMI")

# Combine data frames
data12 = rbind(data1, data2)
data_all = rbind(data12, data3)

# Create model
model <- ' SC ~ c*PA
           BMI ~ a*PA
           SC ~ b*BMI
           ab := a*b
           total := c + (a*b)'

# Analyze model
fit <- sem(model, data = data_all)

# Display results
summary(fit, ci=TRUE)

# Create path diagram
semPaths(fit, style="lisrel", whatLabels="est",
         layout="tree", rotation=2, edge.label.cex=0.8)

#################################################################
# Case with multiple mediating factors
#################################################################
set.seed(1234)

# Generate random numbers
sa = rnorm(20, 2, 1) # Normal distribution: mean 2, standard deviation 1
sb = rnorm(20, 5, 1) # Normal distribution: mean 5, standard deviation 1
sc = rnorm(20, 10, 1) # Normal distribution: mean 10, standard deviation 1

# Create data frames
data11 = data.frame(ta, ea, ra, sc)
data22 = data.frame(tb, eb, rb, sc)
data33 = data.frame(tc, ec, rc, sc)

# Rename data frame columns
names(data11) <- c("SC", "PA", "BMI", "SLP")
names(data22) <- c("SC", "PA", "BMI", "SLP")
names(data33) <- c("SC", "PA", "BMI", "SLP")

# Combine data frames
data123 = rbind(data11, data22)
data_all_2 = rbind(data123, data33)

# Create model
model2 <- ' SC ~ c*PA
           BMI ~ a1*PA
           SLP ~ a2*PA
           SC ~ b1*BMI
           SC ~ b2*SLP
           ab1 := a1*b1
           ab2 := a2*b2
           total := c + (a1*b1) + (a2*b2)'

# Analyze model
fit3 <- sem(model2, data = data_all_2)

# Display results
summary(fit3, ci=TRUE)

# Create path diagram
semPaths(fit3, style="lisrel", whatLabels="est",
         layout="circle2", rotation=1, edge.label.cex=0.8)