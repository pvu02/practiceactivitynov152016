library(haven)
my.data <- read_sav("Lecture 7 regression_example_data.sav")
apa.cor.table(my.data)


# Use correlations to pick out single best predictor - in this case, it is gma

my.regression2 <- lm(jobperf ~ gma + con,data=my.data)
library(apaTables)
apa.reg.table(my.regression2, filename = "myRegressionTable.doc")

my.regression3 <- lm(jobperf ~ gma + ac,data=my.data)
library(apaTables)
apa.reg.table(my.regression3, filename = "myRegressionTable2.doc")

my.regression4 <- lm(jobperf ~ gma + graph,data=my.data)
library(apaTables)
apa.reg.table(my.regression4, filename = "myRegressionTable3.doc")

block1 <- lm(jobperf~gma, data=my.data)
block2 <- lm(jobperf~gma + con, data=my.data)
apa.reg.table(block1,block2)
# Delta R2 tells you the semi-partial correlation of conscientiousness

block3 <- lm(jobperf~gma, data=my.data)
block4 <- lm(jobperf~gma + ac, data=my.data)
apa.reg.table(block3,block4)
# Delta R2 tells you the semi-partial correlation of AC

block5 <- lm(jobperf~gma, data=my.data)
block6 <- lm(jobperf~gma + graph, data=my.data)
apa.reg.table(block5,block6)
# Delta R2 tells you the semi-partial correlation of graph

# Mean GMA = 100, Mean conscientiousness = 120

# CI and PI
x_axis_range <- data.frame(gma = c(100), con = c(120))
CI_data <- predict(my.regression2, newdata = x_axis_range, interval = "confidence", level = 0.95)
CI_data <- as.data.frame(cbind(x_axis_range, CI_data))
print(CI_data)

# The predicted score for an individual with a GMA score of 100 and conscientiousness score of 120 is 101, 95% CI [126.28, 131.67]

x_axis_range <- data.frame(gma = c(100), con = c(120))
PI_data <- predict(my.regression2, newdata = x_axis_range, interval = "prediction", level = 0.95)
PI_data <- as.data.frame(cbind(x_axis_range, PI_data))
print(PI_data)

# The predicted score for an individual with a GMA score of 100 and conscientiousness score of 120 is 101, 95% PI [84.87, 117.13]

