#HW 1
#1

getwd()

#Install Tidyverse for ggplot2 capability
install.packages("tidyverse")
library(tidyverse)

#Access the dplyr library to cleanup the data and change -99 to NA
library(dplyr)

#Load the dataset TreeCityUSA to a dataframe called tree
tree <- read.csv("TreeCityUSA.csv")
summary(tree)

#Cleanup fam_inc_med and pct_bachdeg missing data into R format
tree$fam_inc_med [tree$fam_inc_med ==-99] <- NA
tree$pct_bachdeg [tree$pct_bachdeg ==-99] <- NA

#Combine cleaned fam_inc_med and pct_bachdeg data into a single dataframe called data1
data1 <- select(tree, fam_inc_med, pct_bachdeg)
dim(data1)
summary(data1)

#1(a)

#Make a scatterplot of data1
ggplot(data = data1) + 
  geom_point(mapping = aes(x = fam_inc_med, y = pct_bachdeg)) +
  #geom_smooth(mapping = aes(x = fam_inc_med, y = pct_bachdeg),method=lm) +
  labs(
    title = "Problem 1(a)",
    x = "Median Income, USD ",
    y = "Proportion of Bachelors Degrees "
  ) 

#1(b)
#Yes there appears to be a relationship between the proportion of people with a bachelor's degree and 
#median income.  The relationship appears to be that the given a higher median income the proportion of
#bachelor's degrees is also higher.

#1(c)
lm(data1$pct_bachdeg~data1$fam_inc_med)
#Regression equation:
#Let Y = pct_bachdeg
#Let X = fam_inc_med
#Y = -0.04458 + 0.000004509X + e

#1(d)
#Make a scatterplot of data1 with the regression line added superimposed
ggplot(data = data1) + 
  geom_point(mapping = aes(x = fam_inc_med, y = pct_bachdeg)) +
  geom_smooth(mapping = aes(x = fam_inc_med, y = pct_bachdeg),method=lm, lwd = 2) +
  labs(
    title = "Problem 1(a)",
    x = "Median Income, USD ",
    y = "Proportion of Bachelors Degrees "
  ) 
#The regression line appears to fit the data for median incomes up to about $200,000.  For median
#incomes greater than $200,000 the regression line model does not appear to be a good fit.

#2 - Obtain a sample of 30 observations from the data frame in Problem 1 (data1).  Give this dataframe
#the name sample2.

#use dplyr sample_n function for random sample of 30 rows from data frame data1
sample2 = sample_n(data1, 30)
sample2

#2(a) - Obtain a scatterplot similar to the one in 1(a), using the sample data

ggplot(data = sample2) + 
  geom_point(mapping = aes(x = fam_inc_med, y = pct_bachdeg)) +
  #geom_smooth(mapping = aes(x = fam_inc_med, y = pct_bachdeg),method=lm) +
  labs(
    title = "Problem 2(a)",
    x = "Median Income, USD ",
    y = "Proportion of Bachelors Degrees "
  ) 

#2(b) - Yes, from the scatterplot I would consider a straight line fit for this sample.  Their appears
#to be a linear relationship between family median income and the proportion of bachelors degrees.

#2(c) - Obtain the sample regression equation using the lm function.

lm(sample2$pct_bachdeg~sample2$fam_inc_med)
#Regression equation:
#Let Y = pct_bachdeg
#Let X = fam_inc_med
#Y = -0.01808 + 0.000004140X
#Y = -0.04458 + 0.000004509X

#2(d) - How similar are bo and b1 to beta-0 and beta-1?  Would you expect them to be similar?
#I think that bo and b1 are very similiar to beta-0 and beta-1.  bo and b1 appear to underestimate
#the values of beta-0 and beta-1 slightly for this dataset.  I would expect them to be simliar because
#it appears that a linear model is reasonable for this data.

#3
install.packages("aprean3")
library(aprean3)
attach(dse03f)
head(dse03f)
#dim(dse03f)
#dse03f

#3(c) - CHeck your answers in (b) using lm.
lm(dse03f$y~dse03f$x)

#3(e) - Confirm numerically that the residuals sum to zero.

#vector of the values of X from dse03f
Xi = dse03f$x
#vector of sample predicted Y values
Y_hat = -21.33 + 5*Xi
#calculate the residuals
res = dse03f$y - Y_hat
#sum the residuals to confirm the sum is zero. 
s_res = sum(res)
s_res
#sum of the residuals is not exactly zero due to rounding error of the model

#3(g)
#Compute the SSreg
SSreg <- sum((Y_hat - mean(dse03f$y))^2)
SSreg
#compute the SSres
SSres <- sum((dse03f$y - Y_hat)^2)
SSres
#Compute the SSyy
SSyy <- sum((dse03f$y - mean(dse03f$y))^2)
SSyy

#3(h) - Compute R^2 and give an interpretation of the value
Rsqd = SSreg/SSyy
Rsqd
#75% of the total variation of the sample data about the mean of Y can be explained by the regression.


#3(i)
#Compute the SSbo
n = 12
SSbo = n*(mean(dse03f$y)^2)
SSbo

#Compute the SSb1|bo
SSb1bo = sum((Y_hat - mean(dse03f$y))^2)
SSb1bo

#Compute the sum Y^2
sum_Ysq = sum(dse03f$y^2)
sum_Ysq

#4

#4(a)
ggplot(data = dse03f) + 
  geom_point(mapping = aes(x = x, y = y)) +
  geom_smooth(mapping = aes(x = x, y = y), method=lm, se = FALSE) +
  labs(
    title = "Problem 4(a)",
    x = "Mix Moisture",
    y = "Density"
  ) 

#The regression appears to describe the data fairly well.  The data appears to be evenly distributed
#on either side of the regression line with no apparent outliers.

#4(b) - Compute a 99% confidence interval for Beta_1.
alpha = .01
b1 = 5
n = 12
MSres = SSres/(n-2)
Sxx = sum((dse03f$x - mean(dse03f$x))^2)
tval = qt(1-alpha/2,n-2)
s_b1 = sqrt(MSres/Sxx)
lower = b1 - tval*s_b1
lower
upper = b1 + tval*s_b1
upper

#4(c) 
#If we sampled this data many times we would find that 99% of the time b1 would be contained within
#the interval [2.135,7.865].

#4(d) - Consider the following statement: "Changing the moisture content of a mix has no effet on its
#finished product density."  Based on the CI, would you tend to agree or disagree with that statement?
#Based on the calculated CI in part b I would tend to disagree with that statement.  Since the CI does
#not contain zero there is evidence that changing the moisture content of a mix does have an effect
#on its finished product density.

#4(e) 
#step 1
#Define the hypothesis to be tested
#Ho: Beta_1 = 0 vs. Ha: Beta_1 != 0

#Step 2
#Set significance level
alpha = 0.01

#Step 3
#State assumptions
#Assume that b1 is distributed N(Beta_1, sigma^2/Sxx)

#Step 4
#State the decision rule
#Reject Ho when |t| > t(1-alpha,n-2)
tval = qt(1-alpha/2,n-2)
tval

#Step 5
#Calculate T statistic from the sample
t_calc = b1/s_b1
t_calc

#Step 6
#Calculate the p value
p_value = 2*pt(abs(t_calc),n-2,lower.tail=FALSE)
p_value

#Step 7
#State Conclusion
#Given that our rejection criteria was met we will reject Ho and conclude that we have evidence that
#beta_1 is not equal to zero, and a linear relationship does exist between the moisture content of a 
#mix and the finished product density.

#4(f)
#step 1
#Define the hypothesis to be tested
#Ho: Beta_1 = 0 vs. Ha: Beta_1 != 0

#Step 2
#Set significance level
alpha = 0.01

#Step 3
#State assumptions
#Assume that b1 is distributed N(Beta_1, sigma^2/Sxx)

#Step 4
#State the decision rule
df1 = 1
df2 = n-2
#Reject Ho when |f_calc| > f(1-alpha, df1, df2)
Fval = qf(1-alpha/2,df1, df2)
Fval

#Step 5
#Calculate F statistic from the sample
MSreg = SSreg
f_calc = MSreg/MSres
f_calc

#Step 6
#Calculate the p value
p_value = pf(f_calc,df1,df2,lower.tail=FALSE)
p_value

#Step 7
#State Conclusion
#Given that our rejection criteria was met we will reject Ho and conclude that we have evidence that
#beta_1 is not equal to zero, and a linear relationship does exist between the moisture content of a 
#mix and the finished product density.  (Same conclusion as part (e))

#4(g)
t_squared = t_calc^2
t_squared
f_calc
#t_calc squared and f_calc are indeed equal

#4(h)
#I think my preference would be the t-test methodology.  The t distribution is easy for me to visualize
#and I feel comfortable with it.  Because it is symmetric it is easy to visualize the rejection regions
#and criteria.  In general I like the hypothesis testing methods since they provide a clear and logical way
#to make a decision.  The t test is just a bit more familiar and comfortable to me than the F test.

#4(i)
Syy = sum((dse03f$y - mean(dse03f$y))^2)
Sxy = sum((dse03f$x- mean(dse03f$x))*(dse03f$y-mean(dse03f$y)))
r = Sxy/sqrt(Sxx*Syy)
r
#confirm from problem 3
r_prob3 = sqrt(Rsqd)
r_prob3



