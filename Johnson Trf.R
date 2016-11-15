
##      Johnson Transformation Using jtrans Package in R - Explanation and R Code

##      Johnson Distribution for Normality


# A brief about Johnson Transfromation:

# Johnson Transfromation is a technique used for the transformation of univariate non normal
# data into normality. Johnson family is a comprehensive distribution family that accomodates
# many kinds of non-normal data. A bunch of transfromations, which are SB, SL and SU, are 
# applied on the non normal data and gets back the transformed distribution. The p-values 
# of all the three transformed distributions are calculated using Normality Test which is 
# by default Shapiro-Wilk Test,or based on the user specified normality test 
# (Eg: Anderson-Darling). The final transfromation considered among SB, SL and SU will be 
# the one with higher p-value under the given normality test.



# Below codes helps in understanding how to use "jtrans" function in "jtrans" package.

# The usage, function parameters and output is explained in the code lines.
# Please note function name and package name are same. The above mentioned Three
# Transformations (SB, SL and SU) are incorporated in "jtrans" function. The output of 
# "jtrans" function will tells which transfromation is considered finally. 



library(jtrans)   # for Johnson transformation
library(car)      # for qqPlot
library(nortest)  # for Anderson Darling Test for Normality

# We are Generating a randon unifrom distribution data of 1000 numbers
datavalues <- runif(1000,1,5)


#Plotting and visualizing data
hist(datavalues, breaks = 20, col = "green")


# We are now checking whether the unifrom random numbers generated 
# are normally distributed or not, by using Shapiro-Wilk and Anderson-Darling Tests.
# Both tests are used for checking normality and based on the returned p-value we shall
# determine whether the data is normal or not
shapiro.test(datavalues)  # Shapiro-Wilk Test for Normality says that the Data is not normal.
                          # p-value is very low. So we reject null hypothesis which says Data is normal.
ad.test(datavalues)       # Anderson-Darling Test for Normality says that the Data is not normal.
                          # p-value is very low. So we reject null hypothesis which says Data is normal.


#We shall now plot Normal QQ Plot to confirm the above results
qqnorm(x)
qqline(x)
qqPlot(x)  # Since lots of data values fall apart from the line, the datavalues are not normal.




# Now we are Applying Transfromation on our data to make it normal

# Transforming data using the "jtrans" function to make it normal
j_x <- jtrans(datavales, test = "shapiro.test", exclude_original = TRUE, z_lim = c(0.25,1.25), z_length = 101)


# Arguments

# x                 - 	the non-normal numerical data, here datavalues.
# test	            -   the normality test used to select fits, defaults to shapiro.test
#                       we can use "ad.text", which is Anderson-Darling Test for Normality
# exclude_original	-   whether the original data should be excluded when comparing fits.
# z_lim	            -   two values vector defining the range of the z values, defaults to 0.25 to 1.25.
# z_length	        -   the length of the z vector, default to 101. The number of different fits 
#                       estimated in the algorithm. Set larger z.length value if you want extra precision.



# NOw let us check the values returned by "jtrans" function


j_x$original                  # gives the Original data which is 'datavalues' here
j_x$transformed               # gives the transfromed(normaly distributed) values
j_x$type                      # gives the type of transfromation finally considered among SB, SL and SU
j_x$test                      # gives the type of normality test performed, by default it is Shapiro-Wilk Test
j_x$eta                       # gives eta values used in function
j_x$gamma                     # gives gamma values used in function
j_x$lambda                    # gives lambda values used in function
j_x$epsilon                   # gives epsilon values used in function
j_x$z                         # gives selected z value among 101 values from 0.25 to 1.25.
j_x$p.value                   # gives the p-value of the finally selected transfromation, 
                              # which is the highest among all transformations applied



# Now let us manually check the transformed data for Normality
hist(j_x$transformed, breaks = 20, col = "blue")
qqnorm(j_x$transformed)
qqline(j_x$transformed)
qqPlot(j_x$transformed)       # almost all data points falls on the line

#  <<<<<<<<<<<<<<<<<<<< CODE ENDS HERE>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>  
  
  # Why "jtrans" Package when there are "Johnson" Package as well as "JohnsonDistribution" Package?
  
  # There are now two packages on CRAN can do Johnson normality transformations, Johnson by 
  # Edgar Santos Fernandez and JohnsonDistribution by A.I. McLeod and Leanna King. However, 
  # both of them have certain limitations to performing easy and correct normality 
  # transformation.Although Johnson package is also based on the algorithm by Chou, 
  # Youn Min; Polansky, A. M. M. R. L. (1998), it's a C style implementation and hasn't 
  # been vectorized, so it's hard to debug and it generally takes 10 times longer than jtrans. 
  # It implementes the sample quantile function in a non-standard way (different from the 
  # quantile function from stats package), which will lead to errors in the following 
  # calculations. JohnsonDistribution package is based on I. D. Hill (1976). It aims to provide 
  # Johnson curve distribution and estimation functions, so the design concept is slightly 
  # different from Johnson normality transformation.





# Arguments

# x                 - 	the non-normal numerical data, here datavalues.
# test	            -   the normality test used to select fits, defaults to shapiro.test
#                       we can use "ad.text", which is Anderson-Darling Test for Normality
# exclude_original	-   whether the original data should be excluded when comparing fits.
# z_lim	            -   two values vector defining the range of the z values, defaults to 0.25 to 1.25.
# z_length	        -   the length of the z vector, default to 101. The number of different fits 
#                       estimated in the algorithm. Set larger z.length value if you want extra precision.

# Details

# "jtrans" fits data to a set of distributions from Johnson family. A normality test is used to find 
# the best fit by choosing the fit with maximum p.value under that given test. It returns the 
# transformed data, the corresponding type of Johnson curve (SB, SL or SU) and parameter estimations.
# Since the default Shapiro-Wilk test can only accept sample size between 3 and 5000, 
# one should specify another normality test in the test parameter, generally the "ad.test" (Anderson-Darling
# Test) in the "nortest" package is recommended.
# Sometimes, this algorithm may return poor fits. The most extreme case is that all the transformed 
# data have smaller p.values than the original data's. In such cases, the "exclude_original" flag 
# should be set to "FALSE", so jtrans will return the original data as the transformed data.



# What Value the "jtrans" function returns us?

# A list with two classes: the first one is the type of transformation used, the same as the 
# type component, could be "sb", "su" or "sl"; The second one is "jtrans". The list contains the 
# following components:

# original	                    - original data.
# transformed	                  - transformed data.
# type	                        - type of transformation selected (SB, SL or SU).
# test	                        - normality test used to select transformations.
# z	                            - selected z value among 101 values from 0.25 to 1.25.
# eta, gamma, lambda, epsilon	  - transformation parameters.
# p.value	                      - the maximum p.value returned by test

