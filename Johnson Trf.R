
##      Johnson Distribution for Normality


# A brief about Johnson Transfromation:

# Johnson Transfromation is a technique used for the transfrmation of univariate non normal
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





# From jtrans.doc file

# Below text gives more information and underlying code in "jtrans" function 
# as well as the Fit functions for three Johnson Curves, SB, SL and SU seperately
# As mentioned above the Three Fit Functions are incorported in "jtrans" function.


# "jtrans" function R Code

function (x, test = "shapiro.test", exclude_original = TRUE, 
          z_lim = c(0.25, 1.25), z_length = 101) 
{
  x <- as.numeric(x)
  trans <- x
  type <- "original"
  params <- data.frame(eta = NA, gamma = NA, lambda = NA, epsilon = NA, 
                       z = NA, p.value = do.call(test, list(trans))$p.value)
  for (z in seq(from = z_lim[1], to = z_lim[2], length.out = z_length)) {
    q <- qtls(x, z)
    res <- fit_sl(x, q)
    if (!is.na(res[1])) {
      type <- c(type, "sl")
      params <- rbind(params, c(res$params, do.call(test, 
                                                    list(res$trans))$p.value))
      trans <- rbind(trans, res$trans)
    }
    if (q$QR <= 1) {
      res <- fit_sb(x, q)
      if (!is.na(res[1])) {
        type <- c(type, "sb")
        params <- rbind(params, c(res$params, do.call(test, 
                                                      list(res$trans))$p.value))
        trans <- rbind(trans, res$trans)
      }
    }
    else {
      res <- fit_su(x, q)
      if (!is.na(res[1])) {
        type <- c(type, "su")
        params <- rbind(params, c(res$params, do.call(test, 
                                                      list(res$trans))$p.value))
        trans <- rbind(trans, res$trans)
      }
    }
  }
  if (exclude_original) {
    type <- type[-1]
    trans <- trans[-1, ]
    params <- params[-1, ]
  }
  max <- which.max(params$p.value)
  RVAL <- structure(c(list(original = x, transformed = trans[max, 
                                                             ], type = type[max], test = test), params[max, ]), class = c(type[max], 
                                                                                                                          "jtrans"))
  return(RVAL)
}
<environment: namespace:jtrans>
  

# SL Transformation  
# "fit_sl(x,q)" R Code
# x the non-normal numerical data.
# q the quantiles and some statistics generated by quantiles, it must be the return value of qtls function.
  function (x, q) 
  {
    if (q$xu/q$xm <= 1) 
      return(NA)
    eta <- 2 * q$z/log(q$xu/q$xm)
    gamma <- eta * log((q$xu/q$xm - 1)/sqrt(q$xu * q$xm))
    epsilon <- 0.5 * (q$x2 + q$x3 - q$xm * (q$xu/q$xm + 1)/(q$xu/q$xm - 
                                                              1))
    if (is.nan(gamma) | is.nan(epsilon) | eta <= 0) 
      return(NA)
    if (all(x > epsilon)) {
      return(list(trans = gamma + eta * log(x - epsilon), params = c(eta, 
                                                                     gamma, NA, epsilon, q$z)))
    }
    else return(NA)
  }
<environment: namespace:jtrans>
  
  


# SB Transformation  
# "fit_sb(x,q)" R Code
# x the non-normal numerical data.
# q the quantiles and some statistics generated by quantiles, it must be the return value of qtls function.
  function (x, q) 
  {
    eta <- q$z/acosh(0.5 * sqrt((1 + q$xm/q$xu) * (1 + q$xm/q$xl)))
    gamma <- eta * asinh((q$xm/q$xl - q$xm/q$xu) * sqrt((1 + 
                                                           q$xm/q$xu) * (1 + q$xm/q$xl) - 4)/(2 * (q$xm^2/q$xl/q$xu - 
                                                                                                     1)))
    lambda <- (q$xm * sqrt(((1 + q$xm/q$xu) * (1 + q$xm/q$xl) - 
                              2)^2 - 4)/(q$xm^2/q$xl/q$xu - 1))
    epsilon <- 0.5 * (q$x2 + q$x3 - lambda + q$xm * (q$xm/q$xl - 
                                                       q$xm/q$xu)/(q$xm^2/q$xl/q$xu - 1))
    if (is.nan(gamma) | is.nan(epsilon) | eta <= 0 | lambda <= 
        0) 
      return(NA)
    if (all(x > epsilon) & all(x < epsilon + lambda)) {
      return(list(trans = gamma + eta * log((x - epsilon)/(lambda + 
                                                             epsilon - x)), params = c(eta, gamma, lambda, epsilon, 
                                                                                       q$z)))
    }
    else return(NA)
  }
<environment: namespace:jtrans>


  
# SU Transformation
# "fit_su(x,q)" R Code
# x the non-normal numerical data.
# q the quantiles and some statistics generated by quantiles, it must be the return value of qtls function.
  function (x, q) 
  {
    eta <- 2 * q$z/acosh(0.5 * (q$xu/q$xm + q$xl/q$xm))
    gamma <- eta * asinh((q$xl/q$xm - q$xu/q$xm)/(2 * sqrt(q$xu * 
                                                             q$xl/q$xm^2 - 1)))
    lambda <- (2 * q$xm * sqrt(q$xu * q$xl/q$xm^2 - 1)/(q$xu/q$xm + 
                                                          q$xl/q$xm - 2)/sqrt(q$xu/q$xm + q$xl/q$xm + 2))
    epsilon <- 0.5 * (q$x2 + q$x3 + q$xm * (q$xl/q$xm - q$xu/q$xm)/(q$xu/q$xm + 
                                                                      q$xl/q$xm - 2))
    if (is.nan(gamma) | is.nan(epsilon) | eta <= 0 | lambda <= 
        0) 
      return(NA)
    return(list(trans = gamma + eta * asinh((x - epsilon)/lambda), 
                params = c(eta, gamma, lambda, epsilon, q$z)))
  }
<environment: namespace:jtrans>



  
# Quantiles Function
#qtls(x,z)
# x the non-normal numerical data.
# z a single z value for model fitting. It's returned by jtrans
function (x, z) 
{
  qtls <- quantile(x, probs = pnorm(c(-3 * z, -z, z, 3 * z)))
  q <- list(xl = qtls[2] - qtls[1], xm = qtls[3] - qtls[2], 
            xu = qtls[4] - qtls[3], QR = (qtls[4] - qtls[3]) * (qtls[2] - 
                                                                  qtls[1])/(qtls[3] - qtls[2])^2, z = z, x2 = qtls[2], 
            x3 = qtls[3])
  return(q)
}
<environment: namespace:jtrans>
  
  
  
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

