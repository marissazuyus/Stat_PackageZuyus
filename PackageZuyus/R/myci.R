#' This function is used to calculate a confidence interval, very similar
#' to the results you would get from using t.test(t)$conf.int.
#' The example used for this function is from Lab 11.
#'
#' @param t
#'
#' @return the confidence interval of t
#'
#'
#' @examples
#' set.seed(23);x = rnorm(30,mean=10,sd=12)
#' myci(x)
#'
#' @export
myci <- function(t) {
  n <- length(t) # n is the sample size
  se <- sd(t)/sqrt(n); # Find the standard error of the sample
  m <- mean(t); # Find the sample mean
  cv <- qt(0.975,df=n-1) # cv is a critical value for the t distribution. P( t > cv ) = 0.025 = P( t < -cv )
  c(m-cv*se,m+cv*se) # Return the 95% confidence interval
}
