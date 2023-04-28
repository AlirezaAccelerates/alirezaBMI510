### 1: Rando ###

#' Rando: A wrapper around sample for atomic vectors and data frame-like objects
#'
#' This function checks whether the input `x` is an atomic vector or a
#' data frame-like object (data frame, matrix, or list) and returns either
#' `n` samples or `n` rows accordingly. If the input does not match any of these
#' types, an error message will be shown.
#'
#' @param x An atomic vector or data frame-like object (data frame, matrix, or list).
#' @param n The number of samples or rows to return (default is 1).
#' @param replace Logical; should sampling be with replacement (default is TRUE).
#'
#' @return A vector or data frame-like object containing the sampled elements or rows.
#' @export
#'
#' @examples
#' vec = c(1, 2, 3, 4, 5)
#' df = data.frame(A = c(1, 2, 3), B = c(4, 5, 6))
#'
#' # Test the rando function with an atomic vector and a data frame
#' rando(vec, n = 3, replace = T)
#' rando(df, n = 2, replace = T)
rando = function(x, n = 1, replace = T) {

  if (is.atomic(x) && !is.matrix(x)) {

    return(sample(x, size = n, replace = replace))
  } else if (is.data.frame(x) || is.matrix(x) || is.list(x)) {

    row_indices = sample(1:nrow(x), size = n, replace = replace)
    return(x[row_indices, , drop = FALSE])
  } else {

    stop("Input must be an atomic vector or data frame-like object (data frame, matrix, or list).")
  }
}


### 2: Is_min ###

#' is_min: Check if elements of an atomic vector are equal to its minimum value.
#'
#' This function accepts an atomic vector `x` and returns a logical vector with
#' TRUE where `x` equals its minimum value.
#'
#' @param x An atomic vector.
#' @param na.rm Logical; should NA values be removed before calculating the minimum value (default is TRUE).
#'
#' @return A logical vector with TRUE where `x` equals its minimum value.
#' @export
#'
#' @examples
#' vec = c(1, 2, 3, 1, 5)
#' is_min(vec)
is_min = function(x, na.rm = T) {
  if (!is.atomic(x)) {
    stop("Input must be an atomic vector.")
  }
  min_val = min(x, na.rm = na.rm)
  return(x == min_val)
}



### 3: Is_max ###

#' is_max: Check if elements of an atomic vector are equal to its maximum value.
#'
#' This function accepts an atomic vector `x` and returns a logical vector with
#' TRUE where `x` equals its maximum value.
#'
#' @param x An atomic vector.
#' @param na.rm Logical; should NA values be removed before calculating the maximum value (default is TRUE).
#'
#' @return A logical vector with TRUE where `x` equals its maximum value.
#' @export
#'
#' @examples
#' vec = c(1, 2, 3, 1, 5)
#' is_max(vec)
is_max = function(x, na.rm = T) {
  if (!is.atomic(x)) {
    stop("Input must be an atomic vector.")
  }
  max_val = max(x, na.rm = na.rm)
  return(x == max_val)
}



### 4: Rep_mat ###

#' rep_mat: Replicate rows and columns of a data frame or matrix.
#'
#' This function is a port of the repmat.m function in MATLAB. It accepts a data frame or matrix `x` and
#' returns a new matrix created by replicating the rows M times and the columns N times.
#'
#' @param x A data frame or matrix.
#' @param M The number of times to replicate the rows (default is 1).
#' @param N The number of times to replicate the columns (default is 1).
#'
#' @return A matrix created by replicating the rows and columns of the input data frame or matrix.
#' @export
#'
#' @examples
#' mat = matrix(c(1, 2, 3, 4), nrow = 2, ncol = 2)
#' rep_mat(mat, M = 2, N = 3)
rep_mat = function(x, M = 1, N = 1) {
  if (!(is.data.frame(x) || is.matrix(x))) {
    stop("Input must be a data frame or matrix.")
  }

  x_matrix = as.matrix(x)
  rep_rows = unlist(replicate(M, x_matrix, simplify = FALSE), use.names = FALSE)
  rep_matrix = matrix(rep_rows, nrow = nrow(x_matrix) * M, ncol = ncol(x_matrix), byrow = TRUE)
  result = unlist(replicate(N, rep_matrix, simplify = FALSE), use.names = FALSE)
  return(matrix(result, nrow = nrow(rep_matrix), ncol = ncol(rep_matrix) * N, byrow = FALSE))
}



### 5: Classes ###

#' classes: Get classes of each variable in a tibble.
#'
#' This function returns a character vector containing the classes of each variable in a tibble `x`.
#' It is similar to the `names` function but returns the classes instead of the variable names.
#'
#' @param x A tibble or data frame.
#'
#' @return A character vector containing the classes of each variable in the input tibble or data frame.
#' @export
#'
#' @examples
#' library(tibble)
#' df = tibble(A = 1:3, B = c("a", "b", "c"), C = factor(c("X", "Y", "Z")))
#' classes(df)
classes = function(x) {
  if (!(is.data.frame(x) || is.tibble(x))) {
    stop("Input must be a data frame or tibble.")
  }
  
  return(sapply(x, class))
}



### 6: Df_scale ###

#' dF_scale: Scale numeric variables in a tibble.
#'
#' This function returns a tibble `x` in which the numeric variables have been
#' centered and/or scaled using the `scale` function. It retains the variable
#' attributes for more informative output.
#'
#' @param x A tibble or data frame.
#' @param center Logical; should the numeric variables be centered (default is TRUE).
#' @param scale Logical; should the numeric variables be scaled (default is TRUE).
#'
#' @return A tibble with the numeric variables centered and/or scaled.
#' @export
#'
#' @examples
#' library(tibble)
#' df = tibble(A = 1:3, B = c("a", "b", "c"), C = factor(c("X", "Y", "Z")), D = c(10, 20, 30))
#' df_scale(df)
df_scale = function(x, center = T, scale = T) {
  if (!(is.data.frame(x) || is.tibble(x))) {
    stop("Input must be a data frame or tibble.")
  }

  classes_x = classes(x)
  scaled_x = x

  for (i in seq_along(classes_x)) {
    class = classes_x[i]
    if (class == 'numeric' || class == 'integer') {
      scaled_x[[i]] = as.numeric(scale(scaled_x[[i]], center = center, scale = scale))
    }
  }

  return(scaled_x)
}



### 7: Log_likelihood_norm ###

#' Log-Likelihood of a sample under the normal distribution
#'
#' @param x A numeric vector of sample values.
#' @param mean The mean of the normal distribution.
#' @param sd The standard deviation of the normal distribution.
#'
#' @return The log-likelihood of the sample under the normal distribution.
#' @export
#'
#' @examples
#' x = rnorm(10, mean = 5, sd = 2)
#' log_likelihood_norm(x, mean = 5, sd = 2)
log_likelihood_norm = function(x, mean, sd) {
  log_likelihood = sum(dnorm(x, mean, sd, log = TRUE))
  return(log_likelihood)
}



### 8: Log_likelihood_uniform ###

#' Log-likelihood of a sample under the uniform distribution
#'
#' @param x A numeric vector of sample values.
#' @param min The minimum value of the uniform distribution.
#' @param max The maximum value of the uniform distribution.
#'
#' @return The log-likelihood of the sample under the uniform distribution.
#' @export
#'
#' @examples
#' x = runif(10, min = 0, max = 1)
#' log_likelihood_unif(x, min = 0, max = 1)
log_likelihood_unif = function(x, min, max) {
  log_likelihood = sum(dunif(x, min, max, log = TRUE))
  return(log_likelihood)
}



### 9: Log_likelihood_chisq ###

#' Log-likelihood of a sample under the chi-squared distribution
#'
#' @param x A numeric vector of sample values.
#' @param df The degrees of freedom of the chi-squared distribution.
#'
#' @return The log-likelihood of the sample under the chi-squared distribution.
#' @export
#'
#' @examples
#' x = rchisq(10, df = 5)
#' log_likelihood_chisq(x, df = 5)
log_likelihood_chisq = function(x, df) {
  log_likelihood = sum(dchisq(x, df, log = TRUE))
  return(log_likelihood)
}



### 10: Log_likelihood_f ###

#' Log-likelihood of a sample under the F distribution
#'
#' @param x A numeric vector of sample values.
#' @param df1 The first degrees of freedom parameter of the F distribution.
#' @param df2 The second degrees of freedom parameter of the F distribution.
#'
#' @return The log-likelihood of the sample under the F distribution.
#' @export
#'
#' @examples
#' x = rf(10, df1 = 5, df2 = 10)
#' log_likelihood_f(x, df1 = 5, df2 = 10)
log_likelihood_f = function(x, df1, df2) {
  log_likelihood = sum(df(x, df1, df2, log = TRUE))
  return(log_likelihood)
}



### 11: Log_likelihood_t ###

#' Log-likelihood of a sample under the T distribution
#'
#' @param x A numeric vector of sample values.
#' @param df The degrees of freedom of the t distribution.
#'
#' @return The log-likelihood of the sample under the t distribution.
#' @export
#'
#' @examples
#' x = rt(10, df = 5)
#' log_likelihood_t(x, df = 5)
log_likelihood_t = function(x, df) {
  log_likelihood = sum(dt(x, df, log = TRUE))
  return(log_likelihood)
}



### 12: Sensitivity ###

#' Calculate the sensitivity of a classifier.
#'
#' This function computes the sensitivity (also known as recall or true positive rate) of a classifier.
#' Sensitivity is the proportion of true positive predictions among the actual positive instances (# True Positives / # All Positives).
#'
#' @param pred A binary (logical) vector of predicted labels, where 1 (TRUE) represents a positive prediction and 0 (FALSE) represents a negative prediction.
#'
#' @param truth A binary (logical) vector of true labels, where 1 (TRUE) represents a positive instance and 0 (FALSE) represents a negative instance.
#'
#' @return The sensitivity of the classifier, as a numeric value between 0 and 1.
#' @examples
#' # Test the sensitivity function
#' pred = c(1, 0, 1, 1, 0)
#' truth = c(1, 0, 1, 0, 1)
#' sensitivity(pred, truth)
#'
#' @export
sensitivity = function(pred, truth) {

  if (length(pred) != length(truth)) {
    stop("Error: Prediction and truth vectors must have the same length.")
  }

  if (is.numeric(pred)) {
    pred = pred == 1
  }
  if (is.numeric(truth)) {
    truth = truth == 1
  }

  true_positives = sum(pred & truth)

  false_negatives = sum(!pred & truth)

  sensitivity = true_positives / (true_positives + false_negatives)

  return(sensitivity)
}



### 13: Specificity ###

#' Calculate the specificity of a classifier.
#'
#' This function computes the specificity (also known as true negative rate) of a classifier.
#' Specificity is the proportion of true negative predictions among the actual negative instances (# True Negatives / # All Negatives).
#'
#' @param pred A binary (logical) vector of predicted labels, where 1 (TRUE) represents a positive prediction and 0 (FALSE) represents a negative prediction.
#'
#' @param truth A binary (logical) vector of true labels, where 1 (TRUE) represents a positive instance and 0 (FALSE) represents a negative instance.
#'
#' @return The specificity of the classifier, as a numeric value between 0 and 1.
#' @examples
#' # Test the specificity function
#' pred = c(1, 0, 1, 1, 0)
#' truth = c(1, 0, 1, 0, 1)
#' specificity(pred, truth)
#'
#' @export
specificity = function(pred, truth) {

  if (length(pred) != length(truth)) {
    stop("Error: Prediction and truth vectors must have the same length.")
  }

  if (is.numeric(pred)) {
    pred = pred == 1
  }
  if (is.numeric(truth)) {
    truth = truth == 1
  }

  true_negatives = sum(!pred & !truth)

  false_positives = sum(pred & !truth)

  specificity = true_negatives / (true_negatives + false_positives)

  return(specificity)
}



### 14: Precision ###

#' Calculate the precision of a classifier.
#'
#' This function computes the precision (also known as positive predictive value) of a classifier.
#' Precision is the proportion of true positive predictions among all positive predictions made by the classifier (# True Positives / (# True Positives + # False Positives))..
#'
#' @param pred A binary (logical) vector of predicted labels, where 1 (TRUE) represents a positive prediction and 0 (FALSE) represents a negative prediction.
#'
#' @param truth A binary (logical) vector of true labels, where 1 (TRUE) represents a positive instance and 0 (FALSE) represents a negative instance.
#'
#' @return The precision of the classifier, as a numeric value between 0 and 1.
#' @examples
#' # Test the precision function
#' pred = c(1, 0, 1, 1, 0)
#' truth = c(1, 0, 1, 0, 1)
#' precision(pred, truth)
#'
#' @export
precision = function(pred, truth) {

  if (length(pred) != length(truth)) {
    stop("Error: Prediction and truth vectors must have the same length.")
  }

  if (is.numeric(pred)) {
    pred = pred == 1
  }
  if (is.numeric(truth)) {
    truth = truth == 1
  }

  true_positives = sum(pred & truth)

  false_positives = sum(pred & !truth)

  precision = true_positives / (true_positives + false_positives)

  return(precision)
}



### 15: Recall ###

#' Calculate the recall of a classifier
#'
#' This function computes the recall (also known as sensitivity or true positive rate) of a classifier.
#' Recall is the proportion of true positive predictions among the actual positive instances (# True Positives / # All Positives).
#'
#' @param pred A binary (logical) vector of predicted labels, where 1 (TRUE) represents a positive prediction and 0 (FALSE) represents a negative prediction.
#'
#' @param truth A binary vector of true labels, where 1 represents a positive instance and 0 represents a negative instance.
#'
#' @return The recall of the classifier, as a numeric value between 0 and 1.
#' @examples
#' # Test the recall function
#' pred = c(1, 0, 1, 1, 0)
#' truth = c(1, 0, 1, 0, 1)
#' recall(pred, truth)
#'
#' @export
recall = function(pred, truth) {

  if (length(pred) != length(truth)) {
    stop("Error: Prediction and truth vectors must have the same length.")
  }

  if (is.numeric(pred)) {
    pred = pred == 1
  }
  if (is.numeric(truth)) {
    truth = truth == 1
  }

  true_positives = sum(pred & truth)

  false_negatives = sum(!pred & truth)

  recall = true_positives / (true_positives + false_negatives)

  return(recall)
}



### 16: Accuracy ###

#' Calculate the accuracy of a classifier.
#'
#' This function computes the accuracy of a classifier.
#' Accuracy is the proportion of true positive and true negative predictions among all instances ((# True Positives + # True negatives) / All instances)).
#'
#' @param pred A binary (logical) vector of predicted labels, where 1 (True) represents a positive prediction and 0 (False) represents a negative prediction.
#'
#' @param truth A binary (logical) vector of true labels, where 1 (True) represents a positive instance and 0 (False) represents a negative instance.
#'
#' @return The accuracy of the classifier, as a numeric value between 0 and 1.
#' @examples
#' # Test the accuracy function
#' pred = c(1, 0, 1, 1, 0)
#' truth = c(1, 0, 1, 0, 1)
#' accuracy(pred, truth)
#'
#' @export
accuracy = function(pred, truth) {

  if (length(pred) != length(truth)) {
    stop("Error: Prediction and truth vectors must have the same length.")
  }

  if (is.numeric(pred)) {
    pred = pred == 1
  }
  if (is.numeric(truth)) {
    truth = truth == 1
  }

  true_positives = sum(pred & truth)
  true_negatives = sum(!pred & !truth)

  accuracy = (true_positives + true_negatives) / length(pred)

  return(accuracy)
}



### 17: F1 ###

#' Calculate the F1 score of a classifier.
#'
#' This function computes the F1 score (harmonic mean of precision and recall) of a classifier.
#' The F1 score is defined as 2 x (precision x recall) / (precision + recall).
#'
#' @param pred A binary (logical) vector of predicted labels, where 1 (TRUE) represents a positive prediction and 0 (FALSE) represents a negative prediction.
#'
#' @param truth A binary (logical) vector of true labels, where 1 (TRUE) represents a positive instance and 0 (FALSE) represents a negative instance.
#'
#' @return The F1 score of the classifier, as a numeric value between 0 and 1.
#'
#' @examples
#' # Test the F1 score function
#' pred = c(1, 0, 1, 1, 0)
#' truth = c(1, 0, 1, 0, 1)
#' f1(pred, truth)
#' @export
f1 = function(pred, truth) {

  if (length(pred) != length(truth)) {
    stop("Error: Prediction and truth vectors must have the same length.")
  }

  if (is.numeric(pred)) {
    pred = pred == 1
  }
  if (is.numeric(truth)) {
    truth = truth == 1
  }

  precision = sum(pred & truth) / (sum(pred & truth) + sum(pred & !truth))

  recall = sum(pred & truth) / (sum(pred & truth) + sum(!pred & truth))

  f1 = 2 * (precision * recall) / (precision + recall)

  return(f1)
}



### 18: Minimum_n_per_group ###

#' minimum_n_per_group: Minimum sample size per group for two-sample T-test.
#'
#' This function calculates the minimum sample size per group needed for a two-sample t-test,
#' given an expected Cohen's d effect size and desired statistical power.
#'
#' @param d The expected Cohen's d effect size.
#' @param power The desired statistical power (default is 0.8).
#'
#' @return The minimum sample size per group needed for a two-sample t-test.
#' @export
#'
#' @examples
#' d = 0.5
#' minimum_n_per_group(d)
minimum_n_per_group = function(d, power = 0.8) {
  result = power.t.test(n = NULL, d = d, power = power, type = "two.sample")

  return(ceiling(result$n))
}



### 19: R2 ###

#' r2: Calculate the r-squared statistic between predicted and ground truth continuous variables.
#'
#' This function calculates the R-squared statistic (coefficient of determination)
#' between the predicted values and the ground truth continuous variables.
#'
#' @param pred A numeric vector of predicted values.
#' @param truth A numeric vector of ground truth values.
#'
#' @return The R-squared statistic between the predicted and ground truth values.
#' @export
#'
#' @examples
#' pred = c(2, 4, 6, 8)
#' truth = c(2.2, 3.8, 6.1, 7.9)
#' r2(pred, truth)
r2 = function(pred, truth) {
  if (!is.numeric(pred) || !is.numeric(truth)) {
    stop("Both 'pred' and 'truth' must be numeric vectors.")
  }

  if (length(pred) != length(truth)) {
    stop("Both 'pred' and 'truth' must have the same length.")
  }

  # Calculate the R-squared statistic
  correlation = cor(pred, truth)
  r_squared = correlation^2

  return(r_squared)
}



### 20: AdjR2 ###

#' adjR2: Calculate the adjusted r-squared statistic between predicted and ground truth continuous variables.
#'
#' This function calculates the adjusted r-squared statistic between the predicted values and
#' the ground truth continuous variables, taking into account the number of model parameters.
#'
#' @param pred A numeric vector of predicted values.
#' @param truth A numeric vector of ground truth values.
#' @param n_p The number of model parameters, excluding the intercept.
#'
#' @return The adjusted R-squared statistic between the predicted and ground truth values.
#' @export
#'
#' @examples
#' pred = c(2, 4, 6, 8)
#' truth = c(2.2, 3.8, 6.1, 7.9)
#' n_p = 2
#' adjR2(pred, truth, n_p)
adj_R2 = function(pred, truth, n_p) {
  if (!is.numeric(pred) || !is.numeric(truth)) {
    stop("Both 'pred' and 'truth' must be numeric vectors.")
  }

  if (length(pred) != length(truth)) {
    stop("Both 'pred' and 'truth' must have the same length.")
  }

  if (!is.numeric(n_p) || length(n_p) != 1) {
    stop("The number of model parameters 'n_p' must be a single numeric value.")
  }

  # Calculate the R-squared statistic
  r_squared = r2(pred, truth)

  # Calculate the adjusted R-squared statistic
  n = length(pred)
  adj_r_squared = 1 - ((1 - r_squared) * (n - 1) / (n - n_p - 1))

  return(adj_r_squared)
}


