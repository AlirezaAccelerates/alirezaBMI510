# run with:
# testthat::test_file("bmi510-tests.R")

library(tidyverse)
library(testthat)

x_vec = 1:10
y_hat_vec = 3*x_vec + 4
y_vec = y_hat_vec + 0.2*rnorm(length(x_vec))

x_df = tibble(a = x_vec, b = x_vec + 3, c = factor(a))
x_mat = matrix(x_vec,nrow = 2)
set.seed(510)
x_pred = sample(c(T,F),100,replace=T)
x_truth = x_pred
x_truth[1:10] = !x_truth[1:10]

test_that("rando works", {
  expect_equal(nrow(rando(x_df,2)), 2)
  expect_equal(ncol(rando(x_df,2)), 3)
  expect_equal(length(rando(x_vec,4)), 4)
})

test_that("is_min/is_max works", {
  expect_equal(is_min(x_vec)[1], T)
  expect_equal(is_min(x_vec)[2], F)
  expect_equal(is_max(x_vec)[10], T)
  expect_equal(is_max(x_vec)[9], F)
})

test_that("rep_mat works",{
  replicated = rep_mat(x_mat,2,3)
  expect_equal(nrow(replicated), 4)
  expect_equal(ncol(replicated), 15)
})

test_that("rep_mat works",{
  replicated = rep_mat(x_mat,2,3)
  expect_equal(nrow(replicated), 4)
  expect_equal(ncol(replicated), 15)
})

test_that("classes works",{
  x_df_classes = classes(x_df)
  expect_equal(length(x_df_classes), 3)
  expect_equivalent(x_df_classes[3],"factor")
})

test_that("df_scale works",{
  x_df_scaled = df_scale(x_df)
  expect_equal(nrow(x_df_scaled), 10)
  expect_equal(sum(x_df_scaled$b), 0)
  expect_equal(sd(x_df_scaled$b), 1)
  expect_equal(sd(x_df_scaled$b), 1)
})

test_that("likelihood functions work",{
  expect_equal(log_likelihood_norm(x_vec,0,1),-201.6894, tolerance = 0.01)
  expect_equal(log_likelihood_unif(x_vec,0,11),-23.9789, tolerance = 0.01)
  expect_equal(log_likelihood_chisq(x_vec,12),-41.44169, tolerance = 0.01)
  expect_equal(log_likelihood_f(x_vec,2,12),-43.2251, tolerance = 0.01)
  expect_equal(log_likelihood_t(x_vec,12),-87.65763, tolerance = 0.01)
})

test_that("performance measure functions work",{
  expect_equal(sensitivity(x_pred,x_truth),0.92, tolerance = 0.01)
  expect_equal(specificity(x_pred,x_truth),0.88, tolerance = 0.01)
  expect_equal(precision(x_pred,x_truth),0.88, tolerance = 0.01)
  expect_equal(recall(x_pred,x_truth),0.92, tolerance = 0.01)
  expect_equal(accuracy(x_pred,x_truth),0.90, tolerance = 0.01)
  expect_equal(f1(x_pred,x_truth),0.90, tolerance = 0.01)
})

test_that("minimum n works",{
  expect_equal(minimum_n_per_group(1.2),12, tolerance = 0.01)
  expect_equal(minimum_n_per_group(1.2,0.9),16, tolerance = 0.01)
})

test_that("r2 functions work",{
  expect_equal(as.numeric(r2(y_hat_vec,y_vec)),0.9996, tolerance = 0.001)
  expect_equal(as.numeric(adj_R2(y_hat_vec,y_vec,4)),0.9993, tolerance = 0.001)
})


