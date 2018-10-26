library(testthat)
setwd("~/stat133/stat133-hws-fall17/hw04/code")
source('functions.R')

sink('../output/test-reporter.txt')
test_file('tests.R')
sink()
