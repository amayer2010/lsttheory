
require(lsttheory)
require(testthat)

## run test_local() to test all

# test_file("tests/testthat/tests_growthcomponents.R")

test_file("tests/testthat/tests_lsttheory.R")
test_file("tests/testthat/tests_lsttheory_es.R")

## requires private files
d <- readRDS("private/d2conflict.rds") ## conflict scale FIML
test_file("tests/testthat/tests_lst_models_es.R") ## takes really long > 1h


