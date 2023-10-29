
require(lsttheory)

########### dataset: d_lst_es ###########

dsub <- d_lst_es[,1:9]

## should not give error but does with lsttheory_0.3-1.002
expect_error(m1 <- lsttheory_es(model = 6, ntimepoints = 3, nperiods = 3, data = dsub))


# actual_coefs <- c(m1@lsttheory$rely[[1]],
#                   m1@lsttheory$spey[[2]],
#                   m1@lsttheory$cony[[4]])
# expected_coefs <- c(0.8073018, 0.2654833, 0.4340060)
# expect_equal(actual_coefs, expected_coefs, tolerance=1e-5)



############ simple models using lsttheory_es with d_lst_es ###################

m1 <- lst_models_es(traitmodel="singletrait", ntimepoints=9, data=d_lst_es, 
                    nperiods=3, ar=FALSE, equiv="invar")

actual_coefs <- c(m1@varcomp$rel[[1]],
                  m1@varcomp$spe[[2]],
                  m1@varcomp$con[[4]],
                  m1@varcomp$timepoint[[7]])
expected_coefs <- c(0.8772938, 0.6057496, 0.2715442, 3.0000000)
expect_equal(actual_coefs, expected_coefs, tolerance=1e-5)


m2 <- lst_models_es(traitmodel="day-specific", ntimepoints=9, data=d_lst_es, 
                    nperiods=3, ar=FALSE)

actual_coefs <- c(m2@varcomp$rel[[1]],
                  m2@varcomp$spe[[2]],
                  m2@varcomp$con[[4]],
                  m2@varcomp$timepoint[[7]])
expected_coefs <- c(0.8809884, 0.1932521, 0.6877363, 3.0000000)
expect_equal(actual_coefs, expected_coefs, tolerance=1e-5)


expect_warning(
  m3 <- lst_models_es(traitmodel="indicator-specific", ntimepoints=9, data=d_lst_es, 
                      nperiods=3, ar=FALSE)
)

actual_coefs <- c(m3@varcomp$rel[[1]],
                  m3@varcomp$spe[[2]],
                  m3@varcomp$con[[4]],
                  m3@varcomp$timepoint[[7]])
expected_coefs <- c(0.8773542, 0.6060529, 0.2724302, 3.000000)
expect_equal(actual_coefs, expected_coefs, tolerance=1e-5)


expect_warning(
  m4 <- lst_models_es(traitmodel="day-and-indicator-specific", ntimepoints=9, data=d_lst_es, 
                      nperiods=3, ar=FALSE)
)

actual_coefs <- c(m4@varcomp$rel[[1]],
                  m4@varcomp$spe[[2]],
                  m4@varcomp$con[[4]],
                  m4@varcomp$timepoint[[7]])
expected_coefs <- c(0.8810094, 0.1940352, 0.6884224, 3.0000000)
expect_equal(actual_coefs, expected_coefs, tolerance=1e-5)


