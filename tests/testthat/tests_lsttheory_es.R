
library(lsttheory)


########### dataset: d_lst_es ###########

dsub <- d_lst_es[,1:9]

m1 <- lsttheory_es(model = 6, ntimepoints = 3, nperiods = 3, data = dsub)

# actual_coefs <- c(m1@lsttheory$rely[[1]],
#                   m1@lsttheory$spey[[2]],
#                   m1@lsttheory$cony[[4]])
# expected_coefs <- c(0.8073018, 0.2654833, 0.4340060)
# expect_equal(actual_coefs, expected_coefs, tolerance=1e-5)


