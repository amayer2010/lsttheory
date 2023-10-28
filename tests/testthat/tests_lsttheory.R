
library(lsttheory)


########### dataset: CTT model software appendix ###########

m1 <- lsttheory(1,0,data=d_taucong,
                equiv.assumption=list(tau="cong", theta="cong"))

actual_var_eta <- unlist(subset(parameterEstimates(m1@lavaanres), 
                         label=="psi1", select=est))
expected_var_eta <- 3.3798
names(expected_var_eta) <- "est"
expect_equal(actual_var_eta, 
             expected_var_eta, 
             tolerance=1e-4)

actual_rely <- m1@lsttheory$rely
expected_rely <- c(0.8753, 0.8701, 0.8947)
names(expected_rely) <- c("rely11", "rely21", "rely31")
expect_equal(actual_rely, 
             expected_rely, 
             tolerance=1e-4)

actual_spey <- unlist(m1@lsttheory$spey)
names(actual_spey) <- NULL
expect_equal(is.na(actual_spey[1]), TRUE) 


############## dataset: multistate model software appendix ###############

m1 <- lsttheory(2,0,data=d_multistate,
                equiv.assumption=list(tau="ess", theta="cong"))

actual_var_eta <- unlist(subset(parameterEstimates(m1@lavaanres), 
                                label=="psi1", select=est))
expected_var_eta <- 2.9972
names(expected_var_eta) <- "est"
expect_equal(actual_var_eta, 
             expected_var_eta, 
             tolerance=1e-4)

actual_rely <- m1@lsttheory$rely
expected_rely <- c(0.9055, 0.9325, 0.9031, 0.9400)
names(expected_rely) <- c("rely11", "rely21", "rely12", "rely22")
expect_equal(actual_rely, 
             expected_rely, 
             tolerance=1e-4)

actual_spey <- unlist(m1@lsttheory$spey)
names(actual_spey) <- NULL
expect_equal(is.na(actual_spey[1]), TRUE) 



######## dataset: multistate model (older version with 3 time points) ########

m1 <- lsttheory(3,0,data=d_multistate02,
                equiv.assumption=list(tau="ess", theta="cong"))

actual_var_eta <- unlist(subset(parameterEstimates(m1@lavaanres), 
                                label=="psi1", select=est))
expected_var_eta <- 2.7211 
names(expected_var_eta) <- "est"
expect_equal(actual_var_eta, 
             expected_var_eta, 
             tolerance=1e-4)

actual_rely <- m1@lsttheory$rely
expected_rely <- c(0.71893, 0.66426, 0.73879, 0.73458, 0.63738,
                   0.75516, 0.71474, 0.61858, 0.76556)
names(expected_rely) <- c("rely11", "rely21", "rely31", "rely12", "rely22",
                          "rely32", "rely13", "rely23", "rely33")
expect_equal(actual_rely, 
             expected_rely, 
             tolerance=1e-4)

actual_spey <- unlist(m1@lsttheory$spey)
names(actual_spey) <- NULL
expect_equal(is.na(actual_spey[7]), TRUE) 



################ dataset: multitrait-multistate model ###################

m1 <- lsttheory(4,2, d_multitraitmultistate)

actual_coefs <- c(m1@lsttheory$rely[[1]],
                  m1@lsttheory$spey[[2]],
                  m1@lsttheory$cony[[4]])
expected_coefs <- c(0.7949941, 0.2193545, 0.3946772)
expect_equal(actual_coefs, expected_coefs, tolerance=1e-5)



m1 <- lsttheory(4,0, d_multitraitmultistate)

actual_coefs <- c(m1@lsttheory$rely[[1]],
                  m1@lsttheory$spey[[2]],
                  m1@lsttheory$cony[[4]])
expected_coefs <- c(0.7946963, NA, NA)
expect_equal(actual_coefs, expected_coefs, tolerance=1e-5)


m1 <- lsttheory(1,0, d_multitraitmultistate, 
                equiv.assumption=list(tau="equi", theta="cong"), 
                scale.invariance=list(lait0=FALSE, lait1=TRUE, lat0=TRUE, lat1=TRUE))

actual_coefs <- c(m1@lsttheory$rely[[1]],
                  m1@lsttheory$spey[[2]],
                  m1@lsttheory$cony[[4]])
expected_coefs <- c(0.4011668, NA, NA)
expect_equal(actual_coefs, expected_coefs, tolerance=1e-5)


m1 <- lsttheory(4,2, d_multitraitmultistate, 
                equiv.assumption=list(tau="cong", theta="cong"), 
                scale.invariance=list(lait0=FALSE, lait1=FALSE, 
                                      lat0=FALSE, lat1=FALSE))

actual_coefs <- c(m1@lsttheory$rely[[1]],
                  m1@lsttheory$spey[[2]],
                  m1@lsttheory$cony[[4]])
expected_coefs <- c(0.7949941, 0.2193545, 0.3946772)
expect_equal(actual_coefs, expected_coefs, tolerance=1e-5)



m1 <- lsttheory(4, 2, d_multitraitmultistate, 
                equiv.assumption=list(tau="cong", theta="cong"), 
                scale.invariance=list(lait0=TRUE,lait1=TRUE,lat0=TRUE,lat1=TRUE))

actual_coefs <- c(m1@lsttheory$rely[[1]],
                  m1@lsttheory$spey[[2]],
                  m1@lsttheory$cony[[4]])
expected_coefs <- c(0.8073018, 0.2654833, 0.4340060)
expect_equal(actual_coefs, expected_coefs, tolerance=1e-5)



################# semPlot ##########################

# 
# library(semPlot)
# semPaths(m1@lavaanres, style="lisrel", intercepts=F, layout="tree2", rotation=4, nCharNodes=4, nCharEdges=4, optimizeLatRes=F, residScale=10)
# 
# semPaths(m5@lavaanres, style="lisrel", intercepts=F, layout="tree2", rotation=4, nCharNodes=4, nCharEdges=4, optimizeLatRes=F, residScale=10)
# 
