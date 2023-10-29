
require(lsttheory)

############ Simon's private data for testing ###################
## The following tests only work with private data from Simon Columbus
## more advanced tests of the lower level functions
## lst_models_es_common_trait and lst_models_es_indicator_specific_trait

d <- readRDS("private/d2conflict.rds") ## conflict scale FIML

############ models with common trait with d2conflict data ###################

mtest <- lsttheory:::lst_models_es_common_trait(
  ntimepoints=14, data=d[,1:28], ntraitperiods=1, nzetaperiods=2, nepsperiods=1, 
  la_s_equiv="period.invar", missing="fiml")

actual_coefs <- c(mtest@varcomp$rel[[1]],
                  mtest@varcomp$spe[[2]],
                  mtest@varcomp$con[[4]],
                  mtest@varcomp$timepoint[[7]])
expected_coefs <- c(0.4503012, 0.3137814, 0.1369079, 4.0000000)
expect_equal(actual_coefs, expected_coefs, tolerance=1e-5)


## model 1 (report Simon)
## multistate-singletrait
expect_warning(
lstm1 <- lsttheory:::lst_models_es_common_trait(
  ntimepoints=49, data=d, ntraitperiods=1, nzetaperiods=7, nepsperiods=7,
  la_t_equiv="one", la_s_equiv="zero", la_o_equiv="period.invar",
  vzeta_eqiv="period.invar", veps_equiv="period.invar", vtheta_equiv="free",
  nu_equiv="zero",mtheta_equiv="free",
  missing="fiml"
))

actual_coefs <- c(lstm1@varcomp$rel[[1]],
                  lstm1@varcomp$spe[[2]],
                  lstm1@varcomp$con[[4]],
                  lstm1@varcomp$timepoint[[7]])
expected_coefs <- c(0.4497156, 0.3352938, 0.1403387, 4.0000000)
expect_equal(actual_coefs, expected_coefs, tolerance=1e-5)



## model 2 (report Simon)
## with day-specific traits
expect_warning(
lstm2 <- lsttheory:::lst_models_es_common_trait(
  ntimepoints=49, data=d, ntraitperiods=7, nzetaperiods=7, nepsperiods=7,
  la_t_equiv="one", la_s_equiv="zero", la_o_equiv="period.invar",
  vzeta_eqiv="period.invar", veps_equiv="period.invar", vtheta_equiv="free",
  nu_equiv="zero",mtheta_equiv="free",
  missing="fiml"
))

actual_coefs <- c(lstm2@varcomp$rel[[1]],
                  lstm2@varcomp$spe[[2]],
                  lstm2@varcomp$con[[4]],
                  lstm2@varcomp$timepoint[[7]])
expected_coefs <- c(0.4349170, 0.3025055, 0.1681997, 4.0000000)
expect_equal(actual_coefs, expected_coefs, tolerance=1e-5)


## model 3 (report Simon)
## with day-specific traits and autoregressive effects
expect_warning(
lstm3 <- lsttheory:::lst_models_es_common_trait(
  ntimepoints=49, data=d, ntraitperiods=7, nzetaperiods=7, nepsperiods=7,
  la_t_equiv="one", la_s_equiv="time.invar", la_o_equiv="period.invar",
  vzeta_eqiv="period.invar", veps_equiv="period.invar", vtheta_equiv="free",
  nu_equiv="zero",mtheta_equiv="free",
  missing="fiml"
))

actual_coefs <- c(lstm3@varcomp$rel[[1]],
                  lstm3@varcomp$spe[[2]],
                  lstm3@varcomp$con[[4]],
                  lstm3@varcomp$timepoint[[7]])
expected_coefs <- c(0.4315117, 0.3128772, 0.1589177, 4.0000000)
expect_equal(actual_coefs, expected_coefs, tolerance=1e-5)



############ models with indicator-specific trait with d2conflict data ###################

## minimal specification (most restrictive model)
expect_warning(
m0restrictive <- lsttheory:::lst_models_es_indicator_specific_trait(
  ntimepoints=7, data=d[,1:14], missing="fiml"))

actual_coefs <- c(m0restrictive@varcomp$rel[[1]],
                  m0restrictive@varcomp$spe[[2]],
                  m0restrictive@varcomp$con[[4]],
                  m0restrictive@varcomp$timepoint[[7]])
expected_coefs <- c(0.5168847, 0.3171992, 0.1996855, 4.0000000)
expect_equal(actual_coefs, expected_coefs, tolerance=1e-5)


## model 1b (report Simon)
## model with indicator-specific traits
expect_warning(
lstm1b <- lsttheory:::lst_models_es_indicator_specific_trait(
  ntimepoints=49, data=d, ntraitperiods=1, nzetaperiods=7, nepsperiods=7,
  la_t_equiv="one", la_s_equiv="zero", la_o_equiv="period.invar",
  vzeta_eqiv="period.invar", veps_equiv="period.invar", vtheta_equiv="free",
  nu_equiv="zero", alpha_equiv="zero", mtheta_equiv="free",
  missing="fiml"
))

actual_coefs <- c(lstm1b@varcomp$rel[[1]],
                  lstm1b@varcomp$spe[[2]],
                  lstm1b@varcomp$con[[4]],
                  lstm1b@varcomp$timepoint[[7]])
expected_coefs <- c(0.5366751, 0.3364644, 0.1428363, 4.0000000)
expect_equal(actual_coefs, expected_coefs, tolerance=1e-5)



## model 4 (report Simon)
## with day-specific and indicator-specific traits and autoregressive effects
expect_warning(
lstm4 <- lsttheory:::lst_models_es_indicator_specific_trait(
  ntimepoints=49, data=d, ntraitperiods=7, nzetaperiods=7, nepsperiods=7,
  la_t_equiv="one", la_s_equiv="time.invar", la_o_equiv="period.invar",
  vzeta_eqiv="period.invar", veps_equiv="period.invar", vtheta_equiv="free",
  nu_equiv="zero", alpha_equiv="zero", mtheta_equiv="free",
  missing="fiml"
))

actual_coefs <- c(lstm4@varcomp$rel[[1]],
                  lstm4@varcomp$spe[[2]],
                  lstm4@varcomp$con[[4]],
                  lstm4@varcomp$timepoint[[7]])
expected_coefs <- c(0.5402726, 0.3299175, 0.1429542, 4.0000000)
expect_equal(actual_coefs, expected_coefs, tolerance=1e-5)


## model 5 (report Simon)
## with day-specific and indicator-specific traits, but no autoregressive effects
expect_warning(
lstm5 <- lsttheory:::lst_models_es_indicator_specific_trait(
  ntimepoints=49, data=d, ntraitperiods=7, nzetaperiods=7, nepsperiods=7,
  la_t_equiv="one", la_s_equiv="zero", la_o_equiv="period.invar",
  vzeta_eqiv="period.invar", veps_equiv="period.invar", vtheta_equiv="free",
  nu_equiv="zero", alpha_equiv="zero", mtheta_equiv="free",
  missing="fiml"
))

actual_coefs <- c(lstm5@varcomp$rel[[1]],
                  lstm5@varcomp$spe[[2]],
                  lstm5@varcomp$con[[4]],
                  lstm5@varcomp$timepoint[[7]])
expected_coefs <- c(0.5428293, 0.3206759, 0.1513573, 4.0000000)
expect_equal(actual_coefs, expected_coefs, tolerance=1e-5)


## likelihood ratio test
res.aov <- anova(lstm1@lavaanres,
                 lstm2@lavaanres,
                 lstm3@lavaanres,
                 lstm4@lavaanres,
                 lstm5@lavaanres)

actual_coefs <- c(res.aov$Df[[2]],
                  res.aov$AIC[[3]],
                  res.aov$Chisq[[4]],
                  res.aov$`Chisq diff`[[3]],
                  res.aov$RMSEA[[5]])
expected_coefs <- c(4809, 36170.94865, 17583.96015, 536.79557, 0.09163)
expect_equal(actual_coefs, expected_coefs, tolerance=1e-5)



## liberal specification (the least restrictive model)
expect_warning(
m6liberal <- lsttheory:::lst_models_es_indicator_specific_trait(
  ntimepoints=49, data=d, ntraitperiods=7, nzetaperiods=7, nepsperiods=7,
  la_t_equiv="free", la_s_equiv="free", la_o_equiv="free",
  vzeta_eqiv="free", veps_equiv="free", vtheta_equiv="free",
  nu_equiv="free", mtheta_equiv="free",
  missing="fiml"
))

expect_false(lavInspect(m6liberal@lavaanres, "converged"))





