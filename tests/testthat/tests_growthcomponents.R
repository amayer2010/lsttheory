
model <- '
  eta1 =~ 1*y11 + la211*y21 + la311*y31
  eta2 =~ 1*y12 + la221*y22 + la321*y32
  eta3 =~ 1*y13 + la231*y23 + la331*y33

  y11 ~ 0*1
  y21 ~ la210*1
  y31 ~ la310*1

  y12 ~ 0*1
  y22 ~ la210*1
  y32 ~ la310*1

  y13 ~ 0*1
  y23 ~ la210*1
  y33 ~ la310*1

  pi0 =~ 1*eta1 + 1*eta2 + 1*eta3
  pi1 =~ 1*eta2
  pi2 =~ 1*eta3

  eta1 ~~ 0*eta1
  eta2 ~~ 0*eta2
  eta3 ~~ 0*eta3

  pi0 + pi1 + pi2 ~ 1
  eta1 + eta2 + eta3 ~ 0*1
'

m1.lav <- sem(model, data=d_multistate02)
# summary(m1.lav)


## Demo

# contrasts <- matrix(c(1,0,0, -1,1,0, -1,0,1), byrow=T, nrow=3)
# 
# m1 <- growthcomponents(3, d_multistate02, contrasts)
# summary(m1)
# cat(m1)



# library(semPlot)
# semPaths(m1)
# 
# semPaths(m1, style="lisrel", intercepts=F, 
#          layout="tree2", rotation=4, nCharNodes=4, nCharEdges=4,
#          optimizeLatRes=F, residScale=10)

