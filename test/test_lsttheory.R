
model <- '
eta1 =~ la111*y11 + la211*y21 + la311*y31
eta2 =~ la121*y12 + la221*y22 + la321*y32
eta3 =~ la131*y13 + la231*y23 + la331*y33

xi =~ 1*eta1 + 1*eta2 + 1*eta3

y11 ~ la110*1 + 0*1
y21 ~ la210*1
y31 ~ la310*1

eta1 ~~ vareta1*eta1

y21 ~~ eps21*y21
y11 ~~ eps11*y11

vary11 := 1^2 * vareta1 + eps11
vary21 := la211^2 * vareta1 + eps21
'

m1 <- sem(model, multistate)
summary(m1)
coef(m1, type="user")
inspect(m1,"r2")

test <- parTable(m1)

names(multistate)

m1 <- lsttheory(3,1,data=multistate)
m1
cat(m1@lavaansyntax)
summary(m1@lavaanres)
parTable(m1@lavaanres)

m1@lstmodel
test <- summary(m1@lavaanres)


m2 <- lsttheory(4,2,multitraitmultistate)
m2
cat(m2@lavaansyntax)
summary(m2@lavaanres)
parTable(m2@lavaanres)

m1@lstmodel


m3 <- lsttheory(4,0,multitraitmultistate)
m3
cat(m3@lavaansyntax)
summary(m3@lavaanres)
parTable(m3@lavaanres)


m4 <- lsttheory(1,0,multitraitmultistate, equiv.assumption=list(tau="equi", xi="cong"), scale.invariance=list(lait0=FALSE, lait1=TRUE, gat0=TRUE, gat1=TRUE))
m4
cat(m4@lavaansyntax)
summary(m4@lavaanres)
parTable(m4@lavaanres)


m5 <- lsttheory(4,2,multitraitmultistate, equiv.assumption=list(tau="cong", xi="cong"), scale.invariance=list(lait0=FALSE, lait1=FALSE, gat0=FALSE, gat1=FALSE))
m5
cat(m5@lavaansyntax)
summary(m5@lavaanres)
parTable(m5@lavaanres)



m1 <- lsttheory(4, 2, multitraitmultistate, equiv.assumption=list(tau="cong", xi="cong"), scale.invariance=list(lait0=TRUE,lait1=TRUE,gat0=TRUE,gat1=TRUE))
cat(m1@lavaansyntax)



library(semPlot)
semPaths(m1@lavaanres, style="lisrel", intercepts=F, layout="tree2", rotation=4, nCharNodes=4, nCharEdges=4, optimizeLatRes=F, residScale=10)

semPaths(m5@lavaanres, style="lisrel", intercepts=F, layout="tree2", rotation=4, nCharNodes=4, nCharEdges=4, optimizeLatRes=F, residScale=10)


########## test ###########


number <- list(manifest = 8,
               eta = 4,
               xi = 1,
               etaind = 2,
               xiind = 4
)

