
library(lsttheory)


names(multistate)

m1 <- lsttheory(2,1,data=multistate,
                equiv.assumption=list(tau="equi", xi="cong"))
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

