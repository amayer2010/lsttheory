
library(lsttheory)


########### dataset: CTT model software appendix ###########

# head(taucong)

m1 <- lsttheory(1,0,data=taucong,
                equiv.assumption=list(tau="cong", theta="cong"))
# m1
# summary(m1@lavaanres)


############## dataset: multistate model software appendix ###############

# head(multistate)

m1 <- lsttheory(2,0,data=multistate,
                equiv.assumption=list(tau="ess", theta="cong"))
# m1
# cat(m1@lavaansyntax)
# summary(m1@lavaanres)
# parTable(m1@lavaanres)



######## dataset: multistate model (older version with 3 time points) ########

m1 <- lsttheory(3,0,data=multistate02,
                equiv.assumption=list(tau="ess", theta="cong"))



################ dataset: multitrait-multistate model ###################

m2 <- lsttheory(4,2,multitraitmultistate)

# m2
# cat(m2@lavaansyntax)
# summary(m2@lavaanres)
# parTable(m2@lavaanres)
# m1@lstmodel


m3 <- lsttheory(4,0,multitraitmultistate)
# m3
# cat(m3@lavaansyntax)
# summary(m3@lavaanres)
# parTable(m3@lavaanres)


m4 <- lsttheory(1,0,multitraitmultistate, 
                equiv.assumption=list(tau="equi", theta="cong"), 
                scale.invariance=list(lait0=FALSE, lait1=TRUE, gat0=TRUE, gat1=TRUE))

# m4
# cat(m4@lavaansyntax)
# summary(m4@lavaanres)
# parTable(m4@lavaanres)


m5 <- lsttheory(4,2,multitraitmultistate, 
                equiv.assumption=list(tau="cong", theta="cong"), 
                scale.invariance=list(lait0=FALSE, lait1=FALSE, 
                                      gat0=FALSE, gat1=FALSE))
# m5
# cat(m5@lavaansyntax)
# summary(m5@lavaanres)
# parTable(m5@lavaanres)



m1 <- lsttheory(4, 2, multitraitmultistate, 
                equiv.assumption=list(tau="cong", theta="cong"), 
                scale.invariance=list(lait0=TRUE,lait1=TRUE,gat0=TRUE,gat1=TRUE))
# cat(m1@lavaansyntax)



################# semPlot ##########################

# 
# library(semPlot)
# semPaths(m1@lavaanres, style="lisrel", intercepts=F, layout="tree2", rotation=4, nCharNodes=4, nCharEdges=4, optimizeLatRes=F, residScale=10)
# 
# semPaths(m5@lavaanres, style="lisrel", intercepts=F, layout="tree2", rotation=4, nCharNodes=4, nCharEdges=4, optimizeLatRes=F, residScale=10)
# 
