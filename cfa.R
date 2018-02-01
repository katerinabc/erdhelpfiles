#CFA
#install.packages("lavaan")
library(lavaan)
#help file: http://www.amarkos.gr/research/lavaan/
#For these reasons, using weighted least squares estimation (which is based on polychoric correlations)
#with mean and variance correction has been recommended for categorical, nonnormal data ( Finney and DiStefano, 2006 and Flora and Curran, 2004).
#(see  Mattson; 10.1016/j.aap.2012.02.009)

setwd("~/Dropbox/PhDDropbox/AE/validation")

#aeS1.rec4 check script of EFA

#########################
#E/CFA - version 1
########################

#select anchor item by looking at highest loading item with lowest cross loadings in efa

ThreeFacECFAv1<- '
de =~  NA*ae5  + ae2 + ae3 + ae4 + ae6 + ae7 + ae8 + ae12  + ae16
mc =~  NA*ae9  + ae2 + ae3 + ae4 + ae6 + ae7 + ae8 + ae12  + ae16
ic =~  NA*ae17 + ae2 + ae3 + ae4 + ae6 + ae7 + ae8 + ae12  + ae16
#fix variance of latent variables to unity/Factor variance fixed to 1
de ~~ 1* de
mc ~~ 1* mc
ic ~~ 1 * ic
'

ThreeFacECFA.FitWLSMVSv1<-sem(ThreeFacECFAv1, data=aeS1.rec4, estimator="WLSMVS")
summary(ThreeFacECFA.FitWLSMVSv1, fit.measures=TRUE, rsq=TRUE, modindices=TRUE)
library(semTools)
MI.testv1<-miPowerFit(ThreeFacECFA.FitWLSMVSv1)
MI.testv1[MI.testv1$decision == "M"]
#write.csv(MI.test, "miTest.csv")

miv1 <- modindices(ThreeFacECFA.FitWLSMVSv1)
miv1[miv1$mi > 1,]

TwoFac<- '
first =~  NA*ae16 + ae2 + ae3 + ae4 + ae7 + ae9 + ae10  + ae11 + ae13 + ae17
second =~ NA*ae5 +  ae2 + ae3 + ae4 + ae7 + ae9 + ae10  + ae11 + ae13 + ae17
#fix variance of latent variables to unity/Factor variance fixed to 1
first ~~ 1* first
second ~~ 1* second
'
TwoFacECFA.FitWLSMVSv1<-sem(TwoFac, data=ae_data.rec3, estimator="WLSMVS")
summary(TwoFacECFA.FitWLSMVSv1, fit.measures=TRUE, rsq=TRUE, modindices=TRUE)
#fit statistics lower than with 3 factors. 

#########################
#E/CFA - version 2
########################

#select anchor item by looking at highest loading item with lowest cross loadings in efa
ThreeFacECFAv2<- '
first =~  NA*ae3 + ae1 + ae2 + ae4 + ae5 + ae6 + ae7 + ae9 + ae10  + ae11 + ae12 + ae13 + ae16
second =~ NA*ae8 + ae1 + ae2 + ae4 + ae5 + ae6 + ae7 + ae9 + ae10  + ae11 + ae12 + ae13 + ae16
third =~   NA*ae17 + ae1 + ae2 + ae4 + ae5 + ae6 + ae7 + ae9 + ae10  + ae11 + ae12 + ae13 + ae16
#fix variance of latent variables to unity/Factor variance fixed to 1
first ~~ 1* first
second ~~ 1* second
third ~~ 1 * third
'

ThreeFacECFAv3<- '
first =~  NA*ae3 + ae1 + ae2 + ae4 + ae5 + ae6 + ae7 + ae9 + ae17  + ae11 + ae12 + ae13 + ae16
second =~ NA*ae8 + ae1 + ae2 + ae4 + ae5 + ae6 + ae7 + ae9 + ae17  + ae11 + ae12 + ae13 + ae16
third =~   NA*ae10 + ae1 + ae2 + ae4 + ae5 + ae6 + ae7 + ae9 + ae17  + ae11 + ae12 + ae13 + ae16
#fix variance of latent variables to unity/Factor variance fixed to 1
first ~~ 1* first
second ~~ 1* second
third ~~ 1 * third
'


#ThreeFacECFA.FitWLS<-sem(ThreeFacECFA, data=ae_data.rec2, estimator="WLS")
#ThreeFacECFA.FitWLSM<-sem(ThreeFacECFA, data=ae_data, estimator="WLSM")
ThreeFacECFA.FitWLSMVS<-sem(ThreeFacECFA, data=ae_data.rec2, estimator="WLSMVS", ordered=paste("ae", c(1:13,16:17)))
ThreeFacECFA.FitWLSMVSv2<-sem(ThreeFacECFAv2, data=ae_data.rec2, estimator="WLSMVS")
summary(ThreeFacECFA.FitWLSMVS, fit.measures=TRUE, rsq=TRUE, modindices=TRUE)
summary(ThreeFacECFA.FitWLSMVS, fit.measures=TRUE)
summary(ThreeFacECFA.FitWLSMVSv2, fit.measures=TRUE)
mi <- modindices(ThreeFacECFA.FitWLSMVS)
mi[mi$op == "=~",]
#install.packages("semTools")
library(semTools)
MI.test<-miPowerFit(ThreeFacECFA.FitWLSMVS)
write.csv(MI.test, "miTest.csv")
#summary(ThreeFacECFA.FitWLSM, fit.measures=TRUE, modindices=TRUE)
#summary(ThreeFacECFA.FitWLSMVS, fit.measures=TRUE)
#cut off value for modification indices:
#What estimator to use?
#install.packages("qgraph")
#install.packages("semPlot")
library(semPlot)
library(qgraph)
semPaths(ThreeFacECFA.FitWLSMVS, "est" )

#based on the results of the efa and e/cfa, the following model is tested in the cfa with the student sample
#first = item 1:6, 11:13
#second = item 7:9,13
#third = 10:13, 16:17

############
###E/CFA version 3: 2 Factor solution
############
TwoFacECFA<- '
ae =~ NA*ae1 + ae2 + ae3 + ae4  + ae10  + ae11 + ae12 + ae13 + ae17
re =~ NA*ae5 + ae2 + ae3 + ae4  + ae10  + ae11 + ae12 + ae13 + ae17
#fix variance of latent variables to unity/Factor variance fixed to 1
ae ~~ 1* ae
re ~~ 1* re
'
TwoFacECFA.FitWLSMVS<-sem(TwoFacECFA, data=aeS1.rec13, estimator="WLSMVS")
summary(TwoFacECFA.FitWLSMVS, fit.measures=TRUE, rsq=TRUE)#, modindices=TRUE)


