#################################
###DATA PREP
#################################
#load aes2 via descriptives dataset. check age column to see if this is ok
######################################
###MODEL ESTIMATION
######################################

library(lavaan)
aeS2.v1<- '
de =~ ae2 + ae3 + ae4 + ae5
mc =~ ae6 + ae7 + ae8 + ae9
ic =~ ae12 + ae16 + ae17
'
aeS2Prof<-subset(ae, group == 1)
aeS2Stud<-subset(ae, group == 2)
summary(aeS2v1Prof.FitWLS<-cfa(aeS2.v1, data=ae, estimator="WLS"), fit.measures=TRUE)
summary(aeS2v1Prof.FitWLS<-cfa(aeS2.v1, data=aeS2Prof, estimator="WLS"), fit.measures=TRUE)
summary(aeS2v1Stud.FitWLS<-cfa(aeS2.v1, data=aeS2Stud, estimator="WLS"), fit.measures=TRUE)

summary(aeS2v1.FitWLS, fit.measures=TRUE, rsq=TRUE)
summary(aeS2v1.FitWLS, modindices=TRUE)

aeS2.v2<- '#CFA does nott allow for cross-loadings
de =~ ae2 + ae3 + ae4 + ae5 + ae6 + ae12
mc =~ ae6 + ae7 + ae8 + ae9
ic =~ ae12 + ae16 + ae17 + ae2 + ae3 + ae7
'
#aeS2v2.FitWLS<-cfa(aeS2.v2, data=aeS2, estimator="WLS")
#summary(aeS2v2.FitWLS, fit.measures=TRUE)#, rsq=TRUE, modindices=TRUE)

library(semTools)
MI.S2v1<-miPowerFit(aeS2v1.FitWLS)
MI.S2v2<-miPowerFit(aeS2v2.FitWLS)

aeS2.v3<- '#complete model
de =~ ae1 + ae2 + ae3 + ae4 + ae5
mc =~ ae6 + ae7 + ae8 + ae9 + ae10
ic =~ ae11 + ae12 + ae13 + ae15 + ae16 + ae17
'
aeS2v3.FitWLS<-cfa(aeS2.v3, data=aeS2, estimator="WLS")
summary(aeS2v3.FitWLS, fit.measures=TRUE, rsq=TRUE)

aeS2.v4<- '# collapse all in 1 factor see Farrel 2009; DOI: 10.1016/j.jbusres.2009.05.003
#http://www.sciencedirect.com/science/article/pii/S0148296309001453
ae =~ ae2 + ae3 + ae4 + ae5 + ae6 + ae7 + ae8 + ae9 + ae12 + ae16 + ae17
'
aeS2v4.FitWLS<-cfa(aeS2.v4, data=aeS2, estimator="WLS")
summary(aeS2v4.FitWLS, fit.measures=TRUE, rsq=TRUE)
#this model is even worse.  what about taking metacognitive skills out?
#argument: domain experitse questions include some aspects of metacognitive skills

aeS2.v5<- '
de =~ ae1 + ae2 + ae3 + ae4 + ae5  
ic =~ ae10 + ae11 + ae12 + ae13 + ae15 + ae16 + ae17
'
aeS2v5.FitWLS<-cfa(aeS2.v5, data=aeS2, estimator="WLS")
summary(aeS2v5.FitWLS, fit.measures=TRUE, rsq=TRUE)

aeS2.v6<- '
ae =~ ae2 + ae3 + ae4 + ae5  
re =~ ae1 + ae10 + ae11 + ae12 + ae13 + ae16 + ae17
'
aeS2v6.FitWLS<-cfa(aeS2.v6, data=aeS2, estimator="WLSMV")
summary(aeS2v6.FitWLS, fit.measures=TRUE, rsq=TRUE)


#MI.testStudv1.Misspec<-MI.testStudv1[MI.testStudv1$decision == c("M", "EPC:M")]
#misspecified row 7 (ic- ae2),44 (ae10-ae16),69 (ae12-ae2),111 (ae3-ae4)
#write.csv(MI.testStudv1, "MI.testStudv1.csv")
#even more heywood cases if item 2 taken out
#loadings above 1 => Heywood cases

#find the reason for Heywood cases:
#outliers?
#install.packages("faoutlier")
library(faoutlier)
#na: row 13, 135
ae_stud.postrecNoNA<-ae_stud.postrec[c(1:12,14:134,136:201),]
FSModel<-forward.search(ae_stud.postrecNoNA, studPost)
GCDModel<-gCD(ae_stud.postrecNoNA, studPost)
plot(FSModel)
plot(GCDModel)
#rerun cfa without outlier in row 10 of ae_stud.postrecNoNA
ae_stud.postrecNoNANoOut<-ae_stud.postrecNoNA[c(2:4, 6:12, 14, 16:23, 25:76, 78:107, 109:149, 151:166, 168:173, 175:191, 193:199),]
detach("package:faoutlier", unload=TRUE)
library(lavaan)
studpost.FitWLSnoOut<-cfa(studPost, data=ae_stud.postrec, estimator="WLSMVS")
summary(studpost.FitWLSnoOut, fit.measures=TRUE)
#removing outliers doesn't help to increase model fit (removed the largest 10 based on crook's distance)
#the model is also not underidentified and convergence was achieved.

studPostv2<- '
ic =~ ae10 + ae11 + ae12 + ae13 + ae16 + ae17
de =~ ae2  + ae3  + ae4  + ae5
mc =~ ae7  + ae8  + ae9
#heywood cases due to misspecification. fix covariance error
ae3 ~~ ae4
ae2 ~~ ae12
ae10 ~~ae16
'
studpost.FitWLSv2<-cfa(studPostv2, data=ae_stud.postrec, estimator="WLS")
summary(studpost.FitWLSv2, fit.measures=TRUE)

#test: maybe a 2 factor solution fit better sample 2
studPostv3<-'
first =~ ae1 + ae2 + ae3 + ae4 + ae5 + ae13
second  =~ ae7 + ae8 + ae9 + ae10 + ae11 + ae12 + ae16 + ae17
'
studpost.FitWLSv3<-cfa(studPostv3, data=ae_stud.postrec, estimator="WLS")
summary(studpost.FitWLSv3, fit.measures=TRUE)
rlibrary(semTools)
MI.testStudv3<-miPowerFit(studpost.FitWLSv3)
write.csv(MI.testStudv3, "miteststudv3.csv")


#fit latent loading to 1
studPostv4<- '
ic =~ ae10 + ae11 + ae12 + ae13 + ae16 + ae17
de =~ ae2  + ae3  + ae4  + ae5
mc =~ ae7  + ae8  + ae9
'
studpost.FitWLS4<-cfa(studPostv4, data=ae_stud.postrec, estimator="WLS", std.lv=TRUE, ordered=TRUE, warn=TRUE)
summary(studpost.FitWLS4, fit.measures=TRUE)#, rsq=TRUE, modindices=TRUE)
MI.testStudv4<-miPowerFit(studpost.FitWLS4)
write.csv(MI.testStudv4, "miteststudv4.csv")

studPostv5<- '
ic =~ ae10 + ae11 + ae13 + ae17
de =~ ae2  + ae3  + ae4  + ae5
mc =~ ae7  + ae8  + ae9
'
studpost.FitWLS5<-cfa(studPostv5, data=ae_stud.postrec, estimator="WLS", std.lv=TRUE, ordered=TRUE, warn=TRUE)
summary(studpost.FitWLS5, fit.measures=TRUE)#, rsq=TRUE, modindices=TRUE)


library(semTools)
MI.teststud<-miPowerFit(studpost.FitWLSMVS)


