#### Syntax for Meta-Analysis ####
#load data
load('es_dat.rda')
#packages
library(dplyr)
library(metafor)
#### Meta-Analyses of Patient Level Characteristics ####
### Sex ----
## Males are reference group
sexdat<-filter(es_dat,factor_cat==1&factor=="sex")
# Robotic vs Laparoscopic
print(sexrslp<-rma(yi=LORrslp,sei=seLORrslp,dat=sexdat))
print(robustsexrslp<-robust(sexrslp,cluster=sexdat$cluster_id,adjust=T))
robustsexrslpOR<-predict(robustsexrslp,transf = exp)
# Pubbias
sexrslp_egger<-rma(yi=LORrslp~seLORrslp,sei=seLORrslp,dat=sexdat)
print(sexrslp_egger<-robust(sexrslp_egger,cluster=sexdat$cluster_id,adjust=T))
sexrslp.tf<-trimfill(sexrslp)
sexrslp.tfOR<-predict(sexrslp.tf,transf = exp)
# Robotic vs Open
print(sexrsop<-rma(yi=LORrsop,sei=seLORrsop,dat=sexdat))
print(robustsexrsop<-robust(sexrsop,cluster=sexdat$cluster_id,adjust=T))
robustsexrsopOR<-predict(robustsexrsop,transf = exp)
# Pubbias
sexrsop_egger<-rma(yi=LORrsop~seLORrsop,sei=seLORrsop,dat=sexdat)
print(sexrsop_egger<-robust(sexrsop_egger,cluster=sexdat$cluster_id,adjust=T))
sexrsop.tf<-trimfill(sexrsop)
sexrsop.tfOR<-predict(sexrsop.tf,transf = exp)
# Robotic vs Non-Robotic (type unspecified)
# print(sexrsnrs<-rma(yi=LORrsnrs,sei=seLORrsnrs,dat=sexdat))
# Only 2 studies
## Save Findings to a Data Frame for Export
sexreslp<-data.frame(OR=robustsexrslpOR$pred,LLOR=robustsexrslpOR$ci.lb,ULOR=robustsexrslpOR$ci.ub,
                     k=sexrslp$k,clusters=robustsexrslp$n,tau2=sexrslp$tau2,I2=sexrslp$I2,
                     egger=sexrslp_egger$beta[2],eggerp=sexrslp_egger$pval[2],
                     tfside=sexrslp.tf$side,tfmissing=sexrslp.tf$k0,tfOR=sexrslp.tfOR$pred,
                     row.names = 'Female (Male)')
sexresop<-data.frame(OR=robustsexrsopOR$pred,LLOR=robustsexrsopOR$ci.lb,ULOR=robustsexrsopOR$ci.ub,
                     k=sexrsop$k,clusters=robustsexrsop$n,tau2=sexrsop$tau2,I2=sexrsop$I2,
                     egger=sexrsop_egger$beta[2],eggerp=sexrsop_egger$pval[2],
                     tfside=sexrsop.tf$side,tfmissing=sexrsop.tf$k0,tfOR=sexrsop.tfOR$pred,
                     row.names = 'Female (Male)')

### Age ----
## Older age is reference group
agedat<-filter(es_dat,factor_cat==1&factor=="age")
# Robotic vs Laparoscopic
print(agerslp<-rma(yi=LORrslp,sei=seLORrslp,dat=agedat))
print(robustagerslp<-robust(agerslp,cluster=agedat$cluster_id,adjust=T))
robustagerslpOR<-predict(robustagerslp,transf = exp)
# Pubbias
agerslp_egger<-rma(yi=LORrslp~seLORrslp,sei=seLORrslp,dat=agedat)
print(agerslp_egger<-robust(agerslp_egger,cluster=agedat$cluster_id,adjust=T))
agerslp.tf<-trimfill(agerslp)
agerslp.tfOR<-predict(agerslp.tf,transf = exp)
# Robotic vs Open
print(agersop<-rma(yi=LORrsop,sei=seLORrsop,dat=agedat))
print(robustagersop<-robust(agersop,cluster=agedat$cluster_id,adjust=T))
robustagersopOR<-predict(robustagersop,transf = exp)
# Pubbias
print(agersop_egger<-rma(yi=LORrsop~seLORrsop,sei=seLORrsop,dat=agedat))
#can't estimate robust eggers
agersop.tf<-trimfill(agersop)
agersop.tfOR<-predict(agersop.tf,transf = exp)
# Robotic vs Non-Robotic (type unspecified)
# print(agersnrs<-rma(yi=LORrsnrs,sei=seLORrsnrs,dat=agedat))
# Only 1 study
## Save Findings to a Data Frame for Export
agereslp<-data.frame(OR=robustagerslpOR$pred,LLOR=robustagerslpOR$ci.lb,ULOR=robustagerslpOR$ci.ub,
                     k=agerslp$k,clusters=robustagerslp$n,tau2=agerslp$tau2,I2=agerslp$I2,
                     egger=agerslp_egger$beta[2],eggerp=agerslp_egger$pval[2],
                     tfside=agerslp.tf$side,tfmissing=agerslp.tf$k0,tfOR=agerslp.tfOR$pred,
                     row.names = 'Younger Age (Older)')
ageresop<-data.frame(OR=robustagersopOR$pred,LLOR=robustagersopOR$ci.lb,ULOR=robustagersopOR$ci.ub,
                     k=agersop$k,clusters=robustagersop$n,tau2=agersop$tau2,I2=agersop$I2,
                     egger=agersop_egger$beta[2],eggerp=agersop_egger$pval[2],
                     tfside=agersop.tf$side,tfmissing=agersop.tf$k0,tfOR=agersop.tfOR$pred,
                     row.names = 'Younger Age (Older)')

### Race/Ethnicity ----
## White patients are the reference group
redat<-filter(es_dat,factor_cat==1&factor=="re"&ref=="white")
# Robotic vs Laparoscopic (White compared to "Non-White")
print(rerslp<-rma(yi=LORrslp,sei=seLORrslp,dat=redat))
print(robustrerslp<-robust(rerslp,cluster=redat$cluster_id,adjust=T))
robustrerslpOR<-predict(robustrerslp,transf = exp)
# Pubbias
rerslp_egger<-rma(yi=LORrslp~seLORrslp,sei=seLORrslp,dat=redat)
print(rrerslp_egger<-robust(rerslp_egger,cluster=redat$cluster_id,adjust=T))
rerslp.tf<-trimfill(rerslp)
rerslp.tfOR<-predict(rerslp.tf,transf=exp)
# OR by racial/ethnic sample
# Black patients
print(blkrslp<-rma(yi=LORrslp,sei=seLORrslp,dat=redat[which(redat$comp=='black'),]))
print(rblkrslp<-robust(blkrslp,cluster = redat$cluster_id[which(redat$comp=='black')],adjust=T))
rblkrslpOR<-predict(rblkrslp,transf = exp)
# Pubbias
blkrslp_egger<-rma(yi=LORrslp~seLORrslp,sei=seLORrslp,dat=redat[which(redat$comp=='black'),])
print(blkrslp_egger<-robust(blkrslp_egger,cluster=redat$cluster_id[which(redat$comp=='black')],adjust=T))
blkrslp.tf<-trimfill(blkrslp)
blkrslp.tfOR<-predict(blkrslp.tf,transf=exp)
# "Other" patients
print(othrslp<-rma(yi=LORrslp,sei=seLORrslp,dat=redat[which(redat$comp=='other'),]))
print(rothrslp<-robust(othrslp,cluster = redat$cluster_id[which(redat$comp=='other')],adjust=T))
rothrslpOR<-predict(rothrslp,transf = exp)
# Pubbias
othrslp_egger<-rma(yi=LORrslp~seLORrslp,sei=seLORrslp,dat=redat[which(redat$comp=='other'),])
print(othrslp_egger<-robust(othrslp_egger,cluster=redat$cluster_id[which(redat$comp=='other')],adjust=T))
othrslp.tf<-trimfill(othrslp)
othrslp.tfOR<-predict(othrslp.tf,transf=exp)
# Hispanic/Latinx patients
print(hisprslp<-rma(yi=LORrslp,sei=seLORrslp,dat=redat[which(redat$comp=='hisp'),]))
print(rhisprslp<-robust(hisprslp,cluster = redat$cluster_id[which(redat$comp=='hisp')],adjust=T))
rhisprslpOR<-predict(rhisprslp,transf = exp)
# Pubbias
hisprslp_egger<-rma(yi=LORrslp~seLORrslp,sei=seLORrslp,dat=redat[which(redat$comp=='hisp'),])
print(hisprslp_egger<-robust(hisprslp_egger,cluster=redat$cluster_id[which(redat$comp=='hisp')],adjust=T))
hisprslp.tf<-trimfill(hisprslp)
hisprslp.tfOR<-predict(rhisprslp,transf = exp)
# Asian/Pacfic Islander patients
print(asianrslp<-rma(yi=LORrslp,sei=seLORrslp,dat=redat[which(redat$comp=='asian'),]))
print(rasianrslp<-robust(asianrslp,cluster = redat$cluster_id[which(redat$comp=='asian')],adjust=T))
rasianrslpOR<-predict(rasianrslp,transf = exp)
# Pubbias
asianrslp_egger<-rma(yi=LORrslp~seLORrslp,sei=seLORrslp,dat=redat[which(redat$comp=='asian'),])
print(asianrslp_egger<-robust(asianrslp_egger,cluster=redat$cluster_id[which(redat$comp=='asian')],adjust=T))
asianrslp.tf<-trimfill(asianrslp)
asianrslp.tfOR<-predict(asianrslp.tf,transf = exp)
# AI-AN had only 2 studies
# Robotic vs Open
print(rersop<-rma(yi=LORrsop,sei=seLORrsop,dat=redat))
print(robustrersop<-robust(rersop,cluster=redat$cluster_id,adjust=T))
robustrersopOR<-predict(robustrersop,transf = exp)
# Pubbias
rersop_egger<-rma(yi=LORrsop~seLORrsop,sei=seLORrsop,dat=redat)
print(rersop_egger<-robust(rersop_egger,cluster=redat$cluster_id,adjust=T))
rersop.tf<-trimfill(rersop)
rersop.tfOR<-predict(rersop.tf,transf = exp)
# OR by racial/ethnic sample
# Black patients
print(blkrsop<-rma(yi=LORrsop,sei=seLORrsop,dat=redat[which(redat$comp=='black'),]))
print(rblkrsop<-robust(blkrsop,cluster = redat$cluster_id[which(redat$comp=='black')],adjust=T))
rblkrsopOR<-predict(rblkrsop,transf = exp)
# Pubbias
blkrsop_egger<-rma(yi=LORrsop~seLORrsop,sei=seLORrsop,dat=redat[which(redat$comp=='black'),])
print(blkrsop_egger<-robust(blkrsop_egger,cluster=redat$cluster_id[which(redat$comp=='black')],adjust=T))
blkrsop.tf<-trimfill(blkrsop)
blkrsop.tfOR<-predict(blkrsop.tf,transf = exp)
# "Other" patients
print(othrsop<-rma(yi=LORrsop,sei=seLORrsop,dat=redat[which(redat$comp=='other'),]))
print(rothrsop<-robust(othrsop,cluster = redat$cluster_id[which(redat$comp=='other')],adjust=T))
rothrsopOR<-predict(rothrsop,transf = exp)
# Pubbias
othrsop_egger<-rma(yi=LORrsop~seLORrsop,sei=seLORrsop,dat=redat[which(redat$comp=='other'),])
print(othrsop_egger<-robust(othrsop_egger,cluster=redat$cluster_id[which(redat$comp=='other')],adjust=T))
othrsop.tf<-trimfill(othrsop)
othrsop.tfOR<-predict(othrsop.tf,transf = exp)
# Other racial/ethnic samples had fewer than 5 studies
# Robotic vs Non-Robotic (type unspecified)
print(rersnrs<-rma(yi=LORrsnrs,sei=seLORrsnrs,dat=redat))
print(robrersnrs<-robust(rersnrs,cluster = redat$cluster_id,adjust=T))
robrersnrsOR<-predict(rersnrs,transf = exp)
# Pubbias
rersnrs_egger<-rma(yi=LORrsnrs~seLORrsnrs,sei=seLORrsnrs,dat=redat)
# can't estimate robust eggers
rersnrs.tf<-trimfill(rersnrs)
rersnrs.tfOR<-predict(rersnrs.tf,transf = exp)
## Non-Hispanic patients are the reference group
hispdat<-filter(es_dat,factor_cat==1&factor=="re"&ref=="nonhisp")
# Only 4 studies
## Save Findings to a Data Frame for Export
rereslp<-data.frame(OR=robustrerslpOR$pred,LLOR=robustrerslpOR$ci.lb,ULOR=robustrerslpOR$ci.ub,
                     k=rerslp$k,clusters=robustrerslp$n,tau2=rerslp$tau2,I2=rerslp$I2,
                     egger=rerslp_egger$beta[2],eggerp=rerslp_egger$pval[2],
                     tfside=rerslp.tf$side,tfmissing=rerslp.tf$k0,tfOR=rerslp.tfOR$pred,
                     row.names = 'Non-White (White)')
blkreslp<-data.frame(OR=rblkrslpOR$pred,LLOR=rblkrslpOR$ci.lb,ULOR=rblkrslpOR$ci.ub,
                     k=blkrslp$k,clusters=rblkrslp$n,tau2=blkrslp$tau2,I2=blkrslp$I2,
                     egger=blkrslp_egger$beta[2],eggerp=blkrslp_egger$pval[2],
                     tfside=blkrslp.tf$side,tfmissing=blkrslp.tf$k0,tfOR=blkrslp.tfOR$pred,
                     row.names = 'Black (White)')
othreslp<-data.frame(OR=rothrslpOR$pred,LLOR=rothrslpOR$ci.lb,ULOR=rothrslpOR$ci.ub,
                     k=othrslp$k,clusters=rothrslp$n,tau2=othrslp$tau2,I2=othrslp$I2,
                     egger=othrslp_egger$beta[2],eggerp=othrslp_egger$pval[2],
                     tfside=othrslp.tf$side,tfmissing=othrslp.tf$k0,tfOR=othrslp.tfOR$pred,
                     row.names = 'Other (White)')
hispreslp<-data.frame(OR=rhisprslpOR$pred,LLOR=rhisprslpOR$ci.lb,ULOR=rhisprslpOR$ci.ub,
                     k=hisprslp$k,clusters=rhisprslp$n,tau2=hisprslp$tau2,I2=hisprslp$I2,
                     egger=hisprslp_egger$beta[2],eggerp=hisprslp_egger$pval[2],
                     tfside=hisprslp.tf$side,tfmissing=hisprslp.tf$k0,tfOR=hisprslp.tfOR$pred,
                     row.names = 'Hispanic (White)')
asianreslp<-data.frame(OR=rasianrslpOR$pred,LLOR=rasianrslpOR$ci.lb,ULOR=rasianrslpOR$ci.ub,
                     k=asianrslp$k,clusters=rasianrslp$n,tau2=asianrslp$tau2,I2=asianrslp$I2,
                     egger=asianrslp_egger$beta[2],eggerp=asianrslp_egger$pval[2],
                     tfside=asianrslp.tf$side,tfmissing=asianrslp.tf$k0,tfOR=asianrslp.tfOR$pred,
                     row.names = 'Asian/Pacific Islander (White)')
reresop<-data.frame(OR=robustrersopOR$pred,LLOR=robustrersopOR$ci.lb,ULOR=robustrersopOR$ci.ub,
                     k=rersop$k,clusters=robustrersop$n,tau2=rersop$tau2,I2=rersop$I2,
                     egger=rersop_egger$beta[2],eggerp=rersop_egger$pval[2],
                     tfside=rersop.tf$side,tfmissing=rersop.tf$k0,tfOR=rersop.tfOR$pred,
                     row.names = 'Non-White (White)')
blkresop<-data.frame(OR=rblkrsopOR$pred,LLOR=rblkrsopOR$ci.lb,ULOR=rblkrsopOR$ci.ub,
                     k=blkrsop$k,clusters=rblkrsop$n,tau2=blkrsop$tau2,I2=blkrsop$I2,
                     egger=blkrsop_egger$beta[2],eggerp=blkrsop_egger$pval[2],
                     tfside=blkrsop.tf$side,tfmissing=blkrsop.tf$k0,tfOR=blkrsop.tfOR$pred,
                     row.names = 'Black (White)')
othresop<-data.frame(OR=rothrsopOR$pred,LLOR=rothrsopOR$ci.lb,ULOR=rothrsopOR$ci.ub,
                     k=othrsop$k,clusters=rothrsop$n,tau2=othrsop$tau2,I2=othrsop$I2,
                     egger=othrsop_egger$beta[2],eggerp=othrsop_egger$pval[2],
                     tfside=othrsop.tf$side,tfmissing=othrsop.tf$k0,tfOR=othrsop.tfOR$pred,
                     row.names = 'Other (White)')
reresnrs<-data.frame(OR=robrersnrsOR$pred,LLOR=robrersnrsOR$ci.lb,ULOR=robrersnrsOR$ci.ub,
                     k=rersnrs$k,clusters=robrersnrs$n,tau2=rersnrs$tau2,I2=rersnrs$I2,
                     egger=rersnrs_egger$beta[2],eggerp=rersnrs_egger$pval[2],
                     tfside=rersnrs.tf$side,tfmissing=rersnrs.tf$k0,tfOR=rersnrs.tfOR$pred,
                     row.names = 'Non-White (White)')

### Income ----
## Higher income is reference group
incdat<-filter(es_dat,factor_cat==1&factor=="inc")
# Robotic vs Laparoscopic
print(incrslp<-rma(yi=LORrslp,sei=seLORrslp,dat=incdat))
print(robustincrslp<-robust(incrslp,cluster=incdat$cluster_id,adjust=T))
robustincrslpOR<-predict(robustincrslp,transf = exp)
# Pubbias
incrslp_egger<-rma(yi=LORrslp~seLORrslp,sei=seLORrslp,dat=incdat)
print(incrslp_egger<-robust(incrslp_egger,cluster=incdat$cluster_id,adjust=T))
incrslp.tf<-trimfill(incrslp)
incrslp.tfOR<-predict(incrslp.tf,transf=exp)
# Robotic vs Open
print(incrsop<-rma(yi=LORrsop,sei=seLORrsop,dat=incdat))
print(robustincrsop<-robust(incrsop,cluster=incdat$cluster_id,adjust=T))
robustincrsopOR<-predict(robustincrsop,transf = exp)
# Pubbias
incrsop_egger<-rma(yi=LORrsop~seLORrsop,sei=seLORrsop,dat=incdat)
print(incrsop_egger<-robust(incrsop_egger,cluster=incdat$cluster_id,adjust=T))
incrsop.tf<-trimfill(incrsop)
incrsop.tfOR<-predict(incrsop.tf,transf=exp)
# Robotic vs Non-Robotic (type unspecified)
#print(incrsnrs<-rma(yi=LORrsnrs,sei=seLORrsnrs,dat=incdat))
# Only 3 studies
increslp<-data.frame(OR=robustincrslpOR$pred,LLOR=robustincrslpOR$ci.lb,ULOR=robustincrslpOR$ci.ub,
                     k=incrslp$k,clusters=robustincrslp$n,tau2=incrslp$tau2,I2=incrslp$I2,
                     egger=incrslp_egger$beta[2],eggerp=incrslp_egger$pval[2],
                     tfside=incrslp.tf$side,tfmissing=incrslp.tf$k0,tfOR=incrslp.tfOR$pred,
                     row.names = 'Lower Income (Higher)')
incresop<-data.frame(OR=robustincrsopOR$pred,LLOR=robustincrsopOR$ci.lb,ULOR=robustincrsopOR$ci.ub,
                     k=incrsop$k,clusters=robustincrsop$n,tau2=incrsop$tau2,I2=incrsop$I2,
                     egger=incrsop_egger$beta[2],eggerp=incrsop_egger$pval[2],
                     tfside=incrsop.tf$side,tfmissing=incrsop.tf$k0,tfOR=incrsop.tfOR$pred,
                     row.names = 'Lower Income (Higher)')

### Education (HS Degree) ----
## Larger percentage of high school degree is reference group
edudat<-filter(es_dat,factor_cat==1&factor=="edu")
# Only 4 Studies

### Insurance ----
## Private/commercial is reference group
insdat<-filter(es_dat,factor_cat==1&factor=="ins")
# Robotic vs Laparoscopic
print(insrslp<-rma(yi=LORrslp,sei=seLORrslp,dat=insdat))
print(robustinsrslp<-robust(insrslp,cluster=insdat$cluster_id,adjust=T))
robustinsrslpOR<-predict(robustinsrslp,transf = exp)
# Pubbias
insrslp_egger<-rma(yi=LORrslp~seLORrslp,sei=seLORrslp,dat=insdat)
print(insrslp_egger<-robust(insrslp_egger,cluster=insdat$cluster_id,adjust=T))
insrslp.tf<-trimfill(insrslp)
insrslp.tfOR<-predict(insrslp.tf,transf=exp)
# OR by insurance type
# There were different ways of categorizing insurance, below is focused on Medicare, Medicaid, Uninsured
# Medicare
print(medicarerslp<-rma(yi=LORrslp,sei=seLORrslp,dat=insdat[which(insdat$comp=='medicare'),]))
print(rmedicarerslp<-robust(medicarerslp,cluster = insdat$cluster_id[which(insdat$comp=='medicare')],adjust=T))
rmedicarerslpOR<-predict(rmedicarerslp,transf = exp)
# Pubbias
medicarerslp_egger<-rma(yi=LORrslp~seLORrslp,sei=seLORrslp,dat=insdat[which(insdat$comp=='medicare'),])
print(medicarerslp_egger<-robust(medicarerslp_egger,cluster = insdat$cluster_id[which(insdat$comp=='medicare')],adjust=T))
medicarerslp.tf<-trimfill(medicarerslp)
medicarerslp.tfOR<-predict(medicarerslp.tf,transf=exp)
# Medicaid
print(medicaidrslp<-rma(yi=LORrslp,sei=seLORrslp,dat=insdat[which(insdat$comp=='medicaid'),]))
print(rmedicaidrslp<-robust(medicaidrslp,cluster = insdat$cluster_id[which(insdat$comp=='medicaid')],adjust=T))
rmedicaidrslpOR<-predict(rmedicaidrslp,transf = exp)
# Pubbias
medicaidrslp_egger<-rma(yi=LORrslp~seLORrslp,sei=seLORrslp,dat=insdat[which(insdat$comp=='medicaid'),])
print(medicaidrslp_egger<-robust(medicaidrslp_egger,cluster = insdat$cluster_id[which(insdat$comp=='medicaid')],adjust=T))
medicaidrslp.tf<-trimfill(medicaidrslp)
medicaidrslp.tfOR<-predict(medicaidrslp.tf,transf=exp)
# Uninsured
print(uninsuredrslp<-rma(yi=LORrslp,sei=seLORrslp,dat=insdat[which(insdat$comp=='uninsured'),]))
print(runinsuredrslp<-robust(uninsuredrslp,cluster = insdat$cluster_id[which(insdat$comp=='uninsured')],adjust=T))
runinsuredrslpOR<-predict(runinsuredrslp,transf = exp)
# Pubbias
uninsuredrslp_egger<-rma(yi=LORrslp~seLORrslp,sei=seLORrslp,dat=insdat[which(insdat$comp=='uninsured'),])
print(uninsuredrslp_egger<-robust(uninsuredrslp_egger,cluster = insdat$cluster_id[which(insdat$comp=='uninsured')],adjust=T))
uninsuredrslp.tf<-trimfill(uninsuredrslp)
uninsuredrslp.tfOR<-predict(uninsuredrslp.tf,transf=exp)
# Robotic vs Open
print(insrsop<-rma(yi=LORrsop,sei=seLORrsop,dat=insdat))
print(robustinsrsop<-robust(insrsop,cluster=insdat$cluster_id,adjust=T))
robustinsrsopOR<-predict(robustinsrsop,transf = exp)
# Pubbias
insrsop_egger<-rma(yi=LORrsop~seLORrsop,sei=seLORrsop,dat=insdat)
print(insrsop_egger<-robust(insrsop_egger,cluster=insdat$cluster_id,adjust=T))
insrsop.tf<-trimfill(insrsop)
insrsop.tfOR<-predict(insrsop.tf,transf=exp)
# OR by insurance type
# There were different ways of categorizing insurance, below is focused on Medicare, Medicaid, Uninsured
# Medicare
print(medicarersop<-rma(yi=LORrsop,sei=seLORrsop,dat=insdat[which(insdat$comp=='medicare'),]))
print(rmedicarersop<-robust(medicarersop,cluster = insdat$cluster_id[which(insdat$comp=='medicare')],adjust=T))
rmedicarersopOR<-predict(rmedicarersop,transf = exp)
# Pubbias
medicarersop_egger<-rma(yi=LORrsop~seLORrsop,sei=seLORrsop,dat=insdat[which(insdat$comp=='medicare'),])
print(medicarersop_egger<-robust(medicarersop_egger,cluster = insdat$cluster_id[which(insdat$comp=='medicare')],adjust=T))
medicarersop.tf<-trimfill(medicarersop)
medicarersop.tfOR<-predict(medicarersop.tf,transf=exp)
# Medicaid
print(medicaidrsop<-rma(yi=LORrsop,sei=seLORrsop,dat=insdat[which(insdat$comp=='medicaid'),]))
print(rmedicaidrsop<-robust(medicaidrsop,cluster = insdat$cluster_id[which(insdat$comp=='medicaid')],adjust=T))
rmedicaidrsopOR<-predict(rmedicaidrsop,transf = exp)
# Pubbias
medicaidrsop_egger<-rma(yi=LORrsop~seLORrsop,sei=seLORrsop,dat=insdat[which(insdat$comp=='medicaid'),])
print(medicaidrsop_egger<-robust(medicaidrsop_egger,cluster = insdat$cluster_id[which(insdat$comp=='medicaid')],adjust=T))
medicaidrsop.tf<-trimfill(medicaidrsop)
medicaidrsop.tfOR<-predict(medicaidrsop.tf,transf=exp)
# Uninsured
print(uninsuredrsop<-rma(yi=LORrsop,sei=seLORrsop,dat=insdat[which(insdat$comp=='uninsured'),]))
print(runinsuredrsop<-robust(uninsuredrsop,cluster = insdat$cluster_id[which(insdat$comp=='uninsured')],adjust=T))
runinsuredrsopOR<-predict(runinsuredrsop,transf = exp)
# Pubbias
uninsuredrsop_egger<-rma(yi=LORrsop~seLORrsop,sei=seLORrsop,dat=insdat[which(insdat$comp=='uninsured'),])
print(runinsuredrsop_egger<-robust(uninsuredrsop_egger,cluster = insdat$cluster_id[which(insdat$comp=='uninsured')],adjust=T))
uninsuredrsop.tf<-trimfill(uninsuredrsop)
uninsuredrsop.tfOR<-predict(uninsuredrsop.tf,transf=exp)
# Robotic vs Non-Robotic (type unspecified)
print(insrsnrs<-rma(yi=LORrsnrs,sei=seLORrsnrs,dat=insdat))
print(rinsrsnrs<-robust(insrsnrs,cluster = insdat$cluster_id,adjust=T))
rinsrsnrsOR<-predict(rinsrsnrs,transf = exp)
# Pubbias
insrsnrs_egger<-rma(yi=LORrsnrs~seLORrsnrs,sei=seLORrsnrs,dat=insdat)
# can't estimate robust egger
insrsnrs.tf<-trimfill(insrsnrs)
insrsnrs.tfOR<-predict(insrsnrs.tf,transf=exp)
## Save Findings to a Data Frame for Export
insreslp<-data.frame(OR=robustinsrslpOR$pred,LLOR=robustinsrslpOR$ci.lb,ULOR=robustinsrslpOR$ci.ub,
                     k=insrslp$k,clusters=robustinsrslp$n,tau2=insrslp$tau2,I2=insrslp$I2,
                     egger=insrslp_egger$beta[2],eggerp=insrslp_egger$pval[2],
                     tfside=insrslp.tf$side,tfmissing=insrslp.tf$k0,tfOR=insrslp.tfOR$pred,
                     row.names = 'Non-Private (Private)')
medicarereslp<-data.frame(OR=rmedicarerslpOR$pred,LLOR=rmedicarerslpOR$ci.lb,ULOR=rmedicarerslpOR$ci.ub,
                     k=medicarerslp$k,clusters=rmedicarerslp$n,tau2=medicarerslp$tau2,I2=medicarerslp$I2,
                     egger=medicarerslp_egger$beta[2],eggerp=medicarerslp_egger$pval[2],
                     tfside=medicarerslp.tf$side,tfmissing=medicarerslp.tf$k0,tfOR=medicarerslp.tfOR$pred,
                     row.names = 'Medicare (Private)')
medicaidreslp<-data.frame(OR=rmedicaidrslpOR$pred,LLOR=rmedicaidrslpOR$ci.lb,ULOR=rmedicaidrslpOR$ci.ub,
                     k=medicaidrslp$k,clusters=rmedicaidrslp$n,tau2=medicaidrslp$tau2,I2=medicaidrslp$I2,
                     egger=medicaidrslp_egger$beta[2],eggerp=medicaidrslp_egger$pval[2],
                     tfside=medicaidrslp.tf$side,tfmissing=medicaidrslp.tf$k0,tfOR=medicaidrslp.tfOR$pred,
                     row.names = 'Medicaid (Private)')
uninsuredreslp<-data.frame(OR=runinsuredrslpOR$pred,LLOR=runinsuredrslpOR$ci.lb,ULOR=runinsuredrslpOR$ci.ub,
                     k=uninsuredrslp$k,clusters=runinsuredrslp$n,tau2=uninsuredrslp$tau2,I2=uninsuredrslp$I2,
                     egger=uninsuredrslp_egger$beta[2],eggerp=uninsuredrslp_egger$pval[2],
                     tfside=uninsuredrslp.tf$side,tfmissing=uninsuredrslp.tf$k0,tfOR=uninsuredrslp.tfOR$pred,
                     row.names = 'Uninsured (Private)')

insresop<-data.frame(OR=robustinsrsopOR$pred,LLOR=robustinsrsopOR$ci.lb,ULOR=robustinsrsopOR$ci.ub,
                     k=insrsop$k,clusters=robustinsrsop$n,tau2=insrsop$tau2,I2=insrsop$I2,
                     egger=insrsop_egger$beta[2],eggerp=insrsop_egger$pval[2],
                     tfside=insrsop.tf$side,tfmissing=insrsop.tf$k0,tfOR=insrsop.tfOR$pred,
                     row.names = 'Non-Private (Private)')
medicareresop<-data.frame(OR=rmedicarersopOR$pred,LLOR=rmedicarersopOR$ci.lb,ULOR=rmedicarersopOR$ci.ub,
                     k=medicarersop$k,clusters=rmedicarersop$n,tau2=medicarersop$tau2,I2=medicarersop$I2,
                     egger=medicarersop_egger$beta[2],eggerp=medicarersop_egger$pval[2],
                     tfside=medicarersop.tf$side,tfmissing=medicarersop.tf$k0,tfOR=medicarersop.tfOR$pred,
                     row.names = 'Medicare (Private)')
medicaidresop<-data.frame(OR=rmedicaidrsopOR$pred,LLOR=rmedicaidrsopOR$ci.lb,ULOR=rmedicaidrsopOR$ci.ub,
                     k=medicaidrsop$k,clusters=rmedicaidrsop$n,tau2=medicaidrsop$tau2,I2=medicaidrsop$I2,
                     egger=medicaidrsop_egger$beta[2],eggerp=medicaidrsop_egger$pval[2],
                     tfside=medicaidrsop.tf$side,tfmissing=medicaidrsop.tf$k0,tfOR=medicaidrsop.tfOR$pred,
                     row.names = 'Medicaid (Private)')
uninsuredresop<-data.frame(OR=runinsuredrsopOR$pred,LLOR=runinsuredrsopOR$ci.lb,ULOR=runinsuredrsopOR$ci.ub,
                     k=uninsuredrsop$k,clusters=runinsuredrsop$n,tau2=uninsuredrsop$tau2,I2=uninsuredrsop$I2,
                     egger=uninsuredrsop_egger$beta[2],eggerp=uninsuredrsop_egger$pval[2],
                     tfside=uninsuredrsop.tf$side,tfmissing=uninsuredrsop.tf$k0,tfOR=uninsuredrsop.tfOR$pred,
                     row.names = 'Uninsured (Private)')

insresnrs<-data.frame(OR=rinsrsnrsOR$pred,LLOR=rinsrsnrsOR$ci.lb,ULOR=rinsrsnrsOR$ci.ub,
                     k=insrsnrs$k,clusters=rinsrsnrs$n,tau2=insrsnrs$tau2,I2=insrsnrs$I2,
                     egger=insrsnrs_egger$beta[2],eggerp=insrsnrs_egger$pval[2],
                     tfside=insrsnrs.tf$side,tfmissing=insrsnrs.tf$k0,tfOR=insrsnrs.tfOR$pred,
                     row.names = 'Non-Private (Private)')


### Co-Morbidities ----
## Index score of 0 is reference group
comorbdat<-filter(es_dat,factor_cat==1&factor=="comorb")
# Robotic vs Laparoscopic
print(comorbrslp<-rma(yi=LORrslp,sei=seLORrslp,dat=comorbdat))
print(robustcomorbrslp<-robust(comorbrslp,cluster=comorbdat$cluster_id,adjust=T))
robustcomorbrslpOR<-predict(robustcomorbrslp,transf = exp)
# Pubbias
comorbrslp_egger<-rma(yi=LORrslp~seLORrslp,sei=seLORrslp,dat=comorbdat)
print(comorbrslp_egger<-robust(comorbrslp_egger,cluster=comorbdat$cluster_id,adjust=T))
comorbrslp.tf<-trimfill(comorbrslp)
comorbrslp.tfOR<-predict(comorbrslp.tf,transf=exp)
# Robotic vs Open
print(comorbrsop<-rma(yi=LORrsop,sei=seLORrsop,dat=comorbdat))
print(robustcomorbrsop<-robust(comorbrsop,cluster=comorbdat$cluster_id,adjust=T))
robustcomorbrsopOR<-predict(robustcomorbrsop,transf = exp)
# Pubbias
comorbrsop_egger<-rma(yi=LORrsop~seLORrsop,sei=seLORrsop,dat=comorbdat)
print(comorbrsop_egger<-robust(comorbrsop_egger,cluster=comorbdat$cluster_id,adjust=T))
comorbrsop.tf<-trimfill(comorbrsop)
comorbrsop.tfOR<-predict(comorbrsop.tf,transf=exp)
# Robotic vs Non-Robotic (type unspecified)
#print(comorbrsnrs<-rma(yi=LORrsnrs,sei=seLORrsnrs,dat=comorbdat))
# Only 2 studies
## Specific comorbidities with at least 5+ studies
# Robotic vs Laparoscopic
# Diabetes
print(diabrslp<-rma(yi=LORrslp,sei=seLORrslp,dat=es_dat[which(es_dat$factor=='diabetes'),]))
print(rdiabrslp<-robust(diabrslp,cluster = es_dat$cluster_id[which(es_dat$factor=='diabetes')],adjust=T))
rdiabrslpOR<-predict(rdiabrslp,transf = exp)
# Pubbias
print(diabrslp_egger<-rma(yi=LORrslp~seLORrslp,sei=seLORrslp,dat=es_dat[which(es_dat$factor=='diabetes'),]))
#can't estimate robust eggers
diabrslp.tf<-trimfill(diabrslp)
diabrslp.tfOR<-predict(diabrslp.tf,transf=exp)
# Hypertension
print(hypertenrslp<-rma(yi=LORrslp,sei=seLORrslp,dat=es_dat[which(es_dat$factor=='hyperten'),]))
print(rhypertenrslp<-robust(hypertenrslp,cluster = es_dat$cluster_id[which(es_dat$factor=='hyperten')],adjust=T))
rhypertenrslpOR<-predict(rhypertenrslp,transf = exp)
# Pubbias
hypertenrslp_egger<-rma(yi=LORrsop~seLORrsop,sei=seLORrsop,dat=es_dat[which(es_dat$factor=='hyperten'),])
#can't estimate robust eggers
hypertenrslp.tf<-trimfill(hypertenrslp)
hypertenrslp.tfOR<-predict(hypertenrslp.tf,transf=exp)
## Save Findings to a Data Frame for Export
comorbreslp<-data.frame(OR=robustcomorbrslpOR$pred,LLOR=robustcomorbrslpOR$ci.lb,ULOR=robustcomorbrslpOR$ci.ub,
                     k=comorbrslp$k,clusters=robustcomorbrslp$n,tau2=comorbrslp$tau2,I2=comorbrslp$I2,
                     egger=comorbrslp_egger$beta[2],eggerp=comorbrslp_egger$pval[2],
                     tfside=comorbrslp.tf$side,tfmissing=comorbrslp.tf$k0,tfOR=comorbrslp.tfOR$pred,
                     row.names = 'Higher Comorbidity Index Score (0)')
comorbresop<-data.frame(OR=robustcomorbrsopOR$pred,LLOR=robustcomorbrsopOR$ci.lb,ULOR=robustcomorbrsopOR$ci.ub,
                     k=comorbrsop$k,clusters=robustcomorbrsop$n,tau2=comorbrsop$tau2,I2=comorbrsop$I2,
                     egger=comorbrsop_egger$beta[2],eggerp=comorbrsop_egger$pval[2],
                     tfside=comorbrsop.tf$side,tfmissing=comorbrsop.tf$k0,tfOR=comorbrsop.tfOR$pred,
                     row.names = 'Higher Comorbidity Index Score (0)')

diabreslp<-data.frame(OR=rdiabrslpOR$pred,LLOR=rdiabrslpOR$ci.lb,ULOR=rdiabrslpOR$ci.ub,
                     k=diabrslp$k,clusters=rdiabrslp$n,tau2=diabrslp$tau2,I2=diabrslp$I2,
                     egger=diabrslp_egger$beta[2],eggerp=diabrslp_egger$pval[2],
                     tfside=diabrslp.tf$side,tfmissing=diabrslp.tf$k0,tfOR=diabrslp.tfOR$pred,
                     row.names = 'Diabetes (No)')
hypertenreslp<-data.frame(OR=rhypertenrslpOR$pred,LLOR=rhypertenrslpOR$ci.lb,ULOR=rhypertenrslpOR$ci.ub,
                     k=hypertenrslp$k,clusters=rhypertenrslp$n,tau2=hypertenrslp$tau2,I2=hypertenrslp$I2,
                     egger=hypertenrslp_egger$beta[2],eggerp=hypertenrslp_egger$pval[2],
                     tfside=hypertenrslp.tf$side,tfmissing=hypertenrslp.tf$k0,tfOR=hypertenrslp.tfOR$pred,
                     row.names = 'Hypertension (No)')

### Operation Type ----
opdat<-filter(es_dat,factor_cat==1&factor=="op")
# At least 5 studies compared partial and total procedures
opdat<-filter(opdat,opdat$ref=='partial'&opdat$comp=='total')
# Robotic vs Laparoscopic
print(oprslp<-rma(yi=LORrslp,sei=seLORrslp,dat=opdat))
print(roprslp<-robust(oprslp,cluster=opdat$cluster_id,adjust=T))
roprslpOR<-predict(roprslp,transf = exp)
# Pubbias
oprslp_egger<-rma(yi=LORrslp~seLORrslp,sei=seLORrslp,dat=opdat)
#can't estimate robust eggers
oprslp.tf<-trimfill(oprslp)
oprslp.tfOR<-predict(oprslp.tf,transf=exp)
# Robotic vs Laparoscopic
#print(oprsop<-rma(yi=LORrsop,sei=seLORrsop,dat=opdat))
# Only 2 studies
## Save Findings to a Data Frame for Export
opreslp<-data.frame(OR=roprslpOR$pred,LLOR=roprslpOR$ci.lb,ULOR=roprslpOR$ci.ub,
                     k=oprslp$k,clusters=roprslp$n,tau2=oprslp$tau2,I2=oprslp$I2,
                     egger=oprslp_egger$beta[2],eggerp=oprslp_egger$pval[2],
                     tfside=oprslp.tf$side,tfmissing=oprslp.tf$k0,tfOR=oprslp.tfOR$pred,
                     row.names = 'Total Operation (Partial)')

### Site ----
sitedat<-filter(es_dat,factor_cat==1&factor=="site")
# Fewer than 5 studies examined site specific surgeries with a consistent reference group

### Stage ----
## Less advanced stage (0,1,nonmetastatic,emergent) is reference group
stagedat<-filter(es_dat,factor_cat==1&factor=="stage")
# Robotic vs Laparoscopic
print(stagerslp<-rma(yi=LORrslp,sei=seLORrslp,dat=stagedat))
print(rstagerslp<-robust(stagerslp,cluster=stagedat$cluster_id,adjust=T))
rstagerslpOR<-predict(rstagerslp,transf = exp)
# Pubbias
stagerslp_egger<-rma(yi=LORrslp~seLORrslp,sei=seLORrslp,dat=stagedat)
print(stagerslp_egger<-robust(stagerslp_egger,cluster=stagedat$cluster_id,adjust=T))
stagerslp.tf<-trimfill(stagerslp)
stagerslp.tfOR<-predict(stagerslp.tf,transf = exp)
# Robotic vs Open
print(stagersop<-rma(yi=LORrsop,sei=seLORrsop,dat=stagedat))
print(rstagersop<-robust(stagersop,cluster=stagedat$cluster_id,adjust=T))
rstagersopOR<-predict(rstagersop,transf = exp)
# Pubbias
stagersop_egger<-rma(yi=LORrsop~seLORrsop,sei=seLORrsop,dat=stagedat)
print(stagersop_egger<-robust(stagersop_egger,cluster=stagedat$cluster_id,adjust=T))
stagersop.tf<-trimfill(stagersop)
stagersop.tfOR<-predict(stagersop.tf,transf = exp)
## Save Findings to a Data Frame for Export
stagereslp<-data.frame(OR=rstagerslpOR$pred,LLOR=rstagerslpOR$ci.lb,ULOR=rstagerslpOR$ci.ub,
                     k=stagerslp$k,clusters=rstagerslp$n,tau2=stagerslp$tau2,I2=stagerslp$I2,
                     egger=stagerslp_egger$beta[2],eggerp=stagerslp_egger$pval[2],
                     tfside=stagerslp.tf$side,tfmissing=stagerslp.tf$k0,tfOR=stagerslp.tfOR$pred,
                     row.names = 'More Advanced Stage (Less Advanced)')
stageresop<-data.frame(OR=rstagersopOR$pred,LLOR=rstagersopOR$ci.lb,ULOR=rstagersopOR$ci.ub,
                     k=stagersop$k,clusters=rstagersop$n,tau2=stagersop$tau2,I2=stagersop$I2,
                     egger=stagersop_egger$beta[2],eggerp=stagersop_egger$pval[2],
                     tfside=stagersop.tf$side,tfmissing=stagersop.tf$k0,tfOR=stagersop.tfOR$pred,
                     row.names = 'More Advanced Stage (Less Advanced)')

### Treatment ----
## Treatment received is reference group
treatdat<-filter(es_dat,factor_cat==1&factor=="treat")
# Robotic vs Laparoscopic
print(treatrslp<-rma(yi=LORrslp,sei=seLORrslp,dat=treatdat))
print(rtreatrslp<-robust(treatrslp,cluster=treatdat$cluster_id,adjust=T))
rtreatrslpOR<-predict(rtreatrslp,transf = exp)
# Pubbias
treatrslp_egger<-rma(yi=LORrslp~seLORrslp,sei=seLORrslp,dat=treatdat)
#can't estimate robust eggers
treatrslp.tf<-trimfill(treatrslp)
treatrslp.tfOR<-predict(treatrslp.tf,transf=exp)
# Robotic vs Open
#print(treatrsop<-rma(yi=LORrsop,sei=seLORrsop,dat=treatdat))
#only 3 studies
## Save Findings to a Data Frame for Export
treatreslp<-data.frame(OR=rtreatrslpOR$pred,LLOR=rtreatrslpOR$ci.lb,ULOR=rtreatrslpOR$ci.ub,
                     k=treatrslp$k,clusters=rtreatrslp$n,tau2=treatrslp$tau2,I2=treatrslp$I2,
                     egger=treatrslp_egger$beta[2],eggerp=treatrslp_egger$pval[2],
                     tfside=treatrslp.tf$side,tfmissing=treatrslp.tf$k0,tfOR=treatrslp.tfOR$pred,
                     row.names = 'Treatment (No Treatment)')

### Year ----
## Oldest Year is reference group (2009,2010,2013)
yeardat<-filter(es_dat,factor_cat==1&factor=="year")
# Robotic vs Laparoscopic
print(yearrslp<-rma(yi=LORrslp,sei=seLORrslp,dat=yeardat))
print(ryearrslp<-robust(yearrslp,cluster=yeardat$cluster_id,adjust=T))
ryearrslpOR<-predict(ryearrslp,transf = exp)
# Pubbias
yearrslp_egger<-rma(yi=LORrslp~seLORrslp,sei=seLORrslp,dat=yeardat)
print(yearrslp_egger<-robust(yearrslp_egger,cluster=yeardat$cluster_id,adjust=T))
yearrslp.tf<-trimfill(yearrslp)
yearrslp.tfOR<-predict(yearrslp.tf,transf=exp)
# Robotic vs Open
print(yearrsop<-rma(yi=LORrsop,sei=seLORrsop,dat=yeardat))
print(ryearrsop<-robust(yearrsop,cluster=yeardat$cluster_id,adjust=T))
ryearrsopOR<-predict(ryearrsop,transf = exp)
# Pubbias
yearrsop_egger<-rma(yi=LORrsop~seLORrsop,sei=seLORrsop,dat=yeardat)
yearrsop.tf<-trimfill(yearrsop)
yearrsop.tfOR<-predict(yearrsop.tf,transf=exp)
## Save Findings to a Data Frame for Export
yearreslp<-data.frame(OR=ryearrslpOR$pred,LLOR=ryearrslpOR$ci.lb,ULOR=ryearrslpOR$ci.ub,
                     k=yearrslp$k,clusters=ryearrslp$n,tau2=yearrslp$tau2,I2=yearrslp$I2,
                     egger=yearrslp_egger$beta[2],eggerp=yearrslp_egger$pval[2],
                     tfside=yearrslp.tf$side,tfmissing=yearrslp.tf$k0,tfOR=yearrslp.tfOR$pred,
                     row.names = 'Recent Years 2010-2017 (2009-2013)')
yearresop<-data.frame(OR=ryearrsopOR$pred,LLOR=ryearrsopOR$ci.lb,ULOR=ryearrsopOR$ci.ub,
                     k=yearrsop$k,clusters=ryearrsop$n,tau2=yearrsop$tau2,I2=yearrsop$I2,
                     egger=yearrsop_egger$beta[2],eggerp=yearrsop_egger$pval[2],
                     tfside=yearrsop.tf$side,tfmissing=yearrsop.tf$k0,tfOR=yearrsop.tfOR$pred,
                     row.names = 'Recent Years 2010-2017 (2009-2013)')

#### Meta-Analyses of Hospital Level Characteristics ####
### Type of hospital ----
# 3 studies not included because they combined type and location (e.g., urban teaching hospital)
## Academic/teaching hospitals are reference group
typedat<-filter(es_dat,factor_cat==3&factor=="type")
# Robotic vs Laparoscopic
print(typerslp<-rma(yi=LORrslp,sei=seLORrslp,dat=typedat))
print(rtyperslp<-robust(typerslp,cluster=typedat$cluster_id,adjust=T))
rtyperslpOR<-predict(rtyperslp,transf = exp)
# Pubbias
typerslp_egger<-rma(yi=LORrslp~seLORrslp,sei=seLORrslp,dat=typedat)
print(typerslp_egger<-robust(typerslp_egger,cluster=typedat$cluster_id,adjust=T))
typerslp.tf<-trimfill(typerslp)
typerslp.tfOR<-predict(typerslp.tf,transf=exp)
# OR by specific type (at least 5+ studies)
# Community
print(comrslp<-rma(yi=LORrslp,sei=seLORrslp,dat=typedat[which(typedat$comp=='com'),]))
print(rcomrslp<-robust(comrslp,cluster = typedat$cluster_id[which(typedat$comp=='com')],adjust=T))
rcomrslpOR<-predict(rcomrslp,transf = exp)
# Pubbias
comrslp_egger<-rma(yi=LORrslp~seLORrslp,sei=seLORrslp,dat=typedat[which(typedat$comp=='com'),])
#can't estimate robust eggers
comrslp.tf<-trimfill(comrslp)
comrslp.tfOR<-predict(comrslp.tf,transf = exp)
# Comprehensive Community
print(comprerslp<-rma(yi=LORrslp,sei=seLORrslp,dat=typedat[which(typedat$comp=='compre'),]))
print(rcomprerslp<-robust(comprerslp,cluster = typedat$cluster_id[which(typedat$comp=='compre')],adjust=T))
rcomprerslpOR<-predict(rcomprerslp,transf = exp)
# Pubbias
comprerslp_egger<-rma(yi=LORrslp~seLORrslp,sei=seLORrslp,dat=typedat[which(typedat$comp=='compre'),])
#can't estimate robust eggers
comprerslp.tf<-trimfill(comprerslp)
comprerslp.tfOR<-predict(comprerslp.tf,transf = exp)
#print(integrslp<-rma(yi=LORrslp,sei=seLORrslp,dat=typedat[which(typedat$comp=='integ'),]))
#only 1 cluster making the above SE unreliable
# Robotic vs Open
print(typersop<-rma(yi=LORrsop,sei=seLORrsop,dat=typedat))
print(rtypersop<-robust(typersop,cluster=typedat$cluster_id,adjust=T))
rtypersopOR<-predict(rtypersop,transf = exp)
# Pubbias
typersop_egger<-rma(yi=LORrsop~seLORrsop,sei=seLORrsop,dat=typedat)
print(typersop_egger<-robust(typersop_egger,cluster=typedat$cluster_id,adjust=T))
typersop.tf<-trimfill(typersop)
typersop.tfOR<-predict(typersop.tf,transf = exp)
# OR by specific type (at least 5+ studies)
# Community
print(comrsop<-rma(yi=LORrsop,sei=seLORrsop,dat=typedat[which(typedat$comp=='com'),]))
print(rcomrsop<-robust(comrsop,cluster = typedat$cluster_id[which(typedat$comp=='com')],adjust=T))
rcomrsopOR<-predict(rcomrsop,transf = exp)
# Pubbias
comrsop_egger<-rma(yi=LORrsop~seLORrsop,sei=seLORrsop,dat=typedat[which(typedat$comp=='com'),])
#can't estimate robust eggers
comrsop.tf<-trimfill(comrsop)
comrsop.tfOR<-predict(comrsop.tf,transf = exp)
# Comprehensive Community
print(comprersop<-rma(yi=LORrsop,sei=seLORrsop,dat=typedat[which(typedat$comp=='compre'),]))
print(rcomprersop<-robust(comprersop,cluster = typedat$cluster_id[which(typedat$comp=='compre')],adjust=T))
rcomprersopOR<-predict(rcomprersop,transf = exp)
# Pubbias
comprersop_egger<-rma(yi=LORrsop~seLORrsop,sei=seLORrsop,dat=typedat[which(typedat$comp=='compre'),])
#can't estimate robust eggers
comprersop.tf<-trimfill(comprersop)
comprersop.tfOR<-predict(comprersop.tf,transf = exp)
#print(integrsop<-rma(yi=LORrsop,sei=seLORrsop,dat=typedat[which(typedat$comp=='integ'),]))
#only 1 cluster making the above SE unreliable
## Save Findings to a Data Frame for Export
typereslp<-data.frame(OR=rtyperslpOR$pred,LLOR=rtyperslpOR$ci.lb,ULOR=rtyperslpOR$ci.ub,
                     k=typerslp$k,clusters=rtyperslp$n,tau2=typerslp$tau2,I2=typerslp$I2,
                     egger=typerslp_egger$beta[2],eggerp=typerslp_egger$pval[2],
                     tfside=typerslp.tf$side,tfmissing=typerslp.tf$k0,tfOR=typerslp.tfOR$pred,
                     row.names = 'Non-Academic (Academic)')
comreslp<-data.frame(OR=rcomrslpOR$pred,LLOR=rcomrslpOR$ci.lb,ULOR=rcomrslpOR$ci.ub,
                     k=comrslp$k,clusters=rcomrslp$n,tau2=comrslp$tau2,I2=comrslp$I2,
                     egger=comrslp_egger$beta[2],eggerp=comrslp_egger$pval[2],
                     tfside=comrslp.tf$side,tfmissing=comrslp.tf$k0,tfOR=comrslp.tfOR$pred,
                     row.names = 'Community (Academic)')
comprereslp<-data.frame(OR=rcomprerslpOR$pred,LLOR=rcomprerslpOR$ci.lb,ULOR=rcomprerslpOR$ci.ub,
                     k=comprerslp$k,clusters=rcomprerslp$n,tau2=comprerslp$tau2,I2=comprerslp$I2,
                     egger=comprerslp_egger$beta[2],eggerp=comprerslp_egger$pval[2],
                     tfside=comprerslp.tf$side,tfmissing=comprerslp.tf$k0,tfOR=comprerslp.tfOR$pred,
                     row.names = 'Comprehensive Community (Academic)')

typeresop<-data.frame(OR=rtypersopOR$pred,LLOR=rtypersopOR$ci.lb,ULOR=rtypersopOR$ci.ub,
                     k=typersop$k,clusters=rtypersop$n,tau2=typersop$tau2,I2=typersop$I2,
                     egger=typersop_egger$beta[2],eggerp=typersop_egger$pval[2],
                     tfside=typersop.tf$side,tfmissing=typersop.tf$k0,tfOR=typersop.tfOR$pred,
                     row.names = 'Non-Academic (Academic)')
comresop<-data.frame(OR=rcomrsopOR$pred,LLOR=rcomrsopOR$ci.lb,ULOR=rcomrsopOR$ci.ub,
                     k=comrsop$k,clusters=rcomrsop$n,tau2=comrsop$tau2,I2=comrsop$I2,
                     egger=comrsop_egger$beta[2],eggerp=comrsop_egger$pval[2],
                     tfside=comrsop.tf$side,tfmissing=comrsop.tf$k0,tfOR=comrsop.tfOR$pred,
                     row.names = 'Community (Academic)')
compreresop<-data.frame(OR=rcomprersopOR$pred,LLOR=rcomprersopOR$ci.lb,ULOR=rcomprersopOR$ci.ub,
                     k=comprersop$k,clusters=rcomprersop$n,tau2=comprersop$tau2,I2=comprersop$I2,
                     egger=comprersop_egger$beta[2],eggerp=comprersop_egger$pval[2],
                     tfside=comprersop.tf$side,tfmissing=comprersop.tf$k0,tfOR=comprersop.tfOR$pred,
                     row.names = 'Comprehensive Community (Academic)')

### Save Dataframes of Results Separately for Surgical Comparison ----
rob_lap_results<-bind_rows(sexreslp,agereslp,rereslp,blkreslp,othreslp,hispreslp,asianreslp,
                           increslp,insreslp,medicarereslp,medicaidreslp,uninsuredreslp,
                           comorbreslp,diabreslp,hypertenreslp,opreslp,stagereslp,treatreslp,
                           yearreslp,typereslp,comreslp,comprereslp,.id='type')
rob_lap_results$CI<-paste(round(rob_lap_results$LLOR,2),round(rob_lap_results$ULOR,2),sep=", ")
rob_open_results<-bind_rows(sexresop,ageresop,reresop,blkresop,othresop,incresop,insresop,
                            medicareresop,medicaidresop,uninsuredresop,comorbresop,stageresop,
                            yearresop,typeresop,comresop,compreresop,.id='type')
rob_open_results$CI<-paste(round(rob_open_results$LLOR,2),round(rob_open_results$ULOR,2),sep=", ")
rob_nonrob_results<-bind_rows(reresnrs,insresnrs,.id = 'type')
rob_nonrob_results$CI<-paste(round(rob_nonrob_results$LLOR,2),round(rob_nonrob_results$ULOR,2),sep=", ")

save(rob_lap_results,file='rob_lap_results.rda')
write.csv(rob_lap_results,file='rob_lap_results.csv')
save(rob_open_results,file='rob_open_results.rda')
write.csv(rob_open_results,file='rob_open_results.csv')
save(rob_nonrob_results,file='rob_nonrob_results.rda')
write.csv(rob_nonrob_results,file='rob_nonrob_results.csv')
