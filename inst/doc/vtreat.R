## -----------------------------------------------------------------------------
library("vtreat")
packageVersion("vtreat")
citation('vtreat')

# categorical example
dTrainC <- data.frame(x=c('a', 'a', 'a', 'b', 'b', NA, NA),
   z=c(1, 2, 3, 4, NA, 6, NA),
   y=c(FALSE, FALSE, TRUE, FALSE, TRUE, TRUE, TRUE))
dTestC <- data.frame(x=c('a', 'b', 'c', NA), z=c(10, 20, 30, NA))

# help("designTreatmentsC")

treatmentsC <- designTreatmentsC(dTrainC, colnames(dTrainC), 'y', TRUE,
                                 verbose=FALSE)
print(treatmentsC$scoreFrame[, c('origName', 'varName', 'code', 'rsq', 'sig', 'extraModelDegrees')])

# help("prepare")

dTrainCTreated <- prepare(treatmentsC, dTrainC, pruneSig=1.0, scale=TRUE)
varsC <- setdiff(colnames(dTrainCTreated), 'y')
# all input variables should be mean 0
sapply(dTrainCTreated[, varsC, drop=FALSE], mean)
# all non NA slopes should be 1
sapply(varsC, function(c) { lm(paste('y', c, sep='~'),
   data=dTrainCTreated)$coefficients[[2]]})
dTestCTreated <- prepare(treatmentsC, dTestC, pruneSig=c(), scale=TRUE)
print(dTestCTreated)

## -----------------------------------------------------------------------------
# numeric example
dTrainN <- data.frame(x=c('a', 'a', 'a', 'a', 'b', 'b', NA, NA),
   z=c(1, 2, 3, 4, 5, NA, 7, NA), y=c(0, 0, 0, 1, 0, 1, 1, 1))
dTestN <- data.frame(x=c('a', 'b', 'c', NA), z=c(10, 20, 30, NA))
# help("designTreatmentsN")
treatmentsN = designTreatmentsN(dTrainN, colnames(dTrainN), 'y',
                                verbose=FALSE)
print(treatmentsN$scoreFrame[, c('origName', 'varName', 'code', 'rsq', 'sig', 'extraModelDegrees')])
dTrainNTreated <- prepare(treatmentsN, dTrainN, pruneSig=1.0, scale=TRUE)
varsN <- setdiff(colnames(dTrainNTreated), 'y')
# all input variables should be mean 0
sapply(dTrainNTreated[, varsN, drop=FALSE], mean) 
# all non NA slopes should be 1
sapply(varsN, function(c) { lm(paste('y', c, sep='~'),
   data=dTrainNTreated)$coefficients[[2]]}) 
dTestNTreated <- prepare(treatmentsN, dTestN, pruneSig=c(), scale=TRUE)
print(dTestNTreated)

# for large data sets you can consider designing the treatments on 
# a subset like: d[sample(1:dim(d)[[1]], 1000), ]

# One can also use treatment plans as pipe targets.
dTrainN %.>% 
  treatmentsN %.>% 
  knitr::kable(.)

## ----inst1, eval=FALSE--------------------------------------------------------
#  install.packages("vtreat")

