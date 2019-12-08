## ----setup, include=FALSE-----------------------------------------------------
knitr::opts_chunk$set(fig.width = 7)

## ----echo=FALSE, message=FALSE, warning=FALSE---------------------------------
library(vtreat)
set.seed(23255)

have_rqdatatable = requireNamespace("rqdatatable", quietly=TRUE)
if(have_rqdatatable) {
  library(rqdatatable)
}


## ----echo=FALSE, message=FALSE, warning=FALSE---------------------------------
#
# takes the frame (d) and the outcome column (d$conc)
# from the global environment
#
showGroupingBehavior = function(groupcol, title) {
  print(title)
  
  # display means of each group
  print("Group means:")
  means = tapply(d$conc, d[[groupcol]], mean)
  print(means)
  print(paste("Standard deviation of group means:", sd(means)))
}

## ----data---------------------------------------------------------------------
# panel data for concentration in multiple subjects 
d <- datasets::Theoph
head(d)
summary(d)

## -----------------------------------------------------------------------------
# a somewhat arbitrary split of patients
subnum = as.numeric(as.character(d$Subject))
d$modSplit = as.factor(subnum %% 3)


## -----------------------------------------------------------------------------
print(table(Subject=d$Subject, groupid=d$modSplit))

## -----------------------------------------------------------------------------
# stratify by outcome only
# forces concentration to be equivalent
pStrat <- kWayStratifiedY(nrow(d),3,d,d$conc)
attr(pStrat, "splitmethod")
d$stratSplit <- vtreat::getSplitPlanAppLabels(nrow(d),pStrat)

print(table(Subject=d$Subject, groupid=d$stratSplit))

## -----------------------------------------------------------------------------
# stratify by patient and outcome
# allows concentration to vary amoung individual patients
splitter <- makekWayCrossValidationGroupedByColumn('Subject')
split <- splitter(nrow(d),3,d,d$conc)
attr(split, "splitmethod")
d$subjectSplit <- vtreat::getSplitPlanAppLabels(nrow(d),split)

print(table(Subject=d$Subject, groupid=d$subjectSplit))

## ----echo=FALSE---------------------------------------------------------------
showGroupingBehavior("modSplit", "Arbitrary grouping")

## ----echo=FALSE---------------------------------------------------------------
showGroupingBehavior("subjectSplit", "Group by patient, stratify on y")

