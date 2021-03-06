
test_BO <- function() {
  suppressWarnings({
  # build the common column types we are likely to encounter
  synthFrame <- function(n,censorD) {
    stringReps = c(rep('a',100),rep('b',100),'c',rep('d',20))
    d <- data.frame(xN1=runif(n),
                    xN2=runif(n),
                    xN3=0.0,
                    xL1=sample(c(TRUE,FALSE),size=n,replace=TRUE),
                    xL2=sample(c(TRUE,FALSE,NA),size=n,replace=TRUE),
                    xL3=FALSE,
                    xS1=sample(stringReps,size=n,replace=TRUE),
                    xS2=sample(c(stringReps,NA),size=n,replace=TRUE),
                    xS3='a',
                    xF1=sample(as.factor(c(100:200)),size=n,replace=TRUE),
                    xF2=sample(as.factor(c('a','b','c','d',NA)),size=n,replace=TRUE),
                    xF3=as.factor(c('a')),
                    xI1=sample(as.integer(c(1,2,3)),size=n,replace=TRUE),
                    xI2=sample(as.integer(c(1,2,3,NA)),size=n,replace=TRUE),
                    xI3=as.integer(c(1)),
                    xU1=NA,
                    stringsAsFactors=FALSE)
    now <- Sys.time()
    d$t1 <- as.POSIXct(now+ceiling(as.numeric(now)*runif(n)/10))
    d$t2 <- as.POSIXlt(now+ceiling(as.numeric(now)*runif(n)/10))
    d[sample(1:n,5,replace=T),'xN2'] <- NA
    d[sample(1:n,5,replace=T),'xN2'] <- NaN
    d[sample(1:n,5,replace=T),'t1'] <- NA
    d[sample(1:n,5,replace=T),'t2'] <- NA
    if(censorD) {
      dFree <- rowSums(as.matrix(sapply(d,function(c) {ifelse(is.na(c),0,ifelse(as.character(c)=='d',1.0,0.0))})))<=0
      d <- d[dFree,]
      n <- dim(d)[[1]]
    }
    toNum <- function(v) {
      if(class(v)[[1]]=='character') {
        v <- as.factor(v)
      }
      v <- as.numeric(v)
      meanY <- mean(v,na.rm=TRUE)
      if(is.na(meanY)) {
        meanY <- 0.0
      }
      v[is.na(v)] <- meanY
      v <- v - meanY
      range <- max(1,max(v)-min(v))
      v <- v/range
      v
    }
    dN <- as.matrix(sapply(d,toNum))
    d$yN <- rowSums(dN) + runif(n)
    d$yC <- d$yN >= median(d$yN)
    d
  }
  
  set.seed(26236)
  dTrain <- synthFrame(200,TRUE)
  dTest <- synthFrame(20,FALSE)
  vars <- setdiff(colnames(dTrain),c('yN','yC'))
  verbose=FALSE
  
  for(smFactor in c(0.0,0.5)) {
    for(scale in c(FALSE,TRUE)) {
      if(verbose) {
        print(paste('**********************',smFactor,scale))
        print('# numeric example')
      }
      treatmentsN <- designTreatmentsN(dTrain,vars,'yN',smFactor=smFactor,
                                       rareCount=2,rareSig=0.5,
                                       verbose=verbose)
      dTrainNTreated <- prepare(treatmentsN,dTrain,pruneSig=0.99,
                                scale=scale, 
                                check_for_duplicate_frames=FALSE)
      nvars <- setdiff(colnames(dTrainNTreated),'yN')
      if(scale) {
        # all input variables should be mean 0 when scale is TRUE
        expect_true(max(abs(vapply(dTrainNTreated[,nvars],mean,numeric(1))))<1.0e-5)
        # all slopes should be 1 when scales is TRUE
        expect_true(max(abs(1-
                              vapply(nvars,
                                     function(c) { 
                                       lm(paste('yN',c,sep='~'),
                                          data=dTrainNTreated)$coefficients[[2]]},
                                     numeric(1))))<1.0e-5)
      }
      modelN <- lm(paste('yN',paste(nvars,collapse=' + '),sep=' ~ '),
                   data=dTrainNTreated)
      dTestNTreated <- prepare(treatmentsN,dTest,pruneSig=0.99,scale=scale, 
                               check_for_duplicate_frames=FALSE)
      dTestNTreated$pred <- predict(modelN,newdata=dTestNTreated)
      if(verbose) {
        print(summary(modelN))
      }
      
      if(verbose) {
        print('# caterogic example')
      }
      treatmentsC <- designTreatmentsC(dTrain,vars,'yC',TRUE,smFactor=smFactor,
                                      catScaling=TRUE,
                                       verbose=verbose)
      dTrainCTreated <- prepare(treatmentsC,dTrain,pruneSig=0.99,scale=scale,
                                check_for_duplicate_frames=FALSE)
      cvars <- setdiff(colnames(dTrainCTreated),'yC')
      if(scale) {
        # all input variables should be mean 0 when scale is TRUE
        expect_true(max(abs(vapply(dTrainCTreated[,cvars],mean,numeric(1))))<1.0e-5)
        # all slopes should be 1 when scales is TRUE
        expect_true(max(abs(1-
                              vapply(cvars,
                                     function(c) { 
                                       glm(paste('yC',c,sep='~'),family=binomial,
                                          data=dTrainCTreated)$coefficients[[2]]},
                                     numeric(1))))<1.0e-5)
      }
      modelC <- glm(paste('yC',paste(cvars,collapse=' + '),sep=' ~ '),
                    data=dTrainCTreated,family=binomial(link='logit'))
      dTestCTreated <- prepare(treatmentsC,dTest,pruneSig=0.99,scale=scale, 
                               check_for_duplicate_frames=FALSE)
      dTestCTreated$pred <- predict(modelC,newdata=dTestCTreated,type='response')
      if(verbose) {
        print(summary(modelC))
      }
    }
  }
  })
  
  invisible(NULL)
}

test_BO()

