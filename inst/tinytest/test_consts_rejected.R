
test_consts_rejected <- function() {
  cb <- vtreat:::.mkCatBayes(origVarName = "x", 
                             vcolin = c(1,1,1,1), 
                             rescol= c(1,1,0,0), 
                             resTarget = 1,
                             smFactor = 0, 
                             levRestriction = NULL, 
                             weights = c(1,1,1,1), 
                             catScaling = FALSE)
  expect_true(is.null(cb))
  
  cn <- vtreat:::.mkCatNum(origVarName = "x", 
                           vcolin = c(1,1,1,1), 
                           rescol= c(1,1,0,0), 
                           smFactor = 0, 
                           levRestriction = NULL, 
                           weights = c(1,1,1,1))
  expect_true(is.null(cn))
  
  invisible(NULL)
}

test_consts_rejected()
