% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/mult_class.R
\name{mkCrossFrameMExperiment}
\alias{mkCrossFrameMExperiment}
\title{Function to build multi-outcome vtreat cross frame and treatment plan.}
\usage{
mkCrossFrameMExperiment(
  dframe,
  varlist,
  outcomename,
  ...,
  weights = c(),
  minFraction = 0.02,
  smFactor = 0,
  rareCount = 0,
  rareSig = 1,
  collarProb = 0,
  codeRestriction = NULL,
  customCoders = NULL,
  scale = FALSE,
  doCollar = FALSE,
  splitFunction = vtreat::kWayCrossValidation,
  ncross = 3,
  forceSplit = FALSE,
  catScaling = FALSE,
  y_dependent_treatments = c("catB"),
  verbose = FALSE,
  parallelCluster = NULL,
  use_parallel = TRUE,
  missingness_imputation = NULL,
  imputation_map = NULL
)
}
\arguments{
\item{dframe}{data to learn from}

\item{varlist}{character, vector of indpendent variable column names.}

\item{outcomename}{character, name of outcome column.}

\item{...}{not used, declared to forced named binding of later arguments}

\item{weights}{optional training weights for each row}

\item{minFraction}{optional minimum frequency a categorical level must have to be converted to an indicator column.}

\item{smFactor}{optional smoothing factor for impact coding models.}

\item{rareCount}{optional integer, allow levels with this count or below to be pooled into a shared rare-level.  Defaults to 0 or off.}

\item{rareSig}{optional numeric, suppress levels from pooling at this significance value greater.  Defaults to NULL or off.}

\item{collarProb}{what fraction of the data (pseudo-probability) to collar data at if doCollar is set during \code{\link{prepare.multinomial_plan}}.}

\item{codeRestriction}{what types of variables to produce (character array of level codes, NULL means no restriction).}

\item{customCoders}{map from code names to custom categorical variable encoding functions (please see \url{https://github.com/WinVector/vtreat/blob/main/extras/CustomLevelCoders.md}).}

\item{scale}{optional if TRUE replace numeric variables with regression ("move to outcome-scale").}

\item{doCollar}{optional if TRUE collar numeric variables by cutting off after a tail-probability specified by collarProb during treatment design.}

\item{splitFunction}{(optional) see vtreat::buildEvalSets .}

\item{ncross}{optional scalar>=2 number of cross-validation rounds to design.}

\item{forceSplit}{logical, if TRUE force cross-validated significance calculations on all variables.}

\item{catScaling}{optional, if TRUE use glm() linkspace, if FALSE use lm() for scaling.}

\item{y_dependent_treatments}{character what treatment types to build per-outcome level.}

\item{verbose}{if TRUE print progress.}

\item{parallelCluster}{(optional) a cluster object created by package parallel or package snow.}

\item{use_parallel}{logical, if TRUE use parallel methods.}

\item{missingness_imputation}{function of signature f(values: numeric, weights: numeric), simple missing value imputer.}

\item{imputation_map}{map from column names to functions of signature f(values: numeric, weights: numeric), simple missing value imputers.}
}
\value{
a names list containing cross_frame, treat_m, score_frame, and fit_obj_id
}
\description{
Please see \code{vignette("MultiClassVtreat", package = "vtreat")} \url{https://winvector.github.io/vtreat/articles/MultiClassVtreat.html}.
}
\examples{

# numeric example
set.seed(23525)

# we set up our raw training and application data
dTrainM <- data.frame(
  x = c('a', 'a', 'a', 'a', 'b', 'b', NA, NA),
  z = c(1, 2, 3, 4, 5, NA, 7, NA), 
  y = c(0, 0, 0, 1, 0, 1, 2, 1))

dTestM <- data.frame(
  x = c('a', 'b', 'c', NA), 
  z = c(10, 20, 30, NA))

# we perform a vtreat cross frame experiment
# and unpack the results into treatmentsM,
# dTrainMTreated, and score_frame
unpack[
  treatmentsM = treat_m,
  dTrainMTreated = cross_frame,
  score_frame = score_frame
  ] <- mkCrossFrameMExperiment(
    dframe = dTrainM,
    varlist = setdiff(colnames(dTrainM), 'y'),
    outcomename = 'y',
    verbose = FALSE)

# the score_frame relates new
# derived variables to original columns
score_frame[, c('origName', 'varName', 'code', 'rsq', 'sig', 'outcome_level')] \%.>\%
  print(.)

# the treated frame is a "cross frame" which
# is a transform of the training data built 
# as if the treatment were learned on a different
# disjoint training set to avoid nested model
# bias and over-fit.
dTrainMTreated \%.>\%
  head(.) \%.>\%
  print(.)

# Any future application data is prepared with
# the prepare method.
dTestMTreated <- prepare(treatmentsM, dTestM, pruneSig=NULL)

dTestMTreated \%.>\%
  head(.) \%.>\%
  print(.)

}
\seealso{
\code{\link{prepare.multinomial_plan}}
}
