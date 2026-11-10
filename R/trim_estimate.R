#' TRIM estimation function
#'
#' @param count a numerical vector of count data.
#' @param site an integer/numerical/character/factor vector of site identifiers for each count data point
#' @param year an integer/numerical vector time points for each count data point.
#' @param month an optional integer/character/factor vector of months for each count data point.
#' @param weights an optional numerical vector of weights.
#' @param covars an optional data frame with covariates
#' @param model a model type selector (1, 2 or 3)
#' @param changepoints a numerical vector change points (only for Model 2)
#' @param overdisp a flag indicating of overdispersion has to be taken into account.
#' @param serialcor a flag indication of autocorrelation has to be taken into account.
#' @param autodelete a flag indicating auto-deletion of changepoints with too little observations.
#' @param min_obs minimum number of observations per linear piece in model 2.
#' @param stepwise a flag indicating stepwise refinement of changepoints is to be used.
#' @param covin a list of variance-covariance matrices; one per pseudo-site.
#' @param verbose flag to enable additional output during a single run.
#'
#' @return a list of class \code{trim}, that contains all output, statistics, etc.
#'   Usually this information is retrieved by a set of postprocessing functions
#'
#' @keywords internal
trim_estimate <- function(count, site, year, month, weights, covars
                          , model, changepoints, overdisp, serialcor
                          , autodelete, min_obs=1L, stepwise, covin, verbose=FALSE, ...)
{
  call <- sys.call(1)
  saved_verbosity <- getOption("trim_verbose")
  if (verbose) options(trim_verbose=TRUE)

  # kick out missing/zero sites
  tot_count <- tapply(count, site, function(x) sum(x>0, na.rm=TRUE)) # Count total observations per site
  full_sites  <- names(tot_count)[tot_count>0]
  empty_sites <- names(tot_count)[tot_count==0]
  nkickout <- length(empty_sites)

  if (nkickout>0) {
    msg <- sprintf("Removed %d %s without positive observations: (%s)", nkickout,
                   ifelse(nkickout==1, "site","sites"), paste0(empty_sites, collapse=", "))
    warning(msg)
    idx <- site %in% full_sites
    count <- count[idx]
    site <- site[idx]
    if (is.factor(site)) site <- droplevels(site)
    year  <- year[idx]
    if(!is.null(month)) month <- month[idx]
    if (!is.null(weights)) weights <- weights[idx]
    # Don't forget to adjust the covariates as well!
    if (nrow(covars)>0) covars <- covars[idx, ,drop=FALSE] # prevent data.frame -> vector degradation!
  }

  # Handle "auto" changepoints
  if (model==2 && is.character(changepoints)) {
    changepoints <- tolower(changepoints) # Allow changepoints="All"
    if (changepoints %in% c("all","auto")) {
      if (changepoints == "auto") stepwise=TRUE
      J <- length(unique(year))
      changepoints <- 1 : (J-1)
    }
  }

  if (isTRUE(stepwise)) {
    if (model != 2) stop("Stepwise refinement requires model 2", call.=FALSE)
    if (length(changepoints)<2) stop("Stepwise refinement requires >1 changepoints.", call.=FALSE)
  }

  # Check for independent block problem (only for models > 1)
  if (model > 1) {
    err <- check_blocks(count, site, year)
    nerr <- length(err)
    if (nerr>0) {
      if (nerr==1) {
        msg <- sprintf("Independent blocks found in data, starting at year %d", err)
      } else {
        msg <- sprintf("Independent blocks found in data, starting at years %s",vfmt(err))
      }
      stop(msg, call.=FALSE)
    }
  }

  t1 <- Sys.time()
  if (isTRUE(stepwise)) {
    changepoints <- autodelete(count, site, year, month, covars, changepoints, min_obs=min_obs)
    m <- trim_refine(count, site, year, month, weights, covars,
                     model, changepoints, overdisp, serialcor, autodelete, stepwise, covin, ...)
  } else {
    # data input checks: throw error if not enough counts available.
    if (model == 2 && length(changepoints)>0 && autodelete){
      #changepoints <- autodelete(count, year, changepoints, covars, min_obs=min_obs)
      changepoints <- autodelete(count, site, year, month, covars, changepoints, min_obs=min_obs)
    } else if (model == 2) {

      clean_site   <- clean_sites(site)
      clean_year   <- clean_years(year)
      clean_month  <- clean_months(month)  # could be NULL
      clean_covars <- clean_covars(covars) # could be NULL
      check_model2(count, clean_site, clean_year, clean_month, clean_covars,
                   changepoints, autodeleting=FALSE, min_obs=min_obs)

      #assert_plt_model(count = count, time = year, changepoints = changepoints, covars = covars)

    } else if (model == 3) {

      clean_site   <- clean_sites(site)
      clean_year   <- clean_years(year)
      clean_month  <- clean_months(month)  # could be NULL
      clean_covars <- clean_covars(covars) # could be NULL

      msg <- check_model3(count, clean_site, clean_year, clean_month, clean_covars)
      if (msg!="") stop(msg, call. = FALSE)
      #assert_sufficient_counts(count=count, index=list(year=year))
      #if (!is.null(month)) assert_sufficient_counts(count=count, index=list(month=month))
      #assert_covariate_counts(count=count, time=year, covars=covars, timename="year")
    }

    # compute actual model
    m <- trim_workhorse(count, site, year, month, weights, covars,
                        model, changepoints, overdisp, serialcor, autodelete, stepwise, covin, ...)
  }

  # Store and report the duration of the run
  t2 <- Sys.time()
  m$dt <- difftime(t2,t1)
  rprintf("Running trim took %8.4f %s\n",m$dt,attr(m$dt,"units"))

  # Reset verbosity as it was before
  if (verbose) options(trim_verbose=saved_verbosity)

  # Store the call
  m$call <- call

  # Return the TRIM structure
  return(m)
}
