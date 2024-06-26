
.empty_sites <- function(count, site) {
  # convert site to factor; remember the original value
  # (allowed: integer; numeric; character; factor)
  if (inherits(site, "integer")) {
    site <- factor(site)
  }

}



# Some basic assertions to test whether models can be run.

#' Check whether there are sufficient observations to run a model
#'
#'
#'
#'
#' @param x A \code{\link{trimcommand}} object, a \code{data.frame}, or the location of a TRIM command file.
#' @param ... Parameters passed to other methods.
#'
#' @family modelspec
#'
#' @export
check_observations <- function(x, ...){
  UseMethod("check_observations")
}

#' @param model \code{[numeric]} Model 1, 2 or 3?
#' @param count_col \code{[character|numeric]} column index of the counts in \code{x}
#' @param year_col \code{[character|numeric]} column index of years or time points in \code{x}
#' @param month_col \code{[character|numeric]} optional column index of months in \code{x}
#' @param covars \code{[character|numeric]} column index of covariates in \code{x}
#' @param changepoints \code{[numeric]} Changepoints (model 2 only)
#' @param eps \code{[numeric]} Numbers whose absolute magnitude are lesser than \code{eps} are considered zero.
#'
#' @return A \code{list} with two components. The component \code{sufficient} takes the value
#' \code{TRUE} or \code{FALSE} depending on whether sufficient counts have been found.
#' The component \code{errors} is a \code{list}, of which the structure depends on the chosen model,
#' that indicates under what conditions insufficient data is present to estimate the model.
#'
#' \itemize{
#' \item{For model 3 without covariates, \code{$errors} is a list whose single element is a vector of time
#' points with insufficient counts}.
#' \item{For model 3 with covariates, \code{$errors} is a named list with an element for each covariate
#' for which insufficients counts are encountered. Each element is a two-column \code{data.frame}. The
#' first column indicates the time point, the second column indicates for which covariate value insufficient
#' counts are found.}
#' \item{For Model 2, without covariates \code{$errors} is a list with a single
#' element \code{changepoints}. It points out what changepoints lead to a time
#' slice with zero observations.}
#' \item{For Model 2, with covariates \code{$errors} is a named list with an
#' element for each covariate for which inssufficients counts are encountered.
#' Each element is a two-column \code{data.frame}, The first colum indicates the
#' changepoint, the second column indicates for which covariate value
#' insufficient counts are found.}
#' }
#'
#'
#'
#' @export
#' @rdname check_observations
check_observations.data.frame <- function(x, model, count_col="count", year_col="year", month_col=NULL,
                                          covars = character(0), changepoints=numeric(0), eps=1e-8, ...) {

  if (!isTRUE(model %in% 1:3)) stop("model must be 1, 2, or 3")

  if (!(count_col %in% names(x))) stop(sprintf("Column %s not found in data.frame", count_col))
  if (!(year_col %in% names(x))) stop(sprintf("Column %s not found in data.frame", year_col))
  if (!is.null(month_col)) {
    if (!(month_col %in% names(x))) stop(sprintf("Column %s not found in data.frame", month_col))
  }
  for (cv in covars) {
    if (!(cv %in% names(x))) stop(sprintf("Column %s not found in data.frame", cv))
  }

  out <- list()
  if (model==3 && length(covars) == 0) {
    # model 3, simple mode: annual or annual + monthly
    if (is.null(month_col)) { # annual only
      yt <- tapply(X=x[,count_col], INDEX = x[,year_col], FUN=sum, na.rm=TRUE) # total counts per year
      ii <- yt < eps
      out$sufficient <- !any(ii)
      if (!out$sufficient) {
        out$errors <- list()
        out$errors[[year_col]] <- names(ii)[ii]
      }
    } else { # annual + monthly
      yt <- tapply(X=x[,count_col], INDEX = x[,year_col], FUN=sum, na.rm=TRUE) # total counts per year
      mt <- tapply(X=x[,count_col], INDEX = x[,month_col], FUN=sum, na.rm=TRUE) # per month
      ii <- yt < eps
      jj <- mt < eps
      out$sufficient <- !any(ii) & !any(jj)
      if (!out$sufficient) {
        out$errors <- list()
        if (any(ii)) out$errors[[year_col]] <- names(ii)[ii]
        if (any(jj)) out$errors[[month_col]] <- names(jj)[jj]
      }
    }
  } else if (model == 3 && length(covars>0)) {
    out$errors <- get_cov_count_errlist(x[,count_col],x[,year_col],covars=x[covars],timename=year_col)
    out$sufficient <- length(out$errors) == 0
  } else if ( model == 2 ) {
    pieces <- pieces_from_changepoints(x[,year_col],changepoints)
    ok <- pieces > 0 # allow zero counts for changepoint 0
    if ( length(covars) == 0){
      time_totals <- tapply(X=x[ok,count_col],INDEX=pieces[ok], FUN = sum, na.rm=TRUE)
      ii <- time_totals <= eps
      out$sufficient <- !any(ii)
      out$errors <- list(changepoint = as.numeric(names(time_totals))[ii])
    } else {
      out$errors <- get_cov_count_errlist(x[ok,count_col], pieces[ok], x[ok,covars,drop=FALSE], timename="changepoint")
      out$sufficient <- length(out$errors) == 0
    }
  }

  out
}


#' @export
#' @rdname check_observations
check_observations.trimcommand <- function(x, ...){
  dat <- read_tdf(x$file)
  check_observations.data.frame(x=dat,model=x$model, covars=x$labels[x$covariates]
                                , changepoints = x$changepoints, ...)
}

#' @export
#' @rdname check_observations
check_observations.character <- function(x, ...){
  tc <- read_tcf(x)
  check_observations.trimcommand(tc, ...)
}


# capture how an object is printed in a string.
print_and_capture <- function(x){
  paste(capture.output(print(x)),collapse="\n")
}

# all x positive or an error
assert_positive <- function(x, varname) {
  if (any(x <= 0)){
    i <- which(x<=0)
    msg <- if (is.null(varname)) sprintf("Found zero or less counts for %s", paste(names(x[i]),collapse=", "))
           else                  sprintf("Found zero or less counts for %s %s",varname, paste(names(x[i]),collapse=", "))
    stop(msg,call.=FALSE)
  }
  invisible(TRUE)
}


# sufficient data per index (index=time for model 3, pieces for model 2)
assert_sufficient_counts <- function(count, index) {
  time_totals <- tapply(X=count, INDEX=index, FUN=sum, na.rm=TRUE)
  assert_positive(time_totals, names(index))
}


# Get an indicator for the pieces in 'piecewise linear model'
# that are encoded in changepoints.
pieces_from_changepoints <- function(year, changepoints) {
  # convert actual time from (possibly non-contiguous) years to time points 1..J
  jj <- as.integer(ordered(year))
  J <- max(jj)

  # Changepoints must be converted to 1..J-1 if not already so
  if (length(changepoints)==0) {
    # case 0: no changepoints
    cpts <- integer(0)
  } else if (min(changepoints)>=1 && max(changepoints)<J) {
    # case 1: already in 1..J-1
    cpts <- changepoints
  } else if (all(changepoints %in% year)) {
    # case 2: actual years (used); convert to 1..J-1
    cpts <- match(changepoints, year)
  } else {
    stop("Invalid changepoints specified")
  }

  # Assign each time point to the corresponding change point
  pieces <- integer(length(year)) # Allocate memory;
  # N.B.: we actually use the default value of 0L for cases
  # of no changepoints, or before the first changepoint
  for (cpt in cpts) {
    idx <- which(jj > cpt)
    pieces[idx] <- cpt
  }

  # Ready
  pieces
}


## Check model 2

# sufficient data for piecewise linear trend model
assert_plt_model <- function(count, time, changepoints, covars){

  # First check if the changepoints, are strictly increasing
  if (!all(diff(changepoints)>0) ) {
    msg <- "changepoints not ordered, or containing duplicates"
    stop(msg, call.=FALSE)
  }

  # label the pieces in piecewise linear regression
  pieces <- pieces_from_changepoints(time, changepoints)

  ok = pieces>0 # Allow zero observations for changepoint 0
  if (length(covars)==0){
    assert_sufficient_counts(count[ok], list(changepoint=pieces[ok]))
  } else {
    assert_covariate_counts(count, pieces, covars, timename="changepoint")
  }
}


# get a list of errors: for which time (pieces) and covariate values
# are there zero counts? Result is an empty list or a named list
# of matrices with columns 'timename', value (of the covariate)
get_cov_count_errlist <- function(count, time, covars, timename="time"){
  ERR <- list()
  for (i in seq_along(covars)) { # For all covariates
    covname <- names(covars)[i]
    cov <- covars[[i]]
    index <- list(time=time, value=cov)
    names(index)[1] <- timename
    tab <- tapply(count, INDEX=index, FUN=sum, na.rm=TRUE)
    df <- as.data.frame(as.table(tab))
    # df[,1] = as.integer(df[,1]) # time or piece chareacter->integer
    # allow no-pos-data on time pt 0
    if (timename=="changepoint") {
      idx <- df$changepoint != "0"
      df <- df[idx, ]
    }

    df$Freq[is.na(df$Freq)] <- 0 # replace NA -> 0
    #
    # # Allow no-data at cp 0
    # CP0 = levels(df$time)[1]
    # err <- df[df$Freq==0 & df$time!=CP0, 1:2]
    err <- df[df$Freq==0, 1:2]
    row.names(err) <- NULL
    if (nrow(err) > 0){
      names(err)[2] <- covname
      ERR[[covname]] <- err
    }
  }

  ERR
}

# count: vector of counts
# time: vector of time point or piece ID
# covar: list of covariate vectors
assert_covariate_counts <- function(count, time, covars, timename="time"){
  err <- get_cov_count_errlist(count, time, covars, timename=timename)
  if ( length(err)>0 )
    stop("Zero observations for the following cases:\n"
         , gsub("\\$.*?\n","",print_and_capture(err))
         , call.=FALSE)
  invisible(TRUE)
}


# Return the first changepoint to delete (if any).
# returns the value of the CP, or -1 when nothing
# needs to be deleted.
get_deletion <- function(count, time, changepoints, covars) {
  # browser()
  # if ( changepoints[1] != 1) changepoints <- c(1,changepoints)
  out <- 0L
  if (length(changepoints)==1) return(out) # Never propose to delete a lonely changepoint
  pieces <- pieces_from_changepoints(time, changepoints)
  #cat("pieces"); str(pieces)

  if ( length(covars)> 0){
    err <- get_cov_count_errlist(count, pieces, covars,timename="piece")
    if ( length(err)>0){
      # extract for the first covariant ([[1]]),
      # the first column, representing th piece (second [[1]]).
      # These are chanepoints as factor, so with as.integer() we get their position.
      # however, a '0' changepoints was added earlier, so we have to extract it to find the correct index
      out <- as.integer(err[[1]][[1]][1])-1
      # e <- err[[1]]
      # cat("e:"); str(e); str(e[1,1]); str(as.integer(e[1,1]))
      # out <- as.numeric(as.character(e[1,1]))
    }
  } else {
    tab <- tapply(count, list(pieces=pieces), sum,na.rm=TRUE)
    # print(tab)
    # cat("tab:"); str(tab)
    j <- tab <= 0
    if (any(j)){
      wj = which(j)[1] # just get the index im the list of changepoints
      # cat("wj:"); str(wj)
      # out <- as.numeric(names(tab)[min(wj+1, length(tab))])
      out <- unname(wj)
    }
  }
  out
}

autodelete <- function(count, time, changepoints, covars=NULL) {

  # cat("time: ");  str(time)
  # cat("count:"); str(count)
  # cat("cpts: ");  str(changepoints)
  out <- get_deletion(count, time, changepoints, covars)
  niter <- 1
  tpts <- 1:length(time) # get_deletion always returns out as of time was expressed in time points
  # str(out)
  while (out > 0) {
    # cat("\n")
    # cat("to_del:"); str(out)
    idx  <- as.integer(out)
    if (idx > length(changepoints)) idx <- length(changepoints)
    # str(idx)
    # str(changepoints)
    rprintf("Auto-deleting change point: %d\n", changepoints[idx])
    # cpt
    # cat("tpt:"); str(tpt)
    #
    # # get the actual year or time point, if that has been used)
    # yr <- time[tpt]
    #
    # # which changepoint do we have to delete?
    # if (yr==tpt) {
    #   # using time-points
    #   idx <- which(changepoints==tpt)
    # } else {
    #   # using years
    #   idx <- which(changepoints)
    # }
    #
    # #yr  <- changepoints[cp] # was: time[cp]
    # #idx <- which(changepoints==cp)
    # #yr <- changepoints[idx]
    #
    # yr <- time[cp]
    # idx <- which(changepoints==yr)
    #
    # cat("idx:"); str(idx)
    # cat("yr:"); str(yr)
    # if (cp==yr) printf("Auto-deleting change point #%d\n", cp)
    # else        printf("Auto-deleting change point #%d (%d)\n", cp, yr)
    # delete changepoint
    changepoints <- changepoints[-idx] # was: changepoints[changepoints != out]
    # print(idx)
    # print(changepoints)
    out <- get_deletion(count, time, changepoints, covars)
    niter <- niter+1
    if (niter>100) stop("Infinite loop in autodelete()")
  }
  changepoints
}

# rprintf <- function(fmt, ...) cat(sprintf(fmt,...))
#
# load("../tests/testthat/testdata/131183.RData")
#
# count <- df$count
# year <- df$year
# cpts <- sort(unique(df$year))
# J <- max(as.integer(ordered(year)))
# cpts <- cpts[1:J-1]
#
# out <- autodelete(count, year, cpts)
# print(out)

#trim(count ~ site + year, data=df, model=2, overdisp=TRUE, serialcor=TRUE, changepoints="all", autodelete=TRUE)


# # test code
# cat("\n\n--- testing with time 1... --- should be 4\n")
# time  <- c(1, 2, 3, 4, 5, 6, 7, 8, 9, 10)
# count <- c(1, 1, 1, 1, 0, 0, 0, 1, 1,  1)
# cpts  <- c(         4,       7          ) # cpt 7 is removed to provide data to cpt 4
# out <- autodelete(count, time, cpts)
# print(out)
#
# cat("\n\n--- testing with time 10... --- should be 4\n")
# time  <- 10:19
# count <- c(1, 1, 1, 1, 0, 0, 0, 1, 1,  1)
# cpts  <- c(         4,       7          ) # cpt 7 is removed to provide data to cpt 4
# out <- autodelete(count, time, cpts)
# print(out)
#
# cat("\n\n--- testing with time 10... --- should be 1,4\n")
# time  <- 10:19
# count <- c(1, 1, 1, 1, 0, 0, 0, 1, 1,  1)
# cpts  <- c(1,       4,       7          ) # cpt 7 is removed to provide data to cpt 4
# out <- autodelete(count, time, cpts)
# print(out)
#
# cat("\n\n--- testing with cpts in years --- should be 13\n")
# cpts  <- c(13, 16) # same, but in years
# out <- autodelete(count, time, cpts)
# print(out)
#
# cat("\n\n--- with covars / all fine --- should be 4,7\n")
# time  <- rep(1:10, times=2)
# covar <- rep(letters[1:2], each=10)
# count <- rep(1, 20)
# out <- autodelete(count, time, c(4,7), covars=list(cov=covar))
# print(out)
#
# cat("\n\n--- with covars / delete 7 ---should be 4\n")
# count[8:10] <- 0
# out <- autodelete(count, time, c(4,7), covars=list(cov=covar))
# print(out)
#
# cat("\n\n--- with covars / delete 7 --- should be 1,4 \n")
# count[8:10] <- 0
# out <- autodelete(count, time, c(1,4,7), covars=list(cov=covar))
# print(out)
