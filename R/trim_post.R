# ########################################### TRIM postprocessing functions ####

# ================================================================= Print ======


#' print a 'trim' object
#'
#' @param x a \code{\link{trim}} object
#' @param ... currently unused
#'
#' @export
#' @keywords internal
print.trim <- function(x,...){
  cat("Call:\n")
  print(x$call)
  cat("\n",x$convergence,"\n")
  cat("\nCoefficients:\n")
  print(coef.trim(x))
}

# ================================================================= Summary ====

# ----------------------------------------------------------------- extract ----


#' Summary information for a TRIM job
#'
#' Print a summary of a \code{\link{trim}} object.
#'
#' @param object an object of class \code{\link{trim}}.
#' @param ... Currently unused
#'
#' @return A \code{list} of class \code{trim.summary} containing the call that
#'   created the object, the model code, the coefficients (in additive and
#'   multiplicative form) , the goodness of fit parameters,the overdispersion
#'   and the serial correlation parameters (if computed).
#'
#' @export
#'
#' @family analyses
#' @seealso \code{\link{trim}}
#' @examples
#'
#' data(skylark)
#' z <- trim(count ~ site + time, data=skylark, model=2, overdisp=TRUE)
#'
#' summary(z)
summary.trim <- function(object,...) {

  structure(list(
    call = object$call
    , coefficients = coef.trim(object)
    , gof = gof(object)
    , overdispersion = overdispersion(object)
    , serialcorrelation = serial_correlation(object)
    , model = object$model
    , method = object$method
    , convergence = object$convergence
  ),class="trim.summary")
}


#' @export
#' @keywords internal
print.trim.summary <- function(x,...){

  cl <- paste(capture.output(print(x$call)),collapse="\n")
  printf("Call:\n%s\n",cl)
  printf("\n")

  printf("Model  : %d\n", x$model)
  printf("Method : %s (%s)\n", x$method, x$convergence)

  if (x$model>1) {
    printf("\nCoefficients:\n")
    print(x$coefficients)
    printf("\n")
  }

  printf(" Overdispersion     : %.4f\n",x$overdispersion)
  printf(" Serial Correlation : %.4f\n",x$serialcorrelation)
  printf("\n")

  print(x$gof)
}


#' Extract serial correlation from TRIM object
#'
#' @param x An object of class \code{\link{trim}}
#'
#' @return The serial correlation coefficient if computed, otherwise \code{NULL}.
#'
#' @export
#' @family analyses
serial_correlation <- function(x){
  stopifnot(inherits(x,"trim"))
  x$rho
}


#' Extract overdispersion from trim object
#'
#' @param x An object of class \code{\link{trim}}
#'
#' @return The overdispersion value if computed, otherwise \code{NULL}.
#'
#' @export
#' @family analyses
#'
overdispersion <- function(x){
  stopifnot(inherits(x,"trim"))
  x$sig2
}


# ============================================================ Coefficients ====

# ----------------------------------------------------------------- Extract ----

#' Extract TRIM model coefficients.
#'
#' @section Details:
#'
#' Extract the site, growth or time effect parameters computed with
#' \code{\link{trim}}.
#'
#' @section Additive versus multiplicative representation:
#'
#' In the simplest cases (no covariates, no change points), the trim
#' Model 2 and Model 3 can be summarized as follows:
#'
#' \itemize{
#' \item{Model 2: \eqn{\ln\mu_{ij}=\alpha_i + \beta\times(j-1)} }
#' \item{Model 3: \eqn{\ln\mu_{ij}=\alpha_i + \gamma_j}.}
#' }
#'
#' Here, \eqn{\mu_{ij}} is the estimated number of counts at site \eqn{i}, time
#' \eqn{j}. The parameters \eqn{\alpha_i}, \eqn{\beta} and \eqn{\gamma_j} are
#' refererred to as coefficients in the additive representation. By
#' exponentiating both sides of the above equations, alternative representations
#' can be written down. Explicitly, one can show that
#'
#' \itemize{
#' \item{Model 2: \eqn{\mu_{ij}= a_ib^{(j-1)} = b\mu_{ij-1}}, where \eqn{a_i=e^{\alpha_i}} and \eqn{b=e^\beta}.}
#' \item{Model 3: \eqn{\mu_{ij}=a_ic_j}, where \eqn{a_i=e^{\alpha_i}}, \eqn{c_1=1} and \eqn{c_j=e^{\gamma_j}} for \eqn{j>1}.}
#' }
#'
#' The parameters \eqn{a_i}, \eqn{b} and \eqn{c_j} are referred to as
#' coefficients in the \emph{multiplicative form}.
#'
#' @section Trend and deviation (Model 3 only):
#'
#' The equation for Model 3
#'
#' \eqn{\ln\mu_{ij}  = \alpha_i + \gamma_j},
#'
#' can also be written as an overall slope resulting from a linear regression of
#' the \eqn{\mu_{ij}} over time,  plus site- and time effects that
#' record deviations from this overall slope.  In such a reparametrisation
#' the previous equation can be written as
#'
#' \eqn{\ln\mu_{ij} = \alpha_i^* + \beta^*d_j + \gamma_j^*,}
#'
#' where \eqn{d_j} equals \eqn{j} minus the mean over all \eqn{j} (i.e. if \eqn{j=1,2,\ldots,J}
#' then \eqn{d_j = j-(J+1)/2}). It is not hard to show that
#' \itemize{
#' \item{The \eqn{\alpha_i^*} are the mean \eqn{\ln\mu_{ij}} per site}
#' \item{The \eqn{\gamma_j^*} must sum to zero.}
#' }
#' The coefficients \eqn{\alpha_i^*} and \eqn{\gamma_j^*} are obtained by
#' setting \code{representation="deviations"}. If \code{representation="trend"},
#' the overall trend parameters \eqn{\beta^*} and \eqn{\alpha^*} from the overall
#' slope defined by \eqn{\alpha^* + \beta^*d_j} is returned.
#'
#' Finally, note that both the overall slope and the deviations can be written
#' in multiplicative form as well.
#'
#'
#' @param object TRIM output structure (i.e., output of a call to \code{trim})
#' @param representation \code{[character]} Choose the coefficient
#'   representation. Options \code{"trend"} and \code{"deviations"} are for model 3 only.
#' @param ... currently unused
#'
#' @return A \code{data.frame} containing coefficients and their standard errors,
#' both in additive and multiplicative form.
#'
#' @export
#'
#' @family analyses
#' @examples
#' data(skylark)
#' z <- trim(count ~ site + time, data=skylark, model=2, overdisp=TRUE)
#' coefficients(z)
coef.trim <- function(object,
    representation=c("standard","trend","deviations"),...) {


  representation <- match.arg(representation)

  if (representation %in% c("deviations","trend") && object$model != 3){
    stop(
      sprintf("Cannot extract  %s from TRIM model %d\n",representation,object$model)
      , call.=TRUE)
  }

  switch(representation
    , "standard" = object$coefficients
    , "deviations" = setNames(object$deviations,c("time","add","se_add","mul","se_mul"))
    , "trend" = setNames(object$linear.trend,c("add","se_add","mul","se_mul"))
  )

}


# ============================================================= Time totals ====

# ----------------------------------------------------------------- Extract ----

#' Extract time-totals from TRIM output
#'
#' @param x TRIM output structure (i.e., output of a call to \code{trim})
#' @param which (character) Select what totals to compute (see \code{Details} section).
#' @param obs (logical) Flag to include total observations (or not).
#' @param level (numeric) The confidence level required. If NULL, no confidence intervals are calculated.
#' @param long (logical) Flag to return a tidy long table
#'
#' @return A \code{data.frame} with subclass \code{trim.totals}
#'  (for pretty-printing). The columns are \code{time}, \code{fitted}
#'  and \code{se_fit} (for standard error), and/or \code{imputed}
#'  and \code{se_imp}, depending on the selection.\cr
#'  In case \code{long=TRUE} a long table is returned, and a different naming convention is used,
#'  e.g., imputed/fitted info is in column \code{series},
#'  and standard error are always in column \code{SE}
#'
#' @section Details:
#'
#' The idea of \code{TRIM} is to impute those site-time combinations where
#' no counts are available. Time-totals (i.e. summed over sites) can be obtained
#' for two cases:
#'
#' \itemize{
#' \item{\code{"imputed"}: Time totals are computed after replacing missing values with values predicted by the model}.
#' \item{\code{"fitted"}: Time totals are computed after replacing both missing values and observed values with
#' values predicted by the model.}
#' }
#'
#' @export
#'
#' @family analyses
#' @examples
#' data(skylark)
#' z <- trim(count ~ site + time, data=skylark, model=2, changepoints=c(3,5))
#' totals(z)
#'
#' totals(z, "both") # mimics classic TRIM
#'
totals <- function(x, which=c("imputed","fitted","both"), obs=FALSE, level=NULL, long=FALSE) {
  stopifnot(class(x)=="trim")
  which <- match.arg(which)

  if (long==FALSE) { # Old version, for backwards compatibility
    # Select output columns from the pre-computed time totals
    tt <- switch(which
                 , fitted  = x$time.totals[c(1,2,3)]
                 , imputed = x$time.totals[c(1,4,5)]
                 , both    = x$time.totals[1:5]
    )

    # Optionally add observations
    if (obs) tt$observed <- x$time.totals$observed

    # Optionally add a confidence interval
    if (!is.null(level)) {
      if (ncol(tt)>4) stop("Confidence intervals can only be computed for either imputed or fitted time totals, but not for both")
      mul <- ci_multipliers(lambda=tt[[2]], sig2=x$sig2, level=level)
      tt$lo <- tt[[2]] - tt[[3]] * mul$lo
      tt$hi <- tt[[2]] + tt[[3]] * mul$hi
      # BUGFIX:
      ci <- new_confidence_interval(tt[[2]], tt[[3]], level-level)
      tt$lo <- ci$lo
      tt$hi <- ci$hi
    }
  } else { # New (rtrim 3) version, using a 'long' format
    if (which=="fitted") {
      tt <- data.frame(variable="time_totals",
                       series="fitted",
                       year  =x$time.totals$time,
                       value =x$time.totals$fitted,
                       SE    =x$time.totals$se_fit)
    } else if (which=="imputed") {
      tt <- data.frame(variable="time_totals",
                       series="imputed",
                       year  =x$time.totals$time,
                       value =x$time.totals$imputed,
                       SE    =x$time.totals$se_imp)
    } else if (which=="both") {
      tt1 <-data.frame(variable="time_totals",
                       series="fitted",
                       year  =x$time.totals$time,
                       value =x$time.totals$fitted,
                       SE    =x$time.totals$se_fit)
      tt2 <- data.frame(variable="time_totals",
                        series="imputed",
                        year  =x$time.totals$time,
                        value =x$time.totals$imputed,
                        SE    =x$time.totals$se_imp)
      tt <- rbind(tt1, tt2)
    } else {
      stop("totals(): Invalid value for option 'which':", which)
    }

    # Optionally add observations
    if (obs) {
      tt_obs <- data.frame(series="observed",
                           year  = x$time.totals$time,
                           value = x$time.totals$observed,
                           SE    = NA)
      tt <- rbind(tt,tt_obs)
    }

    # Optionally add a confidence interval
    if (!is.null(level)) {
      mul <- ci_multipliers(lambda=tt$value, sig2=x$sig2, level=level)
      tt$lo <- tt$value - tt$SE * mul$lo
      tt$hi <- tt$value + tt$SE * mul$hi
    }
  }

  # Make recognizable as "time totals"
  old_class <- class(tt)
  new_class <- c("trim.totals", old_class)
  class(tt) <- new_class

  return(tt)
}

#------------------------------------------------------------------ Export ----

# export <- function(x, species, stratum) UseMethod("export")
#
# export.trim.totals <- function(x, species, stratum) {
#   stopifnot(inherits(x, "trim.totals"))
#
#   # Create extra columns to be put before the actual time totals
#   df1 = data.frame(species=species, stratum=stratum)
#   df2 = x$totals
#   df = cbind(df1, df2)
#   print(df, row.names=FALSE)
# }

#------------------------------------------------------------------ Plot -----


#' Plot time-totals from trim output.
#'
#' This function plots a time series of one or more \code{trim.totals} objects, i.e. the output of \code{totals}.
#' Both the time totals themselves, as the associated standard errros will be plotted,
#' the former as a solid line with markers, the latter as a transparent band.
#'
#' Additionally, the observed counts will be plotted (as a line) when this was asked for in the call to \code{totals}.
#'
#' Multiple time-total data sets can be compared in a single plot
#'
#' @param x       an object of class \code{trim.totals}, as resulting from e.g. a call to \code{totals}.
#' @param ...     optional additional \code{trim.totals} objects.
#' @param names   optional character vector with names for the various series.
#' @param xlab    x-axis label. The default value of "auto" will be changed into "Year" or "Time Point", whichever is more appropriate.
#' @param ylab    y-axis label.
#' @param leg.pos legend position, similar as in \code{\link[graphics]{legend}}.
#' @param band    Defines if the uncertainty band will be plotted using standard errors ("se") or confidence intervals ("ci").
#'
#' @export
#'
#' @family graphical post-processing
#'
#' @examples
#'
#' # Simple example
#' data(skylark2)
#' z <- trim(count ~ site + year, data=skylark2, model=3)
#' plot(totals(z))
#'
#' # Extended example
#' z1 <- trim(count ~ site + year + habitat, data=skylark2, model=3)
#' z2 <- trim(count ~ site + year, data=skylark2, model=3)
#' t1 <- totals(z1, obs=TRUE)
#' t2 <- totals(z2, obs=TRUE)
#' plot(t1, t2, names=c("with covariates", "without covariates"), main="Skylark", leg.pos="bottom")
#'
plot.trim.totals <- function(x, ..., names=NULL, xlab="auto", ylab="Time totals", leg.pos="topleft", band="se") {

  special <- "time totals" # distinguish between "time totals" and "index" modes

  # 1. Parse ellipsis (...) arguments: collect trim.totals objects and their names

  zz <- list(x)
  nz <- 1L

  ellipsis <- as.list(substitute(list(...)))[-1L]
  n <- length(ellipsis) # number of ellipsis arguments
  if (n>0) {
    keep <- rep(TRUE, n) # records which (named!) arguments to keep for passing to plot()
    if (is.null(names(ellipsis))) { # none has names
      named <- rep(FALSE, n)
    } else {                        # some have names
      named <- nchar(names(ellipsis)) > 0
    }
    for (i in seq_along(ellipsis)) {
      if (named[i]) next # skip over named arguments that are captured in the ...
      item <- ellipsis[[i]]
      if (is.symbol(item)) item <- eval(item) # needed to convert symbol -> data.frame
      if (inherits(item, "trim.totals")) {
        nz <- nz + 1L
        zz[[nz]] <- item
        keep[i] <- FALSE # additional index data sets are removed from the ellipsis argument
      } else if (inherits(item, "character")) {
        # todo: check if this arguments immediately follows an index argument
        attr(zz[[nz]], "tag") <- item
        keep[i] <- FALSE
      } else stop("Unknown type for unnamed argument")
    }
    ellipsis <- ellipsis[keep]
  }
  stopifnot(nz==length(zz)) # check


  # 2. Create color palette

  brewer_set1 <- c("#E41A1C","#377EB8","#4DAF4A","#984EA3","#FF7F00","#FFFF33","#A65628","#F781BF","#999999")
  opaque <- brewer_set1
  aqua   <- brewer_set1
  for (i in seq_along(aqua)) aqua[i] <- adjustcolor(aqua[i], 0.5)


  # 3. Setup series

  series <- list()
  nseries <- 0

  if (nz==1) {
    x   <- zz[[1]][[1]] # Time point or years
    y   <- zz[[1]][[2]] # Imputed or fitted totals
    err <- zz[[1]][[3]] # Standard error
    obs <- zz[[1]]$observed # might be NULL, which is OK
    y_se_lo = y - err
    y_se_hi = y + err
    y_ci_lo = zz[[1]]$lo # might be NULL, which is OK
    y_ci_hi = zz[[1]]$hi # idem
    if (band=="ci") {
      if (is.null(y_ci_lo) || is.null(y_ci_hi)) stop("No confidence interval present")
      y_se_lo <- y_ci_lo
      y_se_hi <- y_ci_hi
      y_ci_lo <- y_ci_hi <- NULL
    }

    nseries <- 1
    name <- attr(zz[[1]], "tag") # might be NULL
    series[[1]] <- list(x=x, y=y,
                        y_se_lo=y_se_lo, y_se_hi=y_se_hi,
                        y_ci_lo=y_ci_lo, y_ci_hi=y_ci_hi,
                        fill=aqua[1], stroke=opaque[1], lty="solid", name=name, obs=obs)
  } else {
    # Create or handle names
    if (is.null(names)) names <- sprintf("<no name> #%d", 1:nz) # default names
    if (length(names)!=nz) stop("Number of names is not equal to number of series")
    for (i in seq.int(nz)) {
      name <- attr(zz[[i]], "tag")
      if (!is.null(name)) names[i] <- name
    }
    # Now create the series
    for (i in seq.int(nz)) {
      x   <- zz[[i]][[1]] # Time point or years
      y   <- zz[[i]][[2]] # Imputed or fitted totals
      err <- zz[[i]][[3]] # Standard error
      obs <- zz[[i]]$observed # might be NULL, which is OK
      y_se_lo = y - err
      y_se_hi = y + err
      y_ci_lo = zz[[1]]$lo # might be NULL, which is OK
      y_ci_hi = zz[[1]]$hi # idem
      if (band=="ci") {
        if (is.null(y_ci_lo) || is.null(y_ci_hi)) stop("No confidence interval present")
        y_se_lo <- y_ci_lo
        y_se_hi <- y_ci_hi
        y_ci_lo <- y_ci_hi <- NULL
      }
      nseries <- nseries + 1
      series[[i]] <- list(x=x, y=y,
                          y_se_lo=y_se_lo, y_se_hi=y_se_hi,
                          y_ci_lo=y_ci_lo, y_ci_hi=y_ci_hi,
                          fill=aqua[i], stroke=opaque[i], lty="solid", name=names[i], obs=obs)
    }
  }


  # 4. Some further analysis

  # Determine axis labels iff automatic (just use the last 'x' defined)
  if (xlab=="auto") {
    xlab <- ifelse(x[1]==1, "Time Point", "Year")
  }

  xrange <- range(x)
  yrange <- range(series[[1]]$y_se_lo, series[[1]]$y_se_hi)
  if (nseries>1) for (i in 2:nseries) {
    yrange <- range(series[[i]]$y_se_lo, series[[i]]$y_se_hi, yrange)
  }
  # also include optional CI range
  for (i in 1:nseries) {
    yrange <- range(series[[i]]$y_ci_lo, series[[i]]$y_ci_hi, yrange)
  }
  yrange <- range(yrange, 0.0) # honest scaling


  # 5. Plotting

  # Start with an 'empty' plot for starters
  # (we do need some special tricks to pass the plot-specific elements of ...)
  par(las=1)
  args <- ellipsis
  args$x <- NULL
  args$y <- NULL
  args$type='n'
  args <- c(list(x=NULL,y=NULL, type='n', xlim=xrange, ylim=yrange, xlab=xlab, ylab=ylab), ellipsis)
  # plot(NULL, NULL, type='n', xlim=xrange, ylim=yrange, xlab=xlab, ylab=ylab, ...) # won't work
  do.call(plot, args) # does work

  yscale <- 1.0
  if (special=="index") abline(h=yscale, lty="dashed")

  # Bottom layer: error bars

  for (i in rev(1:nseries)) { # reverse order to have the first series on top
    ser <- series[[i]]
    xx <- c(ser$x, rev(ser$x))
    yy <- c(ser$y_se_lo, rev(ser$y_se_hi))
    polygon(xx, yy, col=ser$fill, border=NA)
    segments(ser$x, ser$y_se_lo, ser$x, ser$y_se_hi, col="white", lwd=2)
  }

  # Optionally: confidence interval
  for (i in rev(1:nseries)) {
    ser <- series[[i]]
    if (length(ser$y_ci_lo)==0) next # skip if no CI
    lines(ser$x, ser$y_ci_lo, col=ser$stroke, lwd=1, lty="dashed")
    lines(ser$x, ser$y_ci_hi, col=ser$stroke, lwd=1, lty="dashed")
  }

  # Top layer: lines

  for (i in rev(1:nseries)) {
    ser <- series[[i]]
    lines(ser$x, ser$y, col=ser$stroke, lwd=2, lty=ser$lty)
    points(ser$x, ser$y, col=ser$stroke, pch=16)
  }

  # Optional: observations (sans standard error)

  for (i in rev(1:nseries)) {
    ser <- series[[i]]
    if (!is.null(ser$obs)) {
      lines(ser$x, ser$obs, col=ser$stroke, lwd=2, lty=ser$lty)
    }
  }

  # Optional a legend

  if (nseries>1) {
    leg.names <- leg.colors <- leg.lty <- character(nseries)
    for (i in 1:nseries) {
      leg.names[i]  <- series[[i]]$name
      leg.colors[i] <- series[[i]]$stroke
      leg.lty[i]    <- series[[i]]$lty
    }
    legend(leg.pos, legend=leg.names, col=leg.colors, lty=leg.lty, lwd=2, bty='n', inset=0.02, y.intersp=1.5);
  }


  #### old ####


  # # Build a list of time-totals with optional titles
  # tt = list(t1)
  # optional = list(...)
  #
  # # cat("tt pre:\n")
  # # str(tt)
  #
  # # cat("optional:\n")
  # # str(optional)
  #
  # nopt = length(optional)
  # for (i in seq_len(nopt)) {
  #   x = optional[[i]]
  #   if ("character" %in% class(x)) {
  #     attr(tt[[length(tt)]], "tag") <- x
  #   } else if ("trim.totals" %in% class(x)) {
  #     tt[[length(tt)+1]] <- x
  #   } else {
  #     stop(sprintf("Invalid data type for optional argument %d: %s", i, class(x)))
  #   }
  # }
  #
  # # cat("tt post:\n")
  # # str(tt)
  #
  # # cat("leg.pos:\n")
  # # str(leg.pos)
  #
  # # First pass to compute total range
  # n = length(tt)
  # for (i in 1:n) {
  #   x = tt[[i]][[1]] # Time point or years
  #   y = tt[[i]][[2]] # imputed or fitted
  #   s = tt[[i]][[3]] # Standard error
  #   ylo = y-s
  #   yhi = y+s
  #   if (i==1) {
  #     xrange <- range(x)
  #     yrange <- range(ylo, yhi)
  #   } else {
  #     xrange <- range(xrange, range(x))
  #     yrange <- range(yrange, range(ylo, range(yhi)))
  #   }
  # }
  #
  # # Ensure y-axis starts at 0.0
  # yrange <- range(0.0, yrange)
  #
  # # empty plot for correct axes
  # plot(xrange, yrange, type='n', xlab="Time point", ylab="Time totals")
  #
  # # Second pass: plot them
  # for (i in 1:n) {
  #   x = tt[[i]][[1]] # Time point or years
  #   y = tt[[i]][[2]] # imputed or fitted
  #   s = tt[[i]][[3]] # Standard error
  #   ylo = y-s
  #   yhi = y+s
  #
  #   xx = c(x, rev(x))
  #   ci = c(ylo, rev(yhi))
  #
  #   polygon(xx,ci, col=aqua[i], border=NA)
  #   lines(x,y, col=opaque[i])
  #
  #   # optionally include observed time totals
  #   if ("observed" %in% names(tt[[i]])) {
  #     y = tt[[i]][["observed"]]
  #     lines(x,y, col=opaque[i], lty="dashed")
  #   }
  # }
  #
  # # third pass: legend
  # nnamed  = 0
  # nnoname = 0
  # for (i in 1:n) {
  #   s <- attr(tt[[i]],"tag")
  #   if (is.null(s)) {
  #     nnoname <- nnoname + 1
  #     s <- sprintf("<unnamed> %d", nnoname)
  #   } else {
  #     nnamed = nnamed + 1
  #   }
  #   if (i==1) {
  #     leg.colors <- opaque[i]
  #     leg.names  <- s
  #   } else {
  #     leg.colors <- c(leg.colors, opaque[i])
  #     leg.names <- c(leg.names, s)
  #   }
  # }
  # if (n>1 | nnamed>0) {
  #   legend(leg.pos, legend=leg.names, col=leg.colors, lty=1, lwd=2, bty='n', inset=0.02, y.intersp=1.5);
  # }
}

# #################################################### Confidence Intervals ####


#' Compute Std.err ==> conf.int multipliers.
#'
#' @param lambda mean
#' @param sig2   overdispersion parameter
#' @param level  the confidence level required
#'
#' @return matrix with multipliers. col1=lo; col2=hi
ci_multipliers <- function(lambda, sig2=NULL, level=0.95)
{
  if (is.null(sig2)) sig2 <- 1.0
  if (sig2<1) stop("Overdispersion must be >= 1")
  # Quantiles
  alpha <- 1-level
  qhi <- qgamma(p=1-alpha/2, shape=lambda/sig2, scale=sig2)
  qlo <- qgamma(p=  alpha/2, shape=lambda/sig2, scale=sig2)
  # Standard deviation
  sd <- sqrt(sig2 * lambda)
  # Multipliers
  umul <- (qhi-lambda) / sd
  lmul <- (lambda-qlo) / sd
  out <- data.frame(lomul=lmul, himul=umul)
  out
}

new_confidence_interval <- function(lambda, se, level=0.95)
{
  sig2 <- se^2 / lambda
  # Quantiles
  alpha <- 1-level
  qhi <- qgamma(p=1-alpha/2, shape=lambda/sig2, scale=sig2)
  qlo <- qgamma(p=  alpha/2, shape=lambda/sig2, scale=sig2)
  out <- data.frame(lo=qlo, hi=qhi)
  out
}


# ----------------------------------------------------------------- confint ----

#' Compute time-totals confidence interval
#'
#' Computes confidence intervals for the time-totals of a TRIM model.
#' Both imputed and fitted time-totals are supported, and the confidence level can be specified.
#'
#' @param object a TRIM output object
#' @param parm   parameter specification: imputed or fitted time-totals.
#' @param level  the confidence level required.
#' @param ... not used [included for R compatibility reasons]
#'
#' @export
#'
#' @family analyses
#'
#' @examples
#' data(skylark2)
#' z <- trim(count ~ site + year, data=skylark2, model=3)
#' CI <- confint(z)
confint.trim <- function(object, parm=c("imputed","fitted"), level=0.95, ...) {
  # Get time-totals
  parm <- match.arg(parm)
  tt <- totals(object, parm)
  lambda <- tt[[2]] # imputed or fitted time totals
  se     <- tt[[3]] # std.err.

  # # this used to be: sig2 = 1, which gave err. CI
  # sig2 <- object$sig2
  # if (is.null(sig2)) sig2 <- 1.0
  #
  # # Lower bound:
  # qlo <- qgamma(p=(1-level)/2, shape=lambda) # Compute multipliers
  # lmul <- (lambda-qlo) / sqrt(lambda)
  # lo <- lambda - se * lmul # Compute CI bounds
  # # Upper bound
  # qhi <- qgamma(p=1-(1-level)/2, shape=lambda)
  # umul <- (qhi-lambda) / sqrt(lambda)
  # hi <- lambda + se * umul
  #
  # # BUGFIX (due to Tomas Telensky who spotted it)
  # mul <- ci_multipliers(lambda, sig2, level)
  # lo <- lambda - se * mul$lo
  # hi <- lambda + se * mul$hi

  # Better
  ci <- new_confidence_interval(lambda, se, level)
  lo <- ci$lo
  hi <- ci$hi

  # Combine and label
  CI <- cbind(lo, hi)
  pctlo <- sprintf("%.1f %%", 100 * ((1-level)/2))
  pcthi <- sprintf("%.1f %%", 100 * (1-(1-level)/2))
  colnames(CI) <- c(pctlo,pcthi)
  # Return
  CI
}


# ============================================== Variance-Covariance matrix ====

# ----------------------------------------------------------------- extract ----

#' Extract variance-covariance matrix from TRIM output
#'
#' @param object TRIM output structure (i.e., output of a call to \code{trim})
#' @param which \code{[character]} Selector to distinguish between variance-covariance based on the
#' imputed counts (default), or the fitted counts.
#' @param ... Arguments to pass to or from other methods (currently unused; included for consistency with \code{\link[stats]{vcov}}).
#'
#' @return a J x J matrix, where J is the number of years (or time-points).
#' @export
#'
#' @family analyses
#' @examples
#' data(skylark)
#' z <- trim(count ~ site + time, data=skylark, model=3);
#' totals(z)
#' vcv1 <- vcov(z)          # Use imputed data
#' vcv2 <- vcov(z,"fitted") # Use fitted data
vcov.trim <- function(object, which = c("imputed","fitted"), ... ) {
  stopifnot(inherits(object,"trim"))
  which <- match.arg(which)

  vcv <- switch(which
    , fitted  = object$var_tt_mod
    , imputed = object$var_tt_imp
  )
  vcv
}




# ================================================================= Results ====

# Function \verb!result()! collects and combines the observed, modelled, and imputed
# counts. These results are presented as a data frame, which is readily exported to
# a file by the user.

#' collect observed, modelled, and imputed counts from TRIM output
#'
#' @param z TRIM output structure (i.e., output of a call to \code{trim})
#'
#' @return A \code{data.frame}, one row per site-time combination, with columns for
#' site, year, month (optionally), observed counts, modelled counts and imputed counts.
#' Missing observations are marked as \code{NA}.
#'
#' @export
#'
#' @family analyses
#' @examples
#' data(skylark)
#' z <- trim(count ~ site + time, data=skylark, model=2);
#' out <- results(z)
results <- function(z) {
  stopifnot(inherits(z,"trim"))

  if (z$nmonth==1) {
    # No months
    out <- data.frame(
      site = rep(z$site_id, each=z$ntime),
      time = rep(z$time.id, times=z$nsite),
      observed = as.vector(t(z$f)),
      fitted   = as.vector(t(z$mu)),
      imputed  = as.vector(t(z$imputed))
    )
  } else {
    # With monthts
    out <- data.frame(
      site = rep(z$site_id, each=(z$nyear * z$nmonth)),
      year = rep(z$time.id, each=z$nmonth, times=z$nsite),
      month = rep(z$month_id, times=(z$nsite * z$nyear)),
      observed = as.vector(aperm(z$f, c(3,2,1))),
      fitted   = as.vector(aperm(z$mu, c(3,2,1))),
      imputed  = as.vector(aperm(z$imputed, c(3,2,1)))
    )
  }
  class(out) <- c("trim.results","data.frame")
  out
}

#' @export
plot.trim.results <- function(x, ...) {
  sites = levels(x$site)
  nsite = nlevels(x$site)
  hues = seq(0, 360, length.out = nsite+1)[1:nsite]
  colors = hcl(hues, 100, 65) # C and L Similar to ggplot
  # hues = seq(0, 1, length.out = nsite+1)[1:nsite]
  # colors = hsv(hues, 0.5, 1)
  xrange = range(x$time)
  yrange = range(x$observed, x$modelled, na.rm=TRUE)
  plot(xrange,yrange, type='n', xlab="Time", ylab="Counts")
  for (i in 1: nsite) {
    df = x[x$site == sites[i], ]
    points(df$time, df$observed, pch=16, col=colors[i])
    lines(df$time, df$modelled, col=colors[i])
  }
}

# ================================================================== Advice ====

#' Give advice on further refinement of TRIM models
#'
#' @param z an object of class \code{\link{trim}}.
#'
#' @export
#'
#' @family analyses
#' @seealso \code{\link{trim}}
#' @examples
#'
#' data(skylark)
#' z <- trim(count ~ site + time, data=skylark, model=2, overdisp=TRUE)
#' now_what(z)
now_what <- function(z) {
  stopifnot(inherits(z,"trim"))
  Wald <- wald(z)
  advice_given <- FALSE

  if (!is.null(Wald$dslope)) {
    p = Wald$dslope$p
    if (any(p > 0.2)) {
      ntot = length(p)
      ndel = sum(p > 0.2)
      worst = which.max(p)
      printf("%d out of %d changepoints appear to be insignificant;", ndel, ntot)
      printf("changepoint #%d would be the first candidate to remove.\n", worst)
      advice_given <- TRUE
    }
  }

  if (z$model==3) {
    LR_p <- gof(z)$LR$p
    if (LR_p < 0.05) {
      printf("Model 3 has a bad fit (%g < 0.05); Try a different model.\n", LR_p)
      advice_given <- TRUE
    }
  }

  if (!advice_given) rprintf("Model appears to be adequate; no suggestions for further improvement.\n")
}

