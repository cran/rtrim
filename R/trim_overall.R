# ########################################################### Overall slope ####

#' Compute overall slope
#'
#' The overal slope represents the total growth over the piecewise linear model.
#'
#' @param x an object of class \code{\link{trim}}.
#' @param which \code{[character]} Choose between \code{"imputed"} or
#'   \code{"fitted"} counts.
#' @param changepoints \code{[numeric]} Change points for which to compute the overall slope,
#'   or "model", in which case the changepoints from the model are used (if any)
#' @param bc \code{[logical]} Flag to set backwards compatability with TRIM with respect to trend interpretation.
#'   Defaults to \code{FALSE}.
#'
#' @section Details:
#'
#' The overall slope represents the mean growth or decline over a period of time.
#' This can be determined over the whole time period for which the model is fitted (this is the default)
#' or may be computed over time slices that can be defined with the \code{cp} parameter.
#' The values for \code{changepoints} do not depend on \code{changepoints} that were used when
#' specifying the \code{trim} model (See also the example below).
#'
#' Slopes are computed along with associated confidence intervals (CI) for 1\% and 5\% significance levels,
#' and interpreted using the following table:
#'
#' \tabular{ll}{
#'   \strong{Trend meaning} \tab \strong{Condition} \cr
#'   Strong increase   (more than 5\% per year) \tab lower CI limit > 0.05\cr
#'   Moderate increase (less than 5\% per year) \tab lower CI limit > 0\cr
#'   Moderate decrease (less than 5\% per year) \tab upper CI limit < 0\cr
#'   Strong decrease   (more than 5\% per year) \tab upper CI limit < -0.05\cr
#'   Stable            \tab -0.05 < lower < 0 < upper < 0.05\cr
#'   Uncertain         \tab any other case\cr
#' }
#' where trend strength takes precedence over significance,
#' i.e., a \emph{strong increase (p<0.05)} takes precedence over a \emph{moderate increase (p<0.01)}.
#'
#' Note that the original TRIM erroneously assumed that the estimated overall trend
#' magnitude is t-distributed, while in fact it is normally distributed, which is being used within rtrim.
#' The option \code{bc=TRUE} can be set to force backward compability, for e.g. comparison purposes.
#'
#' @return a list of class \code{trim.overall} containing, a.o., overall slope
#'   coefficients (\code{slope}), augmented with p-values and an interpretation).
#' @export
#'
#' @family analyses
#' @examples
#'
#' # obtain the overall slope accross all change points.
#' data(skylark)
#' z <- trim(count ~ site + time, data=skylark, model=2)
#' overall(z)
#' plot(overall(z))
#'
#' # Overall is a list, you can get information out if it using the $ syntax,
#' # for example
#' L <- overall(z)
#' L$slope
#'
#' # Obtain the slope from changepoint to changepoint
#' z <- trim(count ~ site + time, data=skylark, model=2,changepoints=c(1,4,6))
#' # slope from time point 1 to 5
#' overall(z,changepoints=c(1,5,7))
overall <- function(x, which=c("imputed","fitted"), changepoints=numeric(0), bc=FALSE) {
  stopifnot(class(x)=="trim")
  which <- match.arg(which)

  # Handle automatic selection of changepoints based on the model
  if (is.character(changepoints) && changepoints=="model") {
    changepoints <- x$changepoints
  }

  # Convert year-based changepoints if required
  if (all(changepoints %in% x$time.id)) changepoints <- match(changepoints, x$time.id)

  # extract vars from TRIM output
  tt_mod     <- x$tt_mod
  tt_imp     <- x$tt_imp
  var_tt_mod <- x$var_tt_mod
  var_tt_imp <- x$var_tt_imp
  J <- ntime <- x$ntime

  # Set changepoints in case none are given
  if (length(changepoints)==0) changepoints <- 1

  # if (base>0) { # use index instead
  #     browser()
  #     tt     = tt_mod
  #     var_tt = var_tt_mod
  #     b = base
  #
  #     tau <- tt / tt[b]
  #     J <- length(tt)
  #     var_tau <- numeric(J)
  #     for (j in 1:J) {
  #       d <- matrix(c(-tt[j] / tt[b]^2, 1/tt[b]))
  #       V <- var_tt[c(b,j), c(b,j)]
  #       var_tau[j] <- t(d) %*% V %*% d
  #     }
  #
  #     tt_mod <- tt_imp <- tau
  #     var_tt_mod <- var_tt_imp <- diag(var_tau)
  # }

  .meaning <- function(bhat, berr, df) {
    if (df<=0) return("Unknown (df<=0)")
    alpha = c(0.05, 0.001)
    stopifnot(df>0)
    if (bc) {
      # Backwards compatbility, not recommended
      tval <- qnorm((1-alpha/2))
    } else {
      tval <- qt((1-alpha/2), df)
    }
    blo <- bhat - tval * berr
    bhi <- bhat + tval * berr

    # Trends are linear in the additive domain (log-counts);
    # to interpret them in the counts domain, we have to take the exp()
    # (multiplicattive domain)

    #                            0.95  1.0   1.05
    #                             v     v     v
    # Strong decrease   |---x---| .     .     .
    # Moderate decreate      |---x---|  .     .
    # Stable                      . |---x---| .
    # Moderate increase           .       |---x---|
    # Strong increase             .           .  |---x---|
    # Uncertain                |--------x--------|

    multiplicative <- TRUE
    if (multiplicative) {
      blo <- exp(blo)
      bhi <- exp(bhi)

      # First priority: evidence for a strong trend?
      if (blo[2] > 1.05) return("Strong increase (p<0.01)")
      if (bhi[2] < 0.95) return("Strong decrease (p<0.01)")
      if (blo[1] > 1.05) return("Strong increase (p<0.05)")
      if (bhi[1] < 0.95) return("Strong decrease (p<0.05)")

      # Second prority: evidence for a moderate trend?
      eps = 1e-7 # required to get a correct interpretation for slope=0.0 (Stable)
      if (blo[2] > 1.0+eps) return("Moderate increase (p<0.01)")
      if (bhi[2] < 1.0-eps) return("Moderate decrease (p<0.01)")
      if (blo[1] > 1.0+eps) return("Moderate increase (p<0.05)")
      if (bhi[1] < 1.0-eps) return("Moderate decrease (p<0.05)")

      # Third priority: evidency for stability?
      if (blo[1]>0.95 && bhi[1]<1.05) return("Stable")

      # Leftover category: uncertain
      return("Uncertain")
    } else { # i.e., additive
      # First priority: evidence for a strong trend?
      if (blo[2] > +0.05) return("Strong increase (p<0.01)")
      if (bhi[2] < -0.05) return("Strong decrease (p<0.01)")
      if (blo[1] > +0.05) return("Strong increase (p<0.05)")
      if (bhi[1] < -0.05) return("Strong decrease (p<0.05)")

      # Second prority: evidence for a moderate trend?
      eps = 1e-7 # required to get a correct interpretation for slope=0.0 (Stable)
      if (blo[2] > +eps) return("Moderate increase (p<0.01)")
      if (bhi[2] < -eps) return("Moderate decrease (p<0.01)")
      if (blo[1] > +eps) return("Moderate increase (p<0.05)")
      if (bhi[1] < -eps) return("Moderate decrease (p<0.05)")

      # Third priority: evidency for stability?
      if (blo[1] > -0.05 && bhi[1] < 0.05) return("Stable")

      # Leftover category: uncertain
      return("Uncertain")
    }
  }

  # The overall slope is computed for both the modeled and the imputed $\Mu$'s.
  # So we define a function to do the actual work
  .compute.overall.slope <- function(tpt, tt, var_tt, src) {
    # tpt = time points, either 1..J or year1..yearn
    n <- length(tpt)
    stopifnot(length(tt)==n)
    stopifnot(nrow(var_tt)==n && ncol(var_tt)==n)

    # handle zero time totals (might happen in imputed TT)
    problem = tt<1e-6
    log_tt = log(tt)
    log_tt[problem] <- 0.0
    alt_tt <- exp(log_tt)

    # Use Ordinary Least Squares (OLS) to estimate slope parameter $\beta$
    X <- cbind(1, tpt) # design matrix
    y <- matrix(log_tt)
    #y[tt<1e-6] = 0.0 # Handle zero (or very low) counts
    bhat <- solve(t(X) %*% X) %*% t(X) %*% y # OLS estimate of $b = (\alpha,\beta)^T$
    yhat <- X %*% bhat

    # Apply the sandwich method to take heteroskedasticity into account
    dvtt <- 1/alt_tt # derivative of $\log{\Mu}$
    Om <- diag(dvtt) %*% var_tt %*% diag(dvtt) # $\var{log{\Mu}}$
    var_beta <- solve(t(X) %*% X) %*% t(X) %*% Om %*% X %*% solve(t(X) %*% X)
    b_err <- sqrt(diag(var_beta))

    # Compute the $p$-value, using the $t$-distribution
    df <- n - 2
    t_val <- bhat / b_err
    if (df>0) p <- 2 * pt(abs(t_val), df, lower.tail=FALSE)
    else      p <- c(NA, NA)

    # Also compute effect size as relative change during the monitoring period.
    #effect <- abs(yhat[J] - yhat[1]) / yhat[1]

    # Reverse-engineer the SSR (sum of squared residuals) from the standard error
    j <- 1 : n
    D <- sum((j-mean(j))^2)
    SSR <- b_err[2]^2 * D * (n-2)

    # Export the results
    z <- data.frame(
      add       = bhat,
      se_add    = b_err,
      mul       = exp(bhat),
      se_mul    = exp(bhat) * b_err,
      p         = p,
      row.names = c("intercept","slope")
    )
    z$meaning   = c("<none>", .meaning(z$add[2], z$se_add[2], n-2))

    list(src=src, coef=z, SSR=SSR)
  }

  if (which=="imputed") {
    tt     <- tt_imp
    var_tt <- var_tt_imp
    src = "imputed"
  } else if (which=="fitted") {
    tt     <- tt_mod
    var_tt <- var_tt_mod
    src = "fitted"
  }

  J = length(tt)
  if (length(changepoints)==0) {
    # Normal overall slope
    out <- .compute.overall.slope(1:J, tt, var_tt, src)
    out$type <- "normal" # mark output as 'normal' overall slope
  } else {
    # overall slope per changepoint. First some checks.
    stopifnot(min(changepoints)>=1)
    stopifnot(max(changepoints)<J)
    stopifnot(all(diff(changepoints)>0))
    ncp <- length(changepoints)
    cpx <- c(changepoints, J) # Extend list of overall changepoints with final year
    int.collector <- data.frame() # Here go the intercepts
    slp.collector <- data.frame() # Here go the slopes
    SSR.collector <- numeric(ncp) # Here go the SSR info
    for (i in 1:ncp) {
      idx <- cpx[i] : cpx[i+1]
      tmp <- .compute.overall.slope(idx, tt[idx], var_tt[idx,idx], src)
      prefix <- data.frame(from=x$time.id[cpx[i]], upto=x$time.id[cpx[i+1]])
      intercept <- tmp$coef[1,] # Intercept is on first row of output dataframe
      int.collector <- rbind(int.collector, cbind(prefix, intercept))
      slope <- tmp$coef[2,] # Slope is on second row of output dataframe
      slp.collector <- rbind(slp.collector, cbind(prefix, slope))
      SSR.collector[i] = tmp$SSR
    }
    out <- list(src=src, slope=slp.collector, intercept=int.collector, SSR=SSR.collector)
  }
  out$J = J
  out$tt = tt
  out$err = sqrt(diag(var_tt))
  out$timept <- x$time.id # export time points for proper plotting
  structure(out, class="trim.overall")
}


# ------------------------------------------------------------------- Print ----

#' Print an object of class trim.overall
#'
#' @param x An object of class \code{trim.overall}
#'
#' @export
#' @keywords internal
print.trim.overall <- function(x,...) {
  print(x$slope, row.names=FALSE)
}


#--------------------------------------------------------------- Trendlines ----

#' Extract 'overall' trendlines
#'
#' @param x An object of class \code{trim.overall}
#'
#' @return A data.frame containing the information on all trendline segments and their uncertainty.
#' The data.frame has the following columns:
#' \describe{
#'   \item{\code{segment}}{segment ID, starting at 1}
#'   \item{\code{year}}{year for which \emph{value}, \emph{lo} and \emph{hi} are given}
#'   \item{\code{value}}{the y coordinate of the trendline segment}
#'   \item{\code{lo}}{lower value of the uncertainty band}
#'   \item{\code{hi}}{upper value of the uncertainty interval}
#' }
#'
#' @examples
#' data(skylark2)
#' z <- trim(count ~ site+year, data=skylark2, model=3)
#' tt <- totals(z, long=TRUE)       # collect time-totals
#' tl <- trendlines(overall(z))     # collect overall trend line
#'
#' # define plot limits
#' xr <- range(tt$year)
#' yr <- range(tl$lo, tl$hi, tt$value)
#' plot(xr, yr, type='n', xlab="Year", ylab="Total counts")
#'
#' # Plot uncertainty band
#' ubx <- c(tl$year, rev(tl$year))
#' uby <- c(tl$lo, rev(tl$hi))
#' polygon(ubx, uby, col=gray(0.9), border=NA)
#'
#' # Plot trend line
#' lines(tl$year, tl$value, col="black", lwd=2)
#'
#' # Plot time-totals
#' lines(tt$year, tt$value, col="red", lwd=2)
#' points(tt$year, tt$value, col="red", pch=16, cex=1.5)
#'
#' @export
#' @family analyses
trendlines <- function(x) {
  X   <- x
  tpt <- X$timept
  J   <- X$J

  X$type <- "changept" # Hack for merging overall/changepts
  if (X$type=="normal") {
    # Trend line
    a <- X$coef[[1]][1] # intercept
    b <- X$coef[[1]][2] # slope
    x <- seq(1, J, length.out=100) # continue timepoint 1..J
    ytrend <- exp(a + b*x)
    xtrend <- seq(min(tpt), max(tpt), len=length(ytrend)) # continue year1..yearn
    #trendline = cbind(xtrend, ytrend)
    trendline <- data.frame(year=xtrend, value=ytrend)

    # Confidence band
    xconf <- c(xtrend, rev(xtrend))
    alpha <- 0.05
    df <- J - 2
    t <- qt((1-alpha/2), df)
    j = 1:J
    dx2 <- (x-mean(j))^2
    sumdj2 <- sum((j-mean(j))^2)
    dy <- t * sqrt((X$SSR/(J-2))*(1/J + dx2/sumdj2))
    ylo <- exp(a + b*x - dy)
    yhi <- exp(a + b*x + dy)
    yconf <- c(ylo, rev(yhi))
    conf.band <- cbind(xconf, yconf)
  } else if (X$type=="changept") {
    nsegment = nrow(X$slope)

    trendline <- data.frame() # placeholder

    for (i in 1:nsegment) {
      # Trend line
      a <- X$intercept[i,3]
      b <- X$slope[i,3]
      from <- which(tpt==X$slope[i,1]) # convert year -> time
      upto <- which(tpt==X$slope[i,2])
      delta = (upto-from)*10
      x      <- seq(from, upto, length.out=delta) # continue timepoint 1..J
      ytrend <- exp(a + b*x)
      xtrend <- seq(tpt[from], tpt[upto], length.out=length(ytrend))

      # Confidence band
      #xconf <- c(xtrend, rev(xtrend))
      alpha <- 0.05 # Confidence level
      ntpt <- upto - from + 1 # Number of time points in segment
      df <- ntpt - 2
      if (df<=0) {
        ylo <- yhi <- NA # No confidence band for this segment...
      } else {
        t <- qt((1-alpha/2), df)
        j = from : upto
        dx2 <- (x-mean(j))^2
        sumdj2 <- sum((j-mean(j))^2)
        SSR = X$SSR[i] # Get stored SSR as computed by overall()
        dy <- t * sqrt((SSR/df)*(1/ntpt + dx2/sumdj2))
        ylo <- exp(a + b*x - dy)
        yhi <- exp(a + b*x + dy)
      }
      new_trendline <- data.frame(segment=i, year=xtrend, value=ytrend, lo=ylo, hi=yhi)
      trendline <- rbind(trendline, new_trendline)
    }
  } else stop("Can't happen")

  trendline
}



#--------------------------------------------------------------------- Plot ----

#' Plot overall slope
#'
#' Creates a plot of the overall slope, its 95\% confidence band, the
#' total population per time and their standard errors.
#'
#' @param x An object of class \code{trim.overall} (returned by \code{\link{overall}})
#' @param ... Further options passed to \code{\link[graphics]{plot}}
#'
#' @family analyses
#'
#' @examples
#' data(skylark)
#' m <- trim(count ~ site + time, data=skylark, model=2)
#' plot(overall(m))
#'
#' @export
plot.trim.overall <- function(x, ...) {
  X <- x
  # title <- if (is.null(list(...)$main)){
  #   attr(X, "title")
  # } else {
  #   list(...)$main
  # }

  tpt  <-  X$timept
  J <- X$J

  # Collect all data for plotting: time-totals
  ydata <- X$tt

  # error bars
  y0 = ydata - X$err
  y1 = ydata + X$err

  trendline <- trendlines(X)
  nsegment <-  nrow(X$slope)

  # Compute the total range of all plot elements (but limit the impact of the confidence band)
  xrange  <- range(trendline$year, na.rm=TRUE)
  yrange1 <- range(range(y0), range(y1), range(trendline$value), na.rm=TRUE)
  yrange2 <- range(trendline$lo, trendline$hi, na.rm=TRUE)
  yrange  <- range(yrange1, yrange2, na.rm=TRUE)
  ylim <- 2 * yrange1[2]
  if (yrange[2] > ylim) yrange[2]  <-  ylim

  # Ensure y-axis starts at 0.0
  yrange <- range(0.0, yrange)

  # Now plot layer-by-layer (using ColorBrewer colors)
  cbred <- rgb(228,26,28, maxColorValue = 255)
  cbblue <- rgb(55,126,184, maxColorValue = 255)
  #plot(xrange, yrange, type='n', xlab="Year", ylab="Count", las=1, main=title,...)
  plot(xrange, yrange, type='n', xlab="Year", ylab="Count", las=1, ...)
  # all trendline segments
  for (i in 1:nsegment) {
    idx <- trendline$segment==i
    # confidence band in gray
    xx <- c(trendline$year[idx], rev(trendline$year[idx]))
    yy <- c(trendline$lo[idx], rev(trendline$hi[idx]))
    polygon(xx, yy, col=gray(0.9), lty=0)
    # trendline in red
    lines(trendline$year[idx], trendline$value[idx], col=cbred, lwd=3) # trendline
  }
  segments(tpt,y0, tpt,y1, lwd=3, col=gray(0.5))
  points(tpt, ydata, col=cbblue, type='b', pch=16, lwd=3)
  #invisible(trendline)
}
