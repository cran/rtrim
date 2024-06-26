% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/trim_overall.R
\name{overall}
\alias{overall}
\title{Compute overall slope}
\usage{
overall(
  x,
  which = c("imputed", "fitted"),
  changepoints = numeric(0),
  bc = FALSE
)
}
\arguments{
\item{x}{an object of class \code{\link{trim}}.}

\item{which}{\code{[character]} Choose between \code{"imputed"} or
\code{"fitted"} counts.}

\item{changepoints}{\code{[numeric]} Change points for which to compute the overall slope,
or "model", in which case the changepoints from the model are used (if any)}

\item{bc}{\code{[logical]} Flag to set backwards compatability with TRIM with respect to trend interpretation.
Defaults to \code{FALSE}.}
}
\value{
a list of class \code{trim.overall} containing, a.o., overall slope
  coefficients (\code{slope}), augmented with p-values and an interpretation).
}
\description{
The overal slope represents the total growth over the piecewise linear model.
}
\section{Details}{


The overall slope represents the mean growth or decline over a period of time.
This can be determined over the whole time period for which the model is fitted (this is the default)
or may be computed over time slices that can be defined with the \code{cp} parameter.
The values for \code{changepoints} do not depend on \code{changepoints} that were used when
specifying the \code{trim} model (See also the example below).

Slopes are computed along with associated confidence intervals (CI) for 1\% and 5\% significance levels,
and interpreted using the following table:

\tabular{ll}{
  \strong{Trend meaning} \tab \strong{Condition} \cr
  Strong increase   (more than 5\% per year) \tab lower CI limit > 0.05\cr
  Moderate increase (less than 5\% per year) \tab lower CI limit > 0\cr
  Moderate decrease (less than 5\% per year) \tab upper CI limit < 0\cr
  Strong decrease   (more than 5\% per year) \tab upper CI limit < -0.05\cr
  Stable            \tab -0.05 < lower < 0 < upper < 0.05\cr
  Uncertain         \tab any other case\cr
}
where trend strength takes precedence over significance,
i.e., a \emph{strong increase (p<0.05)} takes precedence over a \emph{moderate increase (p<0.01)}.

Note that the original TRIM erroneously assumed that the estimated overall trend
magnitude is t-distributed, while in fact it is normally distributed, which is being used within rtrim.
The option \code{bc=TRUE} can be set to force backward compability, for e.g. comparison purposes.
}

\examples{

# obtain the overall slope accross all change points.
data(skylark)
z <- trim(count ~ site + time, data=skylark, model=2)
overall(z)
plot(overall(z))

# Overall is a list, you can get information out if it using the $ syntax,
# for example
L <- overall(z)
L$slope

# Obtain the slope from changepoint to changepoint
z <- trim(count ~ site + time, data=skylark, model=2,changepoints=c(1,4,6))
# slope from time point 1 to 5
overall(z,changepoints=c(1,5,7))
}
\seealso{
Other analyses: 
\code{\link{coef.trim}()},
\code{\link{confint.trim}()},
\code{\link{gof}()},
\code{\link{index}()},
\code{\link{now_what}()},
\code{\link{overdispersion}()},
\code{\link{plot.trim.index}()},
\code{\link{plot.trim.overall}()},
\code{\link{plot.trim.smooth}()},
\code{\link{results}()},
\code{\link{serial_correlation}()},
\code{\link{summary.trim}()},
\code{\link{totals}()},
\code{\link{trendlines}()},
\code{\link{trim}()},
\code{\link{vcov.trim}()},
\code{\link{wald}()}
}
\concept{analyses}
