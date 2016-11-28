% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/trim_overall.R
\name{overall}
\alias{overall}
\title{Compute overall slope}
\usage{
overall(x, which = c("imputed", "fitted"), changepoints = numeric(0))
}
\arguments{
\item{x}{an object of class \code{\link{trim}}.}

\item{which}{\code{[character]} Choose between \code{"imputed"} or
\code{"fitted"} counts.}

\item{changepoints}{\code{[numeric]} Change points for which to compute the overall slope,
or "model", in which case the changepoints from the model are used (if any)}
}
\value{
a list of class \code{trim.overall} containing, a.o., overall slope
  coefficients (\code{slope}), augmented wih p-values and an interpretation).
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
Other analyses: \code{\link{coef.trim}}, \code{\link{gof}},
  \code{\link{index}}, \code{\link{now_what}},
  \code{\link{overdispersion}},
  \code{\link{plot.trim.index}},
  \code{\link{plot.trim.overall}}, \code{\link{results}},
  \code{\link{serial_correlation}},
  \code{\link{summary.trim}}, \code{\link{totals}},
  \code{\link{trim}}, \code{\link{vcov.trim}},
  \code{\link{wald}}
}
