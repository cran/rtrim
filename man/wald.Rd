% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/trim_wald.R
\name{wald}
\alias{wald}
\alias{wald.trim}
\title{Test significance of TRIM coefficients  with the Wald test}
\usage{
wald(x)

\method{wald}{trim}(x)
}
\arguments{
\item{x}{TRIM output structure (i.e., output of a call to \code{\link{trim}})}
}
\value{
A model-dependent list of Wald statistics
}
\description{
Test significance of TRIM coefficients  with the Wald test
}
\examples{
data(skylark)
z2 <- trim(count ~ site + time, data=skylark, model=2)
# print info on significance of slope parameters
print(z2)
z3 <- trim(count ~ site + time, data=skylark, model=3)
# print info on significance of deviations from linear trend
wald(z3)
}
\seealso{
Other analyses: 
\code{\link{coef.trim}()},
\code{\link{confint.trim}()},
\code{\link{gof}()},
\code{\link{index}()},
\code{\link{now_what}()},
\code{\link{overall}()},
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
\code{\link{vcov.trim}()}
}
\concept{analyses}
