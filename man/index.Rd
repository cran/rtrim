% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/trim_index.R
\name{index}
\alias{index}
\title{Extract time-indices from TRIM output.}
\usage{
index(
  x,
  which = c("imputed", "fitted", "both"),
  covars = FALSE,
  base = 1,
  level = NULL,
  method = c("formal", "scaled"),
  long = FALSE
)
}
\arguments{
\item{x}{an object of class \code{\link{trim}}}

\item{which}{(character) Selector to distinguish between time indices based on the imputed data (default),
the fitted model, or both.}

\item{covars}{(logical) Switch to compute indices for covariate categories as well.}

\item{base}{(integer or numeric) One or more years, used as as reference for the index.
If just a single year is given, the time total of the corresponding year will be uses as a reference.
If a range of years is given, the average of the corresponding time totals will be used as reference.\cr
Alternatively, the reference year(s) can be identified using their rank number,
i.e. \code{base=1} always refers to the starting year, \code{base=2} to the second year, etc.}

\item{level}{(numeric) the confidence interval required.
Must be in the range 0 to 1. A value of 0.95 results in 95\% confidence intervals.
The default value of NULL results in no confidence interval to be computed.}

\item{method}{(character) Method selector.
Options are \code{"formal"} (default) to use a formal computation of standard errors,
resulting in \eqn{\text{SE}=0} for the reference year,
and \code{"scaled"} to use a simpler approach, based on linear scaling of the time-totals SE.}

\item{long}{(logical) Switch to return 'long' output
(default is 'wide', as in rtrim versions < 2.2)}
}
\value{
A data frame containing indices and their uncertainty expressed as
  standard error. Depending on the chosen output, columns \code{fitted}
  and \code{se_fit}, and/or \code{imputed} and \code{se_imp} are present.
  If \code{covars} is \code{TRUE}, additional indices are computed for the
  individual covariate categories. In this case additional columns
  \code{covariate} and \code{category} are present. The overall indices are
  marked as covariate `Overall' and category 0.\cr
  In case \code{long=TRUE} a long table is returned, and a different naming convention is used.
  e.g., imputed/fitted info is in column \code{series},
  and standard error are always in column \code{SE}.
}
\description{
Indices are obtained by dividing the modelled or imputed time totals by a reference value.
Most commonly, the time totals for the starting year are used as reference.
As a result, the index value for this year will be 1.0, with a standard error of 0.0 by definition.\cr
Alternatively, a range of years can be used as reference. In this case, the mean time totals for this range will be used as
reference, and the standard errors will be larger than 0.0.\cr
Starting with \code{rtrim} 2.2, an additional method can be selected,
which uses a simpler scaling approach to standard errors of the indices
}
\examples{

data(skylark)
z <- trim(count ~ site + time, data=skylark, model=2)
index(z)
# mimic classic TRIM:
index(z, "both")
# Extract standard errors for the imputed data
SE <- index(z,"imputed")$se_mod
# Include covariates
skylark$Habitat <- factor(skylark$Habitat) # hack
z <- trim(count ~ site + time + Habitat, data=skylark, model=2)
ind <- index(z, covars=TRUE)
plot(ind)
# Use alternative base year
index(z, base=3)
# Use average of first 5 years as reference for indexing
index(z, base=1:5)
# Prevent SE=0 for the reference year
index(z, method="scaled")
}
\seealso{
Other analyses: 
\code{\link{coef.trim}()},
\code{\link{confint.trim}()},
\code{\link{gof}()},
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
\code{\link{vcov.trim}()},
\code{\link{wald}()}
}
\concept{analyses}
