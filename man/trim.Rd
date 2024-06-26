% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/trim.R
\name{trim}
\alias{trim}
\alias{trim.data.frame}
\alias{trim.formula}
\alias{trim.trimcommand}
\title{Estimate TRIM model parameters.}
\usage{
trim(object, ...)

\method{trim}{data.frame}(
  object,
  count_col = "count",
  site_col = "site",
  year_col = "year",
  month_col = NULL,
  weights_col = NULL,
  covar_cols = NULL,
  model = 2,
  changepoints = ifelse(model == 2, 1L, integer(0)),
  overdisp = FALSE,
  serialcor = FALSE,
  autodelete = TRUE,
  stepwise = FALSE,
  covin = list(),
  ...
)

\method{trim}{formula}(object, data = NULL, weights = NULL, ...)

\method{trim}{trimcommand}(object, ...)
}
\arguments{
\item{object}{Either a \code{data.frame}, a \code{formula} or a \code{\link{trimcommand}}.
If \code{object} is a \code{formula}, the dependent variable (left-hand-side)
is treated as the 'counts' variable. The first and second independent variable
are treated as the 'site' and 'time' variable, \bold{in that specific order}. All
other variables are treated as covariates.}

\item{...}{More parameters, see below in the details}

\item{count_col}{\code{[character]} name of the column holding species counts}

\item{site_col}{\code{[character]} name of the column holding the site id}

\item{year_col}{\code{[character]} name of the column holding the time of counting}

\item{month_col}{\code{[character]} optional name of the column holding the season of counting}

\item{weights_col}{\code{[numeric]} Optional vector of site weights. The length of}

\item{covar_cols}{\code{[character]} name(s) of column(s) holding covariates}

\item{model}{\code{[numeric]} TRIM model type 1, 2, or 3.}

\item{changepoints}{\code{[numeric]} Indices for changepoints (`Models').}

\item{overdisp}{\code{[logical]} Take overdispersion into account (See `Estimation options').}

\item{serialcor}{\code{[logical]} Take serial correlation into account (See `Estimation details')}

\item{autodelete}{\code{[logical]} Auto-delete changepoints when number of observations is too small. (See
`Demands on data').}

\item{stepwise}{\code{[logical]} Perform stepwise refinement of changepoints.}

\item{covin}{a list of variance-covariance matrices; one per pseudo-site.}

\item{data}{\code{[data.frame]} Data frame containing at least counts, sites, and times}

\item{weights}{\code{[character]} name of the column in \code{data} which respresents weights (optional)}
}
\description{
Given some count observations, estimate a TRIM model and use these to impute the data set if necessary.
}
\details{
All versions of \code{trim} support additional 'experts only' arguments:

\describe{
\item{\code{verbose}}{Logical switch to temporarily enable verbose output. (use \code{option(trim_verbose=TRUE)}) for permanent verbosity.}
\item{\code{constrain_overdisp}}{Numerical value to control overdispersion.
  \itemize{
  \item A value in the range 0..1 uses a Chi-squared oulier detection method.
  \item A value >1 uses Tukey's Fence.
  \item A value of 1.0 (which is the default) results in unconstrained overdispersion.
  }
  See vignette `Taming overdispersion' for more information.}
\item{\code{conv_crit}}{Convergence criterion.
  Used within the iterative model estimation algorithm.
  The default value is \code{1e-5}.).
  May be set to higher values in case models don't converge.}
\item{\code{max_iter}}{Number of iterations. Default value is \code{200}. May be set to higher values in case models don't converge.}
\item{\code{alpha_method}}{Choose between a more precise (method 1) or a more robust (method 2) method to estimate site parameters alpha.
The default is the the more precise method; but consider setting it to the more robust method 2 if method results in warnings.}
\item{\code{premove}}{Probability of removal of changepoints (default value: 0.2). Parameter used in stepwise refinement of models. See the vignette 'Models and statistical methods in rtrim'.}
\item{\code{penter}}{Probability of re-entering of changepoints (default value: 0.15). Similar use as \code{premove}.}
}
}
\section{Models}{


The purpose of \code{trim()} is to estimate population totals over time,
based on a set of counts \eqn{f_{ij}} at sites \eqn{i=1,2,\ldots,I}
and times \eqn{j=1,2,\ldots,J}. If no count data is available at
site and time \eqn{(i,j)}, a value \eqn{\mu_{ij}} will be imputed.

In \bold{Model 1}, the imputed values are modeled as

\eqn{\ln\mu_{ij} = \alpha_i,}

where \eqn{\alpha_i} is the site effect. This model implies that the counts
vary accross sites, not over time. The model-based \link[=totals]{time totals} are equal to
each time point and the model-based \link[=index]{indices} are all equal to one.

In \bold{Model 2}, the imputed values are modeled as

\eqn{\ln\mu_{ij} = \alpha_i + \beta\times(j-1).}

Here, \eqn{\alpha_i} is the log-count of site \eqn{i} averaged over time and
\eqn{\beta} is the mean growth factor that is shared by all sites over all of
time. The assumption of a constant growth rate may be relaxed by passing
a number of \code{changepoints} that indicate at what times the growth
rate is allowed to change. Using a \code{\link[=wald.trim]{wald}} test
one can investigate whether the changes in slope at the changepoints are
significant. Setting \code{stepwise=TRUE} makes \code{trim} automatically
remove changepoints where the slope does not change significantly.

In \bold{Model 3}, the imputed values are modeled as

\eqn{\ln\mu_{ij}=\alpha_i + \beta_j},

where \eqn{\beta_j} is the deviatiation of log-counts at time \eqn{j},
averaged over all sites. To make this model identifiable, the value of
\eqn{\beta_1=0} by definition. Model 3 can be shown to be equivalent to
Model 2 with a changepoint at every time point. Using a
\code{\link[=wald.trim]{wald}} test, one can estimate whether the collection
of deviations \eqn{\beta_i} make the model differ significantly from an
overall linear trend (Model 2 without changepoints).

The parameters \eqn{\alpha_i} and \eqn{\gamma_j} are referred to
as the \emph{additive representation} of the coefficients. Once computed,
they can be represented and extracted in several representations, using the
\code{\link[=coef.trim]{coefficients}} function. (See also the examples
below).

Other model parameters can be extracted using functions such as
\code{\link{gof}} (for goodness of fit), \code{\link[=summary.trim]{summary}}
or \code{\link{totals}}. Refer to the `See also' section for an overview.
}

\section{Using yearly and monthly counts}{


In many data sets will use use only yearly count data, in which case the
time \eqn{j} will reflect the year number.
An extension of \code{trim} is to use monthly (or any other sub-yearly) count data,
in combination with index computations on the yearly time scale.

In this case, counts are given as \eqn{f_{i,j,m}} with \eqn{m=1,2,\ldots,M} the month number.
As before, \eqn{\mu_{i,j,m}} will be imputed in case of missing counts.

The contibution of month factors to the model is always similar to the way year factors are used in Model 3,
that is,

\eqn{\ln\mu_{i,j,m} = \alpha_i + \beta\times(j-1) + \gamma_m}
for Model 2, and
 \eqn{\ln\mu_{i,j,m} = \alpha_i + \beta_j + \gamma_m}
for Model 3.

For the same reason why \eqn{\beta_1=0} for Model 3, \eqn{\gamma_1=0} in case of monthly parameters.
}

\section{Using covariates}{


In the basic case of Models 2 and 3, the growth parameter \eqn{\beta} does
not vary accross sites. If auxiliary information is available (for instance
a classification of the type of soil or vegetation), the effect of these
variables on the per-site growth rate can be taken into account.

For \bold{Model 2 with covariates} the growth factor \eqn{\beta} is
replaced with a factor

\eqn{\beta_0 + \sum_{k=1}^K z_{ijk}\beta_k}.

Here, \eqn{\beta_0} is referred to as the \emph{baseline} and \eqn{z_{ijk}} is a
dummy variable that combines dummy variables for all covariates. Since a
covariate with \eqn{L} classes is modeled by \eqn{L-1} dummy variables, the
value of \eqn{K} is equal to the sum of the numbers of categories for all
covariates minus the number of covariates. Observe that this model allows for
a covariate to change over time at a certain sites. It is therefore possible
to include situations for example where a site turns from farmland to rural
area. The \code{\link[=coef.trim]{coefficients}} function will report every
individual value of \eqn{\beta}. With a \code{\link[=wald.trim]{wald}} test,
the significance of contributions of covariates can be tested.


For \bold{Model 3 with covariates} the parameter \eqn{\beta_j} is replaced by

\eqn{\beta_{j0} + \sum_{k=1}^Kz_{ijk}\beta_{jk}.}

Again, the \eqn{\beta_{j0}} are referred to as baseline parameters and the
\eqn{\beta_{jk}} record mean differences in log-counts within a set of sites
with equal values for the covariates. All coefficients can be extracted with
\code{\link[=coef.trim]{coefficients}} and the significance of covariates can
be investigated with the \code{\link[=wald.trim]{wald}} test.
}

\section{Estimation options}{


In the simplest case, the counts at different times and sites are considered
independently Poisson distributed. The (often too strict) assumption that
counts are independent over time may be dropped, so correlation between time
points at a certain site can be taken into account. The assumption of being
Poisson distributed can be relaxed as well. In general, the
variance-covariance structure of counts \eqn{f_{ij}} at site \eqn{i} for time
\eqn{j} is modeled as
\itemize{
\item{\eqn{\textrm{var}(f_{ij}) = \sigma^2\mu_{ij} }}
\item{\eqn{\textrm{cor}(f_{ij},f_{i,j+1}) = \rho} },
}
where \eqn{\sigma} is called the \emph{overdispersion}, \eqn{\mu_{ij}} is
the estimated count for site \eqn{i}, time \eqn{j} and \eqn{\rho} is called
the \emph{serial correlation}.

If \eqn{\sigma=1}, a pure Poisson distribution is assumed to model the
counts. Setting \code{overdispersion = TRUE} makes \code{trim} relax this
condition. Setting  \code{serialcor=TRUE} allows \code{trim} to assume a
non-zero correlation between adjacent time points, thus relaxing the
assumption of independence over time.
}

\section{Demands on data}{

The data set must contain sufficient counts to be able to estimate the model.
In particular
\itemize{
\item{For model 2 without covariates there must be at least one observation
  for each time segment defined by the change points.}
\item{For model 2 with covariates there must be at least one observation for
  every value of each covariate, at each time segment defined by the change
  points.}
\item{For model 3 without covariates there must be at least one observation
  for each time point.}
\item{For model 3 with covariates there must be at least one observation for
  every value of each covariate, at each time point.}
\item{For montly data, there must be at least one observation for every month.}
}

The function \code{\link{check_observations}} identifies cases where too few
observations are present to compute a model. Setting the option
\code{autodelete=TRUE} (Model 2 only) makes \code{trim} remove changepoints
such that at each time piece sufficient counts are available to estimate the
model.
}

\examples{
data(skylark)
m <- trim(count ~ site + time, data=skylark, model=2)
summary(m)
coefficients(m)

# An example using weights
# set up some random weights (one for each site)
w <- runif(55, 0.1, 0.9)
# match weights to sites
skylark$weights <- w[skylark$site]
# run model
m <- trim(count ~ site + time, data=skylark, weights="weights", model=3)

# An example using change points, a covariate, and overdispersion
# 1 is added as cp automatically
cp <- c(2,6)
m <- trim(count ~ site + time + Habitat, data=skylark, model=2, changepoints=cp, overdisp=TRUE)
coefficients(m)
# check significance of changes in slope
wald(m)
plot(overall(m))
}
\seealso{
\href{../doc/Skylark_example.html}{rtrim by example} for a gentle introduction,
  \href{../doc/rtrim_for_TRIM_users.html}{rtrim for TRIM users} for users of the classic Delphi-based TRIM implementation,
  and \href{../doc/rtrim_2_extensions.html}{rtrim 2 extensions} for the major changes from rtrim v.1 to rtrim v.2

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
\code{\link{vcov.trim}()},
\code{\link{wald}()}

Other modelspec: 
\code{\link{check_observations}()},
\code{\link{read_tcf}()},
\code{\link{read_tdf}()},
\code{\link{set_trim_verbose}()},
\code{\link{trimcommand}()}
}
\concept{analyses}
\concept{modelspec}
