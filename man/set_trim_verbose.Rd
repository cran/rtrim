% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/utils.R
\name{set_trim_verbose}
\alias{set_trim_verbose}
\title{Set verbosity of trim model functions}
\usage{
set_trim_verbose(verbose = FALSE)
}
\arguments{
\item{verbose}{\code{[logical]} toggle verbosity. \code{TRUE} means: be
verbose, \code{FALSE} means be quiet (this is the default).}
}
\description{
Control how much output \code{\link{trim}} writes to the screen while
fitting the model. By default, \code{trim} only returns the output
and does not write any progress to the screen. After calling
\code{set_trim_verbose(TRUE)}, \code{trim} will write information
about running iterations and convergence to the screen during optmization.
}
\seealso{
Other modelspec: 
\code{\link{check_observations}()},
\code{\link{read_tcf}()},
\code{\link{read_tdf}()},
\code{\link{trim}()},
\code{\link{trimcommand}()}
}
\concept{modelspec}
