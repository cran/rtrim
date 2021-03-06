% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/read_tcf.R
\name{trimcommand}
\alias{trimcommand}
\title{Create a trimcommand object}
\usage{
trimcommand(...)
}
\arguments{
\item{...}{Options in the form of \code{key=value}. See below for all options.}
}
\description{
Create a trimcommand object
}
\section{Description}{


A \code{trimcommand} object stores a single TRIM model, including the
specification of the data file. Normally, such an object is defined by
reading a legacy TRIM command file.
}

\section{Options}{


\itemize{
\item{ \code{file}    \code{[character]} name of file containing training data.}
\item{ \code{title}   \code{[character]} A string to be printed in the output file.}
\item{ \code{ntimes}  \code{[character]} Number of time points.}
\item{ \code{ncovars} \code{[character]} Number of covariates.}
\item{ \code{labels}  \code{[character]} Covariate label.}
\item{ \code{missing} \code{[integer]} Missing value indicator.}
\item{ \code{weight}  \code{[logical]} Whether a weight column is present in the \code{file}.}
\item{ \code{comment} \code{[character]} A string to be printed in the output file.}
\item{ \code{weighting} \code{[logical]} Whether weights are to be used in the model.}
\item{ \code{serialcor} \code{[logical]} Whether serial correlation is assumed in the model.}
\item{ \code{overdist} \code{[logical]} Whether overdispersion is taken into account by the model.}
\item{ \code{basetime} \code{[integer]} Position of the base time point (must be positive).}
\item{ \code{model}    \code{[integer]} What model to use (1, 2 or 3).}
\item{ \code{covariates} \code{[integer]} Number of covariates to include.}
\item{ \code{changepoints} \code{[integer]} Positions of the change points to include.}
\item{ \code{stepwise} \code{[logical]} Whether stepwise selection of the changepoints is to be used.}
\item{ \code{autodelete} \code{[logical]} Whether to autodelete change points when number of observations is to low in a time segment.}
\item{ \code{outputfiles} \code{[character]} Type of outputfile to generate ('F' and/or 'S')}
\item{ \code{overallchangepoints} \code{[integer]} Positions of the overall change points.}
\item{ \code{impcovout} \code{[logical]} Whether the covariance matrix of the imputed counts is saved.}
\item{ \code{covin} \code{[logical]} Whether the covariance matrix is read in.}
}
}

\seealso{
\href{../doc/Working_with_tcf.html}{Working with TRIM command files and TRIM data files}.

Other modelspec: 
\code{\link{check_observations}()},
\code{\link{read_tcf}()},
\code{\link{read_tdf}()},
\code{\link{set_trim_verbose}()},
\code{\link{trim}()}
}
\concept{modelspec}
