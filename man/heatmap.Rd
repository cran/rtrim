% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/trim_heatmap.R
\name{heatmap}
\alias{heatmap}
\title{Plot a heatmap representation of observed and/or imputed counts.}
\usage{
heatmap(
  z,
  what = c("data", "imputed", "fitted"),
  log = TRUE,
  xlab = "auto",
  ylab = "Site #",
  ...
)
}
\arguments{
\item{z}{output of a call to \code{\link{trim}}.}

\item{what}{the type of heatmap to be plotted: 'data' (default), 'imputed' or 'fitted'.}

\item{log}{flag to indicate whether the count should be log-transformed first.}

\item{xlab}{x-axis label. The default value "auto" will evaluate to either "Year" or "Time point"}

\item{ylab}{y-axis label}

\item{...}{other parameters to be passed to \code{\link[graphics]{plot}}}
}
\description{
This function organizes the observed and/or imputed counts into a matrix where
rows represent sites and columns represent time points.
A bitmap image is constructed in which each pixel corresponds to an element of this matrix.
Each pixel is colored according the correspondong count status, and the type of heatmap plot requested ('data', 'imputed' or 'fitted').
}
\details{
The 'imputed' heatmap uses the most elaborate color scheme:
Site/time combinations that are observed are colored red, the higher the count, the darker the red.
Site/time combinations that are imputed are colored blue, the higher the estimate, the darker the blue.

For the 'data' heatmap, missing site/time combinations are colored gray.

For the 'fitted' heatmap, all site/time combinations are colored blue.

By default, all counts are log-transformed prior to colorization, and observed counts of 0 are indicates as white pixels.
}
\examples{
data(skylark2)
z <- trim(count ~ site + year, data=skylark2, model=3)
heatmap(z,"imputed")

}
\seealso{
Other graphical post-processing: 
\code{\link{plot.trim.index}()},
\code{\link{plot.trim.totals}()}
}
\concept{graphical post-processing}
