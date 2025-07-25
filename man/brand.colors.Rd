% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/theme.R
\name{brand.colors}
\alias{brand.colors}
\title{Bootstrap color ramp}
\usage{
brand.colors(n = NULL, colors = pal(), alpha = 0.9, ...)
}
\arguments{
\item{n}{number of colors to interpolate}

\item{colors}{colors to interpolate; must be a valid argument to
    \code{\link[grDevices]{col2rgb}()}.}

\item{alpha}{transparency \link{0,1}}

\item{...}{
  Arguments passed on to \code{\link[grDevices:colorRamp]{grDevices::colorRampPalette}}
  \describe{
    \item{\code{}}{}
  }}
}
\value{
vector of n interpolated colors
}
\description{
Qualitative color ramp derived from active branding. This ramp excludes Bootstrap's
\strong{white}, \strong{black}, \strong{light} and \strong{gray} colors, which are typically used for
textual elements. By default the color ramp is 90\% transparent.
}
\examples{
scales::show_col(brand.colors(alpha=1))
scales::show_col(brand.colors(11, alpha=1, interpolate="spline"))
scales::show_col(brand.colors(16))
scales::show_col(brand.colors(4, alpha=.5))

}
