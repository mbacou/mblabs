% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/plot.R
\name{par.labs}
\alias{par.labs}
\title{List of graphical parameters for Bootstrap branded plots}
\usage{
par.labs(...)
}
\arguments{
\item{...}{
  Arguments passed on to \code{\link[graphics:par]{graphics::par}}
  \describe{
    \item{\code{no.readonly}}{logical; if \code{TRUE} and there are no other
    arguments, only parameters are returned which can be set by a
    subsequent \code{par()} call \emph{on the same device}.}
  }}
}
\value{
a modified par() list
}
\description{
Sets graphical device parameters per branding guidelines. Uses \verb{_brand.yml} configuration when provided explicitely via \code{brand()}, or else this package's built-in brand.
}
\examples{
opar <- par(par.labs())
par()
# Restore device to default state
par(opar)

}
