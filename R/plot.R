#' Modified base R plot with Bootstrap branding
#'
#' Shorthand function to generate base R plots with Mel B. Labs branding. Many tips derived from [r-charts.com](https://r-charts.com/base-r/margins/), since base R graphic documentation is often lacking.
#'
#' @param axes position of X (`bottom` or `top`) and Y (`left` or `right`) axes (default: `bottomright`)
#' @inheritParams graphics::plot.default
#' @inheritDotParams graphics::plot.default
#'
#' @returns base R plot
#' @examples
#' set.seed(1)
#' x <- runif(100, min = -5, max = 5)
#' y <- x ^ 3 + rnorm(100, mean = 0, sd = 5)
#'
#' plot(x, y, main="Mel B. Labs Branded Plot", sub="Scatter plot")
#' plot(x, y, nx=NULL,
#'   main="Mel B. Labs Branded Plot", sub="Subtitle",
#'   xlab="X Units", ylab="Y Units")
#' abline(h=0, col=pal("red"), lwd=2)
#'
#' plot(x, type="h", col=pal(c("red", "green"))[(x > 0) + 1],
#'   main="Mel B. Labs Branded Plot", sub="Histogram")
#'
#' par.labs("family")
#' par("fg")
#' palette()
#'
#' @export
plot <- function(
  x,
  y = NULL,
  axes = c("bottomright", "topright", "bottomleft", "topleft"),
  bg = NULL,
  col = pal(1),
  lwd = 2,
  main = NULL,
  sub = NULL,
  xlab = NULL,
  ylab = NULL,
  nx = NA,
  ny = NULL,
  grid.lty = "dotted",
  grid.lwd = 2,
  ...
) {
  axes = match.arg(axes)

  opal = palette()
  opar = par(par.labs())
  on.exit(par(opar))
  on.exit(palette(opal), add = TRUE)

  # Use Bootstrap palette
  palette(pal())

  # Plot background
  b = .globals$brand
  bg = if (missing(bg)) b$color$palette[[b$color$background]][[1]] else bg

  plot.default(
    x,
    y,
    axes = FALSE,
    bg = bg,
    col = col,
    lwd = lwd,
    xlab = NA,
    ylab = NA,
    ...
  )
  axis(1, lty = missing(nx) + 0, lwd = 2, line = 0, gap.axis = 0)
  axis(4, lty = 0, lwd = 2, line = 0)
  grid(nx, ny, col = par("fg"), lty = grid.lty, lwd = grid.lwd)

  title(main = main, cex.main = 1.5, adj = 0, font = 1, line = 3.5)
  mtext(xlab, side = 1, line = 0.5, cex = 1, adj = 1.125, font = 2)
  mtext(ylab, side = 3, line = 0, cex = 1, adj = 0, font = 1)
  mtext(
    sub,
    side = 3,
    line = 2,
    cex = 1.25,
    adj = 0,
    col = adjustcolor(13, alpha = .5)
  )
}

#' List of graphical parameters for Mel B. Labs branded plots
#'
#' Sets graphical device parameters per branding guidelines. Uses `_brand.yml` configuration when provided explicitely via `brand()`, or else this package's built-in brand.
#'
#' @inheritParams graphics::par
#' @inheritDotParams graphics::par
#' @returns a modified par() list
#' @examples
#' opar <- par(par.labs())
#' par()
#' # Restore device to default state
#' par(opar)
#'
#' @export
par.labs <- function(...) {
  b = .globals$brand
  b = if (is.null(b)) brand() else b

  fg = b$color$palette[[b$color[["foreground"]]]][[1]]
  family = b$font[[1]]

  list(
    bty = "n",
    cex.axis = 1.25,
    col = fg,
    family = family,
    fg = fg,
    font.main = 1,
    font.sub = 1,
    las = 1,
    mar = c(4, 2, 5, 5),
    mgp = c(1, 0.5, 0),
    pty = "m",
    tck = 0.025,
    ...
  )
}
