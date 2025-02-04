#' MBLabs brand
#'
#' Make this package compatible with Quarto `brand.yml` branding features. Branding can be read from a per-project file, custom file, or use this package defaults.
#'
#' @param file path to a structured `_brand.yml` file 
#' @inheritDotParams yaml::read_yaml
#' @importFrom yaml read_yaml
#' @return A list of branding elements
#' @seealso [brand.yml](https://posit-dev.github.io/brand-yml/)
#' @examples
#' brand()$color$palette
#'
#' @export
brand <- function(file="_brand.yml", ...) {
  f = read_yaml(file, ...)
  if(is.null(f)) read_yaml(system.file("_brand.yml", package="mblabs"))
  else f
}


#' MBLabs color palette
#'
#' Utility function to return a color palette derived from the active Bootstrap theme and branding elements. By default colors are read from a stuctured `_brand.yaml` file (or from this package default brand).
#'
#' @param x color index or name(s), if missing returns the entire color palette
#' @param named return color names (default: TRUE)
#' @return A vector of (named) hex color codes extracted from Bootstrap branding
#' @seealso [brand.yml](https://posit-dev.github.io/brand-yml/)
#' @examples
#' pal = pal()
#' pie(rep(1, length(pal)), col=pal, labels=names(pal), 
#' border="white", clockwise=TRUE)
#'
#' @export
pal <- function(x, named = TRUE) {
  e = if(missing(x)) brand()$color$palette else brand()$color$palette[x]
  e = unlist(e)
  if(named) e else unname(e)
}

#' Apply branding to **base** and **lattice** graphics using `thematic`
#' 
#' Utility function to set default arguments to `thematic_on()` based on the active Bootstrap theme and branding elements.
#' 
#' @inheritParams thematic::thematic_on
#' @importFrom thematic thematic_on
#' @return a theme object as a list
#' @examples
#' lattice::show.settings()
#' 
#' labs_on()
#' lattice::show.settings()
#' 
#' hist(rchisq(100, df=4), freq=FALSE, ylim=c(0, 0.2), col=1:11, border=12, xlab=NA)
#' grid(NA, NULL, col=4)
#' curve(dchisq(x, df=4), col=2, lty=2, lwd=2, add=TRUE)#' 
#' 
#' @export
brand_on <- function(
  bg = "transparent",
  fg = pal(brand()$color$foreground, FALSE),
  accent = pal(unlist(brand()$color[c("primary", "secondary")]), FALSE),
  font = brand()$typography[["monospace-inline"]],
  sequential = colorRampPalette(pal(c("orange", "light", "green")))(20),
  qualitative = brand.colors()
) thematic_on(bg, fg, accent, font, sequential, qualitative)


#' MBLabs color ramp
#'
#' Qualitative color ramp derived from active branding.
#'
#' @param x number of colors to interpolate
#' @inheritParams grDevices::colorRamp#' 
#' @inheritParams scales::alpha
#' @inheritDotParams grDevices::colorRamp
#' @return A function to interpolate colors
#' @examples
#' x <- rchisq(100, df=4)
#' hist(x, freq=FALSE, ylim=c(0, 0.2), col=brand.colors(20), border="white")
#' hist(x, freq=FALSE, ylim=c(0, 0.2), col=brand.colors(8, alpha=0.5), border="white")
#'
#' @export
brand.colors <- function(
  x = length(pal()),
  colors = pal()[!names(pal()) %in% c("white", "black", "light", "gray")],
  alpha = 0.9,
  ...) alpha(colorRampPalette(unname(colors), ...)(x), alpha)
  

#' MBLabs theme for ggplot2
#'
#' Custom `ggplot2` theme for `mblabs` website matching active branding.
#'
#' @inheritParams ggthemes::theme_foundation
#' @param base_bg Plot, panel, legend background
#' @param base_color Color for text and line elements
#' @inheritDotParams ggplot2::theme
#'
#' @return A branded ggplot2 theme
#' @importFrom ggthemes theme_foundation
#' @examples
#' require(ggplot2)
#'
#' ggplot(mtcars, aes(factor(carb), mpg, fill=factor(carb))) + geom_col() +
#'   guides(y=guide_none(), y.sec=guide_axis()) +
#'   labs(
#'     title = "Plot with default fonts and color scheme",
#'     subtitle = "My very long subtitle with many units",
#'     caption = "My very long plot caption with many references.") +
#'     theme_labs()
#'
#' ggplot(mtcars, aes(factor(carb), mpg, fill=carb)) + geom_col() +
#'   guides(y=guide_none(), y.sec=guide_axis()) +
#'   labs(
#'     title = "Same plot with `mblabs` font and color scheme",
#'     subtitle = "My very long subtitle with many units",
#'     caption = "My very long plot caption with many references.") +
#'     theme_labs()
#'
#' @export
theme_labs <- function(
  base_size = 12,
  base_family = brand()$typography[["monospace-inline"]],
  base_bg = "transparent",
  base_color = pal(brand()$color$foreground, FALSE),
  ...
) theme_foundation(
  base_size = base_size,
  base_family = base_family
  
) + theme(
  
  plot.margin = unit(c(1, 1, 1, 1), "lines"),
  text = element_text(color=base_color, lineheight=0.9),
  line = element_line(linetype=1, color=base_color),
  rect = element_rect(fill=NA, linetype=0, color=NA),
  plot.background = element_rect(fill=base_bg, color=NA),
  panel.background = element_rect(fill=base_bg, color=NA),
  
  panel.grid = element_line(color=NULL, linetype=3),
  panel.grid.major = element_line(color=base_color),
  panel.grid.major.x = element_blank(),
  panel.grid.minor = element_blank(),
  
  plot.title = element_text(face="plain", hjust=0, size=base_size*1.33),
  plot.subtitle = element_text(margin=margin(0,0,.5,0, "lines"),
  face="plain", size=base_size, hjust=0),
  plot.caption = element_text(margin=margin(0,3,0,0, "lines"),
  size=base_size*0.8, hjust=0),
  
  strip.background = element_rect(),
  strip.text = element_text(face="bold", hjust=0, size=base_size),
  
  axis.text = element_text(size=base_size),
  axis.text.x = element_text(color=NULL, size=base_size*0.9),
  axis.text.y = element_text(color=NULL, hjust=0),
  axis.title.x = element_text(color=base_color, size=base_size, 
    face="bold", hjust=1, vjust=-2),
  axis.title.y = element_blank(),
  axis.ticks = element_line(color=NULL),
  axis.ticks.length = unit(0.25, "lines"),
  axis.ticks.y = element_blank(),
  axis.ticks.x = element_line(color=NULL),
  axis.line = element_line(),
  axis.line.y = element_blank(),
  
  legend.background = element_rect(fill=NA, color=NA),
  legend.box = "horizontal",
  legend.box.background = element_rect(fill=NA, color=NA),
  legend.title = element_text(size=base_size*0.9, margin=margin(.5,0,.5,0, "lines")),
  legend.title.position = "top",
  legend.margin = margin(0.25,0,0,0, "lines"),
  legend.key.size = unit(0.8, "lines"),
  legend.key.width = unit(base_size*2, "pt"),
  legend.text = element_text(size=base_size*0.9, hjust=1),
  legend.position="top", legend.justification=0,
  legend.direction="horizontal",
  ...
)
  
  
#' MBLabs discrete color scale
#'
#' Custom `ggplot` discrete color scales to match active branding.
#'
#' @inheritDotParams ggplot2::discrete_scale
#'
#' @importFrom ggplot2 discrete_scale
#' @seealso scale_labs_df
#' @return a color scale
#' @export
scale_brand_dc <- function(...) discrete_scale("color", palette=labs.colors, ...)


#' MBLabs fill color scale
#'
#' Custom `ggplot` discrete fill color scales to match active branding.
#'
#' @inheritDotParams ggplot2::discrete_scale
#'
#' @importFrom ggplot2 discrete_scale
#' @seealso scale_labs_dc
#' @return a fill scale
#' @export
scale_brand_df <- function(...) discrete_scale("fill", palette=labs.colors, ...)


#' MBLabs continuous color scale
#'
#' Custom `ggplot2` continnuous color scale to match active branding.
#'
#' @inheritParams pal
#' @inheritParams scales::alpha
#' @inheritDotParams ggplot2::scale_colour_gradientn
#' @importFrom ggplot2 scale_colour_gradientn
#' @seealso scale_labs_cf
#' @return a color scale
#' @export
scale_brand_cc <- function(
  x = c("orange", "light", "green"), 
  alpha = 0.9, 
  ...) scale_colour_gradientn(
    colours = alpha(pal(x, FALSE), alpha),
    ...)

#' MBLabs continuous fill scale
#'
#' Custom `ggplot2` continnuous fill scale to match active branding.
#'
#' @inheritParams pal
#' @inheritParams scales::alpha
#' @inheritDotParams ggplot2::scale_fill_gradientn
#' @importFrom ggplot2 scale_fill_gradientn
#' @seealso scale_labs_cc
#' @return a fill scale
#' @export
scale_brand_cf <- function(
  x = c("orange", "light", "green"), 
  alpha = 0.9, 
  ...) scale_fill_gradientn(
    colours = alpha(pal(x, FALSE), alpha),
    ...)

#' MBLabs themed `ggplot`
#'
#' Convenience function to generate a themed `ggplot` with custom element sizes,
#' colors and guides matching active branding.
#'
#' @inheritParams ggplot2::ggplot
#' @param pos_x Position of x-axis (bottom or top)
#' @param pos_y Position of y-axis (right or left)
#' @inheritDotParams theme_labs
#'
#' @return A `ggplot2` object with new theme elements applied
#' @import ggplot2
#' @examples
#' require(ggplot2)
#'
#' gglabs(mtcars, aes(factor(carb), mpg, fill=factor(carb))) + 
#'   geom_col() +
#'   guides(y=guide_none(), y.sec=guide_axis()) +
#'   labs(
#'     x = "carb",
#'     title = "My Long and Descriptive Plot Title",
#'     subtitle = "My very long subtitle with many units",
#'     caption = "My very long plot caption with many references.")
#'
#' gglabs(mtcars, aes(wt, mpg)) +
#'   geom_smooth(color=pal("red"), fill=pal("pink")) +
#'   geom_point(size=3, aes(color=carb)) +
#'   labs(
#'   title = "My Title",
#'   subtitle = "My Subtitle",
#'   caption = "My plot caption")
#'
#' gglabs(mtcars, aes(wt, mpg, color=factor(cyl))) +
#'   geom_point(size=3) + 
#'   labs(
#'   x = "cyl",
#'   title = "My Title",
#'   subtitle = "My Subtitle",
#'   caption = "My plot caption")
#'
#' @export
gglabs <- function(
  data = NULL,
  mapping = aes(),
  pos_x = c("bottom", "top"),
  pos_y = c("right", "left"),
  ...) {
    
    pos_x = match.arg(pos_x)
    pos_y = match.arg(pos_y)
    
    options(
      ggplot2.discrete.colour = scale_brand_dc,
      ggplot2.discrete.fill = scale_brand_df,
      ggplot2.continuous.colour = scale_brand_cc,
      ggplot2.continuous.fill = scale_brand_cf
    )
    
    ggplot(data, mapping
    ) + guides(
      x = if(pos_x=="top") guide_none() else guide_axis(),
      x.sec = if(pos_x=="bottom") guide_none() else guide_axis(),
      y = if(pos_y=="right") guide_none() else guide_axis(),
      y.sec = if(pos_y=="left") guide_none() else guide_axis()
      
    ) + theme_labs(...)
  }
  
  