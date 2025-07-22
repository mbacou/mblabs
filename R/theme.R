# Approach used in thematic package
.globals <- new.env(parent = emptyenv())

#' Default Bootstrap branding for Mel B. Labs
#'
#' Branding is read from a user-specified `_brand.yml` file or from this package if
#' no file is found in the working tree.
#' 
#' @name BRAND
#' @docType data
#' @keywords dataset
#' @references [brand.yml](https://posit-dev.github.io/brand-yml/)
#' @format List of default branding elements (colors, fonts)
#' @examples
#' names(BRAND)
#' 
"BRAND"


#' Fetch Bootstrap branding
#'
#' Make this package compatible with `brand.yml` unified branding features. Branding can be read from a configuration file, else it will use one of these package built-in themes. Refer to [brand.yml](https://posit-dev.github.io/brand-yml/) documentation.
#'
#' @param file path to `_brand.yml` brand configuration file, normally this file is auto-detected in the working tree, but may be specified here to swap branding dynamically
#' @param font one of `_brand.yml` font families (`base`, `monospace`, `headings`). Uses the brand's `base` family by default.
#' @importFrom yaml read_yaml
#' @return A list of branding elements
#' @references [brand.yml](https://posit-dev.github.io/brand-yml/)
#' @examples
#' scales::show_col(unlist(brand()$color$palette))
#' .globals$brand$meta$name
#'
#' @export
brand <- function(file = "_brand.yml", font=c("base", "monospace", "headings")) {
  font = match.arg(font)
  b = if(file.exists(file)) { 
    read_yaml(file) 
  } else {
    message("No `", file, "` config found in the working tree. Using default `",
    BRAND$meta$name,  "` theme instead.")
    BRAND
  }
  
  if(!all(c("color", "typography") %in% names(b))) { 
    stop("Boostrap branding needs color and font definitions.")
  }
  
  b$font = b$typography[[font]]
  .globals$brand = b
  invisible(b)
}


#' Bootstrap color palette
#'
#' Color palette extracted from the active Bootstrap theme. By default colors are read from an external `_brand.yaml` configuration file (or uses this package defaults).
#'
#' @param x Color index or name(s), if missing returns the entire color palette
#' @param named keep color names (default: TRUE)
#' @importFrom scales alpha
#' @return A vector of (named) hex color codes extracted from Bootstrap branding
#' @references [brand.yml](https://posit-dev.github.io/brand-yml/)
#' @examples
#' scales::show_col(pal())
#' scales::show_col(pal(c("orange", "red")))
#'
#' @export
pal <- function(x = NULL, named = TRUE) {
  b = .globals$brand
  b = if(is.null(b)) brand() else b
  e = if(missing(x)) b$color$palette else b$color$palette[x]
  e = if(named) e else unname(e)
  unlist(e)
}


#' Bootstrap color ramp
#'
#' Qualitative color ramp derived from active branding. This ramp excludes Bootstrap's
#' **white**, **black**, **light** and **gray** colors, which are typically used for
#' textual elements. By default the color ramp is 90% transparent.
#'
#' @param n number of colors to interpolate
#' @inheritParams grDevices::colorRampPalette
#' @inheritDotParams grDevices::colorRampPalette
#' @importFrom scales alpha
#' @return vector of n interpolated colors
#' @examples
#' scales::show_col(brand.colors(alpha=1))
#' scales::show_col(brand.colors(11, alpha=1, interpolate="spline"))
#' scales::show_col(brand.colors(16))
#' scales::show_col(brand.colors(4, alpha=.5))
#'
#' @export
brand.colors <- function(n = NULL, colors = pal(), alpha = .9, ...) {
  omit = c("white", "black", "gray")
  if(missing(n)) return(colors[!names(colors) %in% omit])
  colors = colors[!names(colors) %in% omit]
  alpha(colorRampPalette(unname(colors), ...)(n), alpha)
}


#' Discrete color scale with Bootstrap colors
#'
#' Custom `ggplot2` discrete color scale to match active Bootstrap brand.
#'
#' @inheritDotParams ggplot2::discrete_scale
#'
#' @importFrom ggplot2 discrete_scale
#' @seealso scale_labs_df
#' @return a color scale
#' @export
scale_brand_dc <- function(...) {
  discrete_scale("color", palette=brand.colors, ...)
}


#' Discrete fill scale with Bootstrap colors
#'
#' Custom `ggplot2` discrete fill color scale to match active Bootstrap brand.
#'
#' @inheritDotParams ggplot2::discrete_scale
#'
#' @importFrom ggplot2 discrete_scale
#' @seealso scale_labs_dc
#' @return a fill scale 
#' @export
scale_brand_df <- function(...) {
  discrete_scale("fill", palette=brand.colors, ...)
}


#' Continuous color scale with Bootstrap colors
#'
#' Custom `ggplot2` continuous color scale to match active Bootstrap brand.
#'
#' @inheritParams pal
#' @inheritDotParams ggplot2::scale_colour_gradientn
#' @importFrom ggplot2 scale_colour_gradientn
#' @seealso scale_labs_cf
#' @return a color scale
#' @export
scale_brand_cc <- function(x = c("orange", "light", "green"), ...) {
  scale_colour_gradientn(colours = pal(x, FALSE), ...)
}


#' Continuous fill scale with Bootstrap colors
#'
#' Custom `ggplot2` continuous fill scale to match active Bootstrap brand.
#'
#' @inheritParams pal
#' @inheritDotParams ggplot2::scale_fill_gradientn
#' @importFrom ggplot2 scale_fill_gradientn
#' @seealso scale_labs_cc
#' @return a fill scale
#' @export
scale_brand_cf <- function(x = c("orange", "light", "green"), ...) {
  scale_fill_gradientn(colours = pal(x, FALSE), ...)
}


#' Apply Bootstrap brand to base, lattice and ggplot2 graphics
#'
#' Applies Bootstrap branding to R graphics using `thematic` R package utilities. This function behaves like `thematic::thematic_on()` but instead of passing individual colors and fonts, the user can provide an external `_brand.yml` configuration file. `brand_on` takes color and font variable names per Boostrap branding (hence, do not provide hex color codes, edit `_brand.yml` instead). 
#' 
#' Typically charts will use Boostrap **sans-serif** font, but as of compiling that variable is not available in `brand.yml` schema, so `brand_theme` will take the **first** font declared in the **typography** tree.
#'
#' @inheritParams brand
#' @param gradient Vector of Bootstrap color (names) to use in plot gradients
#' @param n Number of colors to interpolate in plot gradients (default: 20)
#' @param alpha Transparency for color scales between 0 and 1 (default: .9)
#' @inheritParams thematic::thematic_on
#' @importFrom thematic thematic_on thematic_off font_spec thematic_theme
#' @importFrom scales alpha
#' @importFrom showtext font_add_google showtext_auto
#' @return a theme object as a list
#' @examples
#' thematic::thematic_set_theme(brand_theme())
#' 
#' # base
#' hist(rchisq(100, df=4), freq=FALSE, ylim=c(0, 0.2),
#' col=1:11, border="white", xlab=NA)
#' grid(NA, NULL, col="white")
#' curve(dchisq(x, df=4), col=3, lty=2, lwd=2, add=TRUE)
#'
#' # lattice
#' lattice::show.settings()
#'
#' # ggplot2
#' require(ggplot2)
#' ggplot(mtcars, aes(factor(carb), mpg, fill=carb)) +
#'   geom_col() +
#'   labs(
#'     x = "carb", y = NULL,
#'     title = "Default Plot with Bootstrap Branding",
#'     subtitle = "My very long subtitle with many units",
#'     caption = "My very long plot caption with many references.")
#' 
#' ggplot(mtcars, aes(factor(carb), mpg, fill=carb)) +
#'   geom_col()
#' 
#' ggplot(mtcars, aes(factor(carb), mpg, fill=carb)) +
#'   geom_col() +
#'   guides(y=guide_axis(position="right")) +
#'   theme_brand(base_bg="light")
#' 
#' @export
brand_theme <- function(
  file = NULL,
  font = NULL,
  bg = "background", 
  fg = "foreground", 
  accent = c("primary", "secondary"), 
  sequential = NULL, qualitative = NULL,
  gradient = c("orange", "light", "green"), n = 20, alpha = .9,
  ...
) {

  # Reuse existing brand if any, else use file or this package defaults
  b = if(missing(file)) brand(font=font) else brand(file, font)
  p = unlist(b$color$palette)
  
  # Set sensible arguments to thematic_on
  bg = if(is.na(p[bg])) p[ b$color[[bg]] ] else p[bg]
  fg = if(is.na(p[fg])) p[ b$color[[fg]] ] else p[fg]
  accent = if(is.na(p[accent[1]])) p[ unlist(b$color[accent]) ] else p[accent]
  bg = if(is.na(bg)) "transparent" else bg
  fg = if(is.na(fg)) "black" else fg
  
  # Gradient scale
  sequential = if(missing(sequential)) {
    alpha(colorRampPalette(p[gradient])(n), alpha) 
  } else sequential
  
  # Qualitative palette
  qualitative = if(missing(qualitative)) {
    brand.colors(alpha=alpha) 
  } else qualitative
  
  args = list(bg=unname(bg), fg=unname(fg), accent=unname(accent), 
    font=font_spec(b$font), 
    sequential=sequential, qualitative=qualitative, ...)
  do.call(thematic_theme, args)
}


#' Bootsrap branded ggplot theme
#'
#' Opinionated `ggplot2` theme with unified Bootstrap branding using external `_brand.yml` configuration. This theme does not load any branding, explicitely call `mblabs::brand()` or `mblabs::pal()` instead.
#'
#' @inheritParams ggthemes::theme_foundation
#' @param base_bg plot, panel, legend background
#' @param base_color color for text and line elements
#' @param grid show gridlines `XY`, `X`, `Y` (default) or `n` for no gridline
#' @param legend shorthand for `theme(legend.position="...")` (default: `top`)
#' @inheritDotParams ggplot2::theme
#'
#' @return A ggplot2 theme
#' @importFrom ggthemes theme_foundation
#' @importFrom stringr str_detect
#' @importFrom sysfonts font_families font_add_google
#' @importFrom showtext showtext_auto
#' @examples
#' require(ggplot2)
#'
#' ggplot(mtcars, aes(factor(carb), mpg, fill=factor(carb))) +
#'   geom_col() +
#'   guides(y=guide_axis(position="right")) +
#'   labs(
#'     title = "Plot with default fonts and color palette",
#'     subtitle = "My very long subtitle with many units",
#'     caption = "My very long plot caption with many references.") +
#'     theme_brand()
#'
#' ggplot(mtcars, aes(factor(carb), mpg, fill=carb)) + geom_col() +
#'   guides(y=guide_axis(position="right")) +
#'   labs(
#'     title = "Same Plot with no Bootstrap Branding",
#'     subtitle = "My very long subtitle with many units",
#'     caption = "My very long plot caption with many references.") +
#'     theme_brand(grid="XY", 
#'       base_color="gray", base_bg="white", base_family="Pacifico")
#' 
#' ggplot(mtcars, aes(factor(carb), mpg, fill=carb)) + geom_col() +
#'   guides(y=guide_axis(position="right")) +
#'   labs(
#'     title = "Same plot with Bootstrap Branding",
#'     subtitle = "My very long subtitle with many units",
#'     caption = "My very long plot caption with many references.") +
#'     theme_brand(base_color="light", base_bg="dark")
#'
#' @export
theme_brand <- function(
  base_size = 12,
  base_family = NULL,
  base_bg = NULL,
  base_color = NULL,
  grid = c("Y", "X", "XY", "n"),
  legend = c("top", "bottom", "right", "left"),
  ...
) {
  
  grid = match.arg(grid)
  legend = match.arg(legend)
  
  if(is.null(.globals$brand)) {
    message("Not using any Bootstrap branding. 
      Call `brand()` or `pal()` explicitely to modify session environment.")
  } else {
    base_bg = if(missing(base_bg)) NULL else pal(base_bg)
    base_color  = if(missing(base_color)) NULL else pal(base_color)
    base_family = if(missing(base_family)) .globals$brand$font else base_family
  }
  
  # Get font if noy found, assume Google font
  if(!is.null(base_family) && !base_family %in% font_families()) {
    font_add_google(base_family)
    showtext_auto()
  }
  
  theme_foundation(
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
    panel.grid.minor = element_blank(),
    
    panel.grid.major.x = if(str_detect(grid, "X")) element_line() 
      else element_blank(),
    panel.grid.major.y = if(str_detect(grid, "Y")) element_line() 
      else element_blank(),
    
    plot.title = element_text(face="plain", hjust=0, size=base_size*1.33),
    plot.subtitle = element_text(margin=margin(0,0,1,0, "lines"),
      face="plain", size=base_size, hjust=0),
    plot.caption = element_text(margin=margin(0,3,0,0, "lines"),
      size=base_size*0.8, hjust=0, lineheight=1),
    
    strip.background = element_rect(),
    strip.text = element_text(face="bold", hjust=0, size=base_size),
    
    axis.text = element_text(size=base_size),
    axis.text.x = element_text(color=NULL, size=base_size*0.9),
    axis.text.y = element_text(color=NULL, hjust=0),
    axis.title.x = element_text(color=base_color, size=base_size, face="bold", hjust=1, vjust=-2),
    axis.title.y = element_blank(),
    axis.ticks = element_line(color=NULL),
    axis.ticks.length = unit(0.25, "lines"),
    axis.ticks.y = element_blank(),
    axis.ticks.x = element_line(color=NULL),
    axis.line = element_line(),
    axis.line.y = element_blank(),
    
    legend.background = element_rect(fill=NA, color=NA),
    legend.box.background = element_rect(fill=NA, color=NA),
    legend.title = element_text(size=base_size*0.9, margin=margin(0,0,.75,0, "lines")),
    legend.title.position = "top",
    legend.text = element_text(size=base_size*0.8, hjust=1),
    legend.position = legend, legend.justification = 0
  ) + 
  theme(...)
}


#' Modified ggplot with Bootstrap branding
#'
#' Opinionated `ggplot2` with custom element sizes and color scales derived from Bootstrap branding (per `_brand.yml`). Behind the scenes `gglabs` uses `theme_brand()` to control colors and fonts, and has Y-axis on the right. Axis placement can be modified with argument `axes`.
#'
#' @inheritParams ggplot2::ggplot
#' @param axes 2-length vector to control the position of X (`bottom` or `top`) and Y (`left` or `right`) axes (default: `c("bottom", "right")`)
#' @inheritDotParams theme_brand
#'
#' @return A `ggplot2` object with new theme elements applied
#' @import ggplot2
#' @importFrom data.table `%ilike%`
#' @examples
#' require(ggplot2)
#'
#' gglabs(mtcars, aes(factor(carb), mpg, fill=factor(carb))) +
#'   geom_col() +
#'   labs(
#'     x = "carb",
#'     title = "Default Plot with Y-axis on the Right",
#'     subtitle = "My very long subtitle with many units",
#'     caption = "My very long plot caption with many references.")
#' 
#' # Equivalent to below
#' ggplot(mtcars, aes(factor(carb), mpg, fill=factor(carb))) +
#'   geom_col() +
#'   scale_brand_df() +
#'   guides(y=guide_axis(position="right")) +
#'   theme_brand(grid="XY")
#' 
#' gglabs(mtcars, aes(wt, mpg, color=carb), axes="topright") +
#'   geom_smooth(color=pal("red"), fill=pal("pink")) +
#'   geom_point(size=3) +
#'   guides(color=guide_legend(nrow=1) +
#'   labs(
#'     title = "My Beautiful Plot with X-axis at the Top",
#'     subtitle = "My descriptive subtitle with units",
#'     caption = "My plot caption with many references.")
#'
#' gglabs(mtcars, aes(wt, mpg, color=factor(cyl)), 
#'   grid="XY", legend="bottom", base_family="Pacifico") +
#'   geom_point(size=3) +
#'   labs(
#'     x = "cyl",
#'     title = "My Plot with Full Gridlines and Pacifico Font",
#'     subtitle = "Placed the legend at the bottom",
#'     caption = "My plot caption with many references.")
#'
#' @export
gglabs <- function(
  data = NULL,
  mapping = aes(),
  axes = c("bottomright", "topright", "bottomleft", "topleft"),
  ...
) {
  
  axes = match.arg(axes)
  
  ggplot(data, mapping
  ) + guides(
    x = guide_axis(position = if(axes %ilike% "top") "top" else "bottom"),
    y = guide_axis(position = if(axes %ilike% "right") "right" else "left")
  ) + theme_brand(...)
  
}

