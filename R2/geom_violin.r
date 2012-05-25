#' # Violin plots with continuous x:
#' # Use the group aesthetic to group observations in violins
#' qplot(year, budget, data = movies, geom = "violin")
#' qplot(year, budget, data = movies, geom = "violin", 
#'   group = round_any(year, 10, floor))
#' }
geom_violin1 <- function (mapping = NULL, data = NULL, stat = "ydensity", position = "dodge",
trim = TRUE, scale = "equal", ...) {
  GeomViolin$new(mapping = mapping, data = data, stat = stat, 
  position = position, trim = trim, scale = scale, ...)
}

GeomViolin1 <- proto(ggplot2:::Geom, {
  objname <- "violin"

  reparameterise <- function(., df, params) {
  	browser()
    df$width <- df$width %||% 
      params$width %||% (resolution(df$x, FALSE) * 0.9)

    # ymin, ymax, xmin, and xmax define the bounding rectangle for each group
    ddply(df, .(group), transform,
          ymin = min(y),
          ymax = max(y),
          xmin = x - width / 2,
          xmax = x + width / 2)

  }
  
  draw <- function(., data, ...) { 

    # Find the points for the line to go all the way around
    data <- transform(data, xminv = x - scaled * (x-xmin),
                            xmaxv = x + scaled * (xmax-x))

    # Make sure it's sorted properly to draw the outline
    newdata <- rbind(arrange(transform(data, x = xminv), y),
                     arrange(transform(data, x = xmaxv), -y))

    # Close the polygon: set first and last point the same
    # Needed for coord_polar and such
    newdata <- rbind(newdata, newdata[1,])
browser()
    ggname(.$my_name(), GeomPolygon$draw(newdata, ...))
  }

  guide_geom <- function(.) "polygon"

  default_stat <- function(.) StatYdensity
  default_pos <- function(.) PositionDodge
  default_aes <- function(.) aes(weight=1, colour="grey20", fill="white", size=0.5, alpha = NA, linetype = "solid")
  required_aes <- c("x", "y")

})