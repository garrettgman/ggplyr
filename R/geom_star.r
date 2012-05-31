geom_star <- function(mapping = NULL, glyph.by = NULL, data = NULL, 
  stat = "identity", position = "identity", width = rel(0.95), 
  height = rel(0.95), x_scale = identity, y_scale = identity, 
  merge.overlaps = FALSE, reference = NULL, ...) { 
  
  major <- mapping[c("x", "y")]
  mapping$x <- NULL
  mapping$y <- NULL
  
  glyph(
    GeomStar$new(mapping = mapping, data = data, stat = stat, 
      position = position, ...), 
    major.aes = major, glyph.by = glyph.by, 
      width = width, height = height, x_scale = x_scale, y_scale = y_scale, 
      merge.overlaps = merge.overlaps, reference = reference)
}


GeomStar <- proto(ggplot2:::Geom, {
  objname <- "star"
  
  # turn cartesian coordinates polar
  reparameterise <- function(., df, params) {
    # scale x to be between 0 and 2*pi
    df$theta <- unlist(rescale_2pi(df["x"]))
    df$r <- df$y
    df$x <- df$r * cos(df$theta)
    df$y <- df$r * sin(df$theta)	
    
    include_origin <- function(data) {
      data <- data[order(data$theta, data$r), ]
      if (data$theta[1] > 0.01) {
        first <- data[1, ]
        first$x <- 0
        first$y <- 0
        data <- rbind(first, data)
      }
      data
    }
    ddply(df, "PANEL", include_origin)
  }
  
  draw <- function(., data, scales, coordinates, ...) {
    data <- data[order(data$theta, data$r), ]
    ggname(.$my_name(), 
      gTree(children = gList(
        with(coord_munch(coordinates, data, scales), 
          polygonGrob(x, y, default.units = "native", 					
            gp = gpar(col = colour, fill = alpha(fill, alpha), 
            lwd = size * .pt, lty = linetype)
          )
        )
      ))
    )
  }	
  
  default_stat <- function(.) StatIdentity
  
  default_aes <- function(.) {
    aes(weight = 1, colour = "grey20", fill = "NA", alpha = NA, 
      linetype = "solid", size = 0.5)
  }
  
  required_aes <- c("x", "y")
  
  guide_geom <- function(.) "polygon"
  
  draw_legend <- function(., data, ...) {
    data <- aesdefaults(data, .$default_aes(), list(...))
    with(data, grobTree(
      rectGrob(gp = gpar(col = colour, fill = alpha(fill, alpha), 
        lty = linetype)),
      linesGrob(gp = gpar(col = colour, lwd = size * .pt, 
        lineend="butt", lty = linetype))
    ))
  }
  
  new <- function(., mapping = NULL, data = NULL, stat = NULL, 
    position = NULL, ...) {
    
    missing <- !(c("r", "angle") %in% names(mapping))
    if (any(missing)) {
      stop(paste("Missing required aesthetic(s):", 
        paste(c("r", "angle")[missing], collapse = ", ")), call. = FALSE)
    }
    names(mapping)[names(mapping) == "angle"] <- "x"
    names(mapping)[names(mapping) == "r"] <- "y"
    
    do.call("layer", list(mapping = mapping, data = data, stat = stat, 
      geom = ., position = position, ...))  
  }
  
})