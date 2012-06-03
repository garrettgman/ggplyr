geom_star <- function(mapping = NULL, data = NULL, stat = "identity", 
  position = "identity", ...) { 

    GeomStar$new(mapping = mapping, data = data, stat = stat, 
      position = position, ...)
}


GeomStar <- proto(ggplot2:::Geom, {
  objname <- "star"
  
  # turn cartesian coordinates polar
  reparameterise <- function(., df, params) {    
    # scale x to be between 0 and 2*pi
    df$theta <- unlist(rescale_2pi(df["angle"]))
    df$r <- unlist(rescale_01(df["r"]))
    
    include_origin <- function(data) {
      data <- data[order(data$theta, data$r), ]
      if (data$theta[1] > 0.01) {
        first <- data[1, ]
        first$theta <- 0
        first$r <- 0
        data <- rbind(first, data)
      }
      if (data$theta[length(data$theta)] < 6.27) {
        last <- data[length(data$theta), ]
        last$theta <- 6.28
        last$r <- 0
        data <- rbind(data, last)
      }
      data
    }
    df <- ddply(df, c("group", "PANEL"), include_origin)
    
    df$x <- df$r * cos(df$theta) + df$x
    df$y <- df$r * sin(df$theta) + df$y
    df
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
  
  required_aes <- c("x", "y", "r", "angle")
  
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
    
    missing <- !(c("x", "y", "r", "angle") %in% names(mapping))
    if (any(missing)) {
      stop(paste("Missing required aesthetics for geom_star:",
        paste(c("x", "y", "r", "angle")[missing], collapse = ", ")),
        call. = FALSE)
    }
    
    do.call("layer", list(mapping = mapping, data = data, stat = stat, 
      geom = ., position = position, ...))  
  }
  
})