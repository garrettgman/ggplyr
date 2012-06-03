geom_coxcomb <- function(mapping = NULL, data = NULL, stat = "bin", 
  position = "identity", ...) { 
  
    GeomCoxcomb$new(mapping = mapping, data = data, stat = stat, 
      position = position, ...)
}

GeomCoxcomb <- proto(ggplot2:::Geom, {
  objname <- "coxcomb"
  
  default_stat <- function(.) StatBin
  default_pos <- function(.) PositionIdentity
  default_aes <- function(.) aes(colour=NA, fill="grey20", size=0.5, linetype=1, 
    weight = 1, alpha = NA)
  
  required_aes <- c("x")
  
  reparameterise <- function(., df, params) {
    df <- transform(df, 
      xmin = 2 * pi / max(x) * (x - 1), 
      xmax = 2 * pi / max(x) * x)
    
    # to create equal areas
    adjust_y <- function(df) {
      if (length(df$y) == 1) {
        df$ymin <- 0
        df$ymax <- df$y
        return(df)
      }
      span <- df$xmax[1] - df$xmin[1]
      df$y[1] <- sqrt(2 / span * df$y[1])
      for ( i in 2:length(df$y)) {
        df$y[i] <- sqrt(2 / span * df$y[i - 1])
      }
      df$ymax <- cumsum(df$y)
      df$ymin <- c(0, df$ymax[-length(df$y)])
      df
    }
    df <- ddply(df, c("x", "PANEL"), adjust_y)
    df$section <- id(df[c("x", "group")], drop = TRUE)
    
    # create polygon points
    poly_curve <- function(df, npoints) {
      non.pos <- setdiff(names(df), c(.x_aes, .y_aes, "count", "ndensity", 
        "ncount", "density"))
      theta <- seq(df$xmin, df$xmax, length = npoints)
      theta <- c(theta, theta[length(theta):1])
      r <- c(rep(df$ymin, npoints), rep(df$ymax, npoints))
      x <- r*cos(theta)
      y <- r*sin(theta)
      df <- df[1, non.pos]
      row.names(df) <- NULL
      df <- cbind(df, x, y)
      rbind(df, df[1, ])
    }
    ddply(df, c("section", "group", "PANEL"), poly_curve, params$npoints)
  }
  
  draw <- draw_groups <- function(., data, scales, coordinates, ...) {
    polys <- dlply(data, c("section", "PANEL"), function(df) {
      ggname("polygon", gTree(children=gList(
        with(coord_munch(coordinates, df, scales), 
             polygonGrob(x, y, default.units="native",
                         gp=gpar(col=colour, fill=alpha(fill, alpha), lwd=size * .pt,
                                 lty=linetype))
        )
      )))
    })
    ggname("coxcomb", do.call("grobTree", polys))               
  }
  
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
    position = NULL, npoints = 10, ...) {
    
    missing <- !(c("r") %in% names(mapping))
    if (any(missing)) {
      stop("Missing required aesthetic: r", call. = FALSE)
    }
    names(mapping)[names(mapping) == "r"] <- "x"
    mapping$section <- coxcomb_sections(mapping)
    
    lyr <- do.call("layer", list(mapping = mapping, data = data, stat = stat, geom = ., 
      position = position, ...))  
    lyr$geom_params$npoints <- npoints
    lyr
  }
  
})

coxcomb_sections <- function(mapping) {
  sections <- mapping[c("alpha", "fill", "colour")]
  sections <- sections[!unlist(lapply(sections, is.null))]
  names(sections) <- NULL
  if (is.null(sections)) return(NULL)
  as.call(c(quote(interaction), sections))
}