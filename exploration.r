# okay this works
	
	
f <- qplot(Fertility, Education, data = test.data, color = rank(Catholic), size = I(3)) + facet_wrap(c("lat", "long"))

f <- qplot(Fertility, data = test.data, size = I(3)) + facet_wrap(c("lat", "long"))

fplot <- ggplot_build(f)

fdata <- fplot$data[[1]]

gdata <- ddply(test.data, c("lat", "long"), summarise, X = mean(Fertility), Y = mean(Education))
gdata$.gid <- id(gdata[c("lat", "long")], drop = TRUE)
names(fdata)[names(fdata) == "PANEL"] <- ".gid"

data <- join(fdata, gdata, by = ".gid")

data[c(".gid", "x", "y")] <- ddply(data, ".gid", summarize, x = (x - mean(x)) / var(x) + X, y = (y - mean(y)) / var(y) + Y)

data$PANEL <- 1

g <- qplot(x, y, data = data)
panelist <- ggplot_build(g)$panel

fplot$data <- list(data)
fplot$panel <- panelist
# fplot$plot <- g
fplot$plot$facet <- facet_null()
fplot$plot$scales$scales[[2]] <- panelist$x_scales[[1]]
fplot$plot$scales$scales[[3]] <- panelist$y_scales[[1]]
fplot
gshow(fplot)

gshow <- function(data, newpage = is.null(vp), vp = NULL, ...){
    if (newpage) 
        grid.newpage()
    gtable <- ggplot_gtable(data)
    if (is.null(vp)) {
        grid.draw(gtable)
    }
    else {
        if (is.character(vp)) 
            seekViewport(vp)
        else pushViewport(vp)
        grid.draw(gtable)
        upViewport()
    }
    invisible(data)
}


f <- qplot(Fertility, data = test.data, size = I(3)) + facet_wrap(c("lat", "long"))

fplot <- ggplot_build(f)

fdata <- fplot$data[[1]]

gdata <- ddply(test.data, c("lat", "long"), summarise, X = mean(Fertility), Y = mean(Education))
gdata$.gid <- id(gdata[c("lat", "long")], drop = TRUE)
names(fdata)[names(fdata) == "PANEL"] <- ".gid"

data <- join(fdata, gdata, by = ".gid")

data[c(".gid", "x", "xmin", "xmax", "y", "ymin", "ymax")] <- ddply(data, ".gid", summarize, 
	x = (x - mean(x)) / sqrt(var(x)) + X, 
	xmin = (xmin - mean(x)) / sqrt(var(x)) + X, 
	xmax = (xmax - mean(x)) / sqrt(var(x)) + X, 
	y = (y - mean(y)) / var(y) + Y,
	ymin = (ymin - mean(y)) / var(y) + Y, 
	ymax = (ymax - mean(y)) / var(y) + Y)

data$PANEL <- 1

g <- qplot(c(data$x, data$xmin, data$xmax), c(data$y, data$ymin, data$ymax))
panelist <- ggplot_build(g)$panel

fplot$data <- list(data)
fplot$panel <- panelist
# fplot$plot <- g
fplot$plot$facet <- facet_null()
fplot$plot$scales$scales[[2]] <- panelist$x_scales[[1]]
fplot$plot$scales$scales[[3]] <- panelist$y_scales[[1]]
fplot
gshow(fplot)