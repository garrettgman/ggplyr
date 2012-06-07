#' plyr_stats is just like ggplot2's internal method calculate_stats but it splits on combinations of PANEL and GLYPH instead of just PANEL
plyr_stats <- function (panel, data, layers) {
    lapply(seq_along(data), function(i) {
        d <- data[[i]]
        l <- layers[[i]]
        ddply(d, c("PANEL", "GLYPH"), function(panel_data) {
            scales <- ggplot2:::panel_scales(panel, panel_data$PANEL[1])
            l$calc_statistic(panel_data, scales)
        })
    })
}