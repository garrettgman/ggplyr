\name{geom_violin}
\alias{geom_violin}
\title{# Violin plots with continuous x:
# Use the group aesthetic to group observations in violins
qplot(year, budget, data = movies, geom = "violin")
qplot(year, budget, data = movies, geom = "violin",
group = round_any(year, 10, floor))
}}
\usage{
  geom_violin(mapping = NULL, data = NULL, stat =
  "ydensity", position = "dodge", trim = TRUE, scale =
  "equal", ...)
}
\description{
  # Violin plots with continuous x: # Use the group
  aesthetic to group observations in violins qplot(year,
  budget, data = movies, geom = "violin") qplot(year,
  budget, data = movies, geom = "violin", group =
  round_any(year, 10, floor)) }
}

