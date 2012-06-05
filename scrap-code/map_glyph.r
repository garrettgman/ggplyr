map_glyph <- function(data, layer){
  if("map_glyphs" %in% ls(layer)) {
    data <- layer$map_glyphs(data)
  }
  data
}