diPolygon <- function(top, down){
  function(j, i, x, y, width, height, fill){
    grid.polygon(unit.c(x - 0.5*width, x - 0.5*width, x + 0.5*width),
                 unit.c(y - 0.5*height, y + 0.5*height, y + 0.5*height),
                 gp = gpar(fill = bottomColor(down[i, j]), col = "grey"))
    grid.polygon(unit.c(x + 0.5*width, x + 0.5*width, x - 0.5*width),
                 unit.c(y + 0.5*height, y - 0.5*height, y - 0.5*height),
                 gp = gpar(fill = topColor(top[i, j]), col = "grey"))
  }
}
