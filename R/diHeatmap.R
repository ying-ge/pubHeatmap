#' diHeatmap
#'
#'@param top the top left heatmap matrix
#'@param bottom the bottom right heatmap matrix
#'@param topCol the color of top left heatmap
#'@param bottomCol the color of bottom right heatmap
#'@return
#'@export
#'@examples
#'data(indel)
#'diHeatmap(top, bottom)

diHeatmap <- function(top = top, bottom = bottom,
                      topCol= c("#FFFADD","#AB221F"), bottomCol = c("#FFFADD","#3878C1")){
  # check identical
  identical(rownames(top), rownames(bottom))
  identical(colnames(top), colnames(bottom))

  # sort rowname
  RightOrder <- rev(rownames(top))
  top <- top[RightOrder,]
  bottom <- bottom[RightOrder,]

  # color
  topColor <- colorRamp2(breaks = c(0, 1), colors = topCol)
  bottomColor <- colorRamp2(breaks = c(0, 1), colors = bottomCol)

  # build an empty matrix
  Heatmap(top,
          column_title = "Copy number variation across cancer types", ## 列的标题
          rect_gp = gpar(type = "none"),  #绘制空的数据框
          show_heatmap_legend = F, ##是否显示基本的注释说明
          cluster_rows = T, cluster_columns = T, ## 是否对行列进行聚类
  )

  # annotation
  row_an <-  HeatmapAnnotation(type = c(rep("R", 10), rep("W", 8), rep("E", 2)), ##注释信息的内容。
                               show_annotation_name = F, ## 是否显示注释的标题
                               col = list(type = c("R" = "#5AC9FA", "W" = "#FAC67A", "E" = "#51B743")), ## 注释信息的颜色
                               show_legend = T,  ## 是否显示注释信息的说明
                               annotation_legend_param = list(title = "m6A group",nrow = 1), ## 注释信息图例的个性化说明，nrow表示把所有分类的图例放到一行。
                               which = "row" #对行或者列进行注释
  )

  # draw heatmap
  p1 <- Heatmap(top, column_title = "Copy number variation across cancer types", rect_gp = gpar(type = "none"), show_heatmap_legend = F,  cluster_rows = F, cluster_columns = T, ##绘制空的热图框
                left_annotation = row_an, ##添加左侧注释信息
                cell_fun = diPolygon(top = top, down = bottom) ## 绘制表格内的内容
  ); p1

  # draw legend
  col_fun = colorRamp2(c(-1, 0, 1), c(bottomCol[2], topCol)) ##自定义颜色信息
  lgd <- Legend(title = "CNV Frequency", ## 注释的标题
                col_fun = col_fun, ## 注释的颜色
                at = c(-1,-0.5,0,0.5,1), ## 注释刻度的分组
                labels = c("1","Loss","0","Gain","1"),  ## 注释刻度的重命名
                direction = "horizontal" ## 注释的方向
  )

  # combine
  draw(p1, annotation_legend_list = lgd, annotation_legend_side = "bottom",heatmap_legend_side = "bottom", merge_legend = TRUE)
}
