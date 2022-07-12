

#### leaflet decreasing legend function from mpriem89::- https://github.com/rstudio/leaflet/issues/256#issuecomment-440290201

addLegend_decreasing <- function (map, position = c("topright", "bottomright", "bottomleft","topleft"),
                                  pal, values, na.label = "NA", bins = 7, colors, 
                                  opacity = 0.5, labels = NULL, labFormat = labelFormat(), 
                                  title = NULL, className = "info legend", layerId = NULL, 
                                  group = NULL, data = getMapData(map), decreasing = FALSE) {
  
  position <- match.arg(position)
  type <- "unknown"
  na.color <- NULL
  extra <- NULL
  if (!missing(pal)) {
    if (!missing(colors)) 
      stop("You must provide either 'pal' or 'colors' (not both)")
    if (missing(title) && inherits(values, "formula")) 
      title <- deparse(values[[2]])
    values <- evalFormula(values, data)
    type <- attr(pal, "colorType", exact = TRUE)
    args <- attr(pal, "colorArgs", exact = TRUE)
    na.color <- args$na.color
    if (!is.null(na.color) && col2rgb(na.color, alpha = TRUE)[[4]] == 
        0) {
      na.color <- NULL
    }
    if (type != "numeric" && !missing(bins)) 
      warning("'bins' is ignored because the palette type is not numeric")
    if (type == "numeric") {
      cuts <- if (length(bins) == 1) 
        pretty(values, bins)
      else bins   
      if (length(bins) > 2) 
        if (!all(abs(diff(bins, differences = 2)) <= 
                 sqrt(.Machine$double.eps))) 
          stop("The vector of breaks 'bins' must be equally spaced")
      n <- length(cuts)
      r <- range(values, na.rm = TRUE)
      cuts <- cuts[cuts >= r[1] & cuts <= r[2]]
      n <- length(cuts)
      p <- (cuts - r[1])/(r[2] - r[1])
      extra <- list(p_1 = p[1], p_n = p[n])
      p <- c("", paste0(100 * p, "%"), "")
      if (decreasing == TRUE){
        colors <- pal(rev(c(r[1], cuts, r[2])))
        labels <- rev(labFormat(type = "numeric", cuts))
      }else{
        colors <- pal(c(r[1], cuts, r[2]))
        labels <- rev(labFormat(type = "numeric", cuts))
      }
      colors <- paste(colors, p, sep = " ", collapse = ", ")
    }
    else if (type == "bin") {
      cuts <- args$bins
      n <- length(cuts)
      mids <- (cuts[-1] + cuts[-n])/2
      if (decreasing == TRUE){
        colors <- pal(rev(mids))
        labels <- rev(labFormat(type = "bin", cuts))
      }else{
        colors <- pal(mids)
        labels <- labFormat(type = "bin", cuts)
      }
    }
    else if (type == "quantile") {
      p <- args$probs
      n <- length(p)
      cuts <- quantile(values, probs = p, na.rm = TRUE)
      mids <- quantile(values, probs = (p[-1] + p[-n])/2, na.rm = TRUE)
      if (decreasing == TRUE){
        colors <- pal(rev(mids))
        labels <- rev(labFormat(type = "quantile", cuts, p))
      }else{
        colors <- pal(mids)
        labels <- labFormat(type = "quantile", cuts, p)
      }
    }
    else if (type == "factor") {
      v <- sort(unique(na.omit(values)))
      colors <- pal(v)
      labels <- labFormat(type = "factor", v)
      if (decreasing == TRUE){
        colors <- pal(rev(v))
        labels <- rev(labFormat(type = "factor", v))
      }else{
        colors <- pal(v)
        labels <- labFormat(type = "factor", v)
      }
    }
    else stop("Palette function not supported")
    if (!any(is.na(values))) 
      na.color <- NULL
  }
  else {
    if (length(colors) != length(labels)) 
      stop("'colors' and 'labels' must be of the same length")
  }
  legend <- list(colors = I(unname(colors)), labels = I(unname(labels)), 
                 na_color = na.color, na_label = na.label, opacity = opacity, 
                 position = position, type = type, title = title, extra = extra, 
                 layerId = layerId, className = className, group = group)
  invokeMethod(map, data, "addLegend", legend)
}



#== scatterplot with quantile function

biv_viridis<-c('#e8f4f4','#c1bdd6','#9874a0',#lower layer
               '#def2c1','#86c2c0','#6381a7',#mid layer
               '#fef286','#73cf8e','#218f8c')#upper layer

biv_RdBu<-c("#e8e8e8","#e4acac","#c85a5a",
            "#b0d5df","#ad9ea5","#985356",
            "#64acbe","#627f8c","#574249")#upper layer

biv_PkCn<-c('#e8e8e8','#ace4e4','#5ac8c8',    #lower layer
            '#dfb0d6','#a5add3','#5698b9',#mid layer
            '#be64ac','#8c62aa','#3b4994')#upper layer

biv_GnBu<-c('#e8e8e8','#b5c0da','#6c83b5',#lower layer
            '#b8d6be','#90b2b3','#567994', #mid layer
            '#73ae80','#5a9178','#2a5a5b') #upper layer
# 

bvColors <- c("#f3f3f3","#c2f0ce","#8ae1ae",
              "#eac5dd","#9ec5d3","#7ec5b1",
              "#e6a2d0","#bb9fce","#7a8eae")

  scatterplot_biv <- function(df,var_x, var_y, 
                              lab_x=NA, lab_y=NA,
                              capt=NA,
                              biv_col= c('biv_viridis','biv_RdBu',
                                         'biv_PkCn','biv_GnBu')){
    # palette options
    
    
    h_quan<-quantile(var_x, probs = c(0.33,0.66))# 0%, 33%, 66%, 100% quantiles
    v_quan<-quantile(var_y, probs = c(0.33,0.66))#
    minx<-min(var_x)
    maxx<-max(var_x)
    miny<-min(var_y)
    maxy<-max(var_y)
    
    ptp<-ggplot(df, aes(x=var_x,y=var_y)) + 
      
      annotate("rect", 
               xmin = minx,
               xmax = h_quan[[1]],
               ymin = miny,
               ymax = v_quan[[1]], fill= biv_col[1], alpha = .7)  + 
      annotate("rect",
               xmin = h_quan[[1]],
               xmax = h_quan[[2]],
               ymin = miny,
               ymax = v_quan[[1]],
               fill= biv_col[2], alpha = .7)  + 
      annotate("rect", xmin = h_quan[[2]],
               xmax = maxx,
               ymin = miny,
               ymax = v_quan[[1]], 
               fill= biv_col[3], alpha = .7)  + 
      #=========================================================================================================================#
      annotate("rect",                
               xmin = minx,
               xmax = h_quan[[1]],
               ymin = v_quan[[1]],
               ymax = v_quan[[2]], fill= biv_col[4], alpha = .7)  + 
      annotate("rect",
               xmin = h_quan[[1]],
               xmax = h_quan[[2]],
               ymin = v_quan[[1]],
               ymax = v_quan[[2]], fill= biv_col[5], alpha = .7)  + 
      annotate("rect",
               xmin = h_quan[[2]],
               xmax =  maxx,
               ymin = v_quan[[1]],
               ymax = v_quan[[2]], fill= biv_col[6], alpha = .7)  + 
      #=========================================================================================================================#    
      annotate("rect",  
               xmin = min(var_x),
               xmax = h_quan[[1]],
               ymin = v_quan[[2]],
               ymax = maxy, fill = biv_col[7], alpha = .7)  + 
      annotate("rect", 
               xmin = h_quan[[1]],
               xmax = h_quan[[2]],
               ymin = v_quan[[2]],
               ymax = maxy, fill = biv_col[8], alpha = .7)  + 
      annotate("rect", 
               xmin = h_quan[[2]],
               xmax = maxx,
               ymin = v_quan[[2]],
               ymax = maxy, fill= biv_col[9], alpha = .7)  +
      geom_point(col = 'black', size = 2.2,shape = 21, fill ='grey99') + 
      
      xlim(minx,maxx)+ 
      ylim(miny,maxy)+
      labs(x=lab_x,
           y=lab_y,
           caption=capt)+
      geom_hline(yintercept=v_quan,color="gray20",linetype=2)+
      geom_vline(xintercept=h_quan,color="gray20",linetype=2)+
      theme_bw()
    
    ggplotly(ptp)  %>% style(hoverinfo = "none", traces = 1:9)
  }


  # Header ------------------------------------------------------------------
  # Bivariate choropleth map in R
  # using tmap
  #
  # Bivariate color schemes
  # http://www.joshuastevens.net/cartography/make-a-bivariate-choropleth-map/
  #
  # Author: Stefano De Sabbata
  # Date: 23 November 2018
  
  library(sp)
  library(spdep)
  library(tmap)
  library(classInt)
  library(grid)
  library(gridExtra)
  library(lattice)
  
  # tmap_mode('plot')
  # Color scheme ------------------------------------------------------------
  # Set the colours
  
  
  
  biv_viridis<-c('#e8f4f4','#c1bdd6','#9874a0',#lower layer
                 '#def2c1','#86c2c0','#6381a7',#mid layer
                 '#fef286','#73cf8e','#218f8c')#upper layer
  
  biv_RdBu<-c("#e8e8e8","#e4acac","#c85a5a",
              "#b0d5df","#ad9ea5","#985356",
              "#64acbe","#627f8c","#574249")#upper layer
  
  biv_PkCn<-c('#e8e8e8','#ace4e4','#5ac8c8',    #lower layer
              '#dfb0d6','#a5add3','#5698b9',#mid layer
              '#be64ac','#8c62aa','#3b4994')#upper layer
  
  biv_GnBu<-c('#e8e8e8','#b5c0da','#6c83b5',#lower layer
              '#b8d6be','#90b2b3','#567994', #mid layer
              '#73ae80','#5a9178','#2a5a5b') #upper layer
  # 
  
  bivColors2 <- c("#f3f3f3","#c2f0ce","#8ae1ae",
                  "#eac5dd","#9ec5d3","#7ec5b1",
                  "#e6a2d0","#bb9fce","#7a8eae")
  
  bvColors3 <- c("#e8e8e8","#e4acac","#c85a5a",
                 "#b0d5df","#ad9ea5","#985356",
                 "#64acbe","#627f8c","#574249")
  
  
  
  
  
  # from the following github https://github.com/sdesabbata/BivariateTMap.git

  
  # Print bivariate map -----------------------------------------------------
  # The function below calls the function that creates the map
  # then adds a square legend and prints the plot
  # tmap_mode('view')
  bivariate_choropleth <- function (
    
    # Function parameters
    bivmap_dataset,         # A SpatialPoligonDataFrame
    bivmap_vars,            # A vector of characters containing the name of the two variables
    bivmap_labels=NA,       # A vector of characters containing the labels for the two variables, to use in the legend
    bivmap_style='quantile',# Classification type for the bins
    bvColors,
    bivmap_scale=FALSE,      # Use a scale bar
    biv_title=NA
    
  ) {
    
    # Create the bivatiate map
    bivmap <- get_bivariate_choropleth(
      # Passs parameters on
      # except labels
      bivmap_dataset,         # A SpatialPoligonDataFrame
      bivmap_vars,            # A vector of characters containing the name of the two variables
      bivmap_style='quantile',# Classification type for the bins
      bvColors,
      bivmap_scale=F,
      biv_title=NA
    )
    
    if (is.na(bivmap_labels)){
      bivmap_labels <- bivmap_vars
    }
    
    # Print map
    suppressWarnings(print( bivmap ))
    
    # Create the square legend
    vp <- viewport(x=.8, y=.1, width=.25, height=.25)
    pushViewport(vp)
    print(levelplot(
      matrix(1:9, nrow=3), 
      axes=FALSE, 
      col.regions=bvColors,
      xlab=list(label=bivmap_labels[1],cex=0.8), 
      ylab=list(label=bivmap_labels[2],cex=0.8), 
      cuts=8, 
      colorkey=FALSE,
      scales=list(draw=0)),
      newpage=FALSE)
    
    # Pop viewport
    popViewport()
  }
  
  
  
  # Create bivariate map ----------------------------------------------------
  # This function actually creates the bivariate map using tmap
  
  get_bivariate_choropleth <- function (
    
    # Function parameters
    bivmap_dataset,         # A SpatialPoligonDataFrame
    bivmap_vars,            # A vector of characters containing the name of the two variables
    bivmap_style='quantile',# Classification type for the bins
    bvColors = bivColors2,
    bivmap_scale=F,
    biv_title=NA
    
  ) {
    
    
    # Extract the two specified colums
    # excluding rows with na and infinite values
    #bivmap_sdf <- bivmap_dataset[
    #  !is.na(bivmap_dataset@data[, bivmap_vars[1]]) &
    #    !is.na(bivmap_dataset@data[, bivmap_vars[2]]) &
    #    !is.infinite(bivmap_dataset@data[, bivmap_vars[1]]) &
    #    !is.infinite(bivmap_dataset@data[, bivmap_vars[2]])
    #  ,bivmap_vars]
    bivmap_sdf <- bivmap_dataset[, bivmap_vars]
    
    # Renaming the variables to simplify the code below
    colnames(bivmap_sdf@data) <- c("xvar","yvar")
    
    # Create the 3-class categorization per each variable
    bivmap_sdf$xcat <- findCols(classIntervals( bivmap_sdf$xvar, n=3, bivmap_style))
    cat(bivmap_vars[1], "breaks (x-axis):\n")
    print(classIntervals( bivmap_sdf$xvar, n=3, bivmap_style))
    #
    bivmap_sdf$ycat <- findCols(classIntervals( bivmap_sdf$yvar, n=3, bivmap_style))
    cat(bivmap_vars[2], "breaks (y-axis):\n")
    print(classIntervals( bivmap_sdf$yvar, n=3, bivmap_style))
    
    # Combine the above into one 9-class categorization
    bivmap_sdf$bicat <- bivmap_sdf$xcat + (3 * (bivmap_sdf$ycat - 1))
    
    bivmap_sdf$bicol <- bvColors[bivmap_sdf$bicat]
    bivmap_sdf$bicol <- ifelse(is.na(bivmap_sdf$bicol), "#bdbdbd", bivmap_sdf$bicol)
    
    # Double-check created datasets if necessary
    #View(bivmap_sdf@data)
    #View(cbind(bivmap_sdf@data, bivmap_dataset@data))
    
    # Create the map
    bivmap <- tm_shape(bivmap_sdf) + 
      # Fill
      tm_fill(
        "bicol", alpha = .8) +
      tm_borders(col ='black', lwd = 1.8)+
      # Remove frame
      tm_layout(frame=FALSE, title = biv_title) +
      # Add rhe legend
      tm_legend(scale=0.75)
    
    if (bivmap_scale) {
      bivmap <- bivmap  +
        # Add scale bar
        tm_scale_bar(
          width=0.30,
          position=c("left","bottom"))
      
    }
    bivmap
    # Return bivariate map
    
    
  }
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  #https://github.com/thomasp85/patchwork/blob/master/R/inset_element.R
  
  #' Create an inset to be added on top of the previous plot
  #'
  #' The standard approach of patchwork is to place plots next to each other based
  #' on the provided layout. However, it may sometimes be beneficial to place one
  #' or several plots or graphic elements freely on top or below another plot. The
  #' `inset_element()` function provides a way to create such insets and gives you
  #' full control over placement.
  #'
  #' @param p A grob, ggplot, patchwork, formula, raster, or nativeRaster object
  #' to add as an inset
  #' @param left,bottom,right,top numerics or units giving the location of the
  #' outer bounds. If given as numerics they will be converted to `npc` units.
  #' @param align_to Specifies what `left`, `bottom`, etc should be relative to.
  #' Either `'panel'` (default), `'plot'`, or `'full'`.
  #' @param on_top Logical. Should the inset be placed on top of the other plot or
  #' below (but above the background)?
  #' @param clip Logical. Should clipping be performed on the inset?
  #' @param ignore_tag Logical. Should autotagging ignore the inset?
  #'
  #' @return A `inset_path` object
  #'
  #' @export
  #'
  #' @examples
  #' library(ggplot2)
  #' p1 <- ggplot(mtcars) + geom_point(aes(mpg, disp))
  #' p2 <- ggplot(mtcars) + geom_boxplot(aes(gear, disp, group = gear))
  #'
  #' # Basic use
  #' p1 + inset_element(p2, 0.6, 0.6, 1, 1)
  #'
  #' # Align to the full area instead
  #' p1 + inset_element(p2, 0, 0.6, 0.4, 1, align_to = 'full')
  #'
  #' # Grobs and other objects can be added as insets as well
  #' p1 + inset_element(grid::circleGrob(), 0.4, 0.4, 0.6, 0.6)
  #'
  #' logo <- system.file('help', 'figures', 'logo.png', package = 'patchwork')
  #' logo <- png::readPNG(logo, native = TRUE)
  #' p1 + inset_element(logo, 0.8, 0.8, 1, 1, align_to = 'full')
  #'
  #' # Just as expected insets are still amenable to changes after the fact
  #' p1 +
  #'   inset_element(p2, 0.6, 0.6, 1, 1) +
  #'   theme_classic()
  #'
  #' # Tagging also continues to work as expected
  #' p1 +
  #'   inset_element(p2, 0.6, 0.6, 1, 1) +
  #'   plot_annotation(tag_levels = '1')
  #'
  #' # but can be turned off, like for wrapped plots
  #' p1 +
  #'   inset_element(p2, 0.6, 0.6, 1, 1, ignore_tag = TRUE) +
  #'   plot_annotation(tag_levels = '1')
  #'
  #'
  #'
  inset_element <- function(p, left, bottom, right, top, align_to = 'panel', on_top = TRUE, clip = TRUE, ignore_tag = FALSE) {
    align_to <- match.arg(align_to, c('panel', 'plot', 'full'))
    if (!is.unit(left)) {
      left <- unit(left, 'npc')
    }
    if (!is.unit(bottom)) {
      bottom <- unit(bottom, 'npc')
    }
    if (!is.unit(right)) {
      right <- unit(right, 'npc')
    }
    if (!is.unit(top)) {
      top <- unit(top, 'npc')
    }
    if (!is.ggplot(p)) {
      p <- wrap_elements(full = p, clip = FALSE)
    }
    if (!is.ggplot(p)) {
      p <- wrap_elements(full = p, clip = clip)
    }
    clip <- if (clip) 'on' else 'off'
    attr(p, 'settings') <- list(left = left, bottom = bottom, right = right,
                                top = top, align_to = align_to, on_top = on_top,
                                clip = clip, ignore_tag = ignore_tag)
    class(p) <- c('inset_patch', class(p))
    p
  }
  is_inset_patch <- function(x) inherits(x, 'inset_patch')
  #'
  print.inset_patch <- function(x, newpage = is.null(vp), vp = NULL, ...) {
    print(plot_spacer() + x, newpage = newpage, vp = vp, ...)
  }
  #'
  plot.inset_patch <- print.inset_patch
  #'
  has_tag.inset_patch <- function(x) !attr(x, 'settings')$ignore_tag
  
  
  
  
  
  
  
  
  
