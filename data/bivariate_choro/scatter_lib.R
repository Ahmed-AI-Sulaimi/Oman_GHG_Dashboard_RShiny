
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


# example                
# scatterplot_biv(df =bound_tbl_Summary_gov, 
#                 var_x = bound_tbl_Summary_gov$co2_avg,
#                 var_y =bound_tbl_Summary_gov$ch4_avg,
#                 lab_x = 'co2 avg',lab_y = 'ch4 avg',capt = 'this is caption',
#                 biv_col = bvColors)

quantile(bound_tbl_Summary_gov$population_estimate,  probs = c(0.33,0.66))




# 
# 
# 
# 
# 
# 
