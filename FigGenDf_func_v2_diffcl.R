library("memoise")
FigGenDf<-function(Tp,N_s,m,rho0,r,type,c,p,pprim,g,gprim,effsiz,accept_pwr) {
#FigGenDf<-function(Tp,N_s,m,rho0,r,type,c,p,pprim,g,gprim,effsiz,accept_pwr,dist) {
  
  IterRes<- IterRemove_diffcl_CE(Tp,N_s,m,rho0,r,type,c,p,pprim,g,gprim,effsiz,accept_pwr)
  #IterRes<- IterRemove_diffcl_CE(Tp,N_s,m,rho0,r,type,c,p,pprim,g,gprim,effsiz,accept_pwr,dist)
  melted_varmatexcl<- melt(IterRes[[1]])
  melted_desmatexcl<- melt(IterRes[[2]])
  
  names(melted_desmatexcl)[names(melted_desmatexcl)=="value"] <- "Xdvalue"
  melted_varmatexcl_t<- jointdataset <- merge(melted_varmatexcl, melted_desmatexcl, by = c('Var1','Var2','L1'))
  melted_varmatexcl_t$value<-melted_varmatexcl_t$value
  
  names(melted_varmatexcl_t)[names(melted_varmatexcl_t)=="Var1"] <- "Sequence"
  names(melted_varmatexcl_t)[names(melted_varmatexcl_t)=="Var2"] <- "Period"
  names(melted_varmatexcl_t)[names(melted_varmatexcl_t)=="L1"] <- "iter"
  
  Xdes <- SWdesmat(Tp)
  varmatall<- IterRes[[3]]
  cvec<- IterRes[[4]]
  pwvec<- IterRes[[5]]
  
  Tp <- ncol(Xdes)
  S  <- nrow(Xdes)
  
  iter=1:length(varmatall)

  var=varmatall
  res=data.frame(iter,var)
  res$r <- r
  
  res <- cbind(res,1/res$var,res$var[1]/res$var,(1-(res$var[1]/res$var))*100,pwvec,cvec)
  colnames(res) <- c("iter","var","r","Prec","Rvar","Preloss","power","cost")
  
  res$CE <- (1/(res$var))/(res$cost)
  res$RCE <- (res$CE)/(res$CE[1])
  
  res_na <- melted_varmatexcl_t %>% group_by(iter) %>% summarise(na_percnt = (sum(is.na(Xdvalue))/(Tp*(Tp-1)))*100)
  res_2 <- merge(res, res_na,by = 'iter')
  
  melted_varmatexcl_t <- merge(res_2, melted_varmatexcl_t, by = 'iter', all = TRUE)
  return(list(res_2,melted_varmatexcl_t))
}

generatePlotly <- function(FigRes, xVar, yVar, xTitle, yTitle, hoverText) {
  
  plot_ly(FigRes[[1]], height = 500, width = 800, x = xVar, y = yVar, type = "scatter",
          mode = "markers", hoverinfo = "text", hoverlabel = list(bordercolor = NULL, font = list(size = 16)),
          text = hoverText, line = list(color = "#F8766D", width = 4, dash = "dash")) %>%
    layout(xaxis = list(title = xTitle, titlefont = list(size = 18), showline = TRUE,
                        tickmode = "auto", tickfont = list(size = 16), nticks = 6, ticks = "inside",
                        mirror = TRUE, showgrid = FALSE),
           yaxis = list(title = yTitle, titlefont = list(size = 18), tickfont = list(size = 16),
                        mirror = TRUE, showline = TRUE),
           legend = list(orientation = "h", xanchor = "center", yanchor = "bottom", x = 0.5, y = -0.5, font = list(size = 16)),
           margin = list(l = 100, r = 40))
}
#The FigGenDf function will cache its results for different input combinations, 
#and subsequent calls with the same inputs will not recompute the results.
FigGenDf <- memoise(FigGenDf)

