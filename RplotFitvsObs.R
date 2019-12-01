library(ggplot2)
library(data.table)
library(tidyverse)

# to plot fitted vs observed for all independent varibles 

plotFittedvsObserved = function(df,colActual,colPredict,folder,contColBin,intColBin,factColBin,oneScale,oneScaleYquantile){
  variablesList<-setdiff(names(df),c(colActual,colPredict))
  for (colVar in variablesList){
    temp.df=data.frame(observed=df[,colActual],fitted=df[,colPredict],var=df[,colVar])
    if (length(unique(temp.df$var))==1) {print(paste('The',colVar,'column consists of only one level!!!!!')) }
    else if(typeof(temp.df$var)=='double') {temp.df$var=cut(temp.df$var,contColBin)}
    else if (is.numeric(temp.df$var) & length(unique(temp.df$var))>intColBin) {temp.df$var=cut(temp.df$var,intColBin)}
    else if (is.factor(temp.df$var) & length(unique(temp.df$var))>factColBin) {print(paste('The',colVar,'column consists of ',length(unique(temp.df$var)), 'levels!!!!!')) }
    row_count<- nrow(temp.df)
    temp.df %>%
      group_by(var) %>%
      summarize(mean_actual=mean(observed),
                mean_predict=mean(fitted),
                expo=n()/row_count) ->CumTab
    if (oneScale=='Y') {y1_max=quantile(temp.df$observed,oneScaleYquantile)}
    else {y1_max=max(CumTab$mean_actual,CumTab$mean_predict)}
    y2_max=max(CumTab$expo)
    cols <- c("exposure" = "green","observed" = "blue","fitted" = "red")
    rPlot=ggplot(data=CumTab,aes(x=var, group=1))+
      geom_bar(aes(y=expo*y1_max/y2_max, fill="exposure" ),col="black", alpha=0.2,  stat="identity")+
      geom_line(aes(y=mean_actual, col="observed"))+
      geom_line(aes(y=mean_predict,colour="fitted" ))+
      scale_y_continuous(limits=c(0, y1_max),sec.axis = ~ ./y1_max*y2_max)+
      scale_colour_manual(name="Legend Y_left", values=cols)+
      scale_fill_manual(name="Legend Y_right",values=cols)+
      ylab('Target')+
      xlab(colVar)+
      theme(axis.text.x=element_text(angle=90,hjust=1))+
      ggtitle(paste(colActual,'vs',colPredict))
    #print(rPlot)
    setwd(folder)
    ggsave(paste0(colVar,'.png'))
  }
}


