gt_df <- readRDS(paste0(path_dissertation,"/aim 3/working/cohorts/guatemala/sss_df.RDS")) %>% 
  dplyr::select(id_uni,srq,happiness,
                adladdercommunity,adladdereconomic)

ph_df <- readRDS(paste0(path_dissertation,"/aim 3/working/cohorts/philippines/sss_df.RDS")) %>% 
  dplyr::select(uncchdid,srq,happiness,
                adladdercommunity,adladdereconomic)


sa_df <- readRDS(paste0(path_dissertation,"/aim 3/working/cohorts/south africa/sss_df.RDS")) %>% 
  dplyr::select(bttid,srq,happiness,
                adladdercommunity,adladdereconomic)

library(ggpubr)

format_pval <- function(pval){
  accuracy = 0.001
  pval <- scales::pvalue(pval, accuracy= accuracy, add_p = TRUE)
  
  return(pval)
}

sfig3_plot <- function(df,x_var,y_var,x_label,y_label,cor_size=3){
  df[,"x"] = df[,x_var]
  df[,"y"] = df[,y_var]
  
  df_grouped <- df %>%
      group_by(x,y) %>%
      tally()
  
  df <- df %>% 
    left_join(df_grouped,
              by=c("x","y"))
    
ggplot(data=df,aes(x,y)) +
    geom_point(aes(size=n)) +
    stat_cor(
      label.y.npc="top", label.x.npc = "left",
      r.digits=2,
      aes(label = paste("'R=",..r..,", ", format_pval(..p..),"'", sep = "")),
      size = cor_size
    ) +
    theme_bw() +
    xlab(x_label) +
    ylab(y_label) + 
    scale_size_continuous(name="",breaks=c(50,100)) +
    scale_x_continuous(limits=c(0,max(df$x,na.rm=TRUE)+2),
                       breaks=seq(0,max(df$x,na.rm = TRUE),by=2)) +
    scale_y_continuous(limits=c(0,max(df$y,na.rm=TRUE)+2),
                       breaks=seq(0,max(df$y,na.rm=TRUE),by=2))
  
}

plotA = sfig3_plot(df=gt_df,x_var="adladdercommunity",y_var="adladdereconomic",
           x_label="Perceived Community Status",y_label="Perceived Economic Status")
plotB = sfig3_plot(df=ph_df,x_var="adladdercommunity",y_var="adladdereconomic",
           x_label="Perceived Community Status",y_label="Perceived Economic Status")
plotC = sfig3_plot(df=sa_df,x_var="adladdercommunity",y_var="adladdereconomic",
           x_label="Perceived Community Status",y_label="Perceived Economic Status")

plotD = sfig3_plot(df=gt_df,x_var="srq",y_var="happiness",
                   x_label="WHO SRQ-20",y_label="Subjective Happiness Scale")
plotE = sfig3_plot(df=ph_df,x_var="srq",y_var="happiness",
                   x_label="WHO SRQ-20",y_label="Subjective Happiness Scale")
plotF = sfig3_plot(df=sa_df,x_var="srq",y_var="happiness",
                   x_label="WHO SRQ-20",y_label="Subjective Happiness Scale")


ggarrange(plotA,
          plotB,
          plotC,
          plotD,
          plotE,
          plotF,
          nrow=2,ncol=3,
          labels=LETTERS[1:6])

