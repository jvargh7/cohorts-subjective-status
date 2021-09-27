library(ggpubr)

southafrica_dfa <- readRDS(paste0(path_cohorts_data_for_analysis,"/cohorts_data_for_analysis.RDS")) %>%
  dplyr::filter(site == "soweto") %>% 
  dplyr::mutate(bttid = pin - 50000000) 


sa_fig01_df <- readRDS(paste0(path_dissertation,"/aim 3/working/cohorts/south africa/sss_df.RDS"))


# Histograms ----------

l_c_f <- sa_fig01_df %>% 
  dplyr::filter(male == 0) %>% 
  ggplot(data=.,aes(x=adladdercommunity)) +
  geom_histogram(bins=10)+
  theme_bw() +
  xlab("Community Ladder") +
  scale_y_continuous(limits=c(0,250)) +
  scale_x_continuous(breaks=seq(0,10,by=2)) +
  ggtitle("Female")

l_e_f <- sa_fig01_df %>% 
  dplyr::filter(male == 0) %>% 
  ggplot(data=.,aes(x=adladdereconomic)) +
  geom_histogram(bins=10)+
  theme_bw() +
  xlab("Economic Ladder") +
  scale_y_continuous(limits=c(0,250)) +
  scale_x_continuous(breaks=seq(0,10,by=2)) +
  ggtitle("Female")


l_c_m <- sa_fig01_df %>% 
  dplyr::filter(male == 1) %>% 
  ggplot(data=.,aes(x=adladdercommunity)) +
  geom_histogram(bins=10)+
  theme_bw() +
  xlab("Community Ladder") +
  scale_y_continuous(limits=c(0,250)) +
  scale_x_continuous(breaks=seq(0,10,by=2)) +
  ggtitle("Male")


l_e_m <- sa_fig01_df %>% 
  dplyr::filter(male == 0) %>% 
  ggplot(data=.,aes(x=adladdereconomic)) +
  geom_histogram(bins=10)+
  theme_bw() +
  xlab("Economic Ladder") +
  scale_y_continuous(limits=c(0,250)) +
  scale_x_continuous(breaks=seq(0,10,by=2)) +
  ggtitle("Male")


# Boxplots ---------
ur_c <- sa_fig01_df %>% 
  ggplot(data=.,aes(x=factor(male,labels=c("Female","Male")),
                    y=adladdercommunity,fill=factor(black,labels=c("Non-Black","Black")))) +
  geom_boxplot() + 
  theme_bw() +
  xlab("") +
  ylab("Community Ladder") +
  scale_fill_discrete("") +
  scale_y_continuous(breaks=seq(0,10,by=2))

ur_e <- sa_fig01_df %>% 
  ggplot(data=.,aes(x=factor(male,labels=c("Female","Male")),
                    y=adladdereconomic,fill=factor(black,labels=c("Non-Black","Black")))) +
  geom_boxplot() + 
  theme_bw() +
  xlab("") +
  ylab("Economic Ladder")  +
  scale_fill_discrete("")  +
  scale_y_continuous(breaks=seq(0,10,by=2))

ggarrange(
  ggarrange(l_c_f,
            l_c_m,
            l_e_f,
            l_e_m,
            nrow=2,
            ncol=2,labels = LETTERS[1:4]),
  ggarrange(ur_c,
            ur_e,
            nrow=2,
            ncol=1,labels = LETTERS[5:6]), nrow = 1, ncol = 2
)


