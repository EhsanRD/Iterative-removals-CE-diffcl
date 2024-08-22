##############################################################################################
##########################Figure S1############################################################
##############################################################################################
color_palette <-colorRampPalette(c("white", "grey"))(2)
N_s <- c(8,7,7,7,8)
Tp=6
FigRes <- FigGenDf(6,N_s,7,0.01,0.95,1,2500,140,80,230,0,0.26,0.8)
plot_list =list()
fig1<- ggplot(data =subset(FigRes[[2]],iter==16), aes(x=Period, y=Sequence, fill = factor(Xdvalue))) +
  geom_tile(colour = "grey50") +
  scale_y_reverse(breaks=c(1:(Tp-1))) +
  scale_x_continuous(breaks=c(1:Tp)) +
  theme(plot.title = element_text(hjust = 0,face="bold", colour="Black", size = 14),
        plot.subtitle = element_text(hjust = 0, colour="Black", size = 12),
        axis.title.x = element_text(face="bold", colour="Black", size = 12),
        axis.title.y = element_text(face="bold", colour="Black", size =12),
        axis.text.x = element_text(face="bold", color="Black",
                                   size=12),
        axis.text.y = element_text(face="bold", color="Black",
                                   size=12)) +
  geom_text(aes(Period, Sequence, label = Xdvalue), color = "black", size = 8) +
  scale_fill_manual(values = color_palette) +  theme(legend.position="none")

FigRes <- FigGenDf(6,N_s,7,0.01,0.9,1,2500,140,80,230,0,0.26,0.8)
plot_list =list()
fig2<- ggplot(data =subset(FigRes[[2]],iter==15), aes(x=Period, y=Sequence, fill = factor(Xdvalue))) +
  geom_tile(colour = "grey50") +
  scale_y_reverse(breaks=c(1:(Tp-1))) +
  scale_x_continuous(breaks=c(1:Tp)) +
  theme(plot.title = element_text(hjust = 0,face="bold", colour="Black", size = 14),
        plot.subtitle = element_text(hjust = 0, colour="Black", size = 12),
        axis.title.x = element_text(face="bold", colour="Black", size = 12),
        axis.title.y = element_text(face="bold", colour="Black", size =12),
        axis.text.x = element_text(face="bold", color="Black",
                                   size=12),
        axis.text.y = element_text(face="bold", color="Black",
                                   size=12)) +
  geom_text(aes(Period, Sequence, label = Xdvalue), color = "black", size = 8) +
  scale_fill_manual(values = color_palette) +  theme(legend.position="none")

FigRes <- FigGenDf(6,N_s,7,0.01,0.8,1,2500,140,80,230,0,0.26,0.8)
plot_list =list()
fig3<- ggplot(data =subset(FigRes[[2]],iter==15), aes(x=Period, y=Sequence, fill = factor(Xdvalue))) +
  geom_tile(colour = "grey50") +
  scale_y_reverse(breaks=c(1:(Tp-1))) +
  scale_x_continuous(breaks=c(1:Tp)) +
  theme(plot.title = element_text(hjust = 0,face="bold", colour="Black", size = 14),
        plot.subtitle = element_text(hjust = 0, colour="Black", size = 12),
        axis.title.x = element_text(face="bold", colour="Black", size = 12),
        axis.title.y = element_text(face="bold", colour="Black", size =12),
        axis.text.x = element_text(face="bold", color="Black",
                                   size=12),
        axis.text.y = element_text(face="bold", color="Black",
                                   size=12)) +
  geom_text(aes(Period, Sequence, label = Xdvalue), color = "black", size = 8) +
  scale_fill_manual(values = color_palette) +  theme(legend.position="none")

FigRes <- FigGenDf(6,N_s,7,0.05,0.95,1,2500,140,80,230,0,0.26,0.8)
plot_list =list()
fig4<- ggplot(data =subset(FigRes[[2]],iter==19), aes(x=Period, y=Sequence, fill = factor(Xdvalue))) +
  geom_tile(colour = "grey50") +
  scale_y_reverse(breaks=c(1:(Tp-1))) +
  scale_x_continuous(breaks=c(1:Tp)) +
  theme(plot.title = element_text(hjust = 0,face="bold", colour="Black", size = 14),
        plot.subtitle = element_text(hjust = 0, colour="Black", size = 12),
        axis.title.x = element_text(face="bold", colour="Black", size = 12),
        axis.title.y = element_text(face="bold", colour="Black", size =12),
        axis.text.x = element_text(face="bold", color="Black",
                                   size=12),
        axis.text.y = element_text(face="bold", color="Black",
                                   size=12)) +
  geom_text(aes(Period, Sequence, label = Xdvalue), color = "black", size = 8) +
  scale_fill_manual(values = color_palette) +  theme(legend.position="none")

FigRes <- FigGenDf(6,N_s,7,0.05,0.9,1,2500,140,80,230,0,0.26,0.8)
plot_list =list()
fig5<- ggplot(data =subset(FigRes[[2]],iter==18), aes(x=Period, y=Sequence, fill = factor(Xdvalue))) +
  geom_tile(colour = "grey50") +
  scale_y_reverse(breaks=c(1:(Tp-1))) +
  scale_x_continuous(breaks=c(1:Tp)) +
  theme(plot.title = element_text(hjust = 0,face="bold", colour="Black", size = 14),
        plot.subtitle = element_text(hjust = 0, colour="Black", size = 12),
        axis.title.x = element_text(face="bold", colour="Black", size = 12),
        axis.title.y = element_text(face="bold", colour="Black", size =12),
        axis.text.x = element_text(face="bold", color="Black",
                                   size=12),
        axis.text.y = element_text(face="bold", color="Black",
                                   size=12)) +
  geom_text(aes(Period, Sequence, label = Xdvalue), color = "black", size = 8) +
  scale_fill_manual(values = color_palette) +  theme(legend.position="none")

FigRes <- FigGenDf(6,N_s,7,0.05,0.8,1,2500,140,80,230,0,0.26,0.8)
plot_list =list()
fig6<- ggplot(data =subset(FigRes[[2]],iter==18), aes(x=Period, y=Sequence, fill = factor(Xdvalue))) +
  geom_tile(colour = "grey50") +
  scale_y_reverse(breaks=c(1:(Tp-1))) +
  scale_x_continuous(breaks=c(1:Tp)) +
  theme(plot.title = element_text(hjust = 0,face="bold", colour="Black", size = 14),
        plot.subtitle = element_text(hjust = 0, colour="Black", size = 12),
        axis.title.x = element_text(face="bold", colour="Black", size = 12),
        axis.title.y = element_text(face="bold", colour="Black", size =12),
        axis.text.x = element_text(face="bold", color="Black",
                                   size=12),
        axis.text.y = element_text(face="bold", color="Black",
                                   size=12)) +
  geom_text(aes(Period, Sequence, label = Xdvalue), color = "black", size = 8) +
  scale_fill_manual(values = color_palette) +  theme(legend.position="none")

FigRes <- FigGenDf(6,N_s,7,0.1,0.95,1,2500,140,80,230,0,0.26,0.8)
plot_list =list()
fig7<- ggplot(data =subset(FigRes[[2]],iter==17), aes(x=Period, y=Sequence, fill = factor(Xdvalue))) +
  geom_tile(colour = "grey50") +
  scale_y_reverse(breaks=c(1:(Tp-1))) +
  scale_x_continuous(breaks=c(1:Tp)) +
  theme(plot.title = element_text(hjust = 0,face="bold", colour="Black", size = 14),
        plot.subtitle = element_text(hjust = 0, colour="Black", size = 12),
        axis.title.x = element_text(face="bold", colour="Black", size = 12),
        axis.title.y = element_text(face="bold", colour="Black", size =12),
        axis.text.x = element_text(face="bold", color="Black",
                                   size=12),
        axis.text.y = element_text(face="bold", color="Black",
                                   size=12)) +
  geom_text(aes(Period, Sequence, label = Xdvalue), color = "black", size = 8) +
  scale_fill_manual(values = color_palette) +  theme(legend.position="none")

FigRes <- FigGenDf(6,N_s,7,0.1,0.9,1,2500,140,80,230,0,0.26,0.8)
plot_list =list()
fig8<- ggplot(data =subset(FigRes[[2]],iter==17), aes(x=Period, y=Sequence, fill = factor(Xdvalue))) +
  geom_tile(colour = "grey50") +
  scale_y_reverse(breaks=c(1:(Tp-1))) +
  scale_x_continuous(breaks=c(1:Tp)) +
  theme(plot.title = element_text(hjust = 0,face="bold", colour="Black", size = 14),
        plot.subtitle = element_text(hjust = 0, colour="Black", size = 12),
        axis.title.x = element_text(face="bold", colour="Black", size = 12),
        axis.title.y = element_text(face="bold", colour="Black", size =12),
        axis.text.x = element_text(face="bold", color="Black",
                                   size=12),
        axis.text.y = element_text(face="bold", color="Black",
                                   size=12)) +
  geom_text(aes(Period, Sequence, label = Xdvalue), color = "black", size = 8) +
  scale_fill_manual(values = color_palette) +  theme(legend.position="none")

FigRes <- FigGenDf(6,N_s,7,0.1,0.8,1,2500,140,80,230,0,0.26,0.8)
plot_list =list()
fig9<- ggplot(data =subset(FigRes[[2]],iter==13), aes(x=Period, y=Sequence, fill = factor(Xdvalue))) +
  geom_tile(colour = "grey50") +
  scale_y_reverse(breaks=c(1:(Tp-1))) +
  scale_x_continuous(breaks=c(1:Tp)) +
  theme(plot.title = element_text(hjust = 0,face="bold", colour="Black", size = 14),
        plot.subtitle = element_text(hjust = 0, colour="Black", size = 12),
        axis.title.x = element_text(face="bold", colour="Black", size = 12),
        axis.title.y = element_text(face="bold", colour="Black", size =12),
        axis.text.x = element_text(face="bold", color="Black",
                                   size=12),
        axis.text.y = element_text(face="bold", color="Black",
                                   size=12)) +
  geom_text(aes(Period, Sequence, label = Xdvalue), color = "black", size = 8) +
  scale_fill_manual(values = color_palette) +  theme(legend.position="none")
#initial design
fig1<- fig1+labs(title = "Optimal design 1",subtitle = "ICC=0.01, CAC=0.95, Initial power=94.5%, Power=92.4%") 
fig2<- fig2+labs(title = "Optimal design 2",subtitle = "ICC=0.01, CAC=0.9, Initial power=94.6%, Power=93.4%") 
fig3<- fig3+labs(title = "Optimal design 3",subtitle = "ICC=0.01, CAC=0.8, Initial power=94.7%, Power=93.5%")  
fig4<- fig4+labs(title = "Optimal design 4",subtitle = "ICC=0.05, CAC=0.95, Initial power=89.7%, Power=83.2%")  
fig5<- fig5+labs(title = "Optimal design 5",subtitle = "ICC=0.05, CAC=0.9, Initial power=89.0%, Power=83.9%") 
fig6<- fig6+labs(title = "Optimal design 6",subtitle = "ICC=0.05, CAC=0.8, Initial power=88.2%, Power=83.4%") 
fig7<- fig7+labs(title = "Optimal design 7",subtitle = "ICC=0.1, CAC=0.95, Initial power=87.4%, Power=81.7%") 
fig8<- fig8+labs(title = "Optimal design 8",subtitle = "ICC=0.1, CAC=0.9, Initial power=85.6%, Power=80.1%")  
fig9<- fig9+labs(title = "Optimal design 9",subtitle = "ICC=0.1, CAC=0.8, Initial power=83%, Power=80.2%") 


#save grid graph
add_name = "G:\\Shared drives\\Ehsan PhD work\\paper_2\\Figures_v2_diffcl\\"
#add_name = "G:\\Shared drives\\Ehsan PhD work\\Presentations\\VicBiostat_WIP_08June2023\\"

#jpg format
p1<-grid.arrange(fig1,fig2,fig3,fig4,fig5,fig6,fig7,fig8,fig9, ncol=3,nrow=3)
ggsave(paste0(add_name,"Figure S1",".png"), p1, width=15, height=15, units="in", dpi=900)
##############################################################################################
################################S2-1 Section 3.3##############################################
##############################################################################################
Tp=15
N_s <-rep(1, Tp- 1)
FigRes <- FigGenDf(15,N_s,50,0.15,1,0,2500,80,80,2500,0,0.1,0.8)

plot_list =list()
color_palette <-colorRampPalette(c("white", "grey"))(2)

fig1<- ggplot(data =subset(FigRes[[2]],iter==129), aes(x=Period, y=Sequence, fill = factor(Xdvalue))) +
  geom_tile(colour = "grey50") +
  scale_y_reverse(breaks=c(1:(Tp-1))) +
  scale_x_continuous(breaks=c(1:Tp)) +
  theme(plot.title = element_text(hjust = 0,face="bold", colour="Black", size = 12),
        plot.subtitle = element_text(hjust = 0, colour="Black", size = 12),
        axis.title.x = element_text(face="bold", colour="Black", size = 12),
        axis.title.y = element_text(face="bold", colour="Black", size =12),
        axis.text.x = element_text(face="bold", color="Black",
                                   size=12),
        axis.text.y = element_text(face="bold", color="Black",
                                   size=12)) +
  geom_text(aes(Period, Sequence, label = Xdvalue), color = "black", size = 5) +
  scale_fill_manual(values = color_palette) +  theme(legend.position="none")



FigRes <- FigGenDf(15,N_s,50,0.15,1,0,2500,140,80,0,0,0.1,0.8)
plot_list =list()
color_palette <-colorRampPalette(c("white", "grey"))(2)

fig2<- ggplot(data =subset(FigRes[[2]],iter==127), aes(x=Period, y=Sequence, fill = factor(Xdvalue))) +
  geom_tile(colour = "grey50") +
  scale_y_reverse(breaks=c(1:(Tp-1))) +
  scale_x_continuous(breaks=c(1:Tp)) +
  theme(plot.title = element_text(hjust = 0,face="bold", colour="Black", size = 12),
        plot.subtitle = element_text(hjust = 0, colour="Black", size = 12),
        axis.title.x = element_text(face="bold", colour="Black", size = 12),
        axis.title.y = element_text(face="bold", colour="Black", size =12),
        axis.text.x = element_text(face="bold", color="Black",
                                   size=12),
        axis.text.y = element_text(face="bold", color="Black",
                                   size=12)) +
  geom_text(aes(Period, Sequence, label = Xdvalue), color = "black", size = 5) +
  scale_fill_manual(values = color_palette) +  theme(legend.position="none")

fig1<- fig1+labs(title = "Optimal design 1") 
fig2<- fig2+labs(title = "Optimal design 2") 

#jpg format
p1<-grid.arrange(fig1,fig2, ncol=2,nrow=1)

ggsave(paste0(add_name,"Figure S2",".png"), p1, width=10, height=5, units="in", dpi=900)
