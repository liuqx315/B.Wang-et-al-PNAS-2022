#install.packages("readxl")
## packages
library(readxl)
library(reshape2)
library(ggplot2)
library(hrbrthemes)
setwd("~/Figure S1. Spatial positions of the traveling fronts")
## import data
#Mxph_su.values[:,1]
#Mxph_sp.values[-29:,1]
#Mxsu_sp.values[-31:,2]
# Time
TimeX = seq(from=1982,by=1,length=39)
# ph_su
Mxph_su <- read.csv('Mxph_su.csv')
# ph_sp
Mxph_sp <- read.csv('Mxph_sp.csv')
# su_sp
Mxsu_sp <- read.csv('Mxsu_sp.csv')
# Data
DataAll <- data.frame(Time = TimeX,        ph_su = Mxph_su$X0*30/1000, su_sp = Mxsu_sp$X1*30/1000, ph_sp = Mxph_sp$X0*30/1000) #1982-2020
Data0   <- melt(DataAll, id="Time")
Data1   <- data.frame(Time = TimeX[1:8],   ph_su = Mxph_su$X0[1:8]*30/1000)   #1982-1989
Data2   <- data.frame(Time = TimeX[24:29], su_sp = Mxsu_sp$X1[24:29]*30/1000) #2005-2010
Data3   <- data.frame(Time = TimeX[35:39], ph_sp = Mxph_sp$X0[35:39]*30/1000) #2016-2020

FS=30
# ph_su_sp
p0 <- ggplot(Data0, aes(x=Time, y=value, shape=variable, colour=variable)) +
  geom_point(size=5) +
  scale_fill_brewer(palette = "Set2") + 
  scale_shape_manual(values = c(0,1,2)) +
  labs(x = " ", y = "Location (Land -> Sea)") +
  theme_bw() +
  theme(
    panel.border = element_rect(size = 1.0,colour = "black", linetype=1),
    axis.line=element_line(size = 0.6, colour = "black", linetype=1),
    # axis.line=element_line(size = 0.6, colour = "black", linetype=1),
    legend.position=c(0.1,0.82),
    legend.title = element_blank(), # element_blank()
    legend.text = element_text(size = FS,color = "black"),
    legend.background = element_rect(fill = NA,color = NA),
    # legend.key=element_rect(fill='NA'),
    axis.text.x = element_text(size=FS, vjust = 0.5,hjust=0.5,color = 'black', angle=0),
    axis.text.y = element_text(size=FS, hjust = 1,color = 'black'),
    axis.title.x = element_text(size=FS,margin = margin(t = 5, b = 0)),
    axis.title.y = element_text(size=FS,margin = margin(r = 2))) +
  theme(text=element_text(size=FS,  family="serif"))
p0


# ph_su
# linear trend + confidence interval
p1 <- ggplot(Data1, aes(x=Time, y=ph_su)) +
  geom_point(shape=0,size=5,colour="blue") +
  geom_smooth(method=lm , color="blue", fill="blue", alpha=0.3, se=TRUE) +
  #labs(x = " ", y = "Location (P.a. -> S.s.)") +
  labs(x = " ", y = " ") +
  theme_bw() +
  theme(
    panel.border = element_rect(size = 1.0,colour = "black", linetype=1),
    axis.line=element_line(size = 0.6, colour = "black", linetype=1),
    # axis.line=element_line(size = 0.6, colour = "black", linetype=1),
    # legend.position=c(0.25,0.82),
    legend.title = element_blank(), # element_blank()
    legend.text = element_text(size = FS,color = "black"),
    legend.background = element_rect(fill = NA,color = NA),
    # legend.key=element_rect(fill='NA'),
    axis.text.x = element_text(size=FS, vjust = 0.5,hjust=0.6,color = 'black', angle=0),
    axis.text.y = element_text(size=FS, hjust = 1,color = 'black'),
    axis.title.x = element_text(size=FS,margin = margin(t = 5, b = 0)),
    axis.title.y = element_text(size=FS,margin = margin(r = 2))) +
  theme(text=element_text(size=FS,  family="serif"))
p1
ggsave(p1,filename = "Figure S1b.pdf",width = 6,height = 6,dpi=800)


# su_sp
# linear trend + confidence interval
p2 <- ggplot(Data2, aes(x=Time, y=su_sp)) +
  geom_point(shape=1,size=5,colour="green") +
  geom_smooth(method=lm , color="green", fill="green", alpha=0.3, se=TRUE) +
  #labs(x = " ", y = "Location (S.s. -> S.a.)") +
  labs(x = " ", y = " ") +
  theme_bw() +
  theme(
    panel.border = element_rect(size = 1.0,colour = "black", linetype=1),
    axis.line=element_line(size = 0.6, colour = "black", linetype=1),
    # axis.line=element_line(size = 0.6, colour = "black", linetype=1),
    # legend.position=c(0.25,0.82),
    legend.title = element_blank(), # element_blank()
    legend.text = element_text(size = FS,color = "black"),
    legend.background = element_rect(fill = NA,color = NA),
    # legend.key=element_rect(fill='NA'),
    axis.text.x = element_text(size=FS, vjust = 0.5,hjust=0.7,color = 'black', angle=0),
    axis.text.y = element_text(size=FS, hjust = 1,color = 'black'),
    axis.title.x = element_text(size=FS,margin = margin(t = 5, b = 0)),
    axis.title.y = element_text(size=FS,margin = margin(r = 2))) +
  theme(text=element_text(size=FS,  family="serif"))
p2
ggsave(p2,filename = "Figure S1c.pdf",width = 6,height = 6,dpi=800)


# ph_sp
# linear trend + confidence interval
p3 <- ggplot(Data3, aes(x=Time, y=ph_sp)) +
  geom_point(shape=2,size=5,colour="red") +
  geom_smooth(method=lm , color="red", fill="red", alpha=0.3, se=TRUE) +
  #labs(x = " ", y = "Location (P.a. -> S.a.)") + 
  labs(x = " ", y = " ") +
  theme_bw() +
  theme(
    panel.border = element_rect(size = 1.0,colour = "black", linetype=1),
    axis.line=element_line(size = 0.6, colour = "black", linetype=1),
    # axis.line=element_line(size = 0.6, colour = "black", linetype=1),
    # legend.position=c(0.25,0.82),
    legend.title = element_blank(), # element_blank()
    legend.text = element_text(size = FS,color = "black"),
    legend.background = element_rect(fill = NA,color = NA),
    # legend.key=element_rect(fill='NA'),
    axis.text.x = element_text(size=FS, vjust = 0.5,hjust=0.7,color = 'black', angle=0),
    axis.text.y = element_text(size=FS, hjust = 1,color = 'black'),
    axis.title.x = element_text(size=FS,margin = margin(t = 5, b = 0)),
    axis.title.y = element_text(size=FS,margin = margin(r = 2))) +
  theme(text=element_text(size=FS,  family="serif"))
p3
ggsave(p3,filename = "Figure S1d.pdf",width = 6,height = 6,dpi=800)
