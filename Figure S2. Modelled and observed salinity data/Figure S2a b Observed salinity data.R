#install.packages("readxl")
## packages
library(readxl)
library(reshape2)#融合数据
library(ggplot2)
library(hrbrthemes)
setwd("~/Figure S2. Modelled and observed salinity data")
## import data
# SalinityBefore1990
SalinityBefore1990x <- read.csv('SalinityBefore1990x.csv')
SalinityBefore1990y <- read.csv('SalinityBefore1990y.csv')
Data1 <- data.frame(x=SalinityBefore1990x$Distance, y=SalinityBefore1990y$Salinity)
Data1$z = rep(1, times=22)
# SalinityAfter2010
SalinityAfter2010x <- read.csv('SalinityAfter2010x.csv')
SalinityAfter2010y <- read.csv('SalinityAfter2010y.csv')
Data2 <- data.frame(x=SalinityAfter2010x$Distance, y=SalinityAfter2010y$Salinity)
Data2$z = rep(2, times=59)
# SalinitySim2020
SalinitySim2020x <- read.csv('SalinitySim2020x.csv')
SalinitySim2020y <- read.csv('SalinitySim2020y.csv')
Data3 <- data.frame(x=SalinitySim2020x$X0, y=SalinitySim2020y$X0)
Data3$z = rep(3, times=435)
FS=25

# SalinityBefore1990 plot
p1 <- ggplot(Data1, aes(x=x, y=y)) +
  geom_point(shape=0,size=5,colour="red") +
  geom_smooth(method=lm , color="red", fill="grey", alpha=0.5, se=TRUE) +
  labs(x = " ", y = " ") +
  #labs(x = expression(paste("Distance from land to sea, (", italic("km"),")" )), y = "Salinity (%)") +
  scale_y_continuous(limits = c(0.4, 1.2)) +
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
    axis.text.x = element_text(size=FS, vjust = 0.5,hjust=0.6,color = 'black', angle=0),
    axis.text.y = element_text(size=FS, hjust = 1,color = 'black'),
    axis.title.x = element_text(size=FS,margin = margin(t = 5, b = 0)),
    axis.title.y = element_text(size=FS,margin = margin(r = 2))) +
  theme(text=element_text(size=FS,  family="serif"))
p1
ggsave(p1,filename = "Figure S2a Observed salinity data.pdf",width = 6,height = 6,dpi=1800)

# SalinityBefore1990&After2010 plot
DataBefore1990After2010 = rbind(Data1,Data2)
p0 <- ggplot(DataBefore1990After2010, aes(x=x, y=y, shape=factor(z), colour=factor(z))) +
  geom_point(size=5) +
  geom_smooth(method=lm , aes(color=factor(z)), fill="grey", alpha=0.5, se=TRUE) +
  scale_fill_brewer(palette = "Set2") + 
  scale_shape_manual(values = c(0,1,2)) +
  labs(x = " ", y = " ") +
  #labs(x = expression(paste("Distance from land to sea, (", italic("km"),")" )), y = "Salinity (%)") +
  scale_y_continuous(limits = c(-0.2, 2.3)) +
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
  theme(legend.position="none") +
  theme(text=element_text(size=FS,  family="serif"))
p0
ggsave(p0,filename = "Figure S2b Observed salinity data.pdf",width = 6,height = 6,dpi=1800)
