# Only for figures
# Graphs preprocessed data in Analysis.py
#FFD vs Normalized transition
############

rm(list=ls())
library(ggplot2)
library(gridExtra)
library(nlme)
library(ggExtra)
library(magrittr)
library(Hmisc)


Data = read.csv('Preprocessed Data/StatsOcular.csv', stringsAsFactors=FALSE)
attach(Data)

# Figure 3 A
### FFD vs Transition time    
y     <- as.numeric(FFD[block==2])      #mean
yerr  <- as.numeric(FFD.1[block==2])    #sem
x     <- as.numeric(NormTransition[block==1]) #Para pasar a ms
xerr  <- as.numeric(NormTransition.1[block==1])
lab   <- unique(X)
#Saco los Nans
x     <- x[!is.na(x)]
y     <- y[!is.na(y)]
xerr  <- xerr[!is.na(xerr)]
yerr  <- yerr[!is.na(yerr)]
Etiquetas      <- lab[3:17]
ColoresSilabas <- as.numeric(unique(factor(Etiquetas)))

temp1      <- data.frame(x,y, xerr, yerr, Etiquetas)

ggplot(data=temp1, aes(x, y, label=Etiquetas)) +
  labs(y= "FFD (ms)", x=" Tau'")    +
  geom_point(aes(colour=Etiquetas, size=1.1), show.legend = F) + geom_smooth(method='lm', size = 1.7, alpha=.45) +
  geom_text(aes(colour=Etiquetas), show.legend = FALSE, size = 5.2,  vjust=2.3, hjust=1.1) +
  geom_errorbar( aes(ymin=y-yerr, ymax=y+yerr, width=0.005, colour=Etiquetas, size = 1), show.legend=F) +
  geom_errorbarh(aes(xmax=x+xerr, xmin=x - xerr, height=5, colour=Etiquetas, size= 1), show.legend=F) +
  scale_x_continuous(sec.axis = dup_axis(), limits = c(0.348, 0.45), breaks = c(0.35, 0.40, 0.45)) +  scale_y_continuous(sec.axis = dup_axis(), limits = c(284, 383), breaks = c(280,300, 320, 340, 360, 380)) +
  #scale_y_continuous(limits = c(0.34, 0.46)) +  scale_x_continuous(limits = c(200, 800))   +
  theme(axis.text.x = element_text(face="bold", size=17))  + 
  theme(axis.text=element_text(size=), axis.title=element_text(size=14,face="bold")) +
  theme(axis.text.y = element_text(face="bold", size=17))  + 
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(), panel.background = element_blank(), axis.line = element_line(colour = "black")) 
linearMod <- lm(x ~ y, weights=1/yerr,  data=temp1)  # build linear regression model on full data
summary(linearMod)

# Figure 3 B
## FPRT vs Transition time
y     <- as.numeric(FPRT[block==2])      #mean
yerr  <- as.numeric(FPRT.1[block==2])    #sem
x     <- as.numeric(NormTransition[block==1]) #Para pasar a ms
xerr  <- as.numeric(NormTransition.1[block==1])
lab   <- unique(X)
#Saco los Nans
x     <- x[!is.na(x)]
y     <- y[!is.na(y)]
xerr  <- xerr[!is.na(xerr)]
yerr  <- yerr[!is.na(yerr)]
Etiquetas      <- lab[3:17]
ColoresSilabas <- as.numeric(unique(factor(Etiquetas)))

temp1      <- data.frame(x,y, xerr, yerr, Etiquetas)

ggplot(data=temp1, aes(x, y, label=Etiquetas)) +
  labs(y= "FPRT (ms)", x=" Tau'")    +
  geom_point(aes(colour=Etiquetas, size=1.1), show.legend = F) + geom_smooth(method='lm', size = 1.7, alpha=.45) +
  geom_text(aes(colour=Etiquetas), show.legend = FALSE, size = 5.2,  vjust=2.3, hjust=1.1) +
  geom_errorbar( aes(ymin=y-yerr, ymax=y+yerr, width=0.005, colour=Etiquetas, size = 1), show.legend=F) +
  geom_errorbarh(aes(xmax=x+xerr, xmin=x - xerr, height=5, colour=Etiquetas, size= 1), show.legend=F) +
  scale_x_continuous(sec.axis = dup_axis(), limits = c(0.348, 0.45), breaks = c(0.35, 0.40, 0.45)) +  scale_y_continuous(sec.axis = dup_axis(), limits = c(520, 696), breaks = c(520, 580, 640, 700)) +
  #scale_y_continuous(sec.axis = dup_axis()) +  scale_x_continuous(sec.axis = dup_axis())   +
  theme(axis.text.x = element_text(face="bold", size=17))  + 
  theme(axis.text=element_text(size=), axis.title=element_text(size=14,face="bold")) +
  theme(axis.text.y = element_text(face="bold", size=17))  + 
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(), panel.background = element_blank(), axis.line = element_line(colour = "black")) 
linearMod <- lm(x ~ y, weights=1/yerr,  data=temp1)  # build linear regression model on full data
summary(linearMod)

# Figure 3 C
## TFT vs Transition time
y     <- as.numeric(TFT[block==2])      #mean
yerr  <- as.numeric(TFT.1[block==2])    #sem
x     <- as.numeric(NormTransition[block==1]) #Para pasar a ms
xerr  <- as.numeric(NormTransition.1[block==1])
lab   <- unique(X)
#Saco los Nans
x     <- x[!is.na(x)]
y     <- y[!is.na(y)]
xerr  <- xerr[!is.na(xerr)]
yerr  <- yerr[!is.na(yerr)]
Etiquetas      <- lab[3:17]
ColoresSilabas <- as.numeric(unique(factor(Etiquetas)))

temp1      <- data.frame(x,y, xerr, yerr, Etiquetas)

ggplot(data=temp1, aes(x, y, label=Etiquetas)) +
  labs(y= "TFT (ms)", x=" Tau'")    +
  geom_point(aes(colour=Etiquetas, size=1.1), show.legend = F) + geom_smooth(method='lm', size = 1.7, alpha=.45) +
  geom_text(aes(colour=Etiquetas), show.legend = FALSE, size = 5.2,  vjust=2.3, hjust=1.1) +
  geom_errorbar( aes(ymin=y-yerr, ymax=y+yerr, width=0.005, colour=Etiquetas, size = 1), show.legend=F) +
  geom_errorbarh(aes(xmax=x+xerr, xmin=x - xerr, height=5, colour=Etiquetas, size= 1), show.legend=F) +
  scale_x_continuous(sec.axis = dup_axis(), limits = c(0.348, 0.45), breaks = c(0.35, 0.40, 0.45)) +  scale_y_continuous(sec.axis = dup_axis(), limits = c(540, 720), breaks = c(540, 600, 660, 720)) +
  #scale_y_continuous(sec.axis = dup_axis()) +  scale_x_continuous(sec.axis = dup_axis())   +
  theme(axis.text.x = element_text(face="bold", size=17))  + 
  theme(axis.text=element_text(size=), axis.title=element_text(size=14,face="bold")) +
  theme(axis.text.y = element_text(face="bold", size=17))  + 
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(), panel.background = element_blank(), axis.line = element_line(colour = "black")) 
linearMod <- lm(x ~ y, weights=1/yerr,  data=temp1)  # build linear regression model on full data
summary(linearMod)
