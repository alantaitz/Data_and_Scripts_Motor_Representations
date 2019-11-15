
# Effort
############
Dat = read.csv('Preprocessed Data/CCV_Averages.csv', stringsAsFactors=FALSE)
Estadisticas = Dat[Dat$type==5,]
attach(Estadisticas)

Efforts    = read.csv("Preprocessed Data/Efforts.csv", stringsAsFactors=FALSE, header =F )
x          = Efforts$V5/10000 * 2.7   # Normalization
y          = as.numeric(Estadisticas$NormTransition[Estadisticas$block==1])
yerr       = as.numeric(Estadisticas$NormTransition.1[Estadisticas$block==1])
labels     = Estadisticas$X[Estadisticas$block==1]
y              <- y[!is.na(y)]
yerr           <- yerr[!is.na(yerr)]
Etiquetas      <- labels[3:17]
ColoresSilabas <- as.numeric(unique(factor(Etiquetas)))

temp1  <- data.frame(x,y, yerr, Etiquetas)

ggplot(data=temp1, aes(x, y, label=Etiquetas)) +
  labs(x= "Articulatory Effort arb.unit", y=" Tau'")    +
  geom_point(aes(colour=Etiquetas, size=1.1), show.legend = F) + geom_smooth(method='lm', size = 1.7, alpha=.45) +
  geom_text(aes(colour=Etiquetas), show.legend = FALSE, size = 5.2,  vjust=2.3, hjust=1.1) +
  geom_errorbar( aes(ymin=y-yerr, ymax=y+yerr, width=2, colour=Etiquetas, size = 1), show.legend=F) +
  scale_x_continuous(sec.axis = dup_axis()) +  scale_y_continuous(sec.axis = dup_axis()) +
  #scale_y_continuous(limits = c(0.34, 0.46)) +  scale_x_continuous(limits = c(200, 800))   +
  theme(axis.text.x = element_text(face="bold", size=17))  + 
  theme(axis.text=element_text(size=), axis.title=element_text(size=14,face="bold")) +
  theme(axis.text.y = element_text(face="bold", size=17))  + 
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(), panel.background = element_blank(), axis.line = element_line(colour = "black")) 
cor.test(x,y)
linearMod <- lm(x ~ y, weights=1/yerr,  data=temp1)  # build linear regression model on full data
summary(linearMod)
