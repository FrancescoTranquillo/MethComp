##### Cani ####

##### Data prep #####
#load library
library(MethComp)
library(ggplot2)
#import data
df<-read.csv("cani.csv", header = TRUE, sep = ";", dec= ",", stringsAsFactors = FALSE)

#delete record number 99
df<-df[-c(393:396),]
head(df)

##### Ciclo for: PB regression su tutti i parametri ed estrazione dei coefficienti della regressione####
slopes<-NULL
intercept<-NULL

for(i in c(4:25)){

  #set meth object
  df_Meth<-Meth(df, 2,1,3,i)
  df_Meth$meth<-factor(df_Meth$meth, levels=c("Procyte Idexx", "DF-50"))

  #PB regression
  reg<-PBreg(df_Meth)

  #save coefficients and CI limits
  coeff<-as.data.frame(reg[["coefficients"]])

  #slopes
  varslope<- as.data.frame(coeff[2,])
  row.names(varslope)<-colnames(df[i])
  slopes<-rbind(slopes, varslope)
  #intercepts
  varintercept<- as.data.frame(coeff[1,])
  row.names(varintercept)<-colnames(df[i])
  intercept<-rbind(intercept, varintercept)
  }

##### Error bar plot #####

####df reformatting####

#slopes
library(data.table)
setDT(slopes, keep.rownames = TRUE)[]
colnames(slopes)<-c("Parameter", "Slope", "inf", "sup")

#intercepts
setDT(intercept, keep.rownames = TRUE)[]
colnames(intercept)<-c("Parameter", "Intercept", "inf", "sup")

pdf()
#plot slopes
ggplot(data=slopes, aes(x=Slope, y=Parameter))+
ggtitle("Slopes")+
  geom_errorbarh(aes(xmin=inf, xmax=sup), color="darkgrey")+
  geom_point(color="red")+
  geom_vline(xintercept=1, color="blue")+
  geom_text(aes(x=-15, y=Parameter,
                label=paste("Slope:", round(Slope,2))),
            size=4, hjust="inward")+
  geom_text(aes(x=-9, y=Parameter,
                label=paste("[ ",
                            round(inf,2), " ;",sep="")),
            size = 4, hjust="inward")+
  geom_text(aes(x=-6, y=Parameter,
                label=paste(round(sup,2), " ]", sep="")),
            size = 4, hjust="inward")+
  scale_x_continuous(limits=c(-15,20))+
  theme_bw()+
  theme(plot.title = element_text(hjust = 0.5))+
  labs(title="Slopes", subtitle="The blue line represents the optimal value (1) for the slopes of the regression.\nIf a estimated slope for a parameter is \nhigher or lower than this line, it means that proportional error is\npresent, therefore the two methods are not interchangeable")


# plot intercepts
ggplot(data=intercept, aes(x=Intercept, y=Parameter))+
  geom_errorbarh(aes(xmin=inf, xmax=sup), color="darkgrey")+
  geom_point(color="red")+
  geom_vline(xintercept=0, color="blue")+
  geom_text(aes(x=-20, y=Parameter,
                label=paste("Intercept:", round(Intercept,2))),
            size=4, hjust="inward")+
  geom_text(aes(x=-12, y=Parameter,
                label=paste("[ ",
                            round(inf,2), " ; ",sep="")),
            size = 4, hjust="inward")+
  geom_text(aes(x=-8, y=Parameter,
                label=paste(round(sup,2), " ]", sep="")),
            size = 4, hjust="inward")+
  scale_x_continuous(limits=c(-20,20))+
  theme_bw()+
  labs(title="Intercept", subtitle="The blue line represents the optimal value (0) for the intercepts of the regression.\nIf a estimated intercept for a parameter is \nhigher or lower than this line, it means that constant error is\npresent, therefore the two methods are not interchangeable")+
  theme(plot.title = element_text(hjust = 0.5))

dev.off()
