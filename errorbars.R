##### Cani ####

##### Data prep #####
#load library
library(MethComp)

#import data
df<-read.csv("cani.csv", header = TRUE, sep = ";", dec= ",", stringsAsFactors = FALSE)

#delete record number 99
df<-df[-c(393:396),]
head(df)

##### Ciclo for: PB regression su tutti i parametri ed estrazione dei coefficienti della slope ####
slopes<-NULL

for(i in c(4:25)){
  
  #set meth object
  df_Meth<-Meth(df, 2,1,3,i)
  df_Meth$meth<-factor(df_Meth$meth, levels=c("Procyte Idexx", "DF-50"))
  
  #PB regression
  reg<-PBreg(df_Meth)
  
  #save slope coefficients and CI limits
  coeff<-as.data.frame(reg[["coefficients"]])
  varslope<- as.data.frame(coeff[2,])
  row.names(varslope)<-colnames(df[i])
  slopes<-rbind(slopes, varslope)
  }

##### Error bar plot #####

#slopes df reformatting

library(data.table)
setDT(slopes, keep.rownames = TRUE)[]
colnames(slopes)<-c("Parameter", "Estimate", "inf", "sup")

#plot
ggplot(data=slopes, aes(x=Estimate, y=Parameter))+
  geom_errorbarh(aes(xmin=inf, xmax=sup), color="darkgrey")+
  geom_point(color="red")+
  geom_vline(xintercept=0, color="blue")+
  geom_text(aes(x=-15, y=Parameter, 
                label=paste("Estimate:", round(Estimate,2))),
            size=4, hjust="inward")+
  geom_text(aes(x=-8, y=Parameter, 
                label=paste("[ ",
                            round(inf,2), " ;",sep="")),
            size = 3, hjust="inward")+
  geom_text(aes(x=-6, y=Parameter, 
                label=paste(round(sup,2), " ]", sep="")),
            size = 3, hjust="inward")+
  scale_x_continuous(limits=c(-15,20))+
  theme_bw()
  