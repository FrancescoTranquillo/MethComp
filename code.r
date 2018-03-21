#load library
library(MethComp)

#import data
df<-read.csv("cani.csv", header = TRUE, sep = ";", dec= ",", stringsAsFactors = FALSE)
str(df)

  
#convert data to Meth object type

pdf()

for(i in c(4:25)){
  
  

df_Meth<-Meth(df, 2,1,3,i)


plot(df_Meth)
title(main=list(colnames(df[i]), col="red"), line=3, adj=0.45)
print(i)
}
dev.off()


#Bland-Altman plot

BA.plot(df_Meth, repl.conn=FALSE, meth.names = TRUE)


#passing  bablok regression

reg<-PBreg(df_Meth)
print(reg)

plot(reg,subtype= 1, xlim = c(0,50), ylim= c(0,50))

#concordance correlation coefficient agreement=(misure del target, misure del test)


     