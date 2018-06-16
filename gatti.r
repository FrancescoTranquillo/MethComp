#load library
library(MethComp)

#import data
df<-read.csv("gatti.csv", header = TRUE, sep = ";", dec= ",", stringsAsFactors = FALSE)
# 
# #ignore columns
# df<-df[,1:22]
# 
# head(df)
# str(df)
# #delete record number 99
# df<-df[-c(393:396),]
# 


#convert data to Meth object type and save pdf with meth plot


pdf()

for(i in c(4:22)){


df_Meth<-Meth(df, 2,1,3,i)
df_Meth$meth<-factor(df_Meth$meth, levels=c("Procyte Idexx", "DF-50"))

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


     