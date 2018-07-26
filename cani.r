#load library
library(MethComp)

#import data
df<-read.csv("cani.csv", header = TRUE, sep = ";", dec= ",", stringsAsFactors = FALSE)
# 
# #ignore columns
# df<-df[,1:22]
# 
# head(df)
# str(df)


#delete record number 99
df<-df[-c(393:396),]
df_Meth<-Meth(df, 2,1,3,20)
df_Meth$meth<-factor(df_Meth$meth, levels=c("Procyte Idexx", "DF-50"))
summary(df_Meth)
plot(df_Meth)

BA.plot(df_Meth, repl.conn = FALSE)

PBdf <- PBreg(df_Meth)
# par(mfrow=c(1,2))
plot(PBdf)




plot(df[,-c(1,2,3, 13:25)])








#convert data to Meth object type and save pdf with meth plot


pdf()

for(i in c(4:25)){

df_Meth<-Meth(df, 2,1,3,i)
df_Meth$meth<-factor(df_Meth$meth, levels=c("Procyte Idexx", "DF-50"))
par(mfrow=c(1,2))
plot(df_Meth)
title(main=list(colnames(df[i]), col="red"), line=3, adj=0.45)
# reg<-PBreg(df_Meth)
# print(reg)


print(i)
}
dev.off()



layout(matrix(c(1,1,2),nrow=1,ncol=3, byrow=TRUE))
par(ps=16)
#Bland-Altman plot
mycol<-rgb(193,193,193, maxColorValue = 255, alpha=255/3)
p<-BA.plot(df_Meth, repl.conn=FALSE, meth.names = TRUE, eqax = TRUE, pch.points = 21, bg= mycol, main=2) 

par(mfrow=c(2,1))
#passing  bablok regression

reg<-PBreg(df_Meth)
print(reg)

plot(reg,subtype= 1, main=1)



     