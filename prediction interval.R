library(plotly)
library(ggplot2)
library(MethComp)
library(reshape2)
library(BivRegBLS)
#### Data Preparation ####
df<-read.csv("cani.csv", header = TRUE, sep = ";", dec= ",", stringsAsFactors = FALSE)
#delete record number 99
df<-df[-c(393:396),]
head(df)

#### Table transformation ####

#RBC only
df<-df[,-c(1, 5:25)]

#extract values derived by specific method and replicate number and store them into vectors
new_1<-df[df$methods=="DF-50" & df$replicate==1, 3]
new_2<-df[df$methods=="DF-50" & df$replicate==2, 3]
ref_1<-df[df$methods=="Procyte Idexx" & df$replicate==1, 3]
ref_2<-df[df$methods=="Procyte Idexx" & df$replicate==2, 3]

#creating new table which columns are composed by previous vectors

#table with replicates
tab_rep<-data.frame(new_1, new_2, ref_1, ref_2)

#table without replicates
tab_norep<-data.frame(new_1, ref_1)

#### Starting Plots ####

plot(tab_norep$new_1, tab_norep$ref_1,
     xlab= "RBC new method",
     ylab= "RBC reference method"
)

#### Linear regression ####

fit.lm<- lm(tab_norep$ref_1 ~ tab_norep$ new_1)

i=3
plot(tab_norep$new_1, tab_norep$ref_1,
     xlab= paste0(colnames(df[i])," new method"),
     ylab= paste0(colnames(df[i])," reference method"),
     title(main=list(colnames(df[i]), col="red"))
)



#confidence interval for the linear regression:


CI_ex <- predict(fit.lm, interval="confidence")
colnames(CI_ex)<- c("fit_CI", "lwr_CI", "upr_CI")


#prediction interval for the linear regression:
PI_ex <- predict(fit.lm, interval="prediction")
colnames(PI_ex)<- c("fit_PI", "lwr_PI", "upr_PI")

#storing and plotting of the intervals:
var_results<-cbind(tab_norep,CI_ex, PI_ex)
var_results<-var_results[order(var_results$new_1),]

plot(tab_norep$new_1, tab_norep$ref_1,
     xlab= "RBC new method",
     ylab= "RBC reference method"
)
lines(x=var_results$new_1, y=var_results$lwr_PI)
lines(x=var_results$new_1, y=var_results$upr_PI)
lines(x=var_results$new_1, y=var_results$fit_PI)

#area between the intervals

varx= var(tab_norep$new_1)
vary= var(tab_norep$ref_1)
RCB.BLS=BLS(data=tab_norep, xcol=c("new_1"), ycol=c("ref_1"), var.y = vary ,var.x = varx, conf.level=0.95)
XY.plot(RCB.BLS, graph.int = c("CI", "PI"),accept.int = 5)




