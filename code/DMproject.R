################## DATA MINING PROJECT ####################

##### INPUT AND CLEANING OF DATA
rm(list=ls())

library(haven)
library(dplyr)
ZA2391_v9_0_0 <- read_sav("../data/ZA2391_v9-0-0.sav");
dataset <- ZA2391_v9_0_0;
source("preprocessing.R")
dset = preprocessing(dataset)


##### VISUALIZATIONS AND MODELING
### barplot political leaning over time
years22 = c(1987:1993)
stacked = matrix(0L, nrow = length(years22),ncol = 11);
for(idx in 1:length(years22)){
  vals = table(na.omit(dset[dset["v4"]==years22[idx],"v22"]))
  for(idx2 in names(vals)){
    # Stacked Percent
    stacked[idx,as.numeric(idx2)] = vals[idx2]
  }
}
library(RColorBrewer)
coul = brewer.pal(11, "RdBu") 
stacked = as.data.frame(stacked)
stacked = t(apply(stacked,1,function(x){x*100/sum(x)}))
rownames(stacked) = years22
barplot(t(stacked) , col=rev(coul), border="white",xlab="left-to-right-leaning (% of total)",
        names.arg = years22,horiz = TRUE,las=1)



### Deviation from center
dExtr = na.omit(dset[,c("v4","v22")])
# how much from the center position
dExtr["v22"] = apply(dExtr["v22"],1,function(x) abs(as.numeric(x)-6))
mean_ex = matrix(0L, nrow=1,ncol=dim(unique(dExtr["v4"]))[1])
idx = 1
for(year in t(unique(dExtr["v4"]))){
  mean_ex[idx] = colMeans(dExtr[dExtr["v4"]==year,"v22"])
  idx = idx+1
}
matEx = cbind(unique(dExtr["v4"]),t(mean_ex))
colnames(matEx)=c("year","divergence")
library(ggpubr)
ggscatter(as.data.frame(matEx),x="year",y="divergence", 
          add="reg.line", conf.int=TRUE,
          cor.coef=TRUE,cor.method="pearson",
          xlab="years",ylab="mean divergence from center")


### did people change their opinion who to elect directly after unification
c1 = dataset[!is.na(dataset["v7"])&!is.na(dataset["v6"])&dataset["v4"]==1990,][,c("v7","v6")]

switch_mat = matrix(0L, nrow = 12, ncol = 12) 
for(i in 1:12){
  for(j in 1:12){
    norm_coeff = dim(as.vector(c1[c1["v6"]==j,]["v6"]))[1]
    switch_mat[i,j] = dim(as.vector(c1[c1["v7"]==i&c1["v6"]==j,"v7"]))[1]/norm_coeff
  }
}
parties = labels(attributes(dataset$v6)$labels);
#devtools::install_github("mattflor/chorddiag")
library(chorddiag)
groupColors <- c('blue','black','red','brown','yellow','green')
sm2 = switch_mat[c(1,2,3,4,6,8),c(1,2,3,6,4,8)]
dimnames(sm2) <- list(have = parties[c(1,2,3,6,4,8)],
                      prefer = parties[c(1,2,3,6,4,8)])
chorddiag(t(sm2), groupColors = groupColors)

### GeoData visualization
d_pstate= dataset[!is.na(dataset["v4"])&!is.na(dataset["v4"]),][c("v4","v75","v22")]
pol_cli = matrix(0L, nrow = length(1980:2017), ncol = 17); 
for(i in 1:10){
  for(year in 1980:2017){
    pol_cli[year-1979,i] = mean(sapply(d_pstate[d_pstate["v4"]==year&d_pstate["v75"]==i,]["v22"],as.numeric),na.rm=TRUE)-6
  }
}
pol_cli[,11:17]=NA
rownames(pol_cli) = 1980:2017
colnames(pol_cli) = labels(attributes(dset$v75)$labels)[1:17]
years = 1980:2017
years = years[c(1,3,4,8:24,26:38)]
pol_cli = pol_cli[c(1,3,4,8:24,26:38),]
states = labels(attributes(dset$v75)$labels)[1:17]
order_idx = order(states)[c(1,2,3,5,6,7,8,9,10,11,12,13,14,15,16,17)]
pol_cli = pol_cli[,order_idx]
library(sp)
library(latticeExtra)
gadm <- readRDS("../data/gadm36_DEU_1_sp.rds")
normer = function(x,var2,var3) (x-var2)/(var3-var2);
setToZero = function(x) {
  x[is.na(x)]=0;
  return(x)
}
pc = sapply(pol_cli,normer,var2=min(pol_cli,na.rm = TRUE),var3=max(pol_cli,na.rm = TRUE))
pcm = matrix(pc,ncol=16)
f <-  colorRamp(c("blue","red"))
colors <- rgb(f(pc[!is.na(pc)])/255)
c2 = matrix(c(colors[1:66],rep("#ffffff",each=66),colors[67:165],
              rep("#ffffff",each=33),colors[166:297],rep("#ffffff",each=66),
              colors[301:333],rep("#ffffff",each=33)),ncol=16)
color.bar <- function(lut, min, max=-min, nticks=11, ticks=seq(min, max, len=nticks), title='') {
  scale = (length(lut)-1)/(max-min)
  plot(c(0,10), c(min,max), type='n', bty='n', xaxt='n', xlab='', yaxt='n', ylab='', main=title)
  axis(2, ticks, las=1)
  for (i in 1:(length(lut)-1)) {
    y = (i-1)/scale + min
    rect(0,y,10,y+1/scale, col=lut[i], border=NA)
  }
}
par(mfrow=c(1,3))
color.bar(colorRampPalette(c("blue","red"))(100),1,11)
plot(gadm ,col = c2[4,], main=c("Year = ",years[4]))
plot(gadm ,col = c2[10,], main=c("Year = ",years[10]))


### Regression over political tendencies of each state
d_pstate= dataset[!is.na(dataset["v4"])&!is.na(dataset["v4"]),][c("v4","v75","v22")]
pol_cli = matrix(0L, nrow = length(1980:2017), ncol = 17); 
for(i in 1:10){
  for(year in 1980:2017){
    pol_cli[year-1979,i] = mean(sapply(d_pstate[d_pstate["v4"]==year&d_pstate["v75"]==i,]["v22"],as.numeric),na.rm=TRUE)
  }
}
pol_cli[,11:17]=NA
rownames(pol_cli) = 1980:2017
colnames(pol_cli) = labels(attributes(dset$v75)$labels)[1:17]
years = 1980:2017
years = years[c(1,3,4,8:24,26:38)]
pol_cli = pol_cli[c(1,3,4,8:24,26:38),]
library(easyGgplot2)
library(dplyr)
library(tidyr)
pcy = as.data.frame(pol_cli[,1:10])%>%gather(State,Tendency,labels(attributes(dset$v75)$labels)[1:10])
pcy = cbind(rep(years,10),pcy)
colnames(pcy) = c("Year","State","Tendency")
library(RColorBrewer)
col = brewer.pal(12, "Set3") 
ggplot2.scatterplot(data=pcy, xName='Year',yName='Tendency', 
                    groupName='State', size=3,
                    backgroundColor="white",
                    groupColors=col,
                    addRegLine=TRUE, fullrange=TRUE)  


### ScatterPie chart for 2 most important features (beside v7)
## Feature selection using Boruta
# install.packages("Boruta")
dset["v6"] = apply(dset["v6"],1,as.numeric)
dset["v7"] = apply(dset["v7"],1,as.character)
mask = apply(dset["v6"],1,function(x) x%in%c(1:12,32,43,45))
d2k = dset[mask,]
library(Boruta)
boruta_output <- Boruta(v6 ~ ., data=na.omit(d2k[,!(colnames(d2k) %in% c("v4"))]), doTrace=2) 
boruta_signif <- names(boruta_output$finalDecision[boruta_output$finalDecision %in% c("Confirmed", "Tentative")])
plot(boruta_output, cex.axis=.7, las=2, xlab="", main="Variable Importance")
# install.packages("devtools")
library(devtools)
# devtools::install_github("easyGgplot2", "kassambara")
library(ggplot2)
library(easyGgplot2)
library(pracma)
dset["v6"] = apply(dset["v6"],1,as.numeric)
dset["v7"] = apply(dset["v7"],1,as.character)
mask = apply(dset["v6"],1,function(x) x%in%c(1:12,32,43,45))
d2k = dset[mask,]
tmp = na.omit(d2k[apply(d2k["v6"],1,function(x) as.numeric(x) %in% c(2:7)),c("v6","v9","v25")])
tmp["v9"] = apply(tmp["v9"],1,as.numeric)
tmp["v25"] = apply(tmp["v25"],1,as.numeric)
tmp["v6"] = apply(tmp["v6"],1,as.numeric)
# only take the maximum label for each combination of points
norm_coeff = table(tmp["v6"])
full_mat=matrix(c(0,0,0,0,0,0,0,0,0),ncol=1)
for(i in 1:11){
  for(j in 1:5){
    vals_full = matrix(0L, nrow = 6, ncol = 1)
    radius = dim(tmp[tmp["v9"]==i&tmp["v25"]==j,"v6"])[1]
    vals = table(tmp[tmp["v9"]==i&tmp["v25"]==j,"v6"])
    for(k in 1:length(vals)){
      vals_full[as.numeric(names(vals)[k])-1]=vals[k]
    }
    vals_full = as.vector(vals_full)/as.vector(norm_coeff)
    full_mat = cbind(full_mat,c(i,j,vals_full,1/3*sigmoid(log(radius),a=0.5)))
  }
}
full_mat = t(full_mat)
colnames(full_mat) = c("x","y",parties[2:7],"radius")
full_mat = full_mat[-1,]
full_mat = as.data.frame(full_mat)
# install.packages("scatterpie")
library(scatterpie)
ggplot() + geom_scatterpie(aes(x=x, y=y,r=radius), 
                           data=full_mat,
                           cols=parties[2:7]) +
  coord_fixed()+ xlab("Satisfaction CDU party") +
  ylab("Satisfaction economy")


