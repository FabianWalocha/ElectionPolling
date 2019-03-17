rm(list=ls())

library(haven)
library(dplyr)
ZA2391_v9_0_0 <- read_sav("C:/Users/fwalo/Downloads/ZA2391_v9-0-0.sav/ZA2391_v9-0-0.sav");
dataset <- ZA2391_v9_0_0;
source("R Projects/preprocessing.R")
dset = preprocessing(dataset)

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