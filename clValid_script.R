library(clValid)
library(kohonen)
library(mclust)
set.seed(123)

my_cols <- c("green", "yellow", "red", "blue","pink") 

#swissbank
dat <- read.delim("SwissBankNotes.txt")
mydata<-dat[,1:6]
#mydata=scale(dat[,1:6])
pairs(dat[,1:6], pch = 19,  cex = 0.5,
      col = my_cols[dat[,ncol(dat)]])

## scenari
# 1 - data_scen12
# 2 - data_scen13
# 3 - data_scen14
dat <- read.delim("data_scen14.txt", header = FALSE)
nrow(dat)
rownames(dat)=1:nrow(dat)
mydata<-dat[,-ncol(dat)]
mydata
pairs(scale(mydata), pch = 19,  cex = 0.5,
      col = my_cols[dat[,ncol(dat)]])

#Breast cancer
dat <- read.csv("breast-cancer.csv")
my_cols <- c("#00AFBB", "#E7B800") 
head(dat)
dat=dat[,-1]
label=dat[,1]
label
dat=dat[,-1]
mydata=dat
pairs(scale(mydata), pch = 19,  cex = 0.5,
      col = my_cols[dat[,ncol(dat)]])
rownames(mydata)=1:nrow(mydata)
nrow(mydata)
ncol(mydata)


#EPI 
library(readxl)
Dataset_es <- read_excel("Epi_2018_modificato.xlsx")
Dataset_es
dim(Dataset_es)
anyNA(Dataset_es)
#remove rows with missing values in any column of data frame
colSums(is.na(Dataset_es))
dat_es <- Dataset_es[complete.cases(Dataset_es), ]
dim(dat_es)
data_es=as.data.frame(dat_es)
###now we have 91 states
#The data are already explained in the same unit of measure
data_es
#Rownames
rownames(data_es)=data_es$iso
label=data_es[,1]
label
par(mfrow=c(1,1))
nrow(data_es)
data_es=data_es[,-1]
mydata=data_es





## internal validation 

#valid_test <- clValid(mydata, c(2:5), clMethods = c("kmeans", "model", "som", "pam", "clara", "agnes", "sota", "funny"), validation = c("internal", "stability"))
#Model based clustering
valid_test <- clValid(scale(mydata), c(2:5), clMethods = c("model"), validation = c("internal", "stability"))
#Kmeans clustering
#valid_test <- clValid(scale(mydata), c(2:5), clMethods = c("kmeans"), validation = c("internal", "stability"))

summary(valid_test)
optimalScores(valid_test)
tab<-optimalScores(valid_test)
#plot(valid_test)

###tsne 
library(tsne)
library(plotly)
set.seed(123)
tsne <- tsne(scale(mydata), initial_dims = 2)
tsne <- data.frame(tsne)
mydata1=tsne
rownames(mydata1)<-1:nrow(mydata1)

## internal validation 

#valid_test <- clValid(mydata, c(2:5), clMethods = c("kmeans", "model", "som", "pam", "clara", "agnes", "sota", "funny"), validation = c("internal", "stability"))
#model based clustering
valid_test <- clValid(mydata1, c(2:5), clMethods = c("model"), validation = c("internal", "stability"))
#kmeans clustering
#valid_test <- clValid(mydata1, c(2:5), clMethods = c("kmeans"), validation = c("internal", "stability"))

summary(valid_test)
optimalScores(valid_test)
tab1<-optimalScores(valid_test)
#plot(valid_test)


### umap
library(plotly) 
library(umap) 
umap = umap(scale(mydata), n_components = 2, random_state = 15, n_neighbours=15) 
layout <- umap[["layout"]] 
layout <- data.frame(layout) 
mydata2 = layout
rownames(mydata2)=1:nrow(mydata2)

## internal validation

#valid_test <- clValid(mydata, c(2:5), clMethods = c("kmeans", "model", "som", "pam", "clara", "agnes", "sota", "funny"), validation = c("internal", "stability"))
#model based clustering
valid_test <- clValid(mydata2, c(2:5), clMethods = c("model"), validation = c("internal", "stability"))
#kmeans clustering
#valid_test <- clValid(mydata2, c(2:5), clMethods = c("kmeans"), validation = c("internal", "stability"))


summary(valid_test)
optimalScores(valid_test)
tab2<-optimalScores(valid_test)
tab2
#plot(valid_test)

mydata

####PCA
library(tidyverse)
results_prc <- prcomp(mydata, scale = TRUE)

results_prc$x[,1:2]

valid_test <- clValid(results_prc$x[,1:2], c(2:5), clMethods = c("model"), validation = c("internal", "stability"))
summary(valid_test)
optimalScores(valid_test)
tab4<-optimalScores(valid_test)
tab4
#plot(valid_test)




##MDS
library(StatMatch)
mal_dis=mahalanobis.dist(scale(mydata))
dis_n=as.dist(mal_dis)
mds_result <- cmdscale(dis_n,k=2)
head(mds_result)
mds_result=data.frame(mds_result)
mds_result
#dist=dist(scale(mydata))
#mds_result <- cmdscale(dist, k=2)
#mds_result
valid_test <- clValid(mds_result, c(2:5), clMethods = c("model"), validation = c("internal", "stability"))
summary(valid_test)
optimalScores(valid_test)
tab5<-optimalScores(valid_test)
tab5


library(xtable)
tabb<-cbind(tab[,1],tab1[,1],tab2[,1], tab4[,1],tab5[,1])
tabb<-xtable(tabb)
row.names(tabb)<-row.names(tab)
tabb
