# outliers 10%, sparse pointwise contamination
#clustering with two classes
dat <- read.delim("data_scen13.txt")
my_cols <- c("#00AFBB", "#E7B800", "#FC4E07","#9E0142") 
dat[,6]
#4 cluster
dat[,1:5]
pairs(dat[,1:5], pch = 19,  cex = 0.5,
      col = my_cols[dat[,6]],
      lower.panel=NULL)
dat[,6]
dat$X1=ifelse(dat$X1%in%c(1,2,3),1,2)
dat$X1
#now we have two classes

###tsne 
#install.packages("tsne")
library(tsne)
library(plotly)
set.seed(12)
tsne <- tsne(dat[,1:5], initial_dims = 2)

tsne <- data.frame(tsne)
pdb <- cbind(tsne,dat$X1)
tsne
options(warn = -1)
fig <-  plot_ly(data = pdb ,x =  ~X1, y = ~X2, type = 'scatter', mode = 'markers', split = ~dat$X1)
fig <- fig %>%
  layout(
    plot_bgcolor = "#e5ecf6"
  )
fig

#with scaled data (in this case data need to be scaled)
tsne <- tsne(scale(dat[,1:5]), initial_dims = 2)
scale(dat[,1:5])
tsne_s <- data.frame(tsne)
pdb <- cbind(tsne_s,dat$X1)
options(warn = -1)

fig <-  plot_ly(data = pdb ,x =  ~X1, y = ~X2, type = 'scatter', mode = 'markers', split = ~dat$X1)
fig <- fig %>%
  layout(
    plot_bgcolor = "#e5ecf6"
  )
fig

#Way better
####################### just for fun
##Giving as input mahalanobis distance
#mahalanobis()
#cov_=cov(scale(dat[,1:5]))
#x=dat[,1:5]
#mal=mahalanobis(x, center=colMeans(x),cov= cov_)
#class(mal)
#type(mal)
#mal=as.dist(mal)
#tsne <- tsne(mal, initial_dims = 2)
#tsne_m <- data.frame(tsne)
#pdb <- cbind(tsne_m,dat$X1)
#options(warn = -1)

#fig <-  plot_ly(data = pdb ,x =  ~X1, y = ~X2, type = 'scatter', mode = 'markers', split = ~dat$X1)
#fig <- fig %>%
#  layout(
#    plot_bgcolor = "#e5ecf6"
#  )
#fig
library(factoextra)
#eclust(tsne_m, FUNcluster = "kmeans")
#it will not find any cluster

####it doesn't make any sense




### umap
library(plotly) 
library(umap) 
umap = umap(dat[,1:5], n_components = 2, random_state = 15) 
layout <- umap[["layout"]] 
layout <- data.frame(layout) 
final <- cbind(layout, dat$X1) 

fig <- plot_ly(final, x = ~X1, y = ~X2, color = ~dat$X1, type = 'scatter', mode = 'markers')%>%  
  layout(
    plot_bgcolor = "#e5ecf6",
    legend=list(title=list(text='cluster')), 
    xaxis = list( 
      title = "0"),  
    yaxis = list( 
      title = "1")) 
fig
#Umapseems to be  working  better with this type of data

#With scaled data
umap_s = umap(scale(dat[,1:5]), n_components = 2, random_state = 15, n_neighbours=15) 
layout_s <- umap_s[["layout"]] 
layout_ss <- data.frame(layout_s) 


final <- cbind(layout_ss, dat$X1) 

fig <- plot_ly(final, x = ~X1, y = ~X2, color = ~dat$X1, type = 'scatter', mode = 'markers')%>%  
  layout(
    plot_bgcolor = "#e5ecf6",
    legend=list(title=list(text='cluster')), 
    xaxis = list( 
      title = "0"),  
    yaxis = list( 
      title = "1")) 
fig

#I decided to work only with scaled data


###Kmeans with scaled data
mydata_s=scale(dat[,1:5])
wss_s <- (nrow(mydata_s))*sum(apply(mydata_s,2,var))
for (i in 2:10) wss_s[i] <- sum(kmeans(mydata_s, 
                                       centers=i)$withinss)

wss_s
plot(1:10, wss_s, type="b", xlab="k", ylab="Within Deviance")
##We already know that we have two clusters in this case

library(NbClust)
NbClust(mydata_s, method="kmeans")
#2

#According to the majority rule, the best number of clusters is  2 


## Explore K mean solution 
fit <- kmeans(mydata_s, 2)

aggregate(mydata_s,by=list(fit$cluster),FUN=mean)

#plot results of final k-means model
library(factoextra)
library(cluster)
fviz_cluster(fit, data = mydata_s)



#install.packages("tclust")
library(tclust)
plot (ctlcurves (mydata_s, k = 2, alpha = seq (0, 0.3, by = 0.025)))
clus <- tclust (mydata_s, k = 2, alpha = 0.1, restr.fact = 50)


### silhuette comparison

#library (vegan)
#library (cluster)
#dis_n = dist(mydata_s)
#sil_orig=silhouette(dat$X1,dis_n)
#windows()
#plot(sil_orig)
###################  kmeans
#############################################
dis = dist(mydata_s)^2
res = kmeans(mydata_s,2)
sil_k = silhouette (res$cluster, dis)
windows()
plot(sil_k)
sil_l_m=mean(sil_k[,3])
sil_l_m
#Dunn index
#Dunn index has values betwee zero and infinity and it needs to be maximised
library(clValid)
D_k<-dunn(clusters=res$cluster, distance=dis)
D_k
print(paste("Dunn Index:", D_k))

#Adjusted rand index
library(cluster)
library(fossil)

kmeans_ari=adj.rand.index(dat$X1, res$cluster)

fviz_cluster(res,data=mydata_s,palette = c("#2E9FDF", "#FFA500"), 
             geom = "point",
             ellipse.type = "convex",
             ggtheme = theme_bw())

########################Trimmed k-means clustering
#################################################
#install.packages("trimcluster")
#library(trimcluster)
library(tclust)
t_K=tkmeans(mydata_s,2,alpha=0.1,iter.max =100)
print(t_K)
par(mfrow=c(1,1))
plot(t_K,mydata_s)
table(t_K$cluster)
sil_t_K = silhouette (t_K$cluster, dis)
windows()
plot(sil_t_K)
sil_t_K
sil_t_K_val=mean(sil_t_K[,3])
sil_t_K_val

D_t_k= dunn(cluster=t_K$cluster,distance= dis)
D_t_k
#0.02038168
#ARI
tk_ari=adj.rand.index(dat$X1,t_K$cluster)
tk_ari


par(mfrow=c(1,1))
#####################  tclust
############################################
set.seed(123)
library(tclust)
#Scaled data
dis_s = dist(mydata_s)^2
clus_s <- tclust (mydata_s, k = 2, alpha = 0.1, restr.fact = 50)
sil_t_s = silhouette (clus_s$cluster, dis_s)
windows()
plot(sil_t_s)
sil_t_ss=mean(sil_t_s[,3])
#Dunn index
D_t_s<-dunn(clusters=clus_s$cluster, distance=dis_s)
D_t_s
print(paste("Dunn Index:", D_t_s))
#Adjusted rand index
tclust_ari=adj.rand.index(dat$X1,clus_s$cluster)
par(mfrow=c(1,1))
plot(clus_s)

########PAM: Partitioning around medoid
#######################################
#Pam scaled
pam_result_s <- pam(mydata_s, k = 2)
# Extract cluster assignments from PAM results
cluster_results_s <- pam_result_s$cluster
cluster_results_s
sil_pam_s=silhouette(cluster_results_s,dis_s)
windows()
plot(sil_pam_s)
sil_pam_m_s=mean(sil_pam_s[,3])
#Dunn index
D_p_s<-dunn(clusters=pam_result_s$cluster, distance=dis_s)
D_p_s
print(paste("Dunn Index:", D_p_s))
#ARI
pam_ari=adj.rand.index(dat$X1,pam_result_s$cluster)
fviz_cluster (pam_result_s, 
              palette = c("#00AFBB", "#FC4E07"), # color palette
              ellipse.type = "t",  #Concentration ellipse 
              ggtheme = theme_classic() 
)



####    mclust
###################################
library(mclust)
#Scaled
mclust_model_s <- Mclust(mydata_s, G = 2)
mclust_model_s$classification
mclust_mc_s=silhouette(mclust_model_s$classification,dis_s)
windows()
plot(mclust_mc_s)
sil_mc_s=mean(mclust_mc_s[,3])
#Dunn index
D_m_s<-dunn(clusters=mclust_model_s$classification, distance=dis_s)
D_m_s
print(paste("Dunn Index:", D_m_s))
#Adjusted rand index
mc_ari=adj.rand.index(dat$X1,mclust_model_s$classification)
#plot(mclust_model_s)


###############################
library(cluster)

#Compute rand index with scaled variables
ri_k_s <- rand.index(dat$X1, res$cluster)
ri_k_s
# Calculate the Rand Index
ri_mclust_s <- rand.index(dat$X1,mclust_model_s$classification)
ri_tclust_s <- rand.index(dat$X1,clus_s$cluster)

ri_pam_s <- rand.index(dat$X1, pam_result_s$cluster)


#DB
library(clusterSim)
DB_kmeans=index.DB(mydata_s, cl=res$cluster, d=dis_s, centrotypes="centroids", p=2, q=2)$DB
DB_tclust=index.DB(mydata_s, cl=clus_s$cluster, d=dis_s, centrotypes="centroids", p=2, q=2)$DB
DB_PAM=index.DB(mydata_s, cl=pam_result_s$clustering, d=dis_s, centrotypes="centroids", p=2, q=2)$DB
DB_mclust=index.DB(mydata_s, cl=mclust_model_s$classification, d=dis_s, centrotypes="centroids", p=2, q=2)$DB
DB_tkmeans=index.DB(mydata_s, cl=t_K$cluster, d=dis_s, centrotypes="centroids", p=2, q=2)$DB



############Display the metrics
#######################################################
#RI=c(ri_k_s,ri_tclust_s,ri_pam_s,ri_mclust_s),
#ARI=c(kmeans_ari,tclust_ari,pam_ari,mc_ari),
#Table with scaled data
Scaled_metrics_table <- data.frame(
  Method = c("kmeans", "tclust", "PAM","mclust", "tkmeans"),
  Silhouette_Score = c(sil_l_m, sil_t_ss,sil_pam_m_s ,sil_mc_s, sil_t_K_val),
  DI=c(D_k,D_t_s,D_p_s,D_m_s, D_t_k),
  DB=c(DB_kmeans,DB_tclust,DB_PAM,DB_mclust,DB_tkmeans)
)
# Print the table
print(Scaled_metrics_table)

library(xtable)
xtable(Scaled_metrics_table)


#######################################################
###################INTEGRATED APPROACHES###############
#######################################################


################Integrated approach with tsne as preprocessing
########### tsne+kmeans
############################
NbClust(data=tsne_s, distance="euclidean", method="kmeans")
##In this case according to the majority rule the best number of cluster is 3
NbClust(data=tsne_s, distance="euclidean", method="centroid")
#3


#MCsim
#install.packages("MCSim")
#library(MCSim)
#tsne_s
#MCS(mydata_s,nc=6, method1="kmeans", index="rand", print.stats=TRUE,
#    st.data=FALSE, plot.hc=FALSE)
library(factoextra)
fviz_nbclust(tsne_s, kmeans,method = "wss")
fviz_nbclust(tsne_s, kmeans,method = "silhouette")
#3 clusters


##optimal number of clusters using tkmeans

###try to determine the optimal number of clusters when using tclust




library(fpc)
pam.best=pamk(tsne_s)
pam.best
#3

###determine
require(vegan)
fit <- cascadeKM(tsne_s, 1, 10, iter = 1000)
plot(fit, sortg = TRUE, grpmts.plot = TRUE)
calinski.best <- as.numeric(which.max(fit$results[2,]))
cat("Calinski criterion optimal number of clusters:", calinski.best, "\n")
#10



dis_s = dist(tsne_s)^2
res_t_k_s = kmeans(tsne_s,3)
sil_t_k_s = silhouette (res_t_k_s$cluster, dis_s)
windows()
plot(sil_t_k_s)
tsne_kmeans_sil_s=mean(sil_t_k_s[,3])
fviz_cluster(res_t_k_s,data=tsne_s,palette = c("#2E9FDF", "#00AFBB","#9E0142"), 
             geom = "point",
             ellipse.type = "convex", 
             ggtheme = theme_bw())

###tsne+tkmeans
#library(trimcluster)
t_t_K=tkmeans(tsne_s,3,alpha=0.1,iter.max =100)
print(t_t_K)
par(mfrow=c(1,1))
plot(t_t_K,tsne_s,xlab="variables",ylab="variables")
table(t_K$cluster)
sil_t_t_K = silhouette (t_t_K$cluster, dis_s)
windows()
plot(sil_t_t_K)
sil_t_t_K_val=mean(sil_t_t_K[,3])
sil_t_t_K_val

D_t_t_k= dunn(cluster=t_t_K$cluster,distance= dis_s)
D_t_t_k
#0.02288067


#tsne+tclust
#Scaled
clus_t_t_s <- tclust(tsne_s, k = 3, alpha = 0.1, restr.fact = 50)
sil_ts_tc_s = silhouette (clus_t_t_s$cluster, dis_s)
windows()
plot(sil_ts_tc_s)
tsne_tcl_sil_s=mean(sil_ts_tc_s[,3])
tsne_tcl_sil_s
plot(clus_t_t_s)


#tsne+PAM
pam_result_s <- pam(tsne_s, k = 3)
tsn_pam_s=silhouette(pam_result_s$clustering,dis_s)
windows()
plot(tsn_pam_s)
tsn_pam_ss=mean(tsn_pam_s[,3])
tsn_pam_ss
fviz_cluster (pam_result_s, 
              palette = c("#00AFBB", "#FC4E07","#9E0142"), # color palette
              ellipse.type = "t",  #Concentration ellipse 
              ggtheme = theme_classic() 
)




##tsne+mclust
#understand the number of cluster
d_clust<- Mclust(tsne_s, G=2:4)
m.best <- dim(d_clust$z)[2]
m.best
cat("model-based optimal number of clusters:", m.best, "\n")
plot(d_clust)



tsne_mcl_s <- Mclust(tsne_s, G = 3)
tsne_mcl_s$classification
tsne_mcl_sil_s=silhouette(tsne_mcl_s$classification,dis_s)
windows()
plot(tsne_mcl_sil_s)
sil_r_mc=mean(tsne_mcl_sil_s[,3])
plot(tsne_mcl_s)



###Rand index
ri_tsne_k_s <- rand.index(dat$X1, res_t_k_s$cluster)
ri_tsne_tclust_s <- rand.index(dat$X1,clus_t_t_s$cluster)
ri_tsne_pam_s <- rand.index(dat$X1, pam_result_s$clustering)
ri_tsne_mc_s<- rand.index(dat$X1,tsne_mcl_s$classification)


##Dunn index with scaled variables
D_tsne_k_s<-dunn(clusters=res_t_k_s$cluster, distance=dis_s)
D_tsne_tclust_s<-dunn(clusters=clus_t_t_s$cluster, distance=dis_s)
D_tsne_pam_s<-dunn(clusters=pam_result_s$clustering, distance=dis_s)
D_tsne_mc_s<-dunn(clusters=tsne_mcl_s$classification, distance=dis_s)
D_tsne_tk_s<-dunn(clusters=t_t_K$cluster,distance=dis_s)

#Adjusted rand index
tsne_kmeans_ari=adj.rand.index(dat$X1, res_t_k_s$cluster)
tsne_tclust_ari=adj.rand.index(dat$X1, clus_t_t_s$cluster)
tsne_pam_ari=adj.rand.index(dat$X1, pam_result_s$clustering)
tsne_mc_ari=adj.rand.index(dat$X1, tsne_mcl_s$classification)

#Davies Bouldin index (minimized)
DB_kmeans=index.DB(mydata_s, cl=res_t_k_s$cluster, d=dis_s, centrotypes="centroids", p=2, q=2)$DB
DB_tclust=index.DB(mydata_s, cl=clus_t_t_s$cluster, d=dis_s, centrotypes="centroids", p=2, q=2)$DB
DB_PAM=index.DB(mydata_s, cl=pam_result_s$clustering, d=dis_s, centrotypes="centroids", p=2, q=2)$DB
DB_mclust=index.DB(mydata_s, cl=tsne_mcl_s$classification, d=dis_s, centrotypes="centroids", p=2, q=2)$DB
DB_tkmeans=index.DB(mydata_s, cl=t_t_K$cluster, d=dis_s, centrotypes="centroids", p=2, q=2)$DB


#Integrated approach metrics tables

Scaled_Metrics_table <- data.frame(
  Method = c("t-SNE + kmeans", "t-SNE + tclust", "t-SNE + PAM","t-SNE + mclust", "t-SNE + tkmeans"),
  Silhouette_Score = c(tsne_kmeans_sil_s, tsne_tcl_sil_s, tsn_pam_ss, tsne_kmeans_sil_s, sil_t_t_K_val),
  DI=round(c(D_tsne_k_s,D_tsne_tclust_s,D_tsne_pam_s,D_tsne_mc_s,D_t_t_k),6),
  DB=c(DB_kmeans,DB_tclust,DB_PAM,DB_mclust,DB_tkmeans)
)
# Print the table
print(Scaled_Metrics_table)

#         Method Silhouette_Score        RI       ARI       DI
# t-SNE + kmeans        0.5692363 0.6145645 0.1891762 0.000084
# t-SNE + tclust        0.3790629 0.7924899 0.8986944 0.056470
#    t-SNE + PAM        0.5789387 0.7133446 0.3403034 0.000099
# t-SNE + mclust        0.5692363 0.8958478 0.7008108 0.000052
xtable(Scaled_Metrics_table)



########################## Integrated approach with umap as preprocessing
############umap+kmeans
NbClust(layout_ss, method="kmeans")
library(factoextra)
##best number of cluster is 3


fviz_nbclust(layout_ss, kmeans,method = "wss")
fviz_nbclust(layout_ss, kmeans,method = "silhouette")



dis_ss = dist(layout_ss)^2
res_s = kmeans(layout_ss,3)
sil_umap_km_s = silhouette (res_s$cluster, dis_ss)
windows()
plot(sil_umap_km_s)
umap_km_s=mean(sil_umap_km_s[,3])
umap_km_s
color=c("#2E9FDF", "#00AFBB","#9E0142")
fviz_cluster(res_s,data=layout_ss, palette=color,
             geom = "point",
             ellipse.type = "convex",
             ggtheme = theme_bw())


######Trimmed kmeans

u_t_K=tkmeans(layout_ss,3,alpha=0.1,iter.max =100)
print(u_t_K)
par(mfrow=c(1,1))
plot(u_t_K,layout_ss,xlab="variables",ylab="variables")
table(u_t_K$cluster)
sil_u_t_K = silhouette (u_t_K$cluster, dis_ss)
windows()
plot(sil_u_t_K)
sil_u_t_K_val=mean(sil_u_t_K[,3])
sil_u_t_K_val

D_u_t_k= dunn(cluster=u_t_K$cluster,distance= dis_ss)
D_u_t_k
#0.01437216
#ARI
utk_ari=adj.rand.index(dat$diagnosis,u_t_K$cluster)



#umap+tclust
dis_ss = dist(layout_ss)^2
clus_s <- tclust(layout_ss, k = 3, alpha = 0.1, restr.fact = 50)
sil_umap_tc_s = silhouette (clus_s$cluster, dis_ss)
windows()
plot(sil_umap_tc_s)
umap_tc_s=mean(sil_umap_tc_s[,3])
umap_tc_s

plot(clus_s)


#umap+PAM
library(cluster)
pam_result_s <- pam(layout_ss, k = 3)
sil_pam_s=silhouette(pam_result_s$clustering,dis_ss)
sil_pam_s
windows()
plot(sil_pam_s)
umap_pam_s=mean(sil_pam_s[,3])
umap_pam_s
plot(sil_pam_s)
fviz_cluster(pam_result_s,  # color palette
             ellipse.type = "t",  #Concentration ellipse 
             ggtheme = theme_classic() 
)

###Umap+mclust
umap_mcl_s <- Mclust(layout_s, G = 3)
umap_mcl_s$classification
umap_mcl_sil_s=silhouette(umap_mcl_s$classification,dis_ss)
windows()
plot(umap_mcl_sil_s)
sil_umap_mc_s=mean(tsne_mcl_sil_s[,3])
plot(umap_mcl_s)


###Rand index
library(flexclust)

#Scaled rand index
ri_umap_k_s <- randIndex(dat$X1, res_s$cluster)
ri_umap_tclust_s <- randIndex(dat$X1,clus_s$cluster)
ri_umap_pam_s<- randIndex(dat$X1, pam_result_s$clustering)
ri_umap_mc_s <- randIndex(dat$X1,umap_mcl_s$classification)

#Scaled dunn index
D_umap_k_s<-dunn(clusters=res_s$cluster, distance=dis_ss)
D_umap_tclust_s<-dunn(clusters=clus_s$cluster, distance=dis_ss)
D_umap_pam_s<-dunn(clusters=pam_result_s$clustering, distance=dis_ss)
D_umap_mc_s<-dunn(clusters=umap_mcl_s$classification, distance=dis_ss)

#ARI
umap_kmeans_ari=adj.rand.index(dat$X1,res_s$cluster)
umap_tclust_ari=adj.rand.index(dat$X1, clus_s$cluster)
umap_pam_ari=adj.rand.index(dat$X1, pam_result_s$clustering)
umap_mc_ari=adj.rand.index(dat$X1, umap_mcl_s$classification)

#DB
DB_kmeans=index.DB(mydata_s, cl=res_s$cluster, d=dis_s, centrotypes="centroids", p=2, q=2)$DB
DB_tclust=index.DB(mydata_s, cl=clus_s$cluster, d=dis_s, centrotypes="centroids", p=2, q=2)$DB
DB_PAM=index.DB(mydata_s, cl=pam_result_s$clustering, d=dis_s, centrotypes="centroids", p=2, q=2)$DB
DB_mclust=index.DB(mydata_s, cl=umap_mcl_s$classification, d=dis_s, centrotypes="centroids", p=2, q=2)$DB
DB_tkmeans=index.DB(mydata_s, cl=u_t_K$cluster, d=dis_s, centrotypes="centroids", p=2, q=2)$DB


# Create a Table with metrics with scaled variables
Metrics_table_int_scaled <- data.frame(
  Method = c("UMAP + kmeans", "UMAP + tclust", "UMAP + PAM","UMAP + mclust","UMAP + tkmeans"),
  Silhouette_Score = c(umap_km_s, umap_tc_s, umap_pam_s,sil_umap_mc_s, sil_u_t_K_val),
  DI=round(c(D_umap_k_s,D_umap_tclust_s,D_umap_pam_s,D_umap_mc_s,D_u_t_k),6),
  DB=c(DB_kmeans,DB_tclust,DB_PAM,DB_mclust,DB_tkmeans)
)
# Print the table
print(Metrics_table_int_scaled)

#   Method Silhouette_Score         RI        ARI    DI
# UMAP + kmeans        0.5415706 0.07963598 0.07963598 0.00006
# UMAP + tclust        0.3183134 0.57607254 0.97652319 0.11519
#    UMAP + PAM        0.5861963 0.60814657 0.60814657 0.00018
# UMAP + mclust        0.5622443 0.96039002 0.96039002 0.07096

xtable(Metrics_table_int_scaled)


#PCA
library(tidyverse)
results <- prcomp(mydata_s, scale = FALSE)
res_pca=results$x[,1:2]
head(res_pca)
res_pca=data.frame(res_pca)
is.data.frame(res_pca)
#library(NbClust)
NbClust(res_pca,method="kmeans", distance = "euclidean")
#library(factoextra)
#2 clusters

########################## Integrated approach with PCA as preprocessing
############PCA+kmeans
set.seed(123)
dis_ss = dist(res_pca)^2
res_s = kmeans(res_pca,2)
sil_pca_km_s = silhouette (res_s$cluster, dis_ss)
windows()
plot(sil_pca_km_s)
pca_km_s=mean(sil_pca_km_s[,3])
pca_km_s
res_s$cluster
par(mfrow=c(1,1))
fviz_cluster(res_s,data=res_pca,palette = c("#2E9FDF", "#00AFBB","#9E0142"), 
             geom = "point",
             ellipse.type = "convex", 
             ggtheme = theme_bw())


###
#####Trimmed kmeans
p_t_K=tkmeans(res_pca,2,alpha=0.1,iter.max =100)
print(p_t_K)
par(mfrow=c(1,1))
plot(p_t_K,res_pca,xlab="variables",ylab="variables")
table(p_t_K$cluster)
sil_p_t_K = silhouette (p_t_K$cluster, dis_ss)
windows()

plot(sil_p_t_K)
sil_p_t_K_val=mean(sil_p_t_K[,3])
sil_p_t_K_val
D_p_t_k= dunn(cluster=p_t_K$cluster,distance= dis_ss)
D_p_t_k
library(fossil)




#PCA+tclust
dis_ss = dist(res_pca)^2
clus_s <- tclust(res_pca, k = 2, alpha = 0.1, restr.fact = 100)
sil_pca_tc_s = silhouette (clus_s$cluster, dis_ss)
windows()
plot(sil_pca_tc_s)
pca_tc_s=mean(sil_pca_tc_s[,3])
pca_tc_s

plot(clus_s)


#PCA+PAM
library(cluster)
pam_result_s <- pam(res_pca, k = 2)
sil_pam_s=silhouette(pam_result_s$clustering,dis_ss)
sil_pam_s
windows()
plot(sil_pam_s)
pca_pam_s=mean(sil_pam_s[,3])
pca_pam_s
fviz_cluster(pam_result_s, 
             palette = c("#00AFBB", "#FC4E07","#9E0142"), # color palette
             ellipse.type = "t",  #Concentration ellipse 
             ggtheme = theme_classic() 
)

###PCA+mclust
library(mclust)
pca_mcl_s <- Mclust(res_pca, G = 2)
pca_mcl_s$classification
pca_mcl_sil_s=silhouette(pca_mcl_s$classification,dis_ss)
windows()
plot(pca_mcl_sil_s)
sil_pca_mc_s=mean(pca_mcl_sil_s[,3])

par(mfrow=c(1,1))
plot(pca_mcl_s, what = "classification", 
     addEllipses = TRUE)




#Scaled dunn index
D_pca_k_s<-dunn(clusters=res_s$cluster, distance=dis_ss)
D_pca_tclust_s<-dunn(clusters=clus_s$cluster, distance=dis_ss)
D_pca_pam_s<-dunn(clusters=pam_result_s$clustering, distance=dis_ss)
D_pca_mc_s<-dunn(clusters=pca_mcl_s$classification, distance=dis_ss)
D_pca_tk_s<-dunn(clusters=p_t_K$cluster,distance=dis_ss)

#DB
#DB
library(clusterSim)
DB_pca_kmeans=index.DB(mydata_s, cl=res_s$cluster, d=dis_ss, centrotypes="centroids", p=2, q=2)$DB
DB_pca_tclust=index.DB(mydata_s, cl=clus_s$cluster, d=dis_ss, centrotypes="centroids", p=2, q=2)$DB
DB_pca_PAM=index.DB(mydata_s, cl=pam_result_s$clustering, d=dis_ss, centrotypes="centroids", p=2, q=2)$DB
DB_pca_mclust=index.DB(mydata_s, cl=pca_mcl_s$classification, d=dis_ss, centrotypes="centroids", p=2, q=2)$DB
DB_pca_tkmeans=index.DB(mydata_s, cl=p_t_K$cluster, d=dis_ss, centrotypes="centroids", p=2, q=2)$DB


# Create a Table with metrics with scaled variables

Metrics_table_int_scaled <- data.frame(
  Method = c("PCA + kmeans", "PCA + tclust", "PCA + PAM","PCA + mclust","PCA + tkmeans"),
  Silhouette_Score = c(pca_km_s, pca_tc_s, pca_pam_s,sil_pca_mc_s, sil_p_t_K_val),
  DI=round(c(D_pca_k_s,D_pca_tclust_s,D_pca_pam_s,D_pca_mc_s,D_p_t_k),6),
  DB=c(DB_pca_kmeans,DB_pca_tclust,DB_pca_PAM,DB_pca_mclust,DB_pca_tkmeans)
  
)
# Print the table
print(Metrics_table_int_scaled)
#install.packages("xtable")
library(xtable)
xtable(Metrics_table_int_scaled)



#Pre processing with MDS with the Euclidean distance
library(tidyverse)
dis_n=dist(mydata_s)
mds_result <- cmdscale(dis_n)
head(mds_result)
NbClust(mds_result, method="kmeans", distance = "euclidean")
#2
mds_result=as.data.frame(mds_result)

#Mahalanobis distance
library(StatMatch)
mal_dis=mahalanobis.dist(mydata_s)
mal_dis
class(mal_dis)
dis_n=as.dist(mal_dis)
mds_result <- cmdscale(dis_n,k=2)
head(mds_result)
mds_result=data.frame(mds_result)
mds_result

########################## Integrated approach with mds as preprocessing
############MDS+kmeans
dis_ss = dist(mds_result)^2
res_s = kmeans(mds_result,2)
sil_mds_km_s = silhouette (res_s$cluster, dis_ss)
windows()
plot(sil_mds_km_s)
mds_km_s=mean(sil_mds_km_s[,3])
mds_km_s
plot(res_s$cluster)

par(mfrow=c(1,1))
fviz_cluster(res_s,data=mds_result,palette = c("#E7B800", "#00AFBB"), 
             geom = "point",
             ellipse.type = "convex", 
             ggtheme = theme_bw())


cluster_colors <- c("#2E9FDF", "#00AFBB","#9E0142")
par(mfrow=c(1,1))
plot(mds_result, col = cluster_colors[res_s$cluster], pch = 16, main = "K-Means Clustering")

# Add cluster centers to the plot
points(res_s$centers, col = 1:3, pch = 3, cex = 2)


###
#####Trimmed kmeans
m_t_K=tkmeans(mds_result,2,alpha=0.1,iter.max =100)
print(m_t_K)
plot(m_t_K,mds_result,xlab="variables",ylab="variables")
table(m_t_K$cluster)
sil_m_t_K = silhouette (m_t_K$cluster, dis_ss)
windows()
plot(sil_m_t_K)
sil_m_t_K_val=mean(sil_m_t_K[,3])
sil_m_t_K_val
D_m_t_k= dunn(cluster=m_t_K$cluster,distance= dis_ss)
D_m_t_k



#MDS+tclust
dis_ss = dist(mds_result)^2
clus_s <- tclust(mds_result, k = 2, alpha = 0.1, restr.fact = 10000)
sil_mds_tc_s = silhouette (clus_s$cluster, dis_ss)
windows()
plot(sil_mds_tc_s)
mds_tc_s=mean(sil_mds_tc_s[,3])
mds_tc_s

plot(clus_s)
#MDS+PAM
library(cluster)
pam_result_s <- pam(mds_result, k = 2)
sil_pam_s=silhouette(pam_result_s$clustering,dis_ss)
sil_pam_s
windows()
plot(sil_pam_s)
mds_pam_s=mean(sil_pam_s[,3])
mds_pam_s

fviz_cluster(pam_result_s,
             data=mds_result,
             palette = c("#00AFBB", "#FC4E07"), # color palette
             ellipse.type = "t",  #Concentration ellipse 
             ggtheme = theme_classic() 
)
cluster_colors <- c("#00AFBB", "#FC4E07","#9E0142")

plot(mds_result, col = cluster_colors[pam_result_s$clustering], pch = 16, main = "PAM Clustering")

# Add cluster centers to the plot
points(pam_result_s$medoids, col = 1:3, pch = 3, cex = 2)


###MDS+mclust
library(mclust)
mds_mcl_s <- Mclust(mds_result, G = 2)
mds_mcl_s$classification
mds_mcl_sil_s=silhouette(mds_mcl_s$classification,dis_ss)
windows()
plot(mds_mcl_sil_s)
sil_mds_mc_s=mean(mds_mcl_sil_s[,3])

plot(mds_mcl_s, what = "classification", 
     addEllipses = TRUE)



#Scaled dunn index
D_mds_k_s<-dunn(clusters=res_s$cluster, distance=dis_ss)
D_mds_tclust_s<-dunn(clusters=clus_s$cluster, distance=dis_ss)
D_mds_pam_s<-dunn(clusters=pam_result_s$clustering, distance=dis_ss)
D_mds_mc_s<-dunn(clusters=mds_mcl_s$classification, distance=dis_ss)
D_mds_tk_s<-dunn(clusters=m_t_K$cluster,distance=dis_ss)

#DB
library(clusterSim)
DB_mds_kmeans=index.DB(mydata_s, cl=res_s$cluster, d=dis_ss, centrotypes="centroids", p=2, q=2)$DB
DB_mds_tclust=index.DB(mydata_s, cl=clus_s$cluster, d=dis_ss, centrotypes="centroids", p=2, q=2)$DB
DB_mds_PAM=index.DB(mydata_s, cl=pam_result_s$clustering, d=dis_ss, centrotypes="centroids", p=2, q=2)$DB
DB_mds_mclust=index.DB(mydata_s, cl=mds_mcl_s$classification, d=dis_ss, centrotypes="centroids", p=2, q=2)$DB
DB_mds_tkmeans=index.DB(mydata_s, cl=m_t_K$cluster, d=dis_ss, centrotypes="centroids", p=2, q=2)$DB

# Create a Table with metrics with scaled variables

Metrics_table_int_scaled <- data.frame(
  Method = c("MDS + kmeans", "MDS + tclust", "MDS + PAM","MDS + mclust","MDS + tkmeans"),
  Silhouette_Score = c(mds_km_s, mds_tc_s, mds_pam_s,sil_mds_mc_s, sil_m_t_K_val),
  DI=round(c(D_mds_k_s,D_mds_tclust_s,D_mds_pam_s,D_mds_mc_s,D_m_t_k),5),
  DB=c(DB_mds_kmeans,DB_mds_tclust,DB_mds_PAM,DB_mds_mclust,DB_mds_tkmeans)
  
)
# Print the table
print(Metrics_table_int_scaled)
#install.packages("xtable")
library(xtable)
xtable(Metrics_table_int_scaled)
