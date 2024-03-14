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
#install.packages("corrplot")
library(corrplot)
par(mfrow=c(1,1))
M = cor(data_es[,-1])
corrplot(M, method = 'number')
corrplot(M, method = 'color', order = 'alphabet')


#Determining if there are outliers
# Create a boxplot
par(mfrow=c(1,1))
boxplot(data_es[,-1], las = 1, horizontal = TRUE)
par(mar = c(5, 6, 2, 2))  # Adjust the margin to make room for variable names
boxplot(data_es[, -1], horizontal = TRUE, cex.axis = 0.8, las=1)

#Identify you outliers

Q1 <- quantile(data_es[,2], 0.25)
Q3 <- quantile(data_es[,2], 0.75)

IQR_value <- IQR(data_es[,2])
lower_bound <- Q1 - 1.5 * IQR_value
upper_bound <- Q3 + 1.5 * IQR_value

# Identify outliers
outliers <- which(data_es[,2] < lower_bound | data_es[,2] > upper_bound)

# Print the outlier indices
print(outliers)

Q1 <- quantile(data_es[,3], 0.25)
Q3 <- quantile(data_es[,3], 0.75)

IQR_value <- IQR(data_es[,3])
lower_bound <- Q1 - 1.5 * IQR_value
upper_bound <- Q3 + 1.5 * IQR_value

# Identify outliers
outliers <- which(data_es[,3] < lower_bound | data_es[,3] > upper_bound)
outliers <- which(data_es[,4] < lower_bound | data_es[,4] > upper_bound)

# Print the outlier indices
print(outliers)

library(factoextra)
library(NbClust)

# Elbow method
#We will be working with scaled data
data_es_s=scale(data_es[,-1])
fviz_nbclust(data_es_s, kmeans, method = "wss") +
  geom_vline(xintercept = 2, linetype = 2)+
  labs(subtitle = "Elbow method")

# Silhouette method
fviz_nbclust(data_es_s, kmeans, method = "silhouette")+
  labs(subtitle = "Silhouette method")
#2 clusters? mainly

# Gap statistic
# nboot = 50 to keep the function speedy.
# recommended value: nboot= 500 for your analysis.
# Use verbose = FALSE to hide computing progression.
set.seed(123)
fviz_nbclust(data_es_s, kmeans, nstart = 25, method = "gap_stat", nboot = 50)+
  labs(subtitle = "Gap statistic method")

#eclust function

##cIValid
#install.packages("cIValid")
library(clValid)

intern <- clValid(data_es_s, 2:5, clMethods=c("kmeans","pam"),
                  validation="internal")

## view results
summary(intern)

#internal validation
clmethods <- c("kmeans","pam","hierarchical")

intern <- clValid(data_es_s, nClust = 2:6,
                  clMethods = clmethods, validation = "internal")
# Summary
summary(intern)

#Unknown labels
data_es_s
library(cluster)
library(tsne)
#tsne
dist_=dist(data_es_s)^2
tsne <- tsne(data_es_s, initial_dims = 2)
tsne_s <- data.frame(tsne)
tsne_s
library(ggplot2)
ggplot(tsne_s)+geom_point(aes(x=tsne_s$X1, y=tsne_s$X2))



#umap
library(plotly) 
library(umap) 
umap = umap(data_es_s, n_components = 2, random_state = 15, n_neighbours=15) 
layout <- umap[["layout"]] 
layout <- data.frame(layout) 
layout
ggplot(layout)+geom_point(aes(x=layout$X1, y=layout$X2))


###Sample of how to compute Davies Bouldin index
library(clusterSim)
#data(data_ratio)
#cl1 <- pam(data_ratio, 4)
#d <- dist(data_ratio)
#index_result <- index.DB(data_ratio, cl1$clustering, d, centrotypes = "medoids")
#print(index_result$DB)

##Chalinski and harabaz to choose the number of clusters
library(fpc)

##kmeans with estimated K
#Chalinski and harabaz
kmeansruns(data_es_s,krange=2:10,criterion="ch",
           iter.max=100,runs=100,
           scaledata=FALSE,alpha=0.001,
           critout=FALSE,plot=FALSE)
#Average silhouette
kmeansruns(data_es_s,krange=2:10,criterion="asw",
           runs=100,
           scaledata=FALSE,alpha=0.001,
           critout=FALSE,plot=FALSE)


library (vegan)
library (cluster)

library(NbClust)
NbClust(data_es_s, method="kmeans", distance = "euclidean")
#the best number of clusters is three
par(mfrow=c(1,1))

wss <- (nrow(data_es_s)-1)*sum(apply(data_es_s,2,var))
for (i in 2:15) wss[i] <- sum(kmeans(data_es_s,
                                     centers=i)$withinss)
plot(1:15, wss, type="b", xlab="Number of Clusters",
     ylab="Within groups sum of squares")


##Partitioning around medoids to find the optimal number of clusters
library(fpc)
#pamk.best <- pamk(data_es_s)
#cat("number of clusters estimated by optimum average silhouette width:", pamk.best$nc, "\n")
#plot(pam(data_es_s, pamk.best$nc))


###################  kmeans
#############################################
dis = dist(data_es_s)^2
res = kmeans(data_es_s,2)
sil_k = silhouette (res$cluster, dis)
windows()
plot(sil_k)
sil_l_m=mean(sil_k[,3])
sil_l_m
#0.2541712
#Dunn index
#Dunn index has values betwee zero and infinity and it needs to be maximised
library(clValid)
D_k<-dunn(clusters=res$cluster, distance=dis)
D_k
print(paste("Dunn Index:", D_k))
#install.packages("plotly")
library(plotly)
static_plot<-fviz_cluster(res,data=data_es_s, 
                          geom = "point",
                          ellipse.type = "convex", 
                          ggtheme = theme_bw())
interactive_plot <- ggplotly(static_plot)

# Print the interactive plot
print(interactive_plot)

fviz_cluster(res,data=data_es_s,palette = c("#2E9FDF", "#FFA500"), 
             geom = "point",
             ellipse.type = "convex",
             ggtheme = theme_bw())

###
########################Trimmed k-means clustering
#################################################
#install.packages("trimcluster")
#library(trimcluster)
library(tclust)
t_K=tkmeans(data_es_s,2,alpha=0.08,iter.max =100)
print(t_K)
par(mfrow=c(1,1))
plot(t_K,data_es_s)
table(t_K$cluster)
sil_t_K = silhouette (t_K$cluster, dis)
windows()
plot(sil_t_K)
sil_t_K
sil_t_K_val=mean(sil_t_K[,3])

D_t_k= dunn(cluster=t_K$cluster,distance= dis)
D_t_k
#0.157244





#####################  tclust
############################################
set.seed(123)
library(tclust)
#Scaled data
dis_s = dist(data_es_s)^2
clus_s <- tclust (data_es_s, k = 2, alpha = 0.08, restr.fact = 50)
sil_t_s = silhouette (clus_s$cluster, dis_s)
windows()
plot(sil_t_s)
sil_t_ss=mean(sil_t_s[,3])
#Dunn index
D_t_s<-dunn(clusters=clus_s$cluster, distance=dis_s)
D_t_s
print(paste("Dunn Index:", D_t_s))
plot(clus_s)

########PAM: Partitioning around medoid
#######################################
#Pam scaled
pam_result_s <- pam(data_es_s, k = 2)
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
fviz_cluster(pam_result_s, 
             palette = c("#00AFBB", "#FC4E07"), # color palette
             ellipse.type = "t",  #Concentration ellipse 
             ggtheme = theme_classic() 
)

####    mclust
###################################
library(mclust)
#Scaled
mclust_model_s <- Mclust(data_es_s, G = 2)
mclust_model_s$classification
mclust_mc_s=silhouette(mclust_model_s$classification,dis_s)
windows()
plot(mclust_mc_s)
summary(mclust_model_s)
sil_mc_s=mean(mclust_mc_s[,3])
#Dunn index
D_m_s<-dunn(clusters=mclust_model_s$classification, distance=dis_s)
D_m_s
print(paste("Dunn Index:", D_m_s))



###############################
############Display the metrics
#######################################################
#Davis bouldin index

library(clusterSim)
DB_kmeans=index.DB(data_es_s, cl=res$cluster, d=dis_s, centrotypes="centroids", p=2, q=2)$DB
DB_tclust=index.DB(data_es_s, cl=clus_s$cluster, d=dis_s, centrotypes="centroids", p=2, q=2)$DB
DB_PAM=index.DB(data_es_s, cl=pam_result_s$clustering, d=dis_s, centrotypes="centroids", p=2, q=2)$DB
DB_mclust=index.DB(data_es_s, cl=mclust_model_s$classification, d=dis_s, centrotypes="centroids", p=2, q=2)$DB
DB_tkmeans=index.DB(data_es_s, cl=t_K$cluster, d=dis_s, centrotypes="centroids", p=2, q=2)$DB


#Table with scaled data
Scaled_metrics_table <- data.frame(
  Method = c("kmeans", "tclust", "PAM","mclust","tkmeans"),
  Silhouette_Score = c(sil_l_m, sil_t_ss,sil_pam_m_s ,sil_mc_s, sil_t_K_val),
  DI=c(D_k,D_t_s,D_p_s,D_m_s,D_t_k),
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
#Find the number of cluster
fviz_nbclust(tsne_s, kmeans, method = "silhouette")+
  labs(subtitle = "Silhouette method")
#3

wss <- (nrow(tsne_s)-1)*sum(apply(tsne_s,2,var))
for (i in 1:10) wss[i] <- sum(kmeans(tsne_s,
                                     centers=i)$withinss)
plot(1:10, wss, type="b", xlab="Number of Clusters",
     ylab="Within groups sum of squares")

library(NbClust)
NbClust(tsne_s,distance='euclidean', method='kmeans')
#The best proposed number of cluster is 3
#three clusters
dis_s = dist(tsne_s)^2
res_t_k_s = kmeans(tsne_s,3)
sil_t_k_s = silhouette (res_t_k_s$cluster, dis_s)
windows()
plot(sil_t_k_s)
tsne_kmeans_sil_s=mean(sil_t_k_s[,3])
#Silhouette value is 0.65
fviz_cluster(res_t_k_s,data=tsne_s,palette = c("#2E9FDF", "#00AFBB","#9E0142"), 
             geom = "point",
             ellipse.type = "convex", 
             ggtheme = theme_bw())

###tsne+tkmeans
#library(trimcluster)
t_t_K=tkmeans(tsne_s,3,alpha=0.08,iter.max =100)
print(t_t_K)
par(mfrow=c(1,1))
plot(t_t_K,tsne_s,xlab="X1",ylab="X2")
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
clus_t_t_s <- tclust(tsne_s, k = 3, alpha = 0.08, restr.fact = 50)
sil_ts_tc_s = silhouette (clus_t_t_s$cluster, dis_s)
windows()
plot(sil_ts_tc_s)
tsne_tcl_sil_s=mean(sil_ts_tc_s[,3])
tsne_tcl_sil_s
plot(clus_t_t_s)



#tsne+PAM
#Two clusters
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
#Two clusters
tsne_mcl_s <- Mclust(tsne_s, G = 3)
tsne_mcl_s$classification
tsne_mcl_sil_s=silhouette(tsne_mcl_s$classification,dis_s)
windows()
plot(tsne_mcl_sil_s)
sil_r_mc=mean(tsne_mcl_sil_s[,3])
plot(tsne_mcl_s, what = "classification", 
     addEllipses = TRUE)



##Dunn index with scaled variables
D_tsne_k_s<-dunn(clusters=res_t_k_s$cluster, distance=dis_s)
D_tsne_tclust_s<-dunn(clusters=clus_t_t_s$cluster, distance=dis_s)
D_tsne_pam_s<-dunn(clusters=pam_result_s$clustering, distance=dis_s)
D_tsne_mc_s<-dunn(clusters=tsne_mcl_s$classification, distance=dis_s)


#Davies Bouldin index (minimized)
DB_kmeans=index.DB(data_es_s, cl=res_t_k_s$cluster, d=dis_s, centrotypes="centroids", p=2, q=2)$DB
DB_tclust=index.DB(data_es_s, cl=clus_t_t_s$cluster, d=dis_s, centrotypes="centroids", p=2, q=2)$DB
DB_PAM=index.DB(data_es_s, cl=pam_result_s$clustering, d=dis_s, centrotypes="centroids", p=2, q=2)$DB
DB_mclust=index.DB(data_es_s, cl=tsne_mcl_s$classification, d=dis_s, centrotypes="centroids", p=2, q=2)$DB
DB_tkmeans=index.DB(data_es_s, cl=t_t_K$cluster, d=dis_s, centrotypes="centroids", p=2, q=2)$DB


#Integrated approach metrics tables

Scaled_Metrics_table <- data.frame(
  Method = c("t-SNE + kmeans", "t-SNE + tclust", "t-SNE + PAM","t-SNE + mclust","t-SNE + tkmeans"),
  Silhouette_Score = c(tsne_kmeans_sil_s, tsne_tcl_sil_s, tsn_pam_ss, tsne_kmeans_sil_s, sil_t_t_K_val),
  DI=round(c(D_tsne_k_s,D_tsne_tclust_s,D_tsne_pam_s,D_tsne_mc_s, D_t_t_k),6),
  DB=c(DB_kmeans,DB_tclust,DB_PAM,DB_mclust,DB_tkmeans)
)
# Print the table
print(Scaled_Metrics_table)


#Two clusters
#        Method Silhouette_Score       DI
# t-SNE + kmeans        0.5484692 0.006014
# t-SNE + tclust        0.2447692 0.005990
#    t-SNE + PAM        0.5438710 0.004241
# t-SNE + mclust        0.5484692 0.003236
xtable(Scaled_Metrics_table)




########################## Integrated approach with umap as preprocessing
############umap+kmeans
#Find number of clusters with umap as pre processing
#Find the number of cluster
fviz_nbclust(layout, kmeans, method = "silhouette")+
  labs(subtitle = "Silhouette method")
#5

wss <- (nrow(layout)-1)*sum(apply(tsne_s,2,var))
for (i in 1:10) wss[i] <- sum(kmeans(tsne_s,
                                     centers=i)$withinss)
plot(1:10, wss, type="b", xlab="Number of Clusters",
     ylab="Within groups sum of squares")

library(NbClust)
NbClust(layout,distance='euclidean', method='kmeans')

#In this case the best number of cluster suggested is 2 or 3
dis_ss = dist(layout)^2
res_s = kmeans(layout,3)
sil_umap_km_s = silhouette (res_s$cluster, dis_ss)
windows()
plot(sil_umap_km_s)
umap_km_s=mean(sil_umap_km_s[,3])
umap_km_s
fviz_cluster(res_s,data=layout,palette = c("#E7B800", "#00AFBB","#9E0142"), 
             geom = "point",
             ellipse.type = "convex", 
             ggtheme = theme_bw())

#####Trimmed kmeans
u_t_K=tkmeans(layout,3,alpha=0.08,iter.max =100)
print(u_t_K)
par(mfrow=c(1,1))
plot(u_t_K,layout,xlab="X1",ylab="X2")
table(u_t_K$cluster)
sil_u_t_K = silhouette (u_t_K$cluster, dis_ss)
windows()
plot(sil_u_t_K)
sil_u_t_K_val=mean(sil_u_t_K[,3])
sil_u_t_K_val
D_u_t_k= dunn(cluster=u_t_K$cluster,distance= dis_ss)
D_u_t_k
#0.01437216
layout



#umap+tclust
dis_ss = dist(layout)^2
clus_s <- tclust(layout, k = 3, alpha = 0.08, restr.fact = 50)
sil_umap_tc_s = silhouette (clus_s$cluster, dis_ss)
windows()
plot(sil_umap_tc_s)
umap_tc_s=mean(sil_umap_tc_s[,3])
umap_tc_s

plot(clus_s)


#umap+PAM
library(cluster)
pam_result_s <- pam(layout, k = 3)
sil_pam_s=silhouette(pam_result_s$clustering,dis_s)
sil_pam_s
windows()
plot(sil_pam_s)
umap_pam_s=mean(sil_pam_s[,3])
umap_pam_s
plot(sil_pam_s)
fviz_cluster(pam_result_s, 
             palette = c("#00AFBB", "#FC4E07","#9E0142"), # color palette
             ellipse.type = "t",  #Concentration ellipse 
             ggtheme = theme_classic() 
)

###Umap+mclust
umap_mcl_s <- Mclust(layout, G = 3)
umap_mcl_s$classification
umap_mcl_sil_s=silhouette(umap_mcl_s$classification,dis_s)
windows()
plot(umap_mcl_sil_s)
sil_umap_mc_s=mean(tsne_mcl_sil_s[,3])
plot(umap_mcl_s, what = "classification", 
     addEllipses = TRUE)



#Scaled dunn index
D_umap_k_s<-dunn(clusters=res_s$cluster, distance=dis_ss)
D_umap_tclust_s<-dunn(clusters=clus_s$cluster, distance=dis_ss)
D_umap_pam_s<-dunn(clusters=pam_result_s$clustering, distance=dis_ss)
D_umap_mc_s<-dunn(clusters=umap_mcl_s$classification, distance=dis_ss)

#DB
DB_kmeans=index.DB(data_es_s, cl=res_s$cluster, d=dis_s, centrotypes="centroids", p=2, q=2)$DB
DB_tclust=index.DB(data_es_s, cl=clus_s$cluster, d=dis_s, centrotypes="centroids", p=2, q=2)$DB
DB_PAM=index.DB(data_es_s, cl=pam_result_s$clustering, d=dis_s, centrotypes="centroids", p=2, q=2)$DB
DB_mclust=index.DB(data_es_s, cl=umap_mcl_s$classification, d=dis_s, centrotypes="centroids", p=2, q=2)$DB
DB_tkmeans=index.DB(data_es_s, cl=u_t_K$cluster, d=dis_s, centrotypes="centroids", p=2, q=2)$DB

# Create a Table with metrics with scaled variables
Metrics_table_int_scaled <- data.frame(
  Method = c("UMAP + kmeans", "UMAP + tclust", "UMAP + PAM","UMAP + mclust", "UMAP + tkmeans"),
  Silhouette_Score = c(umap_km_s, umap_tc_s, umap_pam_s,sil_umap_mc_s, sil_u_t_K_val),
  DI=round(c(D_umap_k_s,D_umap_tclust_s,D_umap_pam_s,D_umap_mc_s,D_u_t_k),5),
  DB=c(DB_kmeans,DB_tclust,DB_PAM,DB_mclust,DB_tkmeans)
)
# Print the table
print(Metrics_table_int_scaled )

#Method         Silhouette_Score          DI
# UMAP + kmeans       0.67703845        0.00428
# UMAP + tclust       0.06459105        0.00324
#    UMAP + PAM       0.59347573        0.00486
# UMAP + mclust       0.64572425        0.00399
library(xtable)
xtable(Metrics_table_int_scaled)


#Pre processing with PCA
library(tidyverse)
results <- prcomp(data_es_s, scale = FALSE)
res_pca=results$x[,1:2]
head(res_pca)
#library(NbClust)
NbClust(res_pca,method="kmeans", distance = "euclidean")
#library(factoextra)
#3 clusters
is.data.frame(res_pca)
res_pca=as.data.frame(res_pca)
########################## Integrated approach with PCA as preprocessing
############PCA+kmeans
set.seed(123)
dis_ss = dist(res_pca)^2
res_s = kmeans(res_pca,3)
sil_pca_km_s = silhouette (res_s$cluster, dis_ss)
windows()
plot(sil_pca_km_s)
pca_km_s=mean(sil_pca_km_s[,3])
pca_km_s
res_s$cluster
par(mfrow=c(1,1))
fviz_cluster(res_s,data=res_pca,palette = c("#E7B800", "#00AFBB","#9E0142"), 
             geom = "point",
             ellipse.type = "convex", 
             ggtheme = theme_bw())


###
#####Trimmed kmeans
p_t_K=tkmeans(res_pca,3,alpha=0.08,iter.max = 100)
print(p_t_K)
par(mfrow=c(1,1))
plot(p_t_K,res_pca,xlab="X1",ylab="X2")
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
clus_s <- tclust(res_pca, k = 3, alpha = 0.08,restr.fact = 100)
sil_pca_tc_s = silhouette (clus_s$cluster, dis_ss)
windows()
plot(sil_pca_tc_s)
pca_tc_s=mean(sil_pca_tc_s[,3])
pca_tc_s

plot(clus_s)


#PCA+PAM
library(cluster)
pam_result_s <- pam(res_pca, k = 3)
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
pca_mcl_s <- Mclust(res_pca, G = 3)
pca_mcl_s$classification
pca_mcl_sil_s=silhouette(pca_mcl_s$classification,dis_ss)
windows()
plot(pca_mcl_sil_s)
sil_pca_mc_s=mean(pca_mcl_sil_s[,3])

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
DB_pca_kmeans=index.DB(data_es_s, cl=res_s$cluster, d=dis_ss, centrotypes="centroids", p=2, q=2)$DB
DB_pca_tclust=index.DB(data_es_s, cl=clus_s$cluster, d=dis_ss, centrotypes="centroids", p=2, q=2)$DB
DB_pca_PAM=index.DB(data_es_s, cl=pam_result_s$clustering, d=dis_ss, centrotypes="centroids", p=2, q=2)$DB
DB_pca_mclust=index.DB(data_es_s, cl=pca_mcl_s$classification, d=dis_ss, centrotypes="centroids", p=2, q=2)$DB
DB_pca_tkmeans=index.DB(data_es_s, cl=p_t_K$cluster, d=dis_ss, centrotypes="centroids", p=2, q=2)$DB


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
dis_n=dist(data_es_s)
mds_result <- cmdscale(dis_n)
head(mds_result)
NbClust(mds_result, method="kmeans", distance = "euclidean")
#3 still
mds_result=as.data.frame(mds_result)

#mahalanobis
library(StatMatch)
mal_dis=mahalanobis.dist(data_es_s)
mal_dis
class(mal_dis)
dis_n=as.dist(mal_dis)
mds_result <- cmdscale(dis_n,k=2)
head(mds_result)
mds_result=data.frame(mds_result)
mds_result
NbClust(mds_result, method="kmeans", distance = "euclidean")


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


fviz_cluster(res_s,data=mds_result,palette = c("#E7B800", "#00AFBB","#9E0142"), 
             geom = "point",
             ellipse.type = "convex", 
             ggtheme = theme_bw())


cluster_colors <- c("#E7B800", "#00AFBB","#9E0142")
par(mfrow=c(1,1))
plot(mds_result, col = cluster_colors[res_s$cluster], pch = 16, main = "K-Means Clustering")

# Add cluster centers to the plot
points(res_s$centers, col = 1:3, pch = 3, cex = 2)


###
#####Trimmed kmeans
m_t_K=tkmeans(mds_result,2,alpha=0.08,iter.max = 100)
print(m_t_K)
par(mfrow=c(1,1))
plot(m_t_K,mds_result,xlab="X1",ylab="X2")
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
clus_s <- tclust(mds_result, k = 2, alpha = 0.08, restr.fact = 1000)
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
             palette = c("#00AFBB", "#FC4E07","#9E0142"), # color palette
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
DB_mds_kmeans=index.DB(data_es_s, cl=res_s$cluster, d=dis_ss, centrotypes="centroids", p=2, q=2)$DB
DB_mds_tclust=index.DB(data_es_s, cl=clus_s$cluster, d=dis_ss, centrotypes="centroids", p=2, q=2)$DB
DB_mds_PAM=index.DB(data_es_s, cl=pam_result_s$clustering, d=dis_ss, centrotypes="centroids", p=2, q=2)$DB
DB_mds_mclust=index.DB(data_es_s, cl=mds_mcl_s$classification, d=dis_ss, centrotypes="centroids", p=2, q=2)$DB
DB_mds_tkmeans=index.DB(data_es_s, cl=m_t_K$cluster, d=dis_ss, centrotypes="centroids", p=2, q=2)$DB

# Create a Table with metrics with scaled variables

Metrics_table_int_scaled <- data.frame(
  Method = c("MDS + kmeans", "MDS + tclust", "MDS + PAM","MDS + mclust","MDS + tkmeans"),
  Silhouette_Score = c(mds_km_s, mds_tc_s, mds_pam_s,sil_mds_mc_s, sil_m_t_K_val),
  DI=round(c(D_mds_k_s,D_mds_tclust_s,D_mds_pam_s,D_mds_mc_s,D_m_t_k),6),
  DB=c(DB_mds_kmeans,DB_mds_tclust,DB_mds_PAM,DB_mds_mclust,DB_mds_tkmeans)
  
)
# Print the table
print(Metrics_table_int_scaled)
#install.packages("xtable")
library(xtable)
xtable(Metrics_table_int_scaled)









