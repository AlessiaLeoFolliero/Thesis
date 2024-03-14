#Breast cancer clustering analysis (labels are known)
dat <- read.csv("breast-cancer.csv")
my_cols <- c("#00AFBB", "#E7B800") 
head(dat)
#Deleting the id column
dat=dat[,-1]
head(dat)
anyNA(dat)
dim(dat)
View(dat)
head(dat)
dat$diagnosis=as.factor(dat$diagnosis)
head(dat$diagnosis)
dat$diagnosis


dat[,2:6]

#2 cluster
pairs(scale(dat[,2:11]), pch = 19,  cex = 0.5,
      col = my_cols[dat$diagnosis],
      lower.panel=NULL)



###tsne 
#install.packages("tsne")
library(tsne)
library(plotly)
set.seed(12)

tsne <- tsne(dat[,2:31], initial_dims = 2)

tsne <- data.frame(tsne)
pdb <- cbind(tsne,dat$diagnosis)
tsne
options(warn = -1)
fig <-  plot_ly(data = pdb ,x =  ~X1, y = ~X2, type = 'scatter', mode = 'markers', split = ~dat$diagnosis)
fig <- fig %>%
  layout(
    plot_bgcolor = "#e5ecf6"
  )
fig

#with scaled data (in this case data need to be scaled)
tsne <- tsne(scale(dat[,2:31]), initial_dims = 2)
scale(dat[,2:31])
tsne_s <- data.frame(tsne)
pdb <- cbind(tsne_s,dat$diagnosis)
options(warn = -1)

fig <-  plot_ly(data = pdb ,x =  ~X1, y = ~X2, type = 'scatter', mode = 'markers', split = ~dat$diagnosis)
fig <- fig %>%
  layout(
    plot_bgcolor = "#e5ecf6"
  )
fig



### umap
library(plotly) 
library(umap) 
umap = umap(dat[,2:31], n_components = 2, random_state = 15, n_neighbours=15 )
layout <- umap[["layout"]] 
layout <- data.frame(layout) 
final <- cbind(layout, dat$diagnosis) 

fig <- plot_ly(final, x = ~X1, y = ~X2, color = ~dat$diagnosis, type = 'scatter', mode = 'markers')%>%  
  layout(
    plot_bgcolor = "#e5ecf6",
    legend=list(title=list(text='cluster')), 
    xaxis = list( 
      title = "0"),  
    yaxis = list( 
      title = "1")) 
fig


#With scaled data
umap_s = umap(scale(dat[,2:31]), n_components = 2, random_state = 15, n_neighbours=15) 
layout_s <- umap_s[["layout"]] 
layout_ss <- data.frame(layout_s) 


final <- cbind(layout_ss, dat$diagnosis) 

fig <- plot_ly(final, x = ~X1, y = ~X2, color = ~dat$diagnosis, type = 'scatter', mode = 'markers')%>%  
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
mydata_s=scale(dat[,2:31])
wss_s <- (nrow(mydata_s))*sum(apply(mydata_s,2,var))
for (i in 2:10) wss_s[i] <- sum(kmeans(mydata_s, 
                                       centers=i)$withinss)

wss_s
plot(1:10, wss_s, type="b", xlab="k", ylab="Within Deviance")
##We already know that we have two clusters in this case

## Explore K mean solution 
fit <- kmeans(mydata_s, 2)

aggregate(mydata_s,by=list(fit$cluster),FUN=mean)

#plot results of final k-means model
library(factoextra)
library(cluster)
fviz_cluster(fit, data = mydata_s)


#install.packages("tclust")
library(tclust)
plot (ctlcurves (mydata_s, k = 2:4, alpha = seq (0, 0.3, by = 0.025)))
clus <- tclust (mydata_s, k = 2, alpha = 0.1, restr.fact = 50)


### silhuette comparison

library (vegan)
library (cluster)
head(mydata_s)
dis_n = dist(mydata_s)
dat$diagnosis
dis_n
dat$diagnosis=ifelse(dat$diagnosis=="M",1,2)
sil_orig=silhouette(dat$diagnosis,dis_n)
windows()
plot(sil_orig)
dat$diagnosis


#DatabionicSwarmClustering(dis_n, ClusterNo = 2,
#                          StructureType = TRUE, DistancesMethod = NULL,
#                          PlotTree = FALSE, PlotMap = FALSE,PlotIt=TRUE,
#                          Parallel = FALSE)
###################  kmeans
#############################################
dis = dist(mydata_s)^2
mydata_s
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
res$cluster
dat$diagnosis
kmeans_ari=adj.rand.index(dat$diagnosis, res$cluster)
#plot
fviz_cluster(res, data = mydata_s,
             palette = c("#E7B800", "#00AFBB"), 
             geom = "point",
             ellipse.type = "convex", 
             ggtheme = theme_bw()
)

########################Trimmed k-means clustering
#################################################
#install.packages("trimcluster")
#library(trimcluster)
library(tclust)
t_K=tkmeans(mydata_s,2,alpha=0.1,iter.max =100)
print(t_K)
plot(t_K,mydata_s)
table(t_K$cluster)
sil_t_K = silhouette (t_K$cluster, dis)
windows()
plot(sil_t_K)
sil_t_K
sil_t_K_val=mean(sil_t_K[,3])

D_t_k= dunn(cluster=t_K$cluster,distance= dis)
D_t_k
#0.02038168
#ARI
tk_ari=adj.rand.index(dat$diagnosis,t_K$cluster)

# Subset the third column where the cluster assignments are 1 or 2
###Higher
#cluster_assign=t_K$classification
#subset_sil_scores_tk<- mean(sil_t_K[cluster_assign %in% c(2, 3), 3])
#subset_sil_scores_tk
#0.4768972 

#Dunn
#dunn_index_tk <- dunn(clusters = cluster_assign[cluster_assign %in% c(2, 3)], distance = dis)
#print(paste("Dunn Index for Clusters 1 and 2:", dunn_index_tk))
#0.00167738154314017

#####################  tclust
############################################
set.seed(123)

#Scaled data
dis_s = dist(mydata_s)^2
clus_s <- tclust (mydata_s, k = 2, alpha = 0.1, restr.fact = 50)
table(clus_s$cluster)
sil_t_s = silhouette (clus_s$cluster, dis_s)
sil_t_ss=mean(sil_t_s[,3])
windows()
plot(sil_t_s)
plot(clus_s)

cluster_assignments <- clus_s$cluster

# Compute silhouette scores
sil_scores <- silhouette(cluster_assignments, dis_s)

# Subset the third column where the cluster assignments are 1 or 2
###Higher
subset_sil_scores <- mean(sil_scores[cluster_assignments %in% c(1, 2), 3])
subset_sil_scores
#0.539806 higher




#Dunn index
library(clValid)

D_t_s<-dunn(clusters=clus_s$cluster, distance=dis_s)
D_t_s
print(paste("Dunn Index:", D_t_s))
#Adjusted rand index
tclust_ari=adj.rand.index(dat$diagnosis,clus_s$cluster)
plot(clus_s)

# Compute Dunn index for clusters 1 and 2
#dunn_index_t <- dunn(clusters = cluster_assignments[cluster_assignments %in% c(1, 2)], distance = dis_s)
#print(paste("Dunn Index for Clusters 1 and 2:", dunn_index_t))
#0.00166558084826526 higher

###Table with just the two classes for tkmeans and tclust
#Two_class_table=data.frame(
#  Method=c("tkmeans","tclust"),
#  Silh_score=c(subset_sil_scores_tk,subset_sil_scores),
#  DI=c(dunn_index_tk,dunn_index_t)
#)
#print(Two_class_table)

#library(xtable)
#xtable(Two_class_table)

#\begin{table}[ht]
#\centering
#\begin{tabular}{rlrr}
#\hline
#& Method & Silh\_score & DI \\ 
#\hline
#1 & tkmeans & 0.19 & 0.00 \\ 
#2 & tclust & 0.53 & 0.00 \\ 
#\hline
#\end{tabular}
#\end{table}


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
pam_ari=adj.rand.index(dat$diagnosis,pam_result_s$cluster)
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
#0.39
sil_mc_s=mean(mclust_mc_s[,3])
#Dunn index
D_m_s<-dunn(clusters=mclust_model_s$classification, distance=dis_s)
D_m_s
print(paste("Dunn Index:", D_m_s))
#0.00267923040717929
#Adjusted rand index
mc_ari=adj.rand.index(dat$diagnosis,mclust_model_s$classification)
mc_ari






#################################################################################
#################################################################################
#################################################################################

###############################
library(cluster)
library(flexclust)
#Compute rand index with scaled variables
ri_k_s <- randIndex(dat$diagnosis, res$cluster)
ri_k_s
# Calculate the Rand Index

ri_mclust_s <- randIndex(dat$diagnosis,mclust_model_s$classification)
ri_tclust_s <- randIndex(dat$diagnosis,clus_s$cluster)

ri_pam_s <- randIndex(dat$diagnosis, pam_result_s$cluster)

ri_k_s <- rand.index(dat$diagnosis, res$cluster)
ri_k_s

ri_tk<-randIndex(dat$diagnosis,t_K$cluster)

# Calculate the Rand Index
ri_mclust_s <- rand.index(dat$diagnosis,mclust_model_s$classification)
ri_tclust_s <- rand.index(dat$diagnosis,clus_s$cluster)

ri_pam_s <- rand.index(dat$diagnosis, pam_result_s$cluster)

#ri_tclust_2<-rand.index(dat$diagnosis,cluster_assignments[cluster_assignments %in% c(1, 2)])


############Display the metrics
#######################################################

#Table with scaled data
Scaled_metrics_table <- data.frame(
  Method = c("kmeans", "tclust", "PAM","mclust","tkmeans"),
  Silhouette_Score = c(sil_l_m, sil_t_ss,sil_pam_m_s ,sil_mc_s, sil_t_K_val),
  RI=c(ri_k_s,ri_tclust_s,ri_pam_s,ri_mclust_s,ri_tk),
  ARI=c(kmeans_ari,tclust_ari,pam_ari,mc_ari,tk_ari),
  DI=c(D_k,D_t_s,D_p_s,D_m_s,D_t_k)
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
set.seed(123)
dis_s = dist(tsne_s)^2
res_t_k_s = kmeans(tsne_s,2)
sil_t_k_s = silhouette (res_t_k_s$cluster, dis_s)
windows()
plot(sil_t_k_s)
tsne_kmeans_sil_s=mean(sil_t_k_s[,3])

fviz_cluster(res_t_k_s,data=tsne_s,palette = c("#E7B800", "#00AFBB"), 
             geom = "point",
             ellipse.type = "convex", 
             ggtheme = theme_bw())

########################tsne + Trimmed k-means clustering
#################################################
#install.packages("trimcluster")
#library(trimcluster)
t_t_K=tkmeans(tsne_s,2,alpha=0.1,iter.max =100)
print(t_t_K)
plot(t_t_K,tsne_s,xlab="variables",ylab="variables")
table(t_K$cluster)
sil_t_t_K = silhouette (t_t_K$cluster, dis_s)
windows()
plot(sil_t_t_K)
sil_t_t_K_val=mean(sil_t_t_K[,3])
sil_t_t_K_val

D_t_t_k= dunn(cluster=t_t_K$cluster,distance= dis_s)
D_t_t_k
#0.02038168
#ARI
ttk_ari=adj.rand.index(dat$diagnosis,t_t_K$cluster)

###################################
# Subset the third column where the cluster assignments are 1 or 2
###Higher
#cluster_assign=t_K$classification
#subset_sil_scores_tk<- mean(sil_t_K[cluster_assign %in% c(2, 3), 3])
#subset_sil_scores_tk
#0.4768972 

#Dunn
#dunn_index_tk <- dunn(clusters = cluster_assign[cluster_assign %in% c(2, 3)], distance = dis)
#print(paste("Dunn Index for Clusters 1 and 2:", dunn_index_tk))
#0.00167738154314017


#t_t_K=trimkmeans(tsne_s,2,trim=0.1,runs=100)
#print(t_t_K)
#plot(t_t_K,tsne_s)
#t_t_K$classification
#sil_t_K = silhouette (t_t_K$classification, dis_s)
#windows()
#plot(sil_t_K)

#table(t_t_K$classification)
#D_t_t_k= dunn(t_t_K$classification, dis_s)#Non si riesce a calcolare

# Subset the third column where the cluster assignments are 1 or 2
###Higher
#cluster_assign=t_t_K$classification
#subset_sil_scores_ttk<- mean(sil_t_K[cluster_assign %in% c(1, 2), 3])
#subset_sil_scores_ttk

#Dunn
#dunn_index_ttk <- dunn(clusters = cluster_assign[cluster_assign %in% c(1, 2)], distance = dis_s)
#print(paste("Dunn Index for Clusters 1 and 2:", dunn_index_ttk))
#0.00145152573750879

#################################
#tsne+tclust
#Scaled
clus_t_t_s <- tclust(tsne_s, k = 2, alpha = 0.1, restr.fact = 50)
sil_ts_tc_s = silhouette (clus_t_t_s$cluster, dis_s)
windows()
plot(sil_ts_tc_s)
plot(clus_t_t_s)
tsne_tcl_sil_s=mean(sil_ts_tc_s[,3])
tsne_tcl_sil_s

##With two classes
cluster_assignments <- clus_t_t_s$cluster

# Compute silhouette scores
sil_scores_t <- silhouette(cluster_assignments, dis_s)
table(cluster_assignments)
# Subset the third column where the cluster assignments are 1 or 2
###Higher
subset_sil_scores_t <- mean(sil_scores_t[cluster_assignments %in% c(1, 2), 3])
subset_sil_scores_t
#0.5673403

#Dunn index
library(clValid)

# Compute Dunn index for clusters 1 and 2
#dunn_index_t_t <- dunn(clusters = cluster_assignments[cluster_assignments %in% c(1, 2)], distance = dis_s)
#print(paste("Dunn Index for Clusters 1 and 2:", dunn_index_t_t))

###Table with just the two classes for tkmeans and tclust
#Two_class_table_t=data.frame(
#  Method=c("tsne+tkmeans","tsne+tclust"),
#  Silh_score=c(subset_sil_scores_ttk,subset_sil_scores_t),
#  DI=c(dunn_index_ttk,dunn_index_t_t)
#)
#print(Two_class_table_t)
#Method Silh_score          DI
# tsne+tkmeans  0.2830065 0.001451526
#  tsne+tclust  0.5673403 0.001843080

#xtable(Two_class_table_t)

#\begin{table}[ht]
#\centering
#\begin{tabular}{rlrr}
#\hline
#& Method & Silh\_score & DI \\ 
#\hline
#1 & tsne+tkmeans & 0.28 & 0.00 \\ 
#2 & tsne+tclust & 0.57 & 0.00 \\ 
#\hline
#\end{tabular}
#\end{table}


#tsne+PAM
pam_result_s <- pam(tsne_s, k = 2)
tsn_pam_s=silhouette(pam_result_s$clustering,dis_s)
windows()
plot(tsn_pam_s)
tsn_pam_ss=mean(tsn_pam_s[,3])
tsn_pam_ss

fviz_cluster (pam_result_s, 
              palette = c("#00AFBB", "#FC4E07"), # color palette
              ellipse.type = "t",  #Concentration ellipse 
              ggtheme = theme_classic() 
)


##tsne+mclust
tsne_mcl_s <- Mclust(tsne_s, G = 2)
tsne_mcl_s$classification
tsne_mcl_sil_s=silhouette(tsne_mcl_s$classification,dis_s)
windows()
plot(tsne_mcl_sil_s)
sil_r_mc=mean(tsne_mcl_sil_s[,3])

plot(tsne_mcl_s, what = "classification", 
     addEllipses = TRUE)



###Rand index
ri_tsne_k_s <- rand.index(dat$diagnosis, res_t_k_s$cluster)
ri_tsne_tclust_s <- rand.index(dat$diagnosis,clus_t_t_s$cluster)
ri_tsne_pam_s <- rand.index(dat$diagnosis, pam_result_s$clustering)
ri_tsne_mc_s<- rand.index(dat$diagnosis,tsne_mcl_s$classification)
ri_tsne_tk_s<-rand.index(dat$diagnosis, t_t_K$cluster)


##Dunn index with scaled variables
D_tsne_k_s<-dunn(clusters=res_t_k_s$cluster, distance=dis_s)
D_tsne_tclust_s<-dunn(clusters=clus_t_t_s$cluster, distance=dis_s)
D_tsne_pam_s<-dunn(clusters=pam_result_s$clustering, distance=dis_s)
D_tsne_mc_s<-dunn(clusters=tsne_mcl_s$classification, distance=dis_s)
D_tsne_tkm<-dunn(clusters=t_t_K$cluster,distance=dis_s)

#Adjusted rand index
tsne_kmeans_ari=adj.rand.index(dat$diagnosis, res_t_k_s$cluster)
tsne_tclust_ari=adj.rand.index(dat$diagnosis, clus_t_t_s$cluster)
tsne_pam_ari=adj.rand.index(dat$diagnosis, pam_result_s$clustering)
tsne_mc_ari=adj.rand.index(dat$diagnosis, tsne_mcl_s$classification)
tsne_tk_ari=adj.rand.index(dat$diagnosis, t_t_K$cluster)



#Integrated approach metrics tables

Scaled_Metrics_table <- data.frame(
  Method = c("t-SNE + kmeans", "t-SNE + tclust", "t-SNE + PAM","t-SNE + mclust","t-SNE+ tkmeans"),
  Silhouette_Score = c(tsne_kmeans_sil_s, tsne_tcl_sil_s, tsn_pam_ss, tsne_kmeans_sil_s, sil_t_t_K_val),
  RI=c(ri_tsne_k_s,ri_tsne_tclust_s,ri_tsne_pam_s,ri_tsne_mc_s,ri_tsne_tk_s),
  ARI=c(tsne_kmeans_ari,tsne_tclust_ari,tsne_pam_ari,tsne_mc_ari,tsne_tk_ari),
  DI=round(c(D_tsne_k_s,D_tsne_tclust_s,D_tsne_pam_s,D_tsne_mc_s,D_t_t_k),6)
)
# Print the table
print(Scaled_Metrics_table)
library(xtable)
xtable(Scaled_Metrics_table)
#        Method Silhouette_Score        RI       ARI       DI
# t-SNE + kmeans        0.7356123 0.8307631 0.6600462 0.000647
# t-SNE + tclust        0.5298016 0.8354477 0.8419627 0.075485
#    t-SNE + PAM        0.7356123 0.8307631 0.6600462 0.000647
# t-SNE + mclust        0.7356123 0.8423228 0.6831121 0.000322

#\begin{table}[ht]
#\centering
#\begin{tabular}{rlrrrr}
#\hline
#& Method & Silhouette\_Score & RI & ARI & DI \\ 
#\hline
#1 & t-SNE + kmeans & 0.74 & 0.83 & 0.66 & 0.00 \\ 
#2 & t-SNE + tclust & 0.53 & 0.84 & 0.84 & 0.08 \\ 
#3 & t-SNE + PAM & 0.74 & 0.83 & 0.66 & 0.00 \\ 
#4 & t-SNE + mclust & 0.74 & 0.84 & 0.68 & 0.00 \\ 
#\hline
#\end{tabular}
#\end{table}

########################## Integrated approach with umap as preprocessing
############umap+kmeans
dis_ss = dist(layout_ss)^2
res_s = kmeans(layout_ss,2)
sil_umap_km_s = silhouette (res_s$cluster, dis_ss)
windows()
plot(sil_umap_km_s)
umap_km_s=mean(sil_umap_km_s[,3])
umap_km_s
res_s$cluster

fviz_cluster(res_s,data=layout_ss,palette = c("#E7B800", "#00AFBB"), 
             geom = "point",
             ellipse.type = "convex", 
             ggtheme = theme_bw())


###
#####Trimmed kmeans
u_t_K=tkmeans(layout_ss,2,alpha=0.1,iter.max =100)
print(u_t_K)
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
library(fossil)
utk_ari=adj.rand.index(dat$diagnosis,u_t_K$cluster)



#umap+tclust
dis_ss = dist(layout_ss)^2
clus_s <- tclust(layout_ss, k = 2, alpha = 0.1, restr.fact = 50)
sil_umap_tc_s = silhouette (clus_s$cluster, dis_ss)
windows()
plot(sil_umap_tc_s)
umap_tc_s=mean(sil_umap_tc_s[,3])
umap_tc_s

plot(clus_s)

###Tclust with two classes



#umap+PAM
library(cluster)
pam_result_s <- pam(layout_ss, k = 2)
sil_pam_s=silhouette(pam_result_s$clustering,dis_ss)
sil_pam_s
windows()
plot(sil_pam_s)
umap_pam_s=mean(sil_pam_s[,3])
umap_pam_s
fviz_cluster(pam_result_s, 
             palette = c("#00AFBB", "#FC4E07"), # color palette
             ellipse.type = "t",  #Concentration ellipse 
             ggtheme = theme_classic() 
)

###Umap+mclust
library(mclust)
umap_mcl_s <- Mclust(layout_s, G = 2)
umap_mcl_s$classification
umap_mcl_sil_s=silhouette(umap_mcl_s$classification,dis_ss)
windows()
plot(umap_mcl_sil_s)
sil_umap_mc_s=mean(umap_mcl_sil_s[,3])

plot(umap_mcl_s, what = "classification", 
     addEllipses = TRUE)


###Rand index
library(flexclust)

#Scaled rand index
ri_umap_k_s <- randIndex(dat$diagnosis, res_s$cluster)
ri_umap_tclust_s <- randIndex(dat$diagnosis,clus_s$cluster)
ri_umap_pam_s<- randIndex(dat$diagnosis, pam_result_s$clustering)
ri_umap_mc_s <- randIndex(dat$diagnosis,umap_mcl_s$classification)
ri_umap_tk_s<-randIndex(dat$diagnosis, u_t_K$cluster)

#Scaled dunn index
D_umap_k_s<-dunn(clusters=res_s$cluster, distance=dis_ss)
D_umap_tclust_s<-dunn(clusters=clus_s$cluster, distance=dis_ss)
D_umap_pam_s<-dunn(clusters=pam_result_s$clustering, distance=dis_ss)
D_umap_mc_s<-dunn(clusters=umap_mcl_s$classification, distance=dis_ss)
D_umap_tk_s<-dunn(clusters=u_t_K$cluster,distance=dis_ss)

#ARI
umap_kmeans_ari=adj.rand.index(dat$diagnosis,res_s$cluster)
umap_tclust_ari=adj.rand.index(dat$diagnosis, clus_s$cluster)
umap_pam_ari=adj.rand.index(dat$diagnosis, pam_result_s$clustering)
umap_mc_ari=adj.rand.index(dat$diagnosis, umap_mcl_s$classification)
umap_tk_ari=adj.rand.index(dat$diagnosis,u_t_K$cluster)

# Create a Table with metrics with scaled variables

Metrics_table_int_scaled <- data.frame(
  Method = c("UMAP + kmeans", "UMAP + tclust", "UMAP + PAM","UMAP + mclust","UMAP + tkmeans"),
  Silhouette_Score = c(umap_km_s, umap_tc_s, umap_pam_s,sil_umap_mc_s, sil_u_t_K_val),
  RI=c(ri_umap_k_s,ri_umap_tclust_s,ri_umap_pam_s,ri_umap_mc_s,ri_umap_tk_s),
  ARI=c(umap_kmeans_ari,umap_tclust_ari,umap_pam_ari,umap_mc_ari,umap_tk_ari),
  DI=round(c(D_umap_k_s,D_umap_tclust_s,D_umap_pam_s,D_umap_mc_s,D_u_t_k),6)
  
)
# Print the table
print(Metrics_table_int_scaled)
install.packages("xtable")
library(xtable)
xtable(Metrics_table_int_scaled)

#       Method Silhouette_Score        RI       ARI    DI
#UMAP + kmeans        0.7692338 0.7731007 0.7731007 7e-05
# UMAP + tclust        0.6017425 0.6846392 0.7992701 9e-05
#   UMAP + PAM        0.7692338 0.7731007 0.7731007 7e-05
#UMAP + mclust        0.7360643 0.7242400 0.7242400 7e-05

#\begin{table}[ht]
#\centering
#\begin{tabular}{rlrrrr}
#\hline
#& Method & Silhouette\_Score & RI & ARI & DI \\ 
#\hline
#1 & UMAP + kmeans & 0.77 & 0.77 & 0.77 & 0.00 \\ 
#2 & UMAP + tclust & 0.60 & 0.68 & 0.80 & 0.00 \\ 
#3 & UMAP + PAM & 0.77 & 0.77 & 0.77 & 0.00 \\ 
#4 & UMAP + mclust & 0.74 & 0.72 & 0.72 & 0.00 \\ 
#\hline
#\end{tabular}
#\end{table}



#Pre processing with PCA
library(tidyverse)
results <- prcomp(mydata_s, scale = FALSE)
res_pca=results$x[,1:2]
head(res_pca)
res_pca=as.data.frame(res_pca)

########################## Integrated approach with PCA as preprocessing
############PCA+kmeans
dis_ss = dist(res_pca)^2
res_s = kmeans(res_pca,2)
sil_umap_km_s = silhouette (res_s$cluster, dis_ss)
windows()
plot(sil_umap_km_s)
umap_km_s=mean(sil_umap_km_s[,3])
umap_km_s
res_s$cluster

fviz_cluster(res_s,data=res_pca,palette = c("#E7B800", "#00AFBB"), 
             geom = "point",
             ellipse.type = "convex", 
             ggtheme = theme_bw())


###
#####Trimmed kmeans
u_t_K=tkmeans(res_pca,2,alpha=0.1,iter.max =100)
print(u_t_K)
plot(u_t_K,res_pca,xlab="variables",ylab="variables")
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
library(fossil)
utk_ari=adj.rand.index(dat$diagnosis,u_t_K$cluster)



#PCA+tclust
dis_ss = dist(res_pca)^2
clus_s <- tclust(res_pca, k = 2, alpha = 0.1, restr.fact = 50)
sil_umap_tc_s = silhouette (clus_s$cluster, dis_ss)
windows()
plot(sil_umap_tc_s)
umap_tc_s=mean(sil_umap_tc_s[,3])
umap_tc_s

plot(clus_s)

###Tclust with two classes



#PCA+PAM
library(cluster)
pam_result_s <- pam(res_pca, k = 2)
sil_pam_s=silhouette(pam_result_s$clustering,dis_ss)
sil_pam_s
windows()
plot(sil_pam_s)
umap_pam_s=mean(sil_pam_s[,3])
umap_pam_s
fviz_cluster(pam_result_s, 
             palette = c("#00AFBB", "#FC4E07"), # color palette
             ellipse.type = "t",  #Concentration ellipse 
             ggtheme = theme_classic() 
)

###PCA+mclust
library(mclust)
umap_mcl_s <- Mclust(res_pca, G = 2)
umap_mcl_s$classification
umap_mcl_sil_s=silhouette(umap_mcl_s$classification,dis_ss)
windows()
plot(umap_mcl_sil_s)
sil_umap_mc_s=mean(umap_mcl_sil_s[,3])

plot(umap_mcl_s, what = "classification", 
     addEllipses = TRUE)


###Rand index
library(flexclust)

#Scaled rand index
ri_umap_k_s <- randIndex(dat$diagnosis, res_s$cluster)
ri_umap_tclust_s <- randIndex(dat$diagnosis,clus_s$cluster)
ri_umap_pam_s<- randIndex(dat$diagnosis, pam_result_s$clustering)
ri_umap_mc_s <- randIndex(dat$diagnosis,umap_mcl_s$classification)
ri_umap_tk_s<-randIndex(dat$diagnosis, u_t_K$cluster)

#Scaled dunn index
D_umap_k_s<-dunn(clusters=res_s$cluster, distance=dis_ss)
D_umap_tclust_s<-dunn(clusters=clus_s$cluster, distance=dis_ss)
D_umap_pam_s<-dunn(clusters=pam_result_s$clustering, distance=dis_ss)
D_umap_mc_s<-dunn(clusters=umap_mcl_s$classification, distance=dis_ss)
D_umap_tk_s<-dunn(clusters=u_t_K$cluster,distance=dis_ss)

#ARI
umap_kmeans_ari=adj.rand.index(dat$diagnosis,res_s$cluster)
umap_tclust_ari=adj.rand.index(dat$diagnosis, clus_s$cluster)
umap_pam_ari=adj.rand.index(dat$diagnosis, pam_result_s$clustering)
umap_mc_ari=adj.rand.index(dat$diagnosis, umap_mcl_s$classification)
umap_tk_ari=adj.rand.index(dat$diagnosis,u_t_K$cluster)

# Create a Table with metrics with scaled variables

Metrics_table_int_scaled <- data.frame(
  Method = c("PCA + kmeans", "PCA + tclust", "PCA + PAM","PCA + mclust","PCA + tkmeans"),
  Silhouette_Score = c(umap_km_s, umap_tc_s, umap_pam_s,sil_umap_mc_s, sil_u_t_K_val),
  RI=c(ri_umap_k_s,ri_umap_tclust_s,ri_umap_pam_s,ri_umap_mc_s,ri_umap_tk_s),
  ARI=c(umap_kmeans_ari,umap_tclust_ari,umap_pam_ari,umap_mc_ari,umap_tk_ari),
  DI=round(c(D_umap_k_s,D_umap_tclust_s,D_umap_pam_s,D_umap_mc_s,D_u_t_k),6)
  
)
# Print the table
print(Metrics_table_int_scaled)
#install.packages("xtable")
library(xtable)
xtable(Metrics_table_int_scaled)



#Pre processing with MDS with the Euclidean distance
library(tidyverse)
dis = dist(mydata_s)
mds_result <- cmdscale(dis)
head(mds_result)
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

########################## Integrated approach with PCA as preprocessing
############MDS+kmeans
dis_ss = dist(mds_result)^2
res_s = kmeans(mds_result,2)
sil_umap_km_s = silhouette (res_s$cluster, dis_ss)
windows()
plot(sil_umap_km_s)
umap_km_s=mean(sil_umap_km_s[,3])
umap_km_s
plot(res_s$cluster)
fviz_cluster(res_s,data=mds_result,palette = c("#E7B800", "#00AFBB"), 
             geom = "point",
             ellipse.type = "convex", 
             ggtheme = theme_bw())




colours=c("#E7B800", "#00AFBB")
plot(mds_result, col = colours[res_s$cluster], pch = 16, main = "K-Means Clustering")

# Add cluster centers to the plot
points(res_s$centers, col = 1:3, pch = 3, cex = 2)


###
#####Trimmed kmeans
u_t_K=tkmeans(mds_result,2,alpha=0.1,iter.max =100)
print(u_t_K)
plot(u_t_K,mds_result,xlab="variables",ylab="variables")
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
library(fossil)
utk_ari=adj.rand.index(dat$diagnosis,u_t_K$cluster)



#MDS+tclust
dis_ss = dist(mds_result)^2
clus_s <- tclust(mds_result, k = 2, alpha = 0.1, restr.fact = 50)
sil_umap_tc_s = silhouette (clus_s$cluster, dis_ss)
windows()
plot(sil_umap_tc_s)
umap_tc_s=mean(sil_umap_tc_s[,3])
umap_tc_s

plot(clus_s)

###Tclust with two classes



#MDS+PAM
library(cluster)
pam_result_s <- pam(mds_result, k = 2)
sil_pam_s=silhouette(pam_result_s$clustering,dis_ss)
sil_pam_s
windows()
plot(sil_pam_s)
umap_pam_s=mean(sil_pam_s[,3])
umap_pam_s

fviz_cluster(pam_result_s,
             data=mds_result,
             palette = c("#00AFBB", "#FC4E07"), # color palette
             ellipse.type = "t",  #Concentration ellipse 
             ggtheme = theme_classic() 
)

plot(mds_result, col = pam_result_s$clustering, pch = 16, main = "PAM Clustering")

# Add cluster centers to the plot
points(pam_result_s$medoids, col = 1:3, pch = 3, cex = 2)


###MDS+mclust
library(mclust)
umap_mcl_s <- Mclust(mds_result, G = 2)
umap_mcl_s$classification
umap_mcl_sil_s=silhouette(umap_mcl_s$classification,dis_ss)
windows()
plot(umap_mcl_sil_s)
sil_umap_mc_s=mean(umap_mcl_sil_s[,3])

plot(umap_mcl_s, what = "classification", 
     addEllipses = FALSE)


###Rand index
library(flexclust)

#Scaled rand index
ri_umap_k_s <- randIndex(dat$diagnosis, res_s$cluster)
ri_umap_tclust_s <- randIndex(dat$diagnosis,clus_s$cluster)
ri_umap_pam_s<- randIndex(dat$diagnosis, pam_result_s$clustering)
ri_umap_mc_s <- randIndex(dat$diagnosis,umap_mcl_s$classification)
ri_umap_tk_s<-randIndex(dat$diagnosis, u_t_K$cluster)

#Scaled dunn index
D_umap_k_s<-dunn(clusters=res_s$cluster, distance=dis_ss)
D_umap_tclust_s<-dunn(clusters=clus_s$cluster, distance=dis_ss)
D_umap_pam_s<-dunn(clusters=pam_result_s$clustering, distance=dis_ss)
D_umap_mc_s<-dunn(clusters=umap_mcl_s$classification, distance=dis_ss)
D_umap_tk_s<-dunn(clusters=u_t_K$cluster,distance=dis_ss)

#ARI
umap_kmeans_ari=adj.rand.index(dat$diagnosis,res_s$cluster)
umap_tclust_ari=adj.rand.index(dat$diagnosis, clus_s$cluster)
umap_pam_ari=adj.rand.index(dat$diagnosis, pam_result_s$clustering)
umap_mc_ari=adj.rand.index(dat$diagnosis, umap_mcl_s$classification)
umap_tk_ari=adj.rand.index(dat$diagnosis,u_t_K$cluster)

# Create a Table with metrics with scaled variables

Metrics_table_int_scaled <- data.frame(
  Method = c("MDS + kmeans", "MDS + tclust", "MDS + PAM","MDS + mclust","MDS + tkmeans"),
  Silhouette_Score = c(umap_km_s, umap_tc_s, umap_pam_s,sil_umap_mc_s, sil_u_t_K_val),
  RI=c(ri_umap_k_s,ri_umap_tclust_s,ri_umap_pam_s,ri_umap_mc_s,ri_umap_tk_s),
  ARI=c(umap_kmeans_ari,umap_tclust_ari,umap_pam_ari,umap_mc_ari,umap_tk_ari),
  DI=round(c(D_umap_k_s,D_umap_tclust_s,D_umap_pam_s,D_umap_mc_s,D_u_t_k),6)
  
)
# Print the table
print(Metrics_table_int_scaled)
#install.packages("xtable")
library(xtable)
xtable(Metrics_table_int_scaled)


