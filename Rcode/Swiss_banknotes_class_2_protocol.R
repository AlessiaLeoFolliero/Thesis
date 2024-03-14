
dat <- read.delim("SwissBankNotes.txt")
my_cols <- c("#00AFBB", "#E7B800") 
head(dat)
#Deleting the id column
dat=dat[,-8]
head(dat)
anyNA(dat)
dim(dat)
View(dat)
head(dat)

#2 cluster
pairs(scale(dat[,1:6]), pch = 19,  cex = 0.5,
      col = my_cols[dat$class1],
      lower.panel=NULL)



###tsne 
#install.packages("tsne")
library(tsne)
library(plotly)
set.seed(12)

tsne <- tsne(dat[,1:6], initial_dims = 2)

tsne <- data.frame(tsne)
pdb <- cbind(tsne,dat$class1)
tsne
options(warn = -1)
fig <-  plot_ly(data = pdb ,x =  ~X1, y = ~X2, type = 'scatter', mode = 'markers', split = ~dat$class1)
fig <- fig %>%
  layout(
    plot_bgcolor = "#e5ecf6"
  )
fig

#with scaled data (in this case data need to be scaled)
tsne <- tsne(scale(dat[,1:6]), initial_dims = 2)
scale(dat[,1:6])
tsne_s <- data.frame(tsne)
pdb <- cbind(tsne_s,dat$class1)
options(warn = -1)

fig <-  plot_ly(data = pdb ,x =  ~X1, y = ~X2, type = 'scatter', mode = 'markers', split = ~dat$class1)
fig <- fig %>%
  layout(
    plot_bgcolor = "#e5ecf6"
  )
fig



### umap
library(plotly) 
library(umap) 
umap = umap(dat[,1:6], n_components = 2, random_state = 15, n_neighbours=15) 
layout <- umap[["layout"]] 
layout <- data.frame(layout) 
final <- cbind(layout, dat$class1) 

fig <- plot_ly(final, x = ~X1, y = ~X2, color = ~dat$class1, type = 'scatter', mode = 'markers')%>%  
  layout(
    plot_bgcolor = "#e5ecf6",
    legend=list(title=list(text='cluster')), 
    xaxis = list( 
      title = "0"),  
    yaxis = list( 
      title = "1")) 
fig


#With scaled data
umap_s = umap(scale(dat[,1:6]), n_components = 2, random_state = 15, n_neighbours=15) 
layout_s <- umap_s[["layout"]] 
layout_ss <- data.frame(layout_s) 


final <- cbind(layout_ss, dat$class1) 

fig <- plot_ly(final, x = ~X1, y = ~X2, color = ~dat$class1, type = 'scatter', mode = 'markers')%>%  
  layout(
    plot_bgcolor = "#e5ecf6",
    legend=list(title=list(text='cluster')), 
    xaxis = list( 
      title = "0"),  
    yaxis = list( 
      title = "1")) 
fig

#We will be working only on scaled data


###Kmeans with scaled data
mydata_s=scale(dat[,1:6])
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
plot (ctlcurves (mydata_s, k = 2, alpha = seq (0, 0.3, by = 0.025)))
clus <- tclust (mydata_s, k = 2, alpha = 0.1, restr.fact = 50)


### silhuette comparison

library (vegan)
library (cluster)
dis_n = dist(mydata_s)
sil_orig=silhouette(dat$class1,dis_n)
windows()
plot(sil_orig)
###################  kmeans
#############################################
dis = dist(mydata_s)^2
dis_n=dist(mydata_s)
res = kmeans(mydata_s,2)
sil_k = silhouette (res$cluster, dis)
windows()
plot(sil_k)
sil_l_m=mean(sil_k[,3])
sil_l_m
#0.57
#Dunn index
#Dunn index has values betwee zero and infinity and it needs to be maximised
library(clValid)
D_k<-dunn(clusters=res$cluster, distance=dis)
D_k
print(paste("Dunn Index:", D_k))
#0.0126465574373097
#plot
library(factoextra)
fviz_cluster(res, data = mydata_s,
             palette = c("#E7B800", "#00AFBB"), 
             geom = "point",
             ellipse.type = "convex", 
             ggtheme = theme_bw()
)

#Adjusted rand index
library(cluster)
library(fossil)
kmeans_ari=adj.rand.index(dat$class1, res$cluster)
kmeans_ari
#0.8456292

############Trimmed kmeans
##############################################
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

tk_ari=adj.rand.index(dat$class1,t_K$cluster)




#####################  tclust
############################################
set.seed(123)

#Scaled data
dis_s = dist(mydata_s)^2
clus_s <- tclust (mydata_s, k = 2, alpha = 0.1, restr.fact = 50)
sil_t_s = silhouette (clus_s$cluster, dis_s)
windows()
plot(sil_t_s)
sil_t_ss=mean(sil_t_s[,3])
sil_t_ss
#0.49
#Dunn index
D_t_s<-dunn(clusters=clus_s$cluster, distance=dis_s)
D_t_s
print(paste("Dunn Index:", D_t_s))
#0.0494649156621497
#Adjusted rand index
tclust_ari=adj.rand.index(dat$class1,clus_s$cluster)
tclust_ari

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
#0.56
sil_pam_m_s=mean(sil_pam_s[,3])
#Dunn index
D_p_s<-dunn(clusters=pam_result_s$cluster, distance=dis_s)
D_p_s
print(paste("Dunn Index:", D_p_s))
#0.0168853887544032
#ARI
pam_ari=adj.rand.index(dat$class1,pam_result_s$cluster)
pam_ari
#0.9406018
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
#0.00267923040717929
#Adjusted rand index
mc_ari=adj.rand.index(dat$class1,mclust_model_s$classification)


###############################
library(cluster)
library(factoextra)

#Compute rand index with scaled variables
ri_k_s <- rand.index(dat$class1, res$cluster)
ri_mclust_s <- rand.index(dat$class1,mclust_model_s$classification)
ri_tclust_s <- rand.index(dat$class1,clus_s$cluster)
ri_pam_s <- rand.index(dat$class1, pam_result_s$cluster)
ri_tkm_s<- rand.index(dat$class1, t_K$cluster)



############Display the metrics
#######################################################

#Table with scaled data
Scaled_metrics_table <- data.frame(
  Method = c("kmeans", "tclust", "PAM","mclust","tkmeans"),
  Silhouette_Score = c(sil_l_m, sil_t_ss,sil_pam_m_s ,sil_mc_s, sil_t_K_val),
  RI=c(ri_k_s,ri_tclust_s,ri_pam_s,ri_mclust_s,ri_tkm_s),
  ARI=c(kmeans_ari,tclust_ari,pam_ari,mc_ari,tk_ari),
  DI=c(D_k,D_t_s,D_p_s,D_m_s,D_t_k)
)
# Print the table
print(Scaled_metrics_table)

#Method Silhouette_Score        RI       ARI         DI
# kmeans        0.5669878 0.9228141 0.8456292 0.01264656
# tclust        0.4850781 0.9082915 1.0000000 0.04946492
#    PAM        0.5647499 0.9703015 0.9406018 0.01688539
# mclust        0.5579748 0.9900000 0.9799995 0.04363545
#tkmeans        0.5148033 0.8607035 0.9086540 0.02645343
library(xtable)
xtable(Scaled_metrics_table)


#######################################################
###################INTEGRATED APPROACHES###############
#######################################################


################Integrated approach with tsne as preprocessing
########### tsne+kmeans
############################
dis_s = dist(tsne_s)^2
res_t_k_s = kmeans(tsne_s,2)
sil_t_k_s = silhouette (res_t_k_s$cluster, dis_s)
windows()
plot(sil_t_k_s)
tsne_kmeans_sil_s=mean(sil_t_k_s[,3])

fviz_cluster(res_t_k_s, data = tsne_s,
             palette = c("#E7B800", "#00AFBB"), 
             geom = "point",
             ellipse.type = "convex", 
             ggtheme = theme_bw()
)

####
########################tsne + Trimmed k-means clustering
#################################################
#install.packages("trimcluster")
#library(trimcluster)
t_t_K=tkmeans(tsne_s,2,alpha=0.1,iter.max =100)
print(t_t_K)
plot(t_t_K,tsne_s,xlab="variables",ylab="variables")

sil_t_t_K = silhouette (t_t_K$cluster, dis_s)
windows()
plot(sil_t_t_K)
sil_t_t_K_val=mean(sil_t_t_K[,3])
sil_t_t_K_val

D_t_t_k= dunn(cluster=t_t_K$cluster,distance= dis_s)
D_t_t_k
#0.02038168
#ARI
ttk_ari=adj.rand.index(dat$class1,t_t_K$cluster)

#tsne+tclust
#Scaled
clus_t_t_s <- tclust(tsne_s, k = 2, alpha = 0.1, restr.fact = 50)
sil_ts_tc_s = silhouette (clus_t_t_s$cluster, dis_s)
windows()
plot(sil_ts_tc_s)
tsne_tcl_sil_s=mean(sil_ts_tc_s[,3])
tsne_tcl_sil_s
plot(clus_t_t_s)

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
ri_tsne_k_s <- rand.index(dat$class1, res_t_k_s$cluster)
ri_tsne_tclust_s <- rand.index(dat$class1,clus_t_t_s$cluster)
ri_tsne_pam_s <- rand.index(dat$class1, pam_result_s$clustering)
ri_tsne_mc_s<- rand.index(dat$class1,tsne_mcl_s$classification)
ri_tkmeans_s<-rand.index(dat$class1, t_t_K$cluster)


##Dunn index with scaled variables
D_tsne_k_s<-dunn(clusters=res_t_k_s$cluster, distance=dis_s)
D_tsne_tclust_s<-dunn(clusters=clus_t_t_s$cluster, distance=dis_s)
D_tsne_pam_s<-dunn(clusters=pam_result_s$clustering, distance=dis_s)
D_tsne_mc_s<-dunn(clusters=tsne_mcl_s$classification, distance=dis_s)

#Adjusted rand index
tsne_kmeans_ari=adj.rand.index(dat$class1, res_t_k_s$cluster)
tsne_tclust_ari=adj.rand.index(dat$class1, clus_t_t_s$cluster)
tsne_pam_ari=adj.rand.index(dat$class1, pam_result_s$clustering)
tsne_mc_ari=adj.rand.index(dat$class1, tsne_mcl_s$classification)


#Integrated approach metrics tables

Scaled_Metrics_table <- data.frame(
  Method = c("t-SNE + kmeans", "t-SNE + tclust", "t-SNE + PAM","t-SNE + mclust", "t-SNE + tkmeans"),
  Silhouette_Score = c(tsne_kmeans_sil_s, tsne_tcl_sil_s, tsn_pam_ss, tsne_kmeans_sil_s, sil_t_t_K_val),
  RI=c(ri_tsne_k_s,ri_tsne_tclust_s,ri_tsne_pam_s,ri_tsne_mc_s, ri_tkmeans_s),
  ARI=c(tsne_kmeans_ari,tsne_tclust_ari,tsne_pam_ari,tsne_mc_ari, ttk_ari),
  DI=round(c(D_tsne_k_s,D_tsne_tclust_s,D_tsne_pam_s,D_tsne_mc_s,D_t_t_k),6)
)
# Print the table
print(Scaled_Metrics_table)
########Method Silhouette_Score        RI       ARI       DI
#t-SNE + kmeans        0.8079100 0.9415075 0.8830121 0.010799
#t-SNE + tclust        0.5934329 0.8779899 0.9445254 0.042708
#   t-SNE + PAM        0.8079100 0.9415075 0.8830121 0.010799
# t-SNE + mclust       0.8079100 0.9321106 0.8642194 0.002374
#t-SNE + tkmeans        0.5559699 0.8956784 0.7114496 0.113804
library(xtable)
xtable(Scaled_Metrics_table)

########################## Integrated approach with umap as preprocessing
############umap+kmeans
dis_ss = dist(layout_ss)^2
res_s = kmeans(layout_ss,2)
sil_umap_km_s = silhouette (res_s$cluster, dis_ss)
windows()
plot(sil_umap_km_s)
umap_km_s=mean(sil_umap_km_s[,3])
umap_km_s
fviz_cluster(res_s,data=layout_ss,palette = c("#E7B800", "#00AFBB"), 
             geom = "point",
             ellipse.type = "convex", 
             ggtheme = theme_bw())

####Umap+ tkmeans
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
utk_ari=adj.rand.index(dat$class1,u_t_K$cluster)


#umap+tclust
dis_ss = dist(layout_ss)^2
clus_s <- tclust(layout_ss, k = 2, alpha = 0.1, restr.fact = 50)
sil_umap_tc_s = silhouette (clus_s$cluster, dis_ss)
windows()
plot(sil_umap_tc_s)
umap_tc_s=mean(sil_umap_tc_s[,3])
umap_tc_s
plot(clus_s)

#umap+PAM
library(cluster)
pam_result_s <- pam(layout_ss, k = 2)
sil_pam_s=silhouette(pam_result_s$clustering,dis_ss)
sil_pam_s
windows()
plot(sil_pam_s)
umap_pam_s=mean(sil_pam_s[,3])
umap_pam_s
plot(sil_pam_s)
fviz_cluster(pam_result_s, 
             palette = c("#00AFBB", "#FC4E07"), # color palette
             ellipse.type = "t",  #Concentration ellipse 
             ggtheme = theme_classic() 
)


###Umap+mclust
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
ri_umap_k_s <- randIndex(dat$class1, res_s$cluster)
ri_umap_tclust_s <- randIndex(dat$class1,clus_s$cluster)
ri_umap_pam_s<- randIndex(dat$class1, pam_result_s$clustering)
ri_umap_mc_s <- randIndex(dat$class1,umap_mcl_s$classification)
ri_umap_tk_s<-randIndex(dat$class1, u_t_K$cluster)



#Scaled dunn index
D_umap_k_s<-dunn(clusters=res_s$cluster, distance=dis_ss)
D_umap_tclust_s<-dunn(clusters=clus_s$cluster, distance=dis_ss)
D_umap_pam_s<-dunn(clusters=pam_result_s$clustering, distance=dis_ss)
D_umap_mc_s<-dunn(clusters=umap_mcl_s$classification, distance=dis_ss)

#ARI
umap_kmeans_ari=adj.rand.index(dat$class1,res_s$cluster)
umap_tclust_ari=adj.rand.index(dat$class1, clus_s$cluster)
umap_pam_ari=adj.rand.index(dat$class1, pam_result_s$clustering)
umap_mc_ari=adj.rand.index(dat$class1, umap_mcl_s$classification)
umap_tk_ari=adj.rand.index(dat$class1, u_t_K$cluster)

# Create a Table with metrics with scaled variables
Metrics_table_int_scaled <- data.frame(
  Method = c("UMAP + kmeans", "UMAP + tclust", "UMAP + PAM","UMAP + mclust","UMAP + tkmeans"),
  Silhouette_Score = c(umap_km_s, umap_tc_s, umap_pam_s,sil_umap_mc_s, sil_u_t_K_val),
  RI=c(ri_umap_k_s,ri_umap_tclust_s,ri_umap_pam_s,ri_umap_mc_s, ri_umap_tk_s),
  ARI=c(umap_kmeans_ari,umap_tclust_ari,umap_pam_ari,umap_mc_ari, umap_tk_ari),
  DI=round(c(D_umap_k_s,D_umap_tclust_s,D_umap_pam_s,D_umap_mc_s, D_u_t_k),5)
  
)
# Print the table
print(Metrics_table_int_scaled )

#Method        Silhouette_Score        RI       ARI      DI
# UMAP + kmeans        0.9509181 0.9602001 0.9602001   1.34431
# UMAP + tclust        0.6135392 0.8032575 0.9629856   1.37574
#    UMAP + PAM       0.9509181 0.9602001 0.9602001  1.34431
# UMAP + mclust       0.7883563 0.9602001 0.9602001   1.34431
#UMAP + tkmeans        0.5559699 0.8032575 0.9629856 2.82861
xtable(Metrics_table_int_scaled )




#Pre processing with PCA
library(tidyverse)
results <- prcomp(mydata_s, scale = FALSE)
res_pca=results$x[,1:2]
head(res_pca)
res_pca=data.frame(res_pca)

########################## Integrated approach with PCA as preprocessing
############PCA+kmeans
dis_ss = dist(res_pca)^2
res_s = kmeans(res_pca,2)
sil_pca_km_s = silhouette (res_s$cluster, dis_ss)
windows()
plot(sil_pca_km_s)
pca_km_s=mean(sil_pca_km_s[,3])
pca_km_s
res_s$cluster

fviz_cluster(res_s,data=res_pca,palette = c("#E7B800", "#00AFBB"), 
             geom = "point",
             ellipse.type = "convex", 
             ggtheme = theme_bw())


###
#####Trimmed kmeans
p_t_K=tkmeans(res_pca,2,alpha=0.1,iter.max =100)
print(p_t_K)
plot(p_t_K,res_pca,xlab="variables",ylab="variables")
table(p_t_K$cluster)
sil_p_t_K = silhouette (p_t_K$cluster, dis_ss)
windows()

plot(sil_p_t_K)
sil_p_t_K_val=mean(sil_p_t_K[,3])
sil_p_t_K_val
D_p_t_k= dunn(cluster=p_t_K$cluster,distance= dis_ss)
D_p_t_k
#ARI
library(fossil)
ptk_ari=adj.rand.index(dat$class1,p_t_K$cluster)



#PCA+tclust
dis_ss = dist(res_pca)^2
clus_s <- tclust(res_pca, k = 2, alpha = 0.1, restr.fact = 50)
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
             palette = c("#00AFBB", "#FC4E07"), # color palette
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

plot(pca_mcl_s, what = "classification", 
     addEllipses = TRUE)


###Rand index
library(flexclust)

#Scaled rand index
ri_pca_k_s <- randIndex(dat$class1, res_s$cluster)
ri_pca_tclust_s <- randIndex(dat$class1,clus_s$cluster)
ri_pca_pam_s<- randIndex(dat$class1, pam_result_s$clustering)
ri_pca_mc_s <- randIndex(dat$class1,pca_mcl_s$classification)
ri_pca_tk_s<-randIndex(dat$class1, p_t_K$cluster)

#Scaled dunn index
D_pca_k_s<-dunn(clusters=res_s$cluster, distance=dis_ss)
D_pca_tclust_s<-dunn(clusters=clus_s$cluster, distance=dis_ss)
D_pca_pam_s<-dunn(clusters=pam_result_s$clustering, distance=dis_ss)
D_pca_mc_s<-dunn(clusters=pca_mcl_s$classification, distance=dis_ss)
D_pca_tk_s<-dunn(clusters=p_t_K$cluster,distance=dis_ss)

#ARI
pca_kmeans_ari=adj.rand.index(dat$class1,res_s$cluster)
pca_tclust_ari=adj.rand.index(dat$class1, clus_s$cluster)
pca_pam_ari=adj.rand.index(dat$class1, pam_result_s$clustering)
pca_mc_ari=adj.rand.index(dat$class1, pca_mcl_s$classification)
pca_tk_ari=adj.rand.index(dat$class1,p_t_K$cluster)

# Create a Table with metrics with scaled variables

Metrics_table_int_scaled <- data.frame(
  Method = c("PCA + kmeans", "PCA + tclust", "PCA + PAM","PCA + mclust","PCA + tkmeans"),
  Silhouette_Score = c(pca_km_s, pca_tc_s, pca_pam_s,sil_pca_mc_s, sil_p_t_K_val),
  RI=c(ri_pca_k_s,ri_pca_tclust_s,ri_pca_pam_s,ri_pca_mc_s,ri_pca_tk_s),
  ARI=c(pca_kmeans_ari,pca_tclust_ari,pca_pam_ari,pca_mc_ari,pca_tk_ari),
  DI=round(c(D_pca_k_s,D_pca_tclust_s,D_pca_pam_s,D_pca_mc_s,D_p_t_k),6)
  
)
# Print the table
print(Metrics_table_int_scaled)
#install.packages("xtable")
library(xtable)
xtable(Metrics_table_int_scaled)



#Pre processing with MDS with the Euclidean distance
library(tidyverse)
#Distanza euclidea
dis_n=dist(mydata_s)

#Mahalanobis distance
library(stats)

#dis_n=mahalanobis(mydata_s,colMeans(mydata_s), cov=cov(mydata_s))
#class(dis_n)
#dis_n=as.dist(dis_n)
#mds_result <- cmdscale(dis_n)
#head(mds_result)
#mds_result=data.frame(mds_result)
#dat[,1:6]
#Non scaled data
#dis_n=mahalanobis(dat[,1:6],colMeans(dat[,1:6]), cov=cov(dat[,1:6]))
#class(dis_n)
#dis_n=as.dist(dis_n)
#mds_result <- cmdscale(dis_n,k=2)
#head(mds_result)
#mds_result=data.frame(mds_result)
#mds_result
#dat[,1:6]

#Mahalanobis from statMatch
library(StatMatch)
mal_dis=mahalanobis.dist(mydata_s)
mal_dis
class(mal_dis)
dis_n=as.dist(mal_dis)
mds_result <- cmdscale(dis_n,k=2)
head(mds_result)
mds_result=data.frame(mds_result)
mds_result




########################## Integrated approach with MDS as preprocessing
############MDS+kmeans
dis_ss = dist(mds_result)^2
res_s = kmeans(mds_result,2)
#dis_n=mahalanobis.dist(mds_result)
sil_mds_km_s = silhouette (res_s$cluster, dis_ss)
#sil_mds_km_m=silhouette (res_s$cluster, dis_n)
windows()
plot(sil_mds_km_s)
#plot(sil_mds_km_m)
#sil_mds_km_m

mds_km_s=mean(sil_mds_km_s[,3])


plot(res_s$cluster)


fviz_cluster(res_s,data=mds_result,palette = c("#E7B800", "#00AFBB"), 
             geom = "point",
             ellipse.type = "convex", 
             ggtheme = theme_bw())
cluster_colors <- c("#E7B800", "#00AFBB")
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
library(clValid)
D_m_t_k= dunn(cluster=m_t_K$cluster,distance= dis_ss)
D_m_t_k
#0.01437216
#ARI
library(fossil)
mtk_ari=adj.rand.index(dat$class1,m_t_K$cluster)



#MDS+tclust
dis_ss = dist(mds_result)^2
clus_s <- tclust(mds_result, k = 2, alpha = 0.1, restr.fact = 50)
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

plot(mds_result, col = pam_result_s$clustering, pch = 16, main = "PAM Clustering")

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
     addEllipses = FALSE)


###Rand index
library(flexclust)

#Scaled rand index
ri_mds_k_s <- randIndex(dat$class1, res_s$cluster)
ri_mds_tclust_s <- randIndex(dat$class1,clus_s$cluster)
ri_mds_pam_s<- randIndex(dat$class1, pam_result_s$clustering)
ri_mds_mc_s <- randIndex(dat$class1,mds_mcl_s$classification)
ri_mds_tk_s<-randIndex(dat$class1, m_t_K$cluster)

#Scaled dunn index
D_mds_k_s<-dunn(clusters=res_s$cluster, distance=dis_ss)
D_mds_tclust_s<-dunn(clusters=clus_s$cluster, distance=dis_ss)
D_mds_pam_s<-dunn(clusters=pam_result_s$clustering, distance=dis_ss)
D_mds_mc_s<-dunn(clusters=mds_mcl_s$classification, distance=dis_ss)
D_mds_tk_s<-dunn(clusters=m_t_K$cluster,distance=dis_ss)

#ARI
mds_kmeans_ari=adj.rand.index(dat$class1,res_s$cluster)
mds_tclust_ari=adj.rand.index(dat$class1, clus_s$cluster)
mds_pam_ari=adj.rand.index(dat$class1, pam_result_s$clustering)
mds_mc_ari=adj.rand.index(dat$class1, mds_mcl_s$classification)
mds_tk_ari=adj.rand.index(dat$class1,m_t_K$cluster)

# Create a Table with metrics with scaled variables


Metrics_table_int_scaled <- data.frame(
  Method = c("MDS + kmeans", "MDS + tclust", "MDS + PAM","MDS + mclust","MDS + tkmeans"),
  Silhouette_Score = c(mds_km_s, mds_tc_s, mds_pam_s,sil_mds_mc_s, sil_m_t_K_val),
  RI=c(ri_mds_k_s,ri_mds_tclust_s,ri_mds_pam_s,ri_mds_mc_s,ri_mds_tk_s),
  ARI=c(mds_kmeans_ari,mds_tclust_ari,mds_pam_ari,mds_mc_ari,mds_tk_ari),
  DI=round(c(D_mds_k_s,D_mds_tclust_s,D_mds_pam_s,D_mds_mc_s,D_m_t_k),6)
  
)
# Print the table
print(Metrics_table_int_scaled)
#install.packages("xtable")
library(xtable)
xtable(Metrics_table_int_scaled)





