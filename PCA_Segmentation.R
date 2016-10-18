# Feature hashing on the segmentation data

seg <- read.csv("TMS_McDonalds_ZIP_Append_20151009.csv")
seg<- TMS_McDonalds_ZIP_Append_20151009
seg<-seg[,-grep("SILH",colnames(seg))]
seg<-seg[,-grep("VALUE",colnames(seg))]
seg<-seg[,-grep("ID",colnames(seg))]
# seg_pred[is.na(seg_pred)] <- 0

# library(FeatureHashing)
# 
# seg_hashed = hashed.model.matrix(~., data=seg_pred, transpose=FALSE)
# 
# data_train_matrix <- as.matrix(scale(seg_hashed))

######## 

Sparse_Counter <- apply(seg, 2, function(x) length(which(x == 0 | x == "0"| is.na(x)))/length(x)) 

Col_Sparsity <- data.frame(Sparse_Counter)

Sparse_Name <- colnames(seg)[Sparse_Counter >= 0.9] # drop the columns with more than 90% null. Nulls appear as "" or NA. 

In_Check <- names(seg) %in% Sparse_Name;

seg <- seg[, !In_Check]

######### 

# Recode the dummies

# Factor generation


for(level in unique(seg$CAC_ADDR_CDI)){
  seg[paste("CAC_ADDR_CDI-DM",level, sep = "_")] <- ifelse(seg$CAC_ADDR_CDI == level, 1, -1)
}

for(level in unique(seg$CAC_ADDR_STATE)){
  seg[paste("CAC_ADDR_STATE",level, sep = "_")] <- ifelse(seg$CAC_ADDR_STATE == level, 1, -1)
}

seg$CAC_DEMO_AGE_ENH <- as.factor(seg$CAC_DEMO_AGE_ENH)

for(level in unique(seg$CAC_DEMO_AGE_ENH)){
  seg[paste("CAC_DEMO_AGE_ENH",level, sep = "_")] <- ifelse(seg$CAC_DEMO_AGE_ENH == level, 1, -1)
}

seg$CAC_DEMO_EDUCATION_ENH <- as.factor(seg$CAC_DEMO_EDUCATION_ENH)

for(level in unique(seg$CAC_DEMO_EDUCATION_ENH)){
  seg[paste("CAC_DEMO_EDUCATION_ENH",level, sep = "_")] <- ifelse(seg$CAC_DEMO_EDUCATION_ENH == level, 1, -1)
}

seg$CAC_DEMO_HH_SIZE_ENH <- as.factor(seg$CAC_DEMO_HH_SIZE_ENH)

for(level in unique(seg$CAC_DEMO_HH_SIZE_ENH)){
  seg[paste("CAC_DEMO_HH_SIZE_ENH",level, sep = "_")] <- ifelse(seg$CAC_DEMO_HH_SIZE_ENH == level, 1, -1)
}

seg$CAC_DEMO_HH_TYPE_ENH <- as.factor(seg$CAC_DEMO_HH_TYPE_ENH)

for(level in unique(seg$CAC_DEMO_HH_TYPE_ENH)){
  seg[paste("CAC_DEMO_HH_TYPE_ENH",level, sep = "_")] <- ifelse(seg$CAC_DEMO_HH_TYPE_ENH == level, 1, -1)
}

seg$CAC_DEMO_INCOME_ENH <- as.factor(seg$CAC_DEMO_INCOME_ENH)

for(level in unique(seg$CAC_DEMO_INCOME_ENH)){
  seg[paste("CAC_DEMO_INCOME_ENH",level, sep = "_")] <- ifelse(seg$CAC_DEMO_INCOME_ENH == level, 1, -1)
}

seg$CAC_DEMO_INCOME_NARROW_ENH <- as.factor(seg$CAC_DEMO_INCOME_NARROW_ENH)

for(level in unique(seg$CAC_DEMO_INCOME_NARROW_ENH)){
  seg[paste("CAC_DEMO_INCOME_NARROW_ENH",level, sep = "_")] <- ifelse(seg$CAC_DEMO_INCOME_NARROW_ENH == level, 1, -1)
}

seg$CAC_DEMO_MARITAL_STATUS <- as.factor(seg$CAC_DEMO_MARITAL_STATUS)

for(level in unique(seg$CAC_DEMO_MARITAL_STATUS)){
  seg[paste("CAC_DEMO_MARITAL_STATUS",level, sep = "_")] <- ifelse(seg$CAC_DEMO_MARITAL_STATUS == level, 1, -1)
}

seg$CAC_HOME_OWN <- as.factor(seg$CAC_HOME_OWN)

for(level in unique(seg$CAC_HOME_OWN)){
  seg[paste("CAC_HOME_OWN",level, sep = "_")] <- ifelse(seg$CAC_HOME_OWN == level, 1, -1)
  
}

seg$CAC_INT_POL_DONOR <- as.factor(seg$CAC_INT_POL_DONOR)

for(level in unique(seg$CAC_INT_POL_DONOR)){
  seg[paste("CAC_INT_POL_DONOR",level, sep = "_")] <- ifelse(seg$CAC_INT_POL_DONOR == level, 1, -1)
  
}

seg$CAC_CRED_ANY <- as.factor(seg$CAC_CRED_ANY)

for(level in unique(seg$CAC_CRED_ANY)){
  seg[paste("CAC_CRED_ANY",level, sep = "_")] <- ifelse(seg$CAC_CRED_ANY == level, 1, -1)
  
}

seg$CAC_HOME_DWELL_TYPE <- as.factor(seg$CAC_HOME_DWELL_TYPE)

for(level in unique(seg$CAC_HOME_DWELL_TYPE)){
  seg[paste("CAC_HOME_DWELL_TYPE",level, sep = "_")] <- ifelse(seg$CAC_HOME_DWELL_TYPE == level, 1, -1)
  
}

rm_names <- c("ACOOP_NAME","AZIP5_IN","CACDIRECT_MATCH_KEY","CAC_ADDR_CDI", "CAC_ADDR_CITY", "CAC_ADDR_GEO_MATCH_LEVEL", "CAC_ADDR_STATE", "CAC_ADDR_ZIP", "CAC_CENSUS_ID",
              "CAC_CENSUS_MATCH", "CAC_DEMO_AGE_ENH", "CAC_DEMO_EDUCATION_ENH", "CAC_DEMO_HH_SIZE_ENH", "CAC_DEMO_HH_TYPE_ENH",
              "CAC_DEMO_INCOME_ENH", "CAC_DEMO_INCOME_NARROW_ENH", "CAC_DEMO_MARITAL_STATUS","CAC_HOME_OWN", "CAC_INT_POL_DONOR","CAC_CRED_ANY","CAC_HOME_DWELL_TYPE" 
)

In_Check_2 <- names(seg) %in% rm_names;

seg <- seg[, !In_Check_2]


## Mean Value Replacementinst
for (i in which(sapply(seg, is.numeric))) {
  seg[is.na(seg[, i]), i] <- mean(seg[, i],  na.rm = TRUE)
}

seg2<-seg[,-which(names(seg) %in% c("CAC_INT_MAIL_BUY","CAC_DEMO_OCCUPATION","CAC_HOME_SQ_FOOT","CAC_INT_NUM","CAC_GEO_MATCH","CAC_DEMO_NUM_GENERATIONS_ENH","CAC_INT_POL_DONOR_NA","CEN_HSEVAL_19"))]

seg2<-seg2[,c(889:949,1:949)]

seg_scale<-data.frame(apply(seg2[,c(62:949)],2,scale))

seg_scale<-data.frame(seg2[,c(1:61)],seg_scale)

#PCA 

pca.res <- prcomp(seg_scale, retx=TRUE)
plot(pca.res)
screeplot(pca.res)
screeplot(pca.res, type="lines")
pc<-pca.res$rotation[,1:6]
pc<-abs(pca.res$rotation[,1:6])
rownames(pc[apply(pc, 2, which.max),])

plot(pca.res$x[,1:2], xlim=c(-6,9), ylim=c(-6,9), pch="")
text(pca.res$x[,1:2], labels=abbreviate(TMS_McDonalds_ZIP_Append_20151009[,2]), col="blue")
pca_map<-data.frame(pca.res$x[,1:2])
rownames(pca_map)<-TMS_McDonalds_ZIP_Append_20151009[,2]


#Interest Principal Components
seg_scale_int<-seg_scale[,829:949]
pca.res <- prcomp(seg_scale_int, retx=TRUE)
pc_int<-abs(pca.res$rotation[,1:4])
rownames(pc_int[apply(pc_int, 2, which.max),])
pc_int<-pca.res$rotation[,1:4]


#SOM Interest Segmentation

seg_scale_int_som<-seg_scale_int[,c("GEO_CAC_INT_105","GEO_CAC_INT_27","GEO_CAC_INT_40","GEO_CAC_INT_91","GEO_CAC_INT_21","GEO_CAC_INT_100","GEO_CAC_INT_98","GEO_CAC_INT_92")]
data_train_matrix<-as.matrix(seg_scale_int_som)
som_grid <- somgrid(xdim = 10, ydim=10, topo="hexagonal")
som_model <- som(data_train_matrix,
grid=som_grid, rlen=100, alpha=c(0.05,0.01),
keep.data = TRUE, n.hood="circular")

#Training SOM Model
summary(som_model)
plot(som_model, type = "changes")
plot(som_model, type = "counts", main="Node Counts", palette.name=coolBlueHotRed)
plot(som_model, type = "quality", main="Node Quality/Distance", palette.name=coolBlueHotRed)
plot(som_model, type="dist.neighbours", main = "SOM neighbour distances", palette.name=grey.colors)
plot(som_model, type = "codes")
plot(som_model, type = "property", property = som_model$codes[,4], main=names(som_model$data)[4], palette.name=coolBlueHotRed)

#Clustering Interests
mydata <- som_model$codes
wss <- (nrow(mydata)-1)*sum(apply(mydata,2,var))
for (i in 2:15) wss[i] <- sum(kmeans(mydata,
centers=i)$withinss)
par(mar=c(5.1,4.1,4.1,2.1))
plot(1:15, wss, type="b", xlab="Number of Clusters",
ylab="Within groups sum of squares", main="Within cluster sum of squares (WCSS)")
som_cluster <- cutree(hclust(dist(som_model$codes)), 6)
pretty_palette <- c("#67c4df", '#f7cb31', '#fc6b27', '#8dc63f', '#ed5861', '#45b7ae')
plot(som_model, type="mapping", bgcol = pretty_palette[som_cluster], main = "Clusters")
plot(som_model, type="codes", bgcol = pretty_palette[som_cluster], main = "Clusters")
add.cluster.boundaries(som_model, som_cluster)

#Merging Clusters and Preparing Data
seg_scale_int_ID<- seg_scale_int_som
seg_scale_int_ID$AZIP5_IN<-TMS_McDonalds_ZIP_Append_20151009$AZIP5_IN
cluster_details <- data.frame(AZIP5_IN=seg_scale_int_ID$AZIP5_IN, cluster=som_cluster[som_model$unit.classif])
table(cluster_details$cluster) # Check the distribution of cluster members.
cluster_cust <- merge(seg_scale_int_ID, cluster_details, by = "AZIP5_IN")
model_clust_copy<- seg_scale_int_ID
model_clust_copy$cluster<-0
cluster_cust3<-rbind(cluster_cust,model_clust_copy)

# Preparing Density Charts for Cluster Summary
library(ggplot2)
gg_cluster <- function(tgt_clu, tgt_col_num) {  # tgt_clu is a integer, tgt_var is a var name.
g <- ggplot(subset(cluster_cust3, cluster == 0 | cluster == tgt_clu), aes_string(x = colnames(cluster_cust3)[tgt_col_num]))
g1 <- g + geom_density(aes(fill = factor(cluster) , alpha = 0.6))
g2 <- g1 + theme(legend.position = "right")
g3 <- g2 + guides(fill=guide_legend(title="Cluster"))
return(g3)
}
cluster_sum <- function(tgt_clu) {
g1 <- gg_cluster(tgt_clu, 2)
g2 <- gg_cluster(tgt_clu, 3)
g3 <- gg_cluster(tgt_clu, 4)
g4 <- gg_cluster(tgt_clu, 5)
g5 <- gg_cluster(tgt_clu, 8)
g6 <- gg_cluster(tgt_clu, 9)
multiplot(g1, g2, g3, g4, g5, g6, cols=2)
}
attributes_comp <- function(tgt_col_num) {
g1 <- gg_cluster(1, tgt_col_num)
g2 <- gg_cluster(2, tgt_col_num)
g3 <- gg_cluster(3, tgt_col_num)
g4 <- gg_cluster(4, tgt_col_num)
g5 <- gg_cluster(5, tgt_col_num)
g6 <- gg_cluster(6, tgt_col_num)
g7 <- gg_cluster(7, tgt_col_num)
multiplot(g1, g2, g3, g4, g5, g6, g7, cols=2)
}

#All Principal Components
seg_scale_all<-seg_scale[,1:949]
pca.res <- prcomp(seg_scale_all, retx=TRUE)
pc<-abs(pca.res$rotation[,1:6])
rownames(pc[apply(pc, 2, which.max),])
pc<-pca.res$rotation[,1:6]


#SOM Interest Segmentation

seg_scale_all_som<-seg_scale_all[,c("CEN_HSESIZE_15","CEN_HHINC_2","CEN_POPLABR_7","GEO_CAC_INT_40","CEN_POPMAR_1","GEO_CAC_INT_98","CEN_POPPOV_2","GEO_CAC_INT_91","CEN_POPRACE_2","CEN_POPAGE_1")]
colnames(seg_scale_all_som)[1:10]<-c(">2 Units in HH","Median HH Income","% Female in Labor Force","Gourmet Foods","% Married","Health and Beauty Product Int.","< the Poverty Level","Stocks and Bonds","African-American Pop.","Average Age")
data_train_matrix_all<-as.matrix(seg_scale_all_som)
som_grid <- somgrid(xdim = 10, ydim=10, topo="hexagonal")
som_model <- som(data_train_matrix_all,
                 grid=som_grid, rlen=100, alpha=c(0.05,0.01),
                 keep.data = TRUE, n.hood="circular")

#Training SOM Model
summary(som_model)
plot(som_model, type = "changes")
plot(som_model, type = "counts", main="Node Counts", palette.name=coolBlueHotRed)
plot(som_model, type = "quality", main="Node Quality/Distance", palette.name=coolBlueHotRed)
plot(som_model, type="dist.neighbours", main = "SOM neighbour distances", palette.name=grey.colors)
plot(som_model, type = "codes")
plot(som_model, type = "property", property = som_model$codes[,4], main=names(som_model$data)[4], palette.name=coolBlueHotRed)

#Clustering Interests
mydata <- som_model$codes
wss <- (nrow(mydata)-1)*sum(apply(mydata,2,var))
for (i in 2:15) wss[i] <- sum(kmeans(mydata,
                                     centers=i)$withinss)
par(mar=c(5.1,4.1,4.1,2.1))
plot(1:15, wss, type="b", xlab="Number of Clusters",
     ylab="Within groups sum of squares", main="Within cluster sum of squares (WCSS)")
som_cluster_all <- cutree(hclust(dist(som_model$codes)), 6)
pretty_palette <- c("#67c4df", '#f7cb31', '#fc6b27', '#8dc63f', '#ed5861', '#45b7ae')
plot(som_model, type="mapping", bgcol = pretty_palette[som_cluster_all], main = "Clusters")
plot(som_model, type="codes", bgcol = pretty_palette[som_cluster_all])
add.cluster.boundaries(som_model, som_cluster_all)



#Merging Clusters and Preparing Data
seg_scale_all_ID<- seg_scale_all_som
seg_scale_all_ID$AZIP5_IN<-TMS_McDonalds_ZIP_Append_20151009$AZIP5_IN
cluster_details_all <- data.frame(AZIP5_IN=seg_scale_all_ID$AZIP5_IN, cluster=som_cluster_all[som_model$unit.classif])
table(cluster_details_all$cluster) # Check the distribution of cluster members.
cluster_cust_all <- merge(seg_scale_all_ID, cluster_details_all, by = "AZIP5_IN")
model_clust_all_copy<- seg_scale_all_ID
model_clust_all_copy$cluster<-0
cluster_cust4<-rbind(cluster_cust_all,model_clust_all_copy)
pca_map<-data.frame(pca.res$x[,1:2])
pca_map$AZIP5_IN<- TMS_McDonalds_ZIP_Append_20151009[,2]
pca_map<-merge(pca_map,cluster_cust4, by= "AZIP5_IN")
pca_map$PopulationCount<- TMS_McDonalds_ZIP_Append_20151009$CEN_COUNT_POP
pca_map$Ones<- rep(1,nrow(pca_map))
write.csv(pca_map,"pca_map.csv")
colnames(cluster_cust4)[2:11]<-c("CEN_HSESIZE_15","CEN_HHINC_2","CEN_POPLABR_7","GEO_CAC_INT_40","CEN_POPMAR_1","GEO_CAC_INT_98","CEN_POPPOV_2","GEO_CAC_INT_91","CEN_POPRACE_2","CEN_POPAGE_1")



# Preparing Density Charts for Cluster Summary
library(ggplot2)
gg_cluster <- function(tgt_clu, tgt_col_num) {  # tgt_clu is a integer, tgt_var is a var name.
  g <- ggplot(subset(cluster_cust4, cluster == 0 | cluster == tgt_clu), aes_string(x = colnames(cluster_cust4)[tgt_col_num]))
  g1 <- g + geom_density(aes(fill = factor(cluster) , alpha = 0.6))
  g2 <- g1 + theme(legend.position = "right")
  g3 <- g2 + guides(fill=guide_legend(title="Cluster"))
  return(g3)
}
cluster_sum <- function(tgt_clu) {
  g1 <- gg_cluster(tgt_clu, 2)
  g2 <- gg_cluster(tgt_clu, 3)
  g3 <- gg_cluster(tgt_clu, 4)
  g4 <- gg_cluster(tgt_clu, 5)
  g5 <- gg_cluster(tgt_clu, 6)
  g6 <- gg_cluster(tgt_clu, 7)
  g7 <- gg_cluster(tgt_clu, 8)
  g8 <- gg_cluster(tgt_clu, 9)
  g9 <- gg_cluster(tgt_clu, 10)
  g10 <- gg_cluster(tgt_clu, 11)
  multiplot(g1, g2, g3, g4, g5, g6, g7, g8, g9, g10, cols=2)
}

attributes_comp <- function(tgt_col_num) {
  g1 <- gg_cluster(1, tgt_col_num)
  g2 <- gg_cluster(2, tgt_col_num)
  g3 <- gg_cluster(3, tgt_col_num)
  g4 <- gg_cluster(4, tgt_col_num)
  g5 <- gg_cluster(5, tgt_col_num)
  g6 <- gg_cluster(6, tgt_col_num)
  g7 <- gg_cluster(7, tgt_col_num)
  multiplot(g1, g2, g3, g4, g5, g6, g7, cols=2)
}

wr




