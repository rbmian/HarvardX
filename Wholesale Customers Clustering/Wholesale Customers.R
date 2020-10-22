
# Install required Packages and Libraries

install.packages("RCurl")
install.packages("factoextra")
install.packages("dendextend")

library(corrplot)
library(dendextend)
library(stats)
library(RCurl)
library(factoextra)
library(cluster)
library(gridExtra)
library(tidyverse)

# 2. Data Exploration
wholesale <- read.csv("C:\\Users\\Raheem\\Downloads\\Wholesale customers data.csv")

# 2.1.1 Dimensions
dim(wholesale)

# 2.1.2 Variabls
glimpse(wholesale)

# 2.1.3 Summary Statistics
summary(wholesale)

# Standard Deviation
apply(wholesale, 2, sd)

# 2.1.4 Channel and Region Insights
table(wholesale$Channel)
table(wholesale$Region)

# 2.1.5 Correlation between variables

ws_cor <- cor(wholesale)
corrplot(ws_cor, method = "number")

# 3 Clustering Results
# Exclude categorical variables for clustering
wholesale_sc <- as.matrix(scale(wholesale[3:8]))
head(wholesale_sc)

# 3.1 K Means Clustering
# Selecting number of K's


# Elbow Method
set.seed(123)
fviz_nbclust(wholesale_sc , kmeans, method = "wss")

# Silhoutte Method
set.seed(123)
fviz_nbclust(wholesale_sc, kmeans, method = "silhouette")

# Gap Statistics
set.seed(123)
gap_stat_clust <- clusGap(wholesale_sc, FUN = kmeans, nstart = 25,K.max = 10, B = 50)
fviz_gap_stat(gap_stat_clust)

# Selecting Optimal Clusters

# Compute K-Means for K=2, K=3 and K=5
k2 <- kmeans(wholesale_sc, centers = 2, nstart = 30)
k3 <- kmeans(wholesale_sc, centers = 3, nstart = 30)
k5 <- kmeans(wholesale_sc, centers = 5, nstart = 30)
# Use fviz_cluster() to compare the results
d1 <- fviz_cluster(k2, geom = "point", data = wholesale_sc) + ggtitle("k = 2")
d2 <- fviz_cluster(k3, geom = "point", data = wholesale_sc) + ggtitle("k = 3")
d3 <- fviz_cluster(k5, geom = "point", data = wholesale_sc) + ggtitle("k = 5")
grid.arrange(d1, d2, d3, nrow = 3)

print(k2)
print(k3)
print(k5)

# Compute K-Means clustering with K = 3
set.seed(123)
final_segments <- kmeans(wholesale_sc, centers = 3, nstart = 30)
print(final_segments)

# Visualize the final clusters
fviz_cluster(final_segments, geom = "point", data = wholesale_sc)
cluster_means <- wholesale[3:8] %>%
  mutate(Cluster = final_segments$cluster) %>%
  group_by(Cluster) %>%
  summarise_all(list(mean))
cluster_means

# Average customer spending per segment:
  cluster_means %>%
  gather(Product, MU, Fresh:Delicassen)%>%
  ggplot(aes(x=Product , y = MU, fill = Product)) + geom_col(width = 0.7) +
  facet_grid(.~ Cluster)+
  scale_fill_brewer(palette = "Accent")+
  ylab("Customer Spending in Monetory Units") +
  ggtitle("Average Customer Spending per Segment")+
  theme(axis.text.x = element_blank(),
        axis.ticks.x = element_blank(),
        axis.title.x = element_blank())

# Channel and Region in Cluster 1
wholesale[1:2] %>%
    mutate(Cluster = final_segments$cluster) %>%
    filter(Cluster == 1) %>% ungroup() %>% count(Channel, Region)

# Channel and Region in Cluster 2
wholesale[1:2] %>%
    mutate(Cluster = final_segments$cluster) %>%
    filter(Cluster == 2) %>% ungroup() %>% count(Channel, Region)

# Channel and Region in Cluster 3
wholesale[1:2] %>%
    mutate(Cluster = final_segments$cluster) %>%
    filter(Cluster == 3) %>% ungroup() %>% count(Channel, Region)  

# 3.2 Hierarchical Clustering
wholesale_md <- wholesale[3:8]
head(wholesale_md)

# Use dendextend and stats libraries
# Calculate Euclidean distance for scaled data wholesale_sc
dist_ws <- dist(wholesale_md, method = "euclidean")

# Generate a complete linkage analysis
hc_ws_complete <- hclust(dist_ws, method = "complete")
# Plot
plot(hc_ws_complete)

# Generate a single linkage analysis
hc_ws_single <- hclust(dist_ws, method = "single")
# Plot
plot(hc_ws_single)

# Generate a average linkage analysis
hc_ws_average <- hclust(dist_ws, method = "average")
# Plot
plot(hc_ws_average)

# Generate a ward linkage analysis
hc_ws_ward <- hclust(dist_ws, method = "ward.D")
# Plot
plot(hc_ws_ward)

# Hierarchical Clustering with ward Linkage Method
hc_ws_ward <- hclust(dist_ws, method = "ward.D")
#Create a cluster assignment vector at h = 750000
cl_ws_ward <- cutree(hc_ws_ward, h = 750000)
#Generate the segmented customers data frame
Segment_ws_md <- mutate(wholesale_md, cluster = cl_ws_ward)

# Count the number of customers that fall in to each cluster
count(Segment_ws_md, cluster)

# Create and Color the dendrogram based on the height of cutoff
dend_ws <- as.dendrogram(hc_ws_ward)
dend_ws_color <- color_branches(dend_ws, h = 750000)
plot(dend_ws_color)

# Segmented Clusters
segmented_clusters <- Segment_ws_md %>%
  group_by(cluster) %>%
  summarise_all(list(mean))
segmented_clusters

# Customer Spending per cluster
segmented_clusters %>%
  gather(Product, MU, Fresh:Delicassen)%>%
  ggplot(aes(x=Product , y = MU, fill = Product)) + geom_col(width = 0.7) +
  facet_grid(.~ cluster)+
  scale_fill_brewer(palette = "Accent")+
  ylab("Customer Spending in Monetory Units") +
  ggtitle("Average Customer Spending per Segment")+
  theme(axis.text.x = element_blank(),
        axis.ticks.x = element_blank(),
        axis.title.x = element_blank())

