# Chapter 8 - HCPC
library(FactoMineR)
library(factoextra)

data("USArrests")
USArrests

# Continuous Variables
# First run PCA ####################################################
res.pca <- PCA(USArrests, ncp = 3, graph = FALSE)
res.pca$var$contrib
res.pca$var$cos2


# Second Cluster ###################################################
res.hcpc <- HCPC(res.pca, graph = FALSE)
# View the dendrogram
fviz_dend(res.hcpc,
          cex = 0.7,
          palette = "jco",
          rect = TRUE,
          rect_fill = TRUE,
          rect_border = "jco",
          labels_track_height = 0.8)
# View the Clusters
fviz_cluster(res.hcpc,
             repel = TRUE,
             show.clust.cent = TRUE,
             palette = "jco",
             ggtheme = theme_minimal(),
             main = "Factor Map")


# Categorical Variables
data("tea")
head(tea)
# Perform MCA ###################################################
res.mca <- MCA(tea,
               ncp = 20,
               quanti.sup = 19,
               quali.sup = c(20:36),
               graph = FALSE)
# Perform Clustering ############################################
res.hcpc <- HCPC(res.mca, graph = FALSE, max = 3)
# Visualize dendrogram
fviz_dend(res.hcpc,
          show_labels = FALSE)
# Visualize Clusters
fviz_cluster(res.hcpc,
             geom = "point",
             main = "Factor Map")
