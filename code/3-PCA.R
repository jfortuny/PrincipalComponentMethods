# Chapter 3 - PCA
library(FactoMineR)
library(factoextra)

data("decathlon2")
str(decathlon2)

# split the data so we can predict some
decathlon2.active <- decathlon2[1:23, 1:10]
str(decathlon2.active)

# First PCA model
res.pca <- PCA(X = decathlon2.active,
               scale.unit = TRUE,
               graph = FALSE)
res.pca

# Examining the eigenvalues
eig.val <- get_eigenvalue(res.pca)
eig.val
# visualize the scree plot #########################################
fviz_eig(res.pca,
         addlabels = TRUE,
         ylim = c(0,50))
# Extract the results/variables for the first 5 dimensions
var <- get_pca_var(res.pca)
var
# Plot the correlation circle ######################################
head(var$coord, 10)
fviz_pca_var(res.pca)
# Quality of representation ########################################
head(var$cos2)
# Visualize the quality of representation
library(corrplot)
corrplot(var$cos2, is.corr = FALSE)
fviz_cos2(res.pca, choice = "var", axes = 1:2)
# Color coding the quality of representation
fviz_pca_var(res.pca,
             col.var = "cos2",
             gradient.cols = c("#00AFBB", "#E7B800", "#FC4E07"),
             repel = TRUE)
# Transparency coding the quality of representation
fviz_pca_var(res.pca,
             alpha.var = "cos2",
             repel = TRUE)
# Contributions of Variables to PCs ################################
var$contrib
library("corrplot")
corrplot(var$contrib, is.corr = FALSE)
# Contributions to PC1
fviz_contrib(res.pca,
             choice = "var",
             axes = 1, top = 10)
# Contributions to PC2
fviz_contrib(res.pca,
             choice = "var",
             axes = 2, top = 10)
# Contributions to PC1 and PC2
fviz_contrib(res.pca,
             choice = "var",
             axes = 1:2, top = 10)
# Color coding the contribution
fviz_pca_var(res.pca,
             col.var = "contrib",
             gradient.cols = c("#00AFBB", "#E7B800", "#FC4E07"),
             repel = TRUE)
# Dimension description ###########################################
res_desc <- dimdesc(res.pca, axes = 1:2, proba = 0.05)
# res_desc$Dim.1
# res_desc$Dim.2
res_desc
