# Chapter 5 - MCA
library(FactoMineR)
library(factoextra)

data("poison")
summary(poison)



# subset the data for analysis
poison.active <- poison[1:55, 5:15]
head(poison.active)
summary(poison.active$Fish)
library(caret)
nearZeroVar(poison.active)
nearZeroVar(poison.active$Fish)

res.mca <- MCA(poison.active, graph = FALSE)
res.mca

library(ade4)
res.mca.ade4 <- dudi.acm(poison.active, scannf = FALSE, nf = 5)
res.mca.ade4
library(ExPosition)
res.mca.exp <- epMCA(poison.active, graphs = FALSE, correction = "bg")
res.mca.exp

# Visualize the scree plot
fviz_screeplot(res.mca, addlabels = TRUE, ylim = c(0,45))
fviz_screeplot(res.mca.ade4, addlabels = TRUE, ylim = c(0,45))
fviz_screeplot(res.mca.exp, addlabels = TRUE, ylim = c(0,45))

# Visualize the biplot
fviz_mca_biplot(res.mca,
                repel = TRUE,
                ggtheme = theme_minimal())
# Visualize the variables
var <- get_mca_var(res.mca)
var
var.mca.ade4 <- get_mca_var(res.mca.ade4)
var.mca.ade4

# Visualize correlations between variables and MCA principal dimensions
fviz_mca_var(res.mca,
             choice = "mca.cor",
             repel = TRUE,
             ggtheme = theme_minimal())

# Quality of representation of variables in a dimension
head(var$coord)
fviz_mca_var(res.mca,
             repel = TRUE,
             col.var = "cos2")
fviz_cos2(res.mca,
          choice = "var",
          axes = 1:2)

# Contribution of variables to the dimensions
# to dimension 1
fviz_contrib(res.mca,
             choice = "var",
             axes = 1,
             top = 15)
# to dimension 2
fviz_contrib(res.mca,
             choice = "var",
             axes = 2,
             top = 15)
# to dimensions 1 and s
fviz_contrib(res.mca,
             choice = "var",
             axes = 1:2,
             top = 15)
