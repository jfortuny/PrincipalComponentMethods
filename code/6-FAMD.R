# Chapter 6 - FAMD
library(FactoMineR)
library(factoextra)

data("wine")
df <- wine[, c(1,2,16,22,29,28,30,31)]
str(df)

res.famd <- FAMD(df, graph = FALSE)
res.famd

# Scree plot
fviz_screeplot(res.famd)

# Variables
var <- get_famd_var(res.famd)
print(var)
# Plot variables
fviz_famd_var(res.famd, repel = TRUE)
# Contribution to 1st dimension
fviz_contrib(res.famd, "var", axes = 1)
