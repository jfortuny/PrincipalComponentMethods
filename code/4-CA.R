# Correspondence Analysis
library(FactoMineR)
library(factoextra)

data(housetasks)
str(housetasks)
head(housetasks)

# Graph the relationships in the contingency table
library(gplots)
# Convert the data to table
dt <- as.table((as.matrix(housetasks)))
balloonplot(t(dt),
            main = "Housetasks",
            xlab = "",
            ylab = "",
            label = FALSE,
            show.margins = FALSE)

# For a small table we could use the chisq function:
# H0: row and column variables are independent
# H1: row and column variables are dependent
chisq <- chisq.test(housetasks)
chisq

# You can also use CA from FactoMineR
res.ca <- CA(housetasks, graph = FALSE)
res.ca

eig.value <- get_eigenvalue(res.ca)
eig.value

# Scree plot
fviz_screeplot(res.ca,
               addlabels = TRUE,
               ylim = c(0,50))

fviz_ca_biplot(res.ca,
               repel = TRUE)

# Contributions/Importance of rows and columns
row <- get_ca_row(res.ca)
row
fviz_ca_row(res.ca, repel = TRUE)
# Color the points by the cos2, the degree of association between the row and an axis
fviz_ca_row(res.ca,
            col.row = "cos2",
            repel = TRUE)
library(corrplot)
corrplot(row$cos2, is.corr = FALSE)
# How well represented are the rows in the first two dimensions
fviz_cos2(res.ca,
          choice = "row",
          axes = c(1,2))
# Row contributions to the dimensions
corrplot(row$contrib, is.corr = FALSE)
