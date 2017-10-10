# Chapter 7 - MFA
library(FactoMineR)
library(factoextra)

data("wine")
colnames(wine)

# Variables are ordered by groupings
res.mfa <- MFA(wine,
               group = c(2,5,3,10,9,2),
               type = c("n", rep("s", 5)),
               name.group = c("origin", "odor", "visual", "odor.after.shaking",
                              "taste", "overall"),
               num.group.sup = c(1,6),
               graph = FALSE)
res.mfa
# eigenvalues and variance explained
eig <- get_eigenvalue(res.mfa)
eig
# visualize scree plot
fviz_eig(res.mfa)
fviz_screeplot(res.mfa)

# variable groups
group <- get_mfa_var(res.mfa, element = "group")
group
fviz_mfa_var(res.mfa, choice = "group")
fviz_contrib(res.mfa, choice = "group", axes = 1)
fviz_contrib(res.mfa, choice = "group", axes = 1:2)
# quantitative variables
quanti.var <- get_mfa_var(res.mfa, element = "quanti.var")
quanti.var
fviz_mfa_var(res.mfa, choice = "quanti.var", repel = TRUE)
fviz_contrib(res.mfa, choice = "quanti.var", axes = 1, top = 20)
fviz_contrib(res.mfa, choice = "quanti.var", axes = 1:2, top = 20, palette = "jco")
fviz_cos2(res.mfa, choice = "quanti.var", axes = 1:2)
