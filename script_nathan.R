rm(list = ls())

##script natou

data <- read.csv2("creation.csv")

library(FactoMineR)
library(Factoshiny)
library(factoextra)
library(dplyr)
library(tidyr)
library(ggplot2)
library(ggrepel)

## Question 2

# Départements en noms de lignes
rownames(data) <- data$Departement
data$Departement <- NULL

# Colonnes par année
cols2013 <- grep("^X2013_", colnames(data), value = TRUE)
cols2023 <- grep("^X2023_", colnames(data), value = TRUE)

#Afin de comparer les comportements sectoriels indépendamment de la taille des
#départements, les données ont été transformées en profils 
#(proportions par secteur) avant la mise en œuvre de l’AFM.

# Données par année
X2013 <- data[, cols2013]
X2023 <- data[, cols2023]

# Profils (proportions)
prof2013 <- X2013 / rowSums(X2013)
prof2023 <- X2023 / rowSums(X2023)

# Données finales pour l’AFM
data_mfa <- data.frame(prof2013, prof2023)

# AFM
res.mfa <- MFA(
  data_mfa,
  group = c(length(cols2013), length(cols2023)),
  type = c("s", "s"),
  name.group = c("2013", "2023"),
  ncp = 5,
  graph = FALSE
)


# Listings à commenter
res.mfa$eig
res.mfa$group$contrib
res.mfa$group$RV
res.mfa$quanti.var$contrib

# Graphiques
fviz_screeplot(res.mfa, addlabels = TRUE)
fviz_mfa_ind(res.mfa, repel = TRUE)
fviz_mfa_var(res.mfa, "group", repel = TRUE)
fviz_mfa_var(res.mfa, "quanti.var", repel = TRUE)
fviz_mfa_ind(res.mfa, partial = "all", repel = TRUE)