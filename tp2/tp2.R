head(read.csv('https://marieetienne.github.io/datasets/overshootday_overview.csv'))

EmpEcoP <- read.csv('https://marieetienne.github.io/datasets/overshootday_overview.csv', row.names = c(1))
EmpEco <- read.csv('https://marieetienne.github.io/datasets/overshootday_overview.csv')

str(EmpEco)
summary(EmpEco)

EmpEco2 <- na.omit(EmpEco)
EmpEcoP2 <- na.omit(EmpEcoP)

library(FactoMineR)
library(factoextra)


empeco2.pca <- PCA(X = EmpEco2,
                  scale.unit = TRUE, ## ACP normée ou non ? 
                  ncp = 13, ## nombre de composantes principales à garder, dans le doute on garde tout
                  quali.sup = c(1,5,6), ## numero des colonnes des variables quanti sup
                  row.w = NULL, ## poids des individus
                  graph = FALSE, ## TRUE or FALSe doit on sortir les graphes
)



fviz_pca_var(empeco2.pca)
fviz_pca_var(empeco2.pca, select.var = list(cos2 = 0.8, contrib = NULL))
fviz_pca_ind(empeco2.pca, select.ind = list(cos2= 0.8))

x <- EmpEco2$life_expectancy
y <- EmpEco2$total_cons
z <- EmpEco2$per_capita_gdp
plot(x ~ y)
cov(x, y)/((var(x)*var(y))^(1/2))
cor(x,y, method = "pearson")
cor(x,y, method = "spearman")
cor(x,y, method = "kendall")

cor(x, z, method = "pearson")

