library(readr)
library(dplyr)
library(tidyr)
library(cluster)
library(recipes)
library(janitor)
library(purrr)
library(FactoMineR)

setwd("~/academico/gh_repos/__transferencia/citizenlab/CitizenLab-Research-and-Development/casos_urjc/notebooks/V_model_operation/18_infraestructuras/")

## Solo si hay cambios:
# file.copy("../../dominios_II_y_III/18_infraestructuras/Data/Output/CU_18_05_16_distritos_variables.csv",
#           "cu_18_step_01_input/", overwrite = TRUE)

df <- read_csv("cu_18_step_01_input/CU_18_05_16_distritos_variables.csv")

## Valores perdidos que tendría que haber solucionado en sus
## notebooks pero lo hago aquí

ids <- df |> drop_na(cmun) |> select(1:2)
df |> filter(is.na(cmun))
df <- df |> 
  drop_na(cmun) |> 
  select(-c("cmun", "cdis", "X", "Y"))

## ¿hay perdidos?

df |> 
  map_dbl(~sum(is.na(.x))) |> 
  sum()

## ¿hay varianza cero?
df |> 
  map_dbl(~var(.x, na.rm = TRUE) == 0) |> 
  sum()



# df_complete <- df |>
#   drop_na()

## Imputo por KNN

rec <- recipe(
  ~ .,
  data = df
)

impute_recipe <- rec |>
  step_impute_knn(all_predictors(), neighbors = 3)
impute_recipe2 <- prep(impute_recipe, training = df)
df_imputed <- bake(impute_recipe2, df)

df_imputed |> 
  map_dbl(~sum(is.na(.x))) |> 
  sum()


## Cluster ----

# https://bradleyboehmke.github.io/HOML/model-clustering.html

library(mclust)

dfz <- scale(df_imputed)

df_mc <- Mclust(dfz)

summary(df_mc)

plot(df_mc, what = "density", dimens = 1:4)
plot(df_mc, what = "BIC")



# library(ClusterR)
# 
# opt_gmm = Optimal_Clusters_GMM(dfz, max_clusters = 10, criterion = "BIC", 
#                                
#                                dist_mode = "maha_dist", seed_mode = "random_subset",
#                                
#                                km_iter = 10, em_iter = 10, var_floor = 1e-10, 
#                                
#                                plot_data = T)

# res = external_validation(dietary_survey_IBS$class, pr$cluster_labels, 
#                           
#                           method = "adjusted_rand_index", summary_stats = T)
# 
# res

gmm <- GMM(dfz, 3, dist_mode = "maha_dist", seed_mode = "random_subset", km_iter = 10,
          em_iter = 10, verbose = F)   


## PCA para visuals

dfpca <- PCA(df)
dfpca$ind$coord
summary(dfpca)

dfout <- ids |> 
  bind_cols(dfpca$ind$coord,
            cluster = factor(df_mc$classification),
            df)

library(ggplot2)
p1 <- dfout |> ggplot(aes(x = Dim.1, y = Dim.2, col = cluster)) +
  geom_point(alpha = 0.5)
p2 <- dfout |> ggplot(aes(x = Dim.2, y = Dim.3, col = cluster)) +
  geom_point(alpha = 0.5)
p3 <- dfout |> ggplot(aes(x = Dim.1, y = Dim.3, col = cluster)) +
  geom_point(alpha = 0.5)

library(gridExtra)
grid.arrange(p1, p2, p3)
