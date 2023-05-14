library(readr)
library(dplyr)
library(tidyr)
library(cluster)
library(recipes)
library(janitor)
library(purrr)
library(FactoMineR)
library(mclust)

setwd("~/academico/gh_repos/__transferencia/citizenlab/CitizenLab-Research-and-Development/casos_urjc/notebooks/V_model_operation/18_infraestructuras/")

NIVEL <- "Diario"

if (NIVEL == "Distrito"){
  
  
  
  ## DISTRITOS ----
  
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
  
  
  
  # dfz <- scale(df_imputed)
  
  df_mc <- Mclust(df_imputed)
  
  summary(df_mc)
  
  # plot(df_mc, what = "density", dimens = 1:4)
  # plot(df_mc, what = "BIC")
  
  
  
  ## PCA para visuals
  
  dfpca <- PCA(df_imputed, graph = FALSE)
  # dfpca$ind$coord
  # summary(dfpca)
  
  dfout <- ids |> 
    bind_cols(dfpca$ind$coord,
              cluster = factor(df_mc$classification),
              df_imputed)
  
  
  write_rds(dfout, "cu_18_maestros/datos_cluster_distritos.rds")
  write_rds(df_mc, "cu_18_maestros/modelo_cluster_distritos.rds")
  
} else if (NIVEL == "Diario"){
  
  
  
  ## DIARIO ----
  
  df <- read_csv("cu_18_step_01_input/CU_18_05_20_diario_infra.csv")
  
  ## Valores perdidos que tendría que haber solucionado en sus
  ## notebooks pero lo hago aquí
  
  
  ## ¿hay perdidos?
  
  df |> 
    map_dbl(~sum(is.na(.x))) |> 
    sum()
  
  ## ¿hay varianza cero?
  df |> 
    map_dbl(~var(.x, na.rm = TRUE) == 0) |> 
    sum()
  
  ## Quitamos perdidos ya que la imputación no termina
  
  df_complete <- df |>
    drop_na()
  
  ids <- df_complete |> select(1:2)
  df_complete <- df_complete |> 
    select(-c(1:2))
  
  

  ## Cluster ----
  
  # https://bradleyboehmke.github.io/HOML/model-clustering.html
  
  
  
  # dfz <- scale(df_imputed)
  
  df_mc <- Mclust(df_complete)
  
  summary(df_mc)
  
  # plot(df_mc, what = "density", dimens = 1:4)
  # plot(df_mc, what = "BIC")
  
  
  
  ## PCA para visuals
  
  dfpca <- PCA(df_complete, graph = FALSE)
  # dfpca$ind$coord
  # summary(dfpca)
  
  dfout <- ids |> 
    bind_cols(dfpca$ind$coord,
              cluster = factor(df_mc$classification),
              df_complete)
  
  
  write_rds(dfout, "cu_18_maestros/datos_cluster_diario.rds")
  write_rds(df_mc, "cu_18_maestros/modelo_cluster_diario.rds")
  
}
