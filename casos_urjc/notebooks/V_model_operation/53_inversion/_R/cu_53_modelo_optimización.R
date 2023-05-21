
library(readr)
library(glmnet)
library(lpSolve)

modelo <- read_rds("cu_53_step_04_output/modelo_reg.rds")

cc <- coef(modelo)

newyear <- 2023
limite_total <- 90
minimo_infra <- 50
minimo_tur <- 1
minimo_sanidad <- 2

b0 <- cc[1,1] + newyear*cc[2,1]
f.obj <- cc[3:5]
f.con <- matrix(c(1, 1, 1,
                  1, 0, 0,
                  0, 1, 0,
                  0, 0, 1), nrow = 4, byrow = TRUE) 
f.dir <- c("<=",
           ">=",
           ">=",
           ">=")
f.rhs <- c(limite_total, minimo_infra, minimo_tur, minimo_sanidad)
res <- lp("max", f.obj, f.con, f.dir, f.rhs)

## Resultado optimización, valor óptimo:
res$objval + b0

## Resultado optimización, inversiones necesarias:
data.frame(grupo = rownames(cc)[3:5], 
           porc_inv = res$solution)

