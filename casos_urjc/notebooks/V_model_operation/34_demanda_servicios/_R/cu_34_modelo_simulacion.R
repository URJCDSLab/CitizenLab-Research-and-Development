## simulación clusters

library(readr)
library(purrr)
library(tibble)

escenario <- read_csv("cu_34_step_01_input/ESCENARIO_SERVICIOS.csv")

pfutbol <- sum(escenario$Futbol == 1) / nrow(escenario)
rate_nservicios <- mean(escenario$nservicios)
rate_capacidad <- mean(escenario$capacidad)
m_cont <- apply(escenario[,4:25], 2, mean)
s_cont <- apply(escenario[,4:25], 2, sd)

nsim <- 1000

mat <- matrix(rep(NA_real_, nsim*ncol(escenario)), ncol = ncol(escenario))

mat[, 1] <- rbinom(nsim, size = 1, prob = pfutbol)
mat[, 2:3] <- sapply(c(rate_nservicios, rate_capacidad),
                     function(x) rpois(nsim, lambda = x))

mat[, 4:25] <- sapply((4:25) - 3,
                     function(x) rnorm(nsim, mean = m_cont[x], sd = s_cont[x]))

colnames(mat) <- colnames(escenario)

simulacion <- as_tibble(mat)

