# install.packages("ggcorrplot")
# install.packages('FactoMineR')
library(tidyverse)
library(plotly)
library(ggcorrplot)
library(FactoMineR)

frases3 <- df6[,9:32] |> colnames()

# Matriz de correlaciones
r <- cor(
  df6 |> dplyr::select(all_of(frases3)),
  method = "spearman"
)

# Gráfica

ggplotly(
  ggcorrplot(
    corr = r,
    show.legend = F,
    tl.cex = 6,
    colors = c("red", "white", "blue")
  ) +
  theme(
    axis.text.x = element_blank(),
    panel.grid = element_blank()
  )
)


# PCA: Principal Component Analysis

## Dimensión: Materialismo

materialismo <- frases3[c(2,10,16)]

PCA_materialismo <- FactoMineR::PCA(
  df6 |> select(all_of(materialismo)),
  ncp = 1
)

PCA_materialismo$eig
PCA_materialismo$var$cor
PCA_materialismo$ind$coord

tibble(
  Dim = PCA_materialismo$ind$coord,
  df6 |> select(all_of(materialismo))
)


tradicion <- frases3[c(9,12,14)]
exito <- frases3[c(4,5,19)]
resp_social <- frases3[c(1,3,18)]
social <- frases3[c(20,23)]
introversion <- frases3[c(21,22)]

# Usando bucles

dim_list <- list(
  materialismo = frases3[c(2,10,16)],
  tradicion = frases3[c(9,12,14)],
  exito = frases3[c(4,5,19)],
  resp_social = frases3[c(1,3,18)],
  social = frases3[c(20,23)],
  introversion = frases3[c(21,22)]
)

PCA <- list()

for (dim in names(dim_list)) {
  columnas <- dim_list[[dim]]
  PCA[[dim]] <- FactoMineR::PCA(
    df6 |> select(all_of(columnas)),
    ncp = 1
  )
}

PCA$introversion$eig
PCA$introversion$var$cor

tibble(
  Dim = PCA$introversion$ind$coord,
  df6 |> select(all_of(introversion))
)

# Dta frame con las dimensiones

df10 <- df6 |> 
  mutate(
    materialismo = PCA$materialismo$ind$coord,
    tradicion    = PCA$tradicion$ind$coord *-1,
    exito        = PCA$exito$ind$coord *-1,
    resp_social  = PCA$resp_social$ind$coord,
    social       = PCA$social$ind$coord *-1,
    introversion = PCA$introversion$ind$coord *-1
  )







