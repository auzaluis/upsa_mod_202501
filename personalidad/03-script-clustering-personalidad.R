library(NbClust)
library(FactoMineR)

# Creando df11

df11 <-
  df7 |>
  pivot_wider(
    names_from = app,
    values_from = time
  ) |> 
  select(
    `Marca temporal`,
    `Escribe tu edad exacta`,
    Sexo,
    apps
  ) |> 
  left_join(
    df10 |> 
      select(
        `Marca temporal`,
        `Escribe tu edad exacta`,
        Sexo,
        dimensiones
      ),
    by = join_by(
      `Marca temporal`,
      `Escribe tu edad exacta`,
      Sexo
    )
  ) |> 
  mutate(
    Sexo = ifelse(
      test = Sexo == 'Mujer',
      yes = 1,
      no = 0
    )
  ) |> 
  mutate_if(
    is.numeric,
    scale
  )


# Iteración

clustering <- NbClust(
  data = df11 |> 
    select(
      Sexo,
      dimensiones,
      apps
    ),
  distance = "euclidean",
  method = "ward.D2",
  index = "dunn"
)

clustering

# Tamaño de cada segmento
table(clustering$Best.partition)
prop.table(table(clustering$Best.partition))*100





df13 <- df11 |> 
  mutate(segmento = clustering$Best.partition)





