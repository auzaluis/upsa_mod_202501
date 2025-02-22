
# Tema 01: Carga de datos ----

## Carga local
df <- read.csv(file = "personalidad/Personalidad (respuestas).csv",
               check.names = FALSE)
colnames(df)

## Carga conexión API (en linea)
# install.packages("gsheet")
library(gsheet)

url_google <- "https://docs.google.com/spreadsheets/d/1IQ_RxxTSmBKHTExlxboIRNlMov_F6RyqdcOPrflCv_w/edit?pli=1&gid=1681482327#gid=1681482327"

df <- read.csv(text = gsheet2text(url_google),
               check.names = F)


## Estructura de un data frame
class(df)
class(df$`Escribe tu edad exacta`)
class(df$Sexo)
nrow(df) # Cantidad de filas
ncol(df) # cantidad de columnas



# Tema 02: Transformación de datos ----

## Valores perdidos (NA) ----
# Los NAs pueden ser tratados de 2 maneras
# 1. Reemplazo (imputación)
# 2. Eliminar o ignorar

df$`Escribe tu edad exacta`
is.na(df$`Escribe tu edad exacta`)
summary(is.na(df$`Escribe tu edad exacta`))

# Conociendo el pipe |>
df$`Escribe tu edad exacta` |> 
  is.na() |> 
  summary()


### Imputación (reemplazo por la media)

mean(df$`Escribe tu edad exacta`)
mean(df$`Escribe tu edad exacta`, na.rm = T)

ifelse(test = df$`Escribe tu edad exacta` |> is.na(),
       yes = mean(df$`Escribe tu edad exacta`, na.rm = T),
       no = df$`Escribe tu edad exacta`)


# Esta es la librería más importante para transformar datos
# install.packages("tidyverse")
library(tidyverse)

df2 <- df |>
  mutate(edad2 = ifelse(
    test = df$`Escribe tu edad exacta` |> is.na(),
    yes = mean(df$`Escribe tu edad exacta`, na.rm = T),
    no = df$`Escribe tu edad exacta`)
  ) |> 
  relocate(edad2, .after = `Escribe tu edad exacta`)

### Eliminar toda la fila
df2 <- df2 |> na.omit()


## Estandarización de variables ----

### Normalización
scale(df2$`Escribe tu edad exacta`)
mean(df2$`Escribe tu edad exacta`)

data.frame(
  original = df2$`Escribe tu edad exacta`,
  normalizada = scale(df2$`Escribe tu edad exacta`)
)

df3 <- df2 |> 
  mutate(edadZ = scale(`Escribe tu edad exacta`)) |> 
  relocate(edadZ, .after = edad2)



### Rango (0-1)
library(scales)

df3 <- df3 |> 
  mutate(edadR = rescale(`Escribe tu edad exacta`)) |> 
  relocate(edadR, .after = edadZ)

## Agrupaciones ----

### Rangos numéricos
cut(df3$`Escribe tu edad exacta`,
    breaks = c(-Inf, 18, 21, Inf),
    labels = c("18 o menos", "19 a 21", "Más de 21"))

df4 <- df3 |> 
  mutate(edadGR = cut(
    `Escribe tu edad exacta`,
    breaks = c(-Inf, 18, 21, Inf),
    labels = c("18 o menos", "19 a 21", "Más de 21"))
  ) |> 
  relocate(edadGR, .after = edadR)


### Categorías
unique(df4$`Según tu forma de ser ¿Cuál de las siguientes frases te describe mejor: [No discrimino y trato a todos por igual]`)
colnames(df4)
unique(df4[,9])
summary(factor(df4[,9]))

# usando ifelse, convertir la df4[,9] en 1, cuando responde 
# Un poco verdadero o Totalmente verdadero, caso contrario 0
ifelse(
  test = df4[,9] == "Totalmente verdadero" | df4[,9] == "Un poco verdadero",
  yes = 1,
  no = 0
)

# Bucles (loops)

## Paso 1: Crear un vector que contenga los nombres
## de las columnas que quiero trabajar

# df4[,9:32] |> colnames()
frases <- df4 |> select(starts_with("Según tu forma de ser")) |> colnames()

## Paso 2: Ejecutar el buble
df5 <- df4

for (frase in frases) {
  df5[,frase] <- ifelse(
    test = df4[,frase] == "Un poco verdadero" | df4[,frase] == "Totalmente verdadero",
    yes = 1,
    no = 0
  )
}


# Tema 03: Manipulación de datos ----

# Convertir el data frame en un tibble
df5 <- df5 |> as_tibble()

## Selección de columnas ----
df5 |> select(Sexo)
df5 |> select(Sexo, `Escribe tu edad exacta`)
df5 |> select(`Marca temporal`:Sexo)
df5 |> select(-`Marca temporal`)
df5 |> select(starts_with("edad"))
df5 |> select(contains("edad"))
df5 |> select(ends_with("00"))

## Filtrado de filas ----
df5 |> select(Sexo) |> filter(Sexo == "Mujer")
df5 |> select(Sexo) |> filter(Sexo != "Mujer")

df5 |>
  select(Sexo, `Escribe tu edad exacta`) |>
  filter(`Escribe tu edad exacta` > 21)

df5 |>
  select(Sexo, `Escribe tu edad exacta`) |>
  filter(`Escribe tu edad exacta` <= 21)

df5 |> 
  select(Sexo, `Escribe tu edad exacta`) |>
  filter(`Escribe tu edad exacta` >= 18 & `Escribe tu edad exacta` <= 21)

df5 |> 
  select(Sexo, `Escribe tu edad exacta`) |>
  filter(`Escribe tu edad exacta` >= 18,
         `Escribe tu edad exacta` <= 21)

df5 |> 
  select(Sexo, `Escribe tu edad exacta`) |>
  filter(between(`Escribe tu edad exacta`, 18, 21))

df5 |> 
  select(Sexo, `Escribe tu edad exacta`) |>
  filter(`Escribe tu edad exacta` %in% 18:21)

df5 |> 
  select(Sexo, `Escribe tu edad exacta`) |>
  filter(`Escribe tu edad exacta` %in% 18:21,
         Sexo == "Mujer")


## Nombres de columnas

### Apps

df6 <- df5

#### Paso 1: Crear un vector con los nuevos nombres
apps <- c("TikTok", "Instagram", "Facebook", "YouTube")

#### Paso 2: Reemplazo
colnames(df6)[34:37] <- apps
colnames(df6)


### Frases

#### Paso 1: Crear un vector con los nuevos nombres

frases2 <- frases |> 
  as_tibble() |> 
  separate(col = value,
           into = c("value", "frases"),
           sep = "\\[") |> 
  separate(col = frases,
           into = c("frases", "jaja"),
           sep = "\\]") |> 
  select(frases) |> 
  as_vector()

### Paso 2: Reemplazo
colnames(df6)[9:32] <- frases2


## Pivot

### Longer
df7 <- df6 |> 
  pivot_longer(
    cols = all_of(apps),
    names_to = "app",
    values_to = "time"
  )

df8 <- df7 |> 
  pivot_wider(names_from = app,
              values_from = time)



# Tema 04: Outliers ----

## De horas a número
df7$time |> class()
strsplit("03:45:00", split = ":")

## Transformación
df7$time <- sapply(
  strsplit(df7$time, split = ":"),
  function(x) {
    x <- as.numeric(x)
    x[1] + x[2]/60 + x[3]/60^2
  }
)

df7$time


## Detección gráfica

# Boxplot feo
boxplot(df7$time)

# Boxplot bonito :)
install.packages("plotly")
library(plotly)

ggplotly(
  df7 |> 
    ggplot(aes(x = app, y = time, fill = app)) +
    geom_boxplot() +
    theme_minimal() +
    labs(x = "", y = "Promedio de horas a la semana") +
    theme(legend.position = "none")
)


## Tarea: Añadir al boxplot la variable edad y sexo





















