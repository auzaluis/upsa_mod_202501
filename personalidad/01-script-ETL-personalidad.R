
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



# Temas 02: Transformación de datos ----

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

























