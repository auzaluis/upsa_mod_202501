
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



# Temas 02: Transformación de datos

## Valores perdidos (NA)
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
install.packages("tidyverse")
library(tidyverse)

df2 <- df |>
  mutate(edad2 = ifelse(
    test = df$`Escribe tu edad exacta` |> is.na(),
    yes = mean(df$`Escribe tu edad exacta`, na.rm = T),
    no = df$`Escribe tu edad exacta`)
  ) |> 
  relocate(edad2, .after = `Escribe tu edad exacta`)














