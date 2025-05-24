
library(gsheet)
library(tidyverse)
library(arules)
library(arulesViz)
library(viridis)

# Importar la base de datos
url <- "https://docs.google.com/spreadsheets/d/1NTjA8nrmcWltvZn4oq5KJK7-R4Mb-_is_5vVsoBDCv0/edit?usp=sharing"

df <- read.csv(
  text = gsheet2text(url, format = "csv"),
  stringsAsFactors = F
)

# basket_id
## id canasta
df2 <- df |> 
  mutate(basket_id = paste(Member_number, Date, sep = "_")) |> 
  select(basket_id, itemDescription)


## Convirtiendo a lista
df_to_list <- split(
  x = df2$itemDescription,
  f = df2$basket_id
)


## Convirtiendo a canasta
list_to_basket <- as(
  object = df_to_list,
  Class = "transactions"
)

class(list_to_basket)
