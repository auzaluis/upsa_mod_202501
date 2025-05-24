
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


# Análisis descriptivo

itemFrequencyPlot(
  x = list_to_basket,
  topN = 20,
  horiz = T
)

# Reglas ----

rules <- apriori(
  data = list_to_basket,
  parameter = list(
    supp = 0.0001,
    conf = 0.15,
    minlen = 2,
    maxlen = 2
  )
)


inspect(rules)


plot(
  rules,
  method = "graph",
  engine = "htmlwidget"
)


# Fijar la mano derecha

rules_rhs <- apriori(
  data = list_to_basket,
  parameter = list(
    supp = 0.001,
    conf = 0.07,
    minlen = 2,
    maxlen = 2
  ),
  appearance = list(
    rhs = "sausage",
    default = "lhs"
  )
)


inspect(rules_rhs)

# Fijar la mano izquierda

rules_lhs <- apriori(
  data = list_to_basket,
  parameter = list(
    supp = 0.001,
    conf = 0.05,
    minlen = 2,
    maxlen = 3
  ),
  appearance = list(
    lhs = "whole milk",
    default = "rhs"
  )
)

inspect(rules_lhs)











