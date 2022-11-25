
rm(list = ls())

{if(!require(tidyverse)) install.packages('tidyverse')
  require(tidyverse)}
{if(!require(skimr)) install.packages('skimr')
  require(skimr)}
{if(!require(GGally)) install.packages('GGally')
  require(GGally)}
{if(!require(Metrics)) install.packages('Metrics')
  require(Metrics)}

# Iportando dados https://www.kaggle.com/datasets/aariyan101/usa-housingcsv
USAhouse <- read.csv2(file.choose(), sep = ',', dec = '.') # USA_Housing.csv

# Selecionando as variaves de interesse
bd <- USAhouse %>% dplyr::select(Avg..Area.Income, Avg..Area.House.Age, Avg..Area.Number.of.Rooms, Avg..Area.Number.of.Bedrooms, Area.Population, Price) 

# Resumo dos dados 
bd %>% skimr::skim()

# Matriz de correlacao
bd %>% GGally::ggpairs()

# Construcao do modelo
md <- lm(Price ~ ., data = bd)

# Resumo do modelo
summary(md)

# Analise de Variancia do modelo
anova(md)

# Analise dos residuos do modelo
par(mfrow = c(2,2),mai=c(0.8, 0.8, 0.2, 0.5))
plot(md)

# Comparacao entre valor real e previsto
par(mfrow = c(1,1),mai=c(0.8, 0.8, 0.2, 0.5))
plot(bd$Price,md$fitted.values, xlab = 'Price', ylab = 'Fitted')

# Parametros de avaliacao do modelo
Metrics::mae(bd$Price,fitted(md))
Metrics::mse(bd$Price,fitted(md))
Metrics::rmse(bd$Price,fitted(md))
Metrics::mape(bd$Price,fitted(md))
