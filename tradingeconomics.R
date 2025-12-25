# Funciones para obtener datos de trading economics

# devtools::install_github("tradingeconomics/tradingeconomics/R/tradingeconomics")
library(tradingeconomics)

login("69a2c2a053a544e:uylny62mglro48t")



getHistoricalData(country = 'united kingdom', indicator = 'imports')

get_te_data <- function(ind){
  login('69a2c2a053a544e:uylny62mglro48t')
  world_ind <- getIndicatorData(country = "all", indicator = ind, outType = 'df')
  world_ind$Country <- as.character(world_ind$Country)
  world_ind[world_ind == "United States"] <- "USA"
  world_ind[world_ind == "United Kingdom"] <- "UK"
  merge_dfs(world_ind)
}

# Documentación -----------------------------------------------------------
# https://github.com/tradingeconomics/tradingeconomics/tree/master/R

