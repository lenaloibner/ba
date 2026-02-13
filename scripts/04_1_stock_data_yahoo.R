library(quantmod)
library(tidyverse)

# ISO-Namen definieren
tickers <- c(CH="^SSMI", NO="^OSEBX", CA="^GSPTSE", JP="^N225", CZ="^PX", 
             CO="^COLCAP", TR="XU100.IS", PK="^KSE100", EG="^EGX30", PK="PSEI.PS")

# transformieren
imap_dfr(tickers, ~ {
  d <- try(getSymbols(.x, from="2013-01-01", to="2023-12-31", auto.assign=F))
  if(inherits(d, "try-error")) return(NULL)
  
  m <- to.monthly(d, indexAt="firstof")
  tibble(country=.y, date=as.Date(index(m)), close=as.numeric(Cl(m)))
}) %>% 
  write_csv("outputs/stock_data_yahoo.csv")

