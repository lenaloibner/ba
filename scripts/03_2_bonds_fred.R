library(tidyverse)
library(quantmod)
library(lubridate)

output_path <- "outputs"

fred_codes <- tribble(
  ~country, ~code,
  "CH",     "IRLTLT01CHM156N", # Schweiz
  "NO",     "IRLTLT01NOM156N", # Norwegen
  "CA",     "IRLTLT01CAM156N", # Kanada
  "JP",     "IRLTLT01JPM156N", # Japan
  "CZ",     "IRLTLT01CZM156N", # Tschechien
  "CO",     "IRLTLT01COM156N", # Kolumbien
  "TR",     "IRLTLT01TRM156N", # Türkei
  "PK",     "INTGSTPKM193N",    # Pakistan
  "EG",     "INTGSTEGM193N",    # Ägypten
  "PH",     "INTGSTPHM193N"     # Philippinen
)

# Download fred
safe_get_fred <- function(code) {
  out <- tryCatch({

    data_xts <- getSymbols(code, src = "FRED", auto.assign = FALSE)
    return(data_xts)
  }, error = function(e) {
    message(paste("⚠️ Fehler bei Code:", code))
    return(NULL)
  })
  return(out)
}

# Daten abrufen, transformieren und filtern
fred_data <- fred_codes %>%
  mutate(raw_data = map(code, safe_get_fred)) %>%
  
  # Zeilen ohne Daten entfernen
  filter(!map_lgl(raw_data, is.null)) %>%
  mutate(tidied_data = map(raw_data, ~{
    df <- as.data.frame(.x)
    tibble(
      date = as.Date(rownames(df)),
      yield = as.numeric(df[[1]])
    )
  })) %>%
  select(country, tidied_data) %>%
  unnest(tidied_data) %>%
  
  # eingrenzen (2013-2023)
  filter(date >= as.Date("2013-01-01"), date <= as.Date("2023-12-31")) %>%
  
  # Auf Monatsebene normieren
  mutate(date = floor_date(date, "month")) %>%
  group_by(country, date) %>%
  summarize(yield = mean(yield, na.rm = TRUE), .groups = "drop") %>%
  
  # 2 Dezimalstellen
  mutate(yield = round(yield, 2))

write_csv(fred_data, file.path(output_path, "bond_yields_fred.csv"))

print(head(fred_data, 10))

print(unique(fred_data$country))