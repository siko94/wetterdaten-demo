# Dieses Skript lädt tägliche, historische Wetterdaten für eine Wetterstation 
# vom OpenData-Server des Deutschen Wetterdienstes (DWD) herunter
# https://opendata.dwd.de/climate_environment/CDC/observations_germany/climate/daily/kl/
# und berechnet daraus für eine bestimmte Zahl an aktuellen Tagen einen Normalbereich

# Normalbereich: 20. bis 80. Perzentil des Vergleichszeitraums (30 Jahre, hier: 1961-1990)


###############################################
# 0. Packages laden und bei Bedarf installieren
if (!require(needs)) install.packages("needs")
needs(
  tidyverse,
  rdwd, # Package, mit dem der DWD-Server direkt angesprochen wird
  janitor, # Hilfsfunktionen, um z.B. Spaltennamen zu säubern
  lubridate,
  DatawRappr # Wichtig: Package muss vorab von Github installiert und ein API-Token hinterlegt werden: https://github.com/munichrocker/DatawRappr/tree/v1.2
)

###############################################
# 1. Daten laden

# ID der gewünschten Wetterstation (Quelle: https://opendata.dwd.de/climate_environment/CDC/observations_germany/climate/daily/kl/historical/KL_Tageswerte_Beschreibung_Stationen.txt)
station_id <- "04177" # Beispiel: Wetterstation Rheinstetten bei KA, aktiv seit 1936

# gewünschte Wetterdaten auswählen
dwd_link <- selectDWD(
  id = station_id, 
  res = "daily", # Tageswerte
  var = "kl", # Klimadaten (Temperaturen, Niederschlag, etc.)
  per = "hr", # historische und aktuelle Daten gemeinsam
  current = TRUE
)
# ... und herunterladen (es entsteht eine Liste aus zwei Dataframes)
download <- dataDWD(dwd_link, force = TRUE, overwrite = TRUE, read = TRUE, varnames = TRUE)

###############################################
# 2. Daten bereinigen

# gemeinsamer Dataframe aus historischen und aktuellen Daten
station_data <- bind_rows(
  download[[1]],
  # aktuelle Daten filtern: nur ab dem Ende der historischen Daten
  download[[2]] %>% 
    filter(MESS_DATUM > max(download[[1]]$MESS_DATUM))
) %>% 
  # Spaltennamen lesbarer machen (janitor-Package)
  janitor::clean_names() %>% 
  # gewünschte Spalten auswählen und gleichzeitig umbenennen
  select(
    date = mess_datum,
    t_max = txk_lufttemperatur_max
  ) %>% 
  mutate(
    # Fehlwerte (in den DWD-Rohdaten als -999) in NAs umwandeln
    t_max = if_else(t_max == -999, true = NA, false = t_max)
  )

# Erster Blick in die Daten
glimpse(station_data)


###############################################
# 3. Daten analysieren

# Funktion für Normalbereich: braucht dataframe, Startjahr des 
#Vergleichszeitraums und die Anzahl Tage, die betrachtet werden sollen
tempWithReferenceRange <- function(data, min_year, hist_range) {
  
  # Tage, Monate, Jahre separieren
  data <- data %>%
    select(date, t_max) %>% 
    mutate(year = year(date),
           month = month(date),
           day = day(date))
  
  # Daten der letzten Tage separieren
  current_data <- data %>% 
    arrange(date) %>% 
    tail(n = hist_range)
  
  # historische Daten auf 30-Jahres-Vergleichszeitraum filtern
  historical_data <- data %>% 
    filter(year >= min_year & year < (min_year + 30))
  
  # Normalbereich berechnen
  reference_data <- current_data %>% 
    mutate(
      # neue Spalte, für jede Zeile 20. und 80. Perzentil
      ref = map2(
        # jeweils Monat und Tag der aktuellen Zeile verwenden
        .x = month,
        .y = day,
        .f = function(x, y) {
          historical_data %>%
            # historische Vergleichsdaten auf selbe Tag/Monat-Kombination filtern
            filter(month == x & day == y) %>% 
            # Mittelwert und Perzentile berechnen
            summarise(max_20 = quantile(t_max, probs = 0.2, names = FALSE, na.rm = TRUE),
                      max_80 = quantile(t_max, probs = 0.8, names = FALSE, na.rm = TRUE))
        }
      )
    ) %>% 
    # in getrennte Spalten bringen
    unnest_wider(ref)
  
  return(reference_data)
}

# Funktion ausführen: Basisjahr 1961, 30 Tage zurückliegend
temp_range <- tempWithReferenceRange(data = station_data, 
                                     min_year = 1961, 
                                     hist_range = 30)

# Daten inspizieren
glimpse(temp_range)

###############################################
# 4. Mit Datawrapper visualisieren

# Chart erstellen - Achtung: setzt hinterlegten API-Key voraus
chart <- dw_create_chart(
  # Überschrift des Diagramms
  title = "So warm ist es aktuell - im historischen Vergleich",
  # Typ: Liniendiagramm
  type = "d3-lines"
)

# relevante Spalten wählen und Daten an das Diagramm schicken
dw_data_to_chart(
  temp_range %>% 
    select(date, t_max, max_20, max_80), 
  chart_id = chart
)


# Darstellung, Beschriftung etc. bearbeiten
dw_edit_chart(
  chart_id = chart,
  intro = "Aktuelle Temperaturen im Vergleich zu dem, was 1961-1990 normal war",
  annotate = "Wetterstation Rheinstetten",
  source_name = "DWD",
  source_url = "https://opendata.dwd.de/climate_environment/CDC/observations_germany/climate/daily/kl/",
  visualize = list(
    lines = list(
      t_max = list(
        directLabel = FALSE
      ),
      max_20 = list(
        directLabel = FALSE,
        dash = "style2",
        width = "style0"
      ),
      max_80 = list(
        directLabel = FALSE,
        dash = "style2",
        width = "style0"
      )
    ),
    `color-category` = list(
      map = list(
        t_max = "#da373f",
        max_20 = "#858585",
        max_80 = "#858585"
      )
    ),
    `custom-area-fills` = list(
      list(
        from = "max_80",
        to = "max_20",
        color = "#cccccc",
        opacity = 0.3
      )
    )
  )
)

# Veröffentlichen und URL erhalten
dw_publish_chart(chart_id = chart)