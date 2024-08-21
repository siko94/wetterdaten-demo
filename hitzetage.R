# Dieses Skript lädt tägliche, historische Wetterdaten für eine Wetterstation 
# vom OpenData-Server des Deutschen Wetterdienstes (DWD) herunter
# https://opendata.dwd.de/climate_environment/CDC/observations_germany/climate/daily/kl/
# berechnet für jedes Jahr seit Aufzeichnungsbeginn die Zahl der Hitzetage 
# mit 30 Grad oder mehr und erstellt daraus ein Datawrapper-Diagramm

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
station_id <- "2014" # Beispiel: Wetterstation Hannover-Flughafen, aktiv seit 1936

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

# Prüfen, wie viele Fehlwerte in den Daten auftauchen
check_na <- station_data %>% 
  mutate(check = is.na(t_max)) %>% 
  count(check) 

print(check_na)

###############################################
# 3. Daten analysieren

heat_days <- station_data %>% 
  mutate(    
    # Jahr in neue Spalte extrahieren für spätere Gruppierung
    year = lubridate::year(date),
    # Hilfsspalte mit Prüfung, ob ein Tag mind. 30 Grad hatte
    heat_day = t_max >= 30
  ) %>% 
  # nach Jahr gruppieren
  group_by(year) %>% 
  # SUmme der Hitzetage pro Jahr berechnen
  summarise(heat_days = sum(heat_day))

glimpse(heat_days)

###############################################
# 4. Mit Datawrapper visualisieren

# Chart erstellen - Achtung: setzt hinterlegten API-Key voraus
chart <- dw_create_chart(
  # Überschrift des Diagramms
  title = "So hat sich die Zahl der Hitzetage entwickelt",
  # Typ: Liniendiagramm
  type = "column-chart"
)

# Daten an das Diagramm schicken
dw_data_to_chart(heat_days, chart_id = chart)

# Darstellung, Beschriftung etc. bearbeiten
dw_edit_chart(
  chart_id = chart,
  intro = "Hitzetag = Tag mit mindestens 30 Grad",
  annotate = "Wetterstation Hannover-Flughafen",
  source_name = "DWD",
  source_url = "https://opendata.dwd.de/climate_environment/CDC/observations_germany/climate/daily/kl/",
  visualize = list(
    `x-grid` = "off",
    `x-grid-format` = "YYYY",
    `y-grid` = "on",
    valueLabels = list(
      show = "hover"
    )
  )
)

# Veröffentlichen und URL erhalten
dw_publish_chart(chart_id = chart)