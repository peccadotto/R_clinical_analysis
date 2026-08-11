# Setup

  packages <- c(
    "tidyverse",
    "formattable",
    "janitor",
    "lubridate",
    "quarto",
    "scales",
    "writexl"
  )

  for (pkg in packages) {
    if (!requireNamespace(pkg, quietly = TRUE)) {
      message("!! Package '", pkg, "' not found. Installing it...")
      tryCatch(
        install.packages(pkg, dependencies = TRUE),
        error = function(e) {
          message("!! Error while trying to install the '", pkg, "' package: ", e$message)
        }
      )
    }
    suppressPackageStartupMessages(library(pkg, character.only = TRUE))
  }
  
  rm(list = ls())
  as_euro <- function (x) {currency(x, symbol = "€", big.mark = ".", decimal.mark = ",", sep = " ")}
  Sys.setenv(QUARTO_PATH = "C:\\Program Files\\RStudio\\resources\\app\\bin\\quarto\\bin\\quarto.exe")
  
# Importa dati
  
  df_camozzi <- read.csv("data/camozzi.csv", sep = ";", header = TRUE)     |> mutate(Sede = "CAM")
  df_humanitas <- read.csv("data/humanitas.csv", sep = ";", header = TRUE)   |> mutate(Sede = "ICH")
  df_medcare  <- read.csv("data/medcare.csv", sep = ";", header = TRUE)     |> mutate(Sede = "HMC")
  df_modoetia <- read.csv("data/modoetia.csv", sep = ";", header = TRUE)    |> mutate(Sede = "MOD")
  df_policlinico <- read.csv("data/policlinico.csv", sep = ";", header = TRUE) |> mutate(Sede = "POL")
  df_levante <- read.csv("data/salerno.csv", sep = ";", header = TRUE)     |> mutate(Sede = "SAL")
  df_zucchi <- read.csv("data/zucchi.csv", sep = ";", header = TRUE)      |> mutate(Sede = "ICZ")

# Unisci dati
  
  nomi_df <- mget(ls(pattern = "^df_")); df <- bind_rows(nomi_df)
  df <- df |>
    mutate(
    Data = dmy(Data),
    Ora = hm(Ora),
    Prestazione = factor(Prestazione, levels = c("VISORT01", "VISORT02", "ECTBAC", "MISC")),
    Netto = parse_number(Netto, locale = locale(decimal_mark = ",", grouping_mark = "."))/100,
    Anno = year(Data),
    Mese = month(Data)
  ) |>
    # Raddoppia le righe con ECTBAC (per contare il n° di ecografie e non i singoli pz)
    mutate(moltiplicatore = if_else(stringr::str_trim(Prestazione) == "ECTBAC", 2, 1)) |> 
    uncount(moltiplicatore) |>
    # Correggi il costo della singola ecografia
    mutate(Netto = ifelse(Prestazione == "ECTBAC", Netto / 2, Netto)) |>
    arrange(Data, Ora)

# Crea report

  df_pvt_sede <- df |>
    group_by(Sede) |>
    summarise(
      N = n(),
      Inizio = min(Data),
      Fine = max(Data),
      Durata = sub(" 0H 0M 0S$", "", as.period(Inizio %--% Fine)),
      N_yr = if_else(time_length(Inizio %--% Fine, unit = "years") >= 1, as.integer(N / time_length(Inizio %--% Fine, unit = "years")), 0),
      EUR = sum(Netto, na.rm = TRUE),
      EUR_yr = if_else(time_length(Inizio %--% Fine, unit = "years") >= 1, EUR / time_length(Inizio %--% Fine, unit = "years"), 0),
      EUR_pz = sum(Netto[Netto > 0], na.rm = TRUE) / sum(Netto > 0, na.rm = TRUE)
    ) |>
    mutate(
      EUR = as_euro(EUR),
      EUR_yr = as_euro(EUR_yr),
      EUR_pz = as_euro(EUR_pz)
    ) |>
    relocate (N, .after = Durata) |>
    arrange(Inizio) |>
    adorn_totals(where = "row", fill = "", na.rm = TRUE, name = "Totale", columns = c("N", "EUR")) |>
    rename(
      'N/anno' = N_yr,
      Fatturato = EUR,
      '€/anno' = EUR_yr,
      '€/paziente' = EUR_pz
    ) |>
    as_tibble() |> print()
  
  df_pvt_anno <- df |>
    group_by(Anno) |>
    summarise(
      N = n(),
      EUR = sum(Netto, na.rm = TRUE)
    ) |>
    mutate(
      N_cum = cumsum(N), 
      EUR = as_euro(EUR),
      EUR_cum = as_euro(cumsum(EUR))
    ) |>
    relocate(N_cum, .after = N) |>
    rename(
      'N (cum)' = N_cum,
      Fatturato = EUR,
      'Fatturato (cum)' = EUR_cum
    ) |>
    as_tibble() |> print() 

  df_pvt_prestazione <- df |>
    group_by(Prestazione) |>
    summarise(
      N = n(),
      EUR = sum(Netto, na.rm = TRUE),
      Paganti = sum(Netto > 0, na.rm = TRUE),
      EUR_pz = if_else(Paganti > 0, EUR / Paganti, 0)
    ) |>
    select(-Paganti) |>
    arrange(desc(N)) |>
    mutate(
      Pct = N / sum(N),
      Pct_cum = cumsum(Pct),
    ) |>
    mutate(
      Pct = percent(Pct, accuracy = 0.01),
      Pct_cum = percent(Pct_cum, accuracy = 0.01),
      EUR = as_euro(EUR),
      EUR_pz = as_euro(EUR_pz)
    ) |>
    relocate(Pct:Pct_cum, .after = N) |>
    rename(
      '%' = Pct,
      '% (cum)' = Pct_cum,
      Fatturato = EUR,
      'Fatturato/paziente' = EUR_pz
    ) |>
    as_tibble() |> print()  

  df_pvt_regime <- df |>
    group_by(Regime) |>
    summarise(
      N = n(),
      EUR = sum(Netto, na.rm = TRUE),
      EUR_pz = EUR / N
    ) |>
    arrange(desc(N)) |>
    mutate(
      Pct = N / sum(N),
      Pct_cum = cumsum(Pct)
    ) |>
    mutate(
      Regime = ifelse(Regime != "", Regime, "TBD"),
      Pct = percent(Pct, accuracy = 0.01),
      Pct_cum = percent(Pct_cum, accuracy = 0.01), 
      EUR = as_euro(EUR),
      EUR_pz = as_euro(EUR_pz)
    ) |>
    relocate(Pct:Pct_cum, .after = N) |>
    rename(
      '%' = Pct,
      '% (cum)' = Pct_cum,
      Fatturato = EUR,
      'Fatturato/paziente' = EUR_pz
    ) |>
    as_tibble() |> print()
  
  df_pvt_orario <- df |>
    filter(Ora > 0) |>
    filter(((Ora >= hm("08:00")) & Ora <= hm("20:00"))) |>
    mutate(
      Orario = case_when(
        Ora >= hm("08:00") & Ora < hm("10:00") ~ "08:00 - 10:00",
        Ora >= hm("10:00") & Ora < hm("12:00") ~ "10:00 - 12:00",
        Ora >= hm("12:00") & Ora < hm("14:00") ~ "12:00 - 14:00",
        Ora >= hm("14:00") & Ora < hm("16:00") ~ "14:00 - 16:00",
        Ora >= hm("16:00") & Ora < hm("18:00") ~ "16:00 - 18:00",
        Ora >= hm("18:00") & Ora < hm("20:00") ~ "18:00 - 20:00"
      )
    ) |>
    group_by(Orario) |>
    summarise(
      N = n(),
      Giorni_Lavorati = n_distinct(Data), 
      .groups = "drop"
    ) |>
    mutate(
      Slot_Massimi = Giorni_Lavorati * 6,
      Pct = N / sum(N),
      Pct_cum = cumsum(Pct),
      Sat = N / Slot_Massimi
    ) |>
    mutate(
      Pct = percent(Pct, accuracy = 0.01),
      Pct_cum = percent(Pct_cum, accuracy = 0.01),
      Sat = percent(Sat, accuracy = 0.01)
    ) |>
    select(-c(Slot_Massimi, Giorni_Lavorati)) |>
    rename(
      '%' = Pct,
      '% (cum)' = Pct_cum,
      '% saturazione' = Sat
    ) |>
    as_tibble() |> print()
  
  df_pvt_sede_wide <- df |>
    select(Sede, Anno) |>
    pivot_wider(
      names_from = Sede,
      values_from = Sede,
      values_fn = length,
      values_fill = 0
    ) |> 
    arrange(Anno) |>
    adorn_totals(where = c("row", "col"), name = "Totale") |>
    as_tibble() |> print()
  
  df_pvt_prestazione_wide <- df |>
    count(Prestazione, Anno) |>
    pivot_wider(
      names_from = Prestazione,
      values_from = n,
      values_fill = 0
    ) |> 
    arrange(Anno) |>
    adorn_totals(where = c("row", "col"), name = "Totale") |>
    as_tibble() |> print()

# Esporta dati
  ls2save <- list(
    "0) Df completo" = df,
    "1) Pvt sede" = df_pvt_sede,
    "2) Pvt anno" = df_pvt_anno,
    "3) Pvt prestazione" = df_pvt_prestazione,
    "4) Pvt regime" = df_pvt_regime,
    "5) Pvt orario" = df_pvt_orario,
    "6) Pvt sede per anno" = df_pvt_sede_wide,
    "7) Pvt prestazione per anno" = df_pvt_prestazione_wide
  )
  saveRDS(ls2save, file = "data/export.rds")
  write_xlsx(ls2save, path = "data/export.xlsx")
  ls2keep <- ls(pattern = "^df$|^df_pvt")
  rm(list = setdiff(ls(), ls2keep))
  
  quarto_render("report_ver_2.qmd")
  utils::browseURL("report_ver_2.pdf")