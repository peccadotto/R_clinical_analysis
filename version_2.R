# Setup

  packages <- c(
    "tidyverse",
    "formattable",
    "janitor",
    "lubridate",
    "quarto",
    "scales",
    "treemap",
    "writexl"
  )

  for (pkg in packages) {
    if (!requireNamespace(pkg, quietly = TRUE)) {
      message("!! Package '", pkg, "' not found. Installing it...")
      tryCatch(
        install.packages(pkg, dependencies = TRUE),
        error = function(e) {
          message(
            "!! Error while trying to install the '",
            pkg,
            "' package: ",
            e$message
          )
        }
      )
    }
    suppressPackageStartupMessages(library(pkg, character.only = TRUE))
  }
  
  rm(list = ls())
  
# 1) Data import
  
  df_camozzi <- read.csv("data/camozzi.csv", sep = ";", header = TRUE) |>
    select(-Income) |> mutate(Place = "CAM")
  df_humanitas <- read.csv("data/humanitas.csv", sep = ";", header = TRUE) |>
    select(-Income) |> mutate(Place = "ICH")
  df_medcare  <- read.csv("data/medcare.csv", sep = ";", header = TRUE) |>
    select(-Income) |> mutate(Place = "HMC")
  df_modoetia <- read.csv("data/modoetia.csv", sep = ";", header = TRUE) |>
    select(-Income) |> mutate(Place = "MOD")
  df_poli <- read.csv("data/policlinico.csv", sep = ";", header = TRUE) |>
    select(-Income) |> mutate(Place = "POL")
  df_levante <- read.csv("data/salerno.csv", sep = ";", header = TRUE) |>
    select(-Income) |> mutate(Place = "SAL")
  df_zucchi <- read.csv("data/zucchi.csv", sep = ";", header = TRUE) |>
    select(-Income) |> mutate(Place = "ICZ")

# 2) Data merge
  
  df_names <- mget(ls(pattern = "^df_"))
  df <- bind_rows(df_names)
  df <- df |>
    mutate(
    Date = dmy(Date),
    Time = hm(Time),
    Service = factor(
      Service,
      levels = c("VISORT01", "VISORT02", "ECTBAC", "MISC")
    ),
    Year = year(Date),
    Month = month(Date)
  ) |>
    mutate(multiplier = if_else(Service == "ECTBAC", 2, 1)) |> 
      # Double the records with Service = ECTBAC
      # (count ultrasounds, not patients)
    uncount(multiplier) |>
    arrange(Date, Time)

# 3) Report creation

  df_pvt_place <- df |>
    group_by(Place) |>
    summarise(
      Patients = n(),
      Start = min(Date),
      End = max(Date),
      Span = sub(" 0H 0M 0S$", "", as.period(Start %--% End)),
      `Patients/year` = if_else(
        time_length(Start %--% End, unit = "years") >= 1,
        as.integer(Patients / time_length(Start %--% End, unit = "years")),
        0
      ),
    ) |>
    arrange(Start) |>
    mutate(
      `Patients (running)` = cumsum(Patients)
    ) |>
    relocate (
      c(Patients, `Patients (running)`),
      .after = Span
    ) |>
    as_tibble() |> print()
  
  df_pvt_year <- df |>
    group_by(Year) |>
    summarise(
      Patients = n()
    ) |>
    mutate(
      `Patients (running)` = cumsum(Patients)
    ) |>
    as_tibble() |> print() 

  df_pvt_month <- df |>
    mutate(
      Month = factor(
        Month,
        levels = 1:12, 
        labels = c("Jan", "Feb", "Mar", "Apr", "May", "Jun",
                   "Jul", "Aug", "Sep", "Oct", "Nov", "Dec"), 
        ordered = TRUE)
    ) |>
    group_by(Month) |>
    summarise(
      Patients = n()
    ) |>
    mutate(
      `%` = Patients/sum(Patients),
      `% (running)` = cumsum(`%`)
    ) |>
    mutate(
      `%` = percent(`%`, accuracy = 0.01),
      `% (running)` = percent(`% (running)`, accuracy = 0.01)
    ) |>
    as_tibble() |> print() 

  df_pvt_slot <- df |>
    filter(Time > 0) |>
    filter(((Time >= hm("08:00")) & Time <= hm("20:00"))) |>
    mutate(
      Slot = case_when(
        Time >= hm("08:00") & Time < hm("10:00") ~ "08:00 - 10:00",
        Time >= hm("10:00") & Time < hm("12:00") ~ "10:00 - 12:00",
        Time >= hm("12:00") & Time < hm("14:00") ~ "12:00 - 14:00",
        Time >= hm("14:00") & Time < hm("16:00") ~ "14:00 - 16:00",
        Time >= hm("16:00") & Time < hm("18:00") ~ "16:00 - 18:00",
        Time >= hm("18:00") & Time < hm("20:00") ~ "18:00 - 20:00"
      )
    ) |>
    group_by(Slot) |>
    summarise(
      Patients = n(),
      Worked_days = n_distinct(Date), 
      .groups = "drop"
    ) |>
    mutate(
      Slot_max = Worked_days * 6,
      `%` = Patients / sum(Patients),
      `% (running)` = cumsum(`%`),
      `% occupancy` = Patients / Slot_max
    ) |>
    mutate(
      `%` = percent(`%`, accuracy = 0.01),
      `% (running)` = percent(`% (running)`, accuracy = 0.01),
      `% occupancy` = percent(`% occupancy`, accuracy = 0.01)
    ) |>
    select(-c(Slot_max, Worked_days)) |>
    as_tibble() |> print()
  
  df_pvt_service <- df |>
    group_by(Service) |>
    summarise(
      Patients = n()
    ) |>
    arrange(desc(Patients)) |>
    mutate(
      `%` = Patients / sum(Patients),
      `% (running)` = cumsum(`%`),
    ) |>
    mutate(
      `%` = percent(`%`, accuracy = 0.01),
      `% (running)` = percent(`% (running)`, accuracy = 0.01)
    ) |>
    as_tibble() |> print()  

  df_pvt_system <- df |>
    group_by(System) |>
    summarise(
      Patients = n()
    ) |>
    arrange(desc(Patients)) |>
    mutate(
      `%` = Patients / sum(Patients),
      `% (running)` = cumsum(`%`)
    ) |>
    mutate(
      System = ifelse(System != "", System, "TBD"),
      `%` = percent(`%`, accuracy = 0.01),
      `% (running)` = percent(`% (running)`, accuracy = 0.01)
    ) |>
    as_tibble() |> print()
  
df_pvt_place_wide <- df |>
    select(Place, Year) |>
    pivot_wider(
      names_from = Place,
      values_from = Place,
      values_fn = length,
      values_fill = 0
    ) |> 
    arrange(Year) |>
    adorn_totals(where = c("row", "col"), name = "Total") |>
    as_tibble() |> print()
  
  df_pvt_service_wide <- df |>
    count(Service, Year) |>
    pivot_wider(
      names_from = Service,
      values_from = n,
      values_fill = 0
    ) |> 
    arrange(Year) |>
    adorn_totals(where = c("row", "col"), name = "Total") |>
    as_tibble() |> print()

# 4) Data export

 ls2save <- list(
    "0) Complete" = df,
    "1) PVT by PLACE" = df_pvt_place,
    "2) PVT by YEAR" = df_pvt_year,
    "3) PVT by MONTH" = df_pvt_month,
    "4) PVT by SLOT" = df_pvt_slot,
    "5) PVT by SERVICE" = df_pvt_service,
    "6) PVT by SYSTEM" = df_pvt_system,
    "7) PVT by PLACE-YEAR" = df_pvt_place_wide,
    "8) PVT by SERVICE-YEAR" = df_pvt_service_wide
  )
  saveRDS(ls2save, file = "data/export.rds")
  write_xlsx(ls2save, path = "data/export.xlsx")
  ls2keep <- ls(pattern = "^df$|^df_pvt")
  rm(list = setdiff(ls(), ls2keep))
  
  Sys.setenv(QUARTO_PATH = "C:\\Program Files\\RStudio\\resources\\app\\bin\\quarto\\bin\\quarto.exe")
  quarto_render("report_version_2.qmd")
  quarto_render("report_version_2_plot.qmd")
  utils::browseURL("report_version_2.pdf")
  utils::browseURL("report_version_2_plot.html")