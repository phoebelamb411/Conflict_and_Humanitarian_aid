# ============================================================================
# Conflict Deaths vs Humanitarian Aid Analysis  — CLEAN + PLOTS
# ============================================================================

# --- Project locations (adjust only these two lines if you move the project) --
setwd("/Users/student/Desktop/Passion project/Conflict and Humanitarian Aid")
DATA_DIR   <- "Conflict_Humanitarian_Aid/Conflict_and_Humanitarian_aid"
OUTPUT_DIR <- file.path(DATA_DIR, "figures")  # all charts saved here

cat("Working directory:", getwd(), "\n")
cat("Data directory:", DATA_DIR, "\n\n")

# ---- Load Packages ---------------------------------------------------------
required_packages <- c("tidyverse", "readxl", "janitor", "scales", "patchwork", "ggrepel")

missing <- required_packages[!(required_packages %in% installed.packages()[,"Package"])]
if (length(missing) > 0) {
  cat("Installing:", paste(missing, collapse = ", "), "\n")
  install.packages(missing)
}

library(tidyverse)
library(readxl)
library(janitor)
library(scales)
library(patchwork)
library(ggrepel)

# Ensure output folder exists
if (!dir.exists(OUTPUT_DIR)) {
  dir.create(OUTPUT_DIR, recursive = TRUE)
  cat("✓ Created figures folder at:", OUTPUT_DIR, "\n\n")
} else {
  cat("✓ Figures folder exists at:", OUTPUT_DIR, "\n\n")
}

# ---- Helpers ---------------------------------------------------------------
theme_clean <- function() {
  theme_minimal(base_size = 12) +
    theme(
      panel.grid.major.y = element_blank(),
      plot.title = element_text(face = "bold", size = 14, margin = margin(b = 6)),
      plot.subtitle = element_text(color = "grey30"),
      plot.caption = element_text(size = 9, color = "grey40")
    )
}

normalize_country <- function(x) {
  x <- str_trim(str_replace_all(as.character(x), "\\s+", " "))
  case_when(
    x %in% c("Ukraine", "Ukraine (Govt)") ~ "Ukraine",
    x %in% c("Russia", "Russian Federation") ~ "Russia",
    x %in% c("Israel", "State of Israel") ~ "Israel",
    x %in% c("Palestine", "Palestine, State of", "State of Palestine",
             "Palestinian Territory", "West Bank and Gaza", "West Bank & Gaza",
             "Gaza Strip", "occupied Palestinian territory", "OPT", "oPt",
             "oPt (occupied Palestinian territory)") ~ "West Bank & Gaza",
    x %in% c("Sudan", "Republic of the Sudan") ~ "Sudan",
    x %in% c("Syria", "Syrian Arab Republic") ~ "Syria",
    x %in% c("Ethiopia", "Federal Democratic Republic of Ethiopia") ~ "Ethiopia",
    x %in% c("Somalia", "Federal Republic of Somalia") ~ "Somalia",
    x %in% c("Congo, Dem. Rep.", "DR Congo", "DRC", "Congo (DRC)",
             "Democratic Republic of the Congo", 
             "The Democratic Republic of the Congo") ~ "Democratic Republic of the Congo",
    x %in% c("Mali", "Republic of Mali") ~ "Mali",
    x %in% c("Cameroon", "Republic of Cameroon") ~ "Cameroon",
    TRUE ~ x
  )
}

extract_country_from_plan <- function(plan_title) {
  cleaned <- str_squish(as.character(plan_title))
  cleaned <- str_remove_all(cleaned, "\\([^\\)]*\\)")

  plan_types <- c(
    "Humanitarian Needs and Response Plan","Humanitarian Response Plan",
    "Regional Refugee and Resilience Plan","Regional Refugee Response Plan",
    "Flash Appeal","Joint Response Plan","Response Plan",
    "Plan de Réponse Humanitaire","Plan de Réponse","Besoins Humanitaires et Plan de Réponse"
  )
  for (type in plan_types) {
    cleaned <- str_remove(cleaned, paste0("\\b", type, "\\b.*$"))
  }
  cleaned <- str_remove(cleaned, "\\b\\d{4}(\\s*-\\s*\\d{4})?$")

  # French to English
  cleaned <- recode(cleaned,
    "République Démocratique du Congo" = "Democratic Republic of the Congo",
    "République Centrafricaine" = "Central African Republic",
    "Haïti" = "Haiti",
    "Tchad" = "Chad",
    .default = cleaned
  )

  str_squish(cleaned)
}

# ---- Load Death Data -------------------------------------------------------
cat("Loading death data...\n")

death_files <- c(
  file.path(DATA_DIR, "Africa_aggregated_data_up_to-2025-08-30.xlsx"),
  file.path(DATA_DIR, "Middle-East_aggregated_data_up_to-2025-08-30.xlsx"),
  file.path(DATA_DIR, "Europe-Central-Asia_aggregated_data_up_to-2025-08-23.xlsx")
)

for (f in death_files) {
  if (file.exists(f)) cat("  ✓ Found:", basename(f), "\n") else cat("  ✗ Missing:", basename(f), "\n")
}
cat("\n")

read_deaths <- function(filepath) {
  if (!file.exists(filepath)) {
    warning("File not found: ", filepath); return(tibble())
  }
  cat("  Reading:", basename(filepath), "...")
  df <- suppressMessages(read_excel(filepath)) %>% clean_names()
  if (!all(c("country","fatalities") %in% names(df))) { cat(" ⚠️  Missing columns\n"); return(tibble()) }
  if (!("year" %in% names(df))) {
    if ("week" %in% names(df)) {
      df <- df %>% mutate(year = as.integer(substr(as.character(week), 1, 4)))
    } else { cat(" ⚠️  No year column\n"); return(tibble()) }
  }
  out <- df %>%
    transmute(country = normalize_country(country),
              year = as.integer(year),
              deaths = as.numeric(fatalities)) %>%
    filter(!is.na(country), !is.na(year), !is.na(deaths))
  cat(" ✓", comma(nrow(out)), "rows\n")
  out
}

all_deaths <- map_dfr(death_files, read_deaths)
cat("\n✓ Total death records loaded:", comma(nrow(all_deaths)), "\n")
if (nrow(all_deaths) == 0) stop("❌ No death data loaded! Check file locations.")
years <- sort(unique(all_deaths$year))
cat("  Years available:", paste(years, collapse = ", "), "\n\n")

deaths_2024 <- all_deaths %>%
  filter(year == 2024) %>%
  group_by(country) %>%
  summarize(total_deaths = sum(deaths, na.rm = TRUE), .groups = "drop") %>%
  arrange(desc(total_deaths))
cat("✓ Deaths aggregated for 2024\n")
cat("  Countries:", nrow(deaths_2024), "\n")
cat("  Total deaths:", comma(sum(deaths_2024$total_deaths)), "\n\n")

# ---- Load Funding Data (OCHA) ----------------------------------------------
cat("Loading OCHA funding data...\n")
ocha_file <- file.path(DATA_DIR, "ocha_funding.csv")
if (!file.exists(ocha_file)) stop("❌ Cannot find ocha_funding.csv in ", DATA_DIR)
cat("  ✓ Found:", basename(ocha_file), "\n\n")

funding_raw <- read_csv(ocha_file, show_col_types = FALSE) %>% clean_names()
cat("✓ OCHA data loaded:", comma(nrow(funding_raw)), "plans\n\n")

# Clean + aggregate (fixes included)
funding_2024 <- funding_raw %>%
  mutate(
    country_extracted = extract_country_from_plan(name),

    # Map “Escalation of Hostilities in the OPT” -> West Bank & Gaza
    country_extracted = dplyr::case_when(
      str_detect(country_extracted, regex("^Escalation of Hostilities in the OPT$", ignore_case = TRUE)) ~ "West Bank & Gaza",
      str_detect(country_extracted, regex("^Not specified$", ignore_case = TRUE)) ~ NA_character_,
      TRUE ~ country_extracted
    ),
    country = normalize_country(country_extracted),

    # Clean funding strings: anything starting with "#" is an error -> NA
    funding_clean = as.character(funding),
    funding_clean = dplyr::if_else(str_starts(funding_clean, "#"), NA_character_, funding_clean),

    # Parse numeric safely
    funding_usd = suppressWarnings(readr::parse_number(funding_clean))
  ) %>%
  filter(year == 2024, !is.na(country), !is.na(funding_usd), funding_usd > 0) %>%
  group_by(country) %>%
  summarize(total_funding_usd = sum(funding_usd, na.rm = TRUE), .groups = "drop") %>%
  arrange(desc(total_funding_usd))

cat("✓ Funding aggregated for 2024\n")
cat("  Countries with funding:", nrow(funding_2024), "\n")
cat("  Total funding: $", comma(sum(funding_2024$total_funding_usd) / 1e9, accuracy = 0.1), "B\n\n", sep = "")

# ---- Merge + define groups -------------------------------------------------
cat("Merging datasets...\n")
combined <- deaths_2024 %>%
  full_join(funding_2024, by = "country") %>%
  mutate(
    total_deaths = replace_na(total_deaths, 0),
    total_funding_usd = if_else(is.na(total_funding_usd) | total_funding_usd == 0, NA_real_, total_funding_usd)
  )

prominent <- c("Ukraine", "Russia", "Israel", "West Bank & Gaza", "Sudan", "Syria")
underrated <- c("Ethiopia", "Cameroon", "Somalia", "Democratic Republic of the Congo", "Mali")

table_prominent <- combined %>% filter(country %in% prominent)
table_underrated <- combined %>% filter(country %in% underrated)

cat("✓ Data merged\n")
cat("  Prominent conflicts found:", nrow(table_prominent), "of", length(prominent), "\n")
cat("  Underrated conflicts found:", nrow(table_underrated), "of", length(underrated), "\n\n")

# Totals for gap summary
prom_deaths  <- sum(table_prominent$total_deaths, na.rm = TRUE)
prom_funding <- sum(table_prominent$total_funding_usd, na.rm = TRUE)
prom_per_death <- if_else(prom_deaths > 0, prom_funding / prom_deaths, NA_real_)

und_deaths  <- sum(table_underrated$total_deaths, na.rm = TRUE)
und_funding <- sum(table_underrated$total_funding_usd, na.rm = TRUE)
und_per_death <- if_else(und_deaths > 0, und_funding / und_deaths, NA_real_)

# ---- CHARTS ---------------------------------------------------------------

# 1) Top 10 by deaths
top_deaths <- deaths_2024 %>% slice_max(total_deaths, n = 10) %>%
  mutate(country = fct_reorder(country, total_deaths))

p_top_deaths <- ggplot(top_deaths, aes(x = total_deaths, y = country)) +
  geom_col(width = 0.6, fill = "#3A3A3A") +
  scale_x_continuous(labels = label_number(scale_cut = cut_short_scale())) +
  labs(title = "Top 10 Countries by Conflict Deaths (2024)", x = NULL, y = NULL,
       caption = "Source: Regional aggregates") +
  theme_clean()

ggsave(file.path(OUTPUT_DIR, "top10_deaths_2024.png"),
       p_top_deaths, width = 10, height = 6, dpi = 300, bg = "white")

# 2) Top 10 by funding
top_funding <- funding_2024 %>% slice_max(total_funding_usd, n = 10) %>%
  mutate(country = fct_reorder(country, total_funding_usd))

p_top_funding <- ggplot(top_funding, aes(x = total_funding_usd, y = country)) +
  geom_col(width = 0.6, fill = "#6B8EFA") +
  scale_x_continuous(labels = label_dollar(scale = 1e-9, suffix = "B", accuracy = 0.1)) +
  labs(title = "Top 10 Countries by Humanitarian Funding (2024)", x = NULL, y = NULL,
       caption = "Source: OCHA FTS (plan-level funding)") +
  theme_clean()

ggsave(file.path(OUTPUT_DIR, "top10_funding_2024.png"),
       p_top_funding, width = 10, height = 6, dpi = 300, bg = "white")

# 3) Scatter — deaths vs funding (log-log)
scatter_df <- combined %>% filter(!is.na(total_funding_usd), total_deaths > 0)

label_countries <- unique(c(
  prominent, underrated,
  scatter_df %>% slice_max(total_deaths, n = 5) %>% pull(country),
  scatter_df %>% slice_max(total_funding_usd, n = 5) %>% pull(country),
  "West Bank & Gaza"
))

p_scatter <- ggplot(scatter_df, aes(x = total_deaths, y = total_funding_usd)) +
  geom_point(alpha = 0.75, size = 2.5) +
  scale_x_log10(labels = label_number(scale_cut = cut_short_scale())) +
  scale_y_log10(labels = label_dollar(scale = 1e-9, suffix = "B", accuracy = 0.1)) +
  geom_smooth(method = "lm", se = FALSE, linewidth = 0.7, linetype = 2, color = "grey40") +
  ggrepel::geom_text_repel(
    data = scatter_df %>% filter(country %in% label_countries),
    aes(label = country), size = 3, max.overlaps = 30
  ) +
  labs(
    title = "Do Dollars Track Deaths? (2024)",
    subtitle = "Each point = one country | Log–log scale; dashed line = linear trend",
    x = "Total conflict deaths (log scale)",
    y = "Humanitarian funding (log scale, USD)",
    caption = "Sources: Regional aggregates; OCHA FTS"
  ) +
  theme_clean()

ggsave(file.path(OUTPUT_DIR, "scatter_deaths_vs_funding_2024.png"),
       p_scatter, width = 9, height = 7, dpi = 300, bg = "white")

# 4) Funding per death (guarded against NA)
per_death_df <- bind_rows(
  table_prominent %>% mutate(group = "Prominent"),
  table_underrated %>% mutate(group = "Underrated")
) %>%
  filter(total_deaths > 0) %>%
  mutate(funding_per_death = total_funding_usd / total_deaths) %>%
  filter(!is.na(funding_per_death))

if (nrow(per_death_df) > 0) {
  p_per_death <- per_death_df %>%
    mutate(country = fct_reorder(country, funding_per_death)) %>%
    ggplot(aes(x = funding_per_death, y = country, fill = group)) +
    geom_col(width = 0.6) +
    scale_x_continuous(labels = label_dollar(accuracy = 1)) +
    scale_fill_manual(values = c("Prominent" = "#C65D5D", "Underrated" = "#3A3A3A")) +
    labs(
      title = "Funding per Death (2024)",
      subtitle = "Comparing prominent vs underrated conflicts",
      x = NULL, y = NULL, fill = NULL,
      caption = "Note: Countries with missing funding are excluded."
    ) +
    theme_clean() +
    theme(legend.position = "top")

  ggsave(file.path(OUTPUT_DIR, "funding_per_death_groups_2024.png"),
         p_per_death, width = 10, height = 6.5, dpi = 300, bg = "white")
} else {
  message("No countries have both deaths > 0 and funding; skipping funding_per_death chart.")
}

# ---- Console Summary -------------------------------------------------------
cat("\n======================================\n")
cat("📊 ANALYSIS RESULTS\n")
cat("======================================\n\n")

cat("PROMINENT (High Media):\n")
cat("  Total deaths: ", comma(prom_deaths), "\n", sep = "")
cat("  Total funding: $", comma(prom_funding / 1e9, accuracy = 0.1), "B\n", sep = "")
cat("  Avg per death: $", comma(prom_per_death, accuracy = 1), "\n\n", sep = "")

cat("UNDERRATED (Low Media):\n")
cat("  Total deaths: ", comma(und_deaths), "\n", sep = "")
cat("  Total funding: $", comma(und_funding / 1e9, accuracy = 0.1), "B\n", sep = "")
cat("  Avg per death: $", comma(und_per_death, accuracy = 1), "\n\n", sep = "")

if (!is.na(prom_per_death) && !is.na(und_per_death) && und_per_death > 0) {
  gap_ratio <- prom_per_death / und_per_death
  cat("GAP: Prominent receive ", comma(gap_ratio, accuracy = 0.1), "× more funding per death.\n", sep = "")
} else {
  cat("GAP: Unable to compute due to missing values.\n")
}

cat("\n✓ Charts saved to:\n  -", file.path(OUTPUT_DIR, "top10_deaths_2024.png"), "\n",
    " -", file.path(OUTPUT_DIR, "top10_funding_2024.png"), "\n",
    " -", file.path(OUTPUT_DIR, "scatter_deaths_vs_funding_2024.png"), "\n",
    " -", file.path(OUTPUT_DIR, "funding_per_death_groups_2024.png"), "\n\n", sep = "")
