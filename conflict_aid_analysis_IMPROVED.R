# ============================================================================
# Conflict Deaths vs Humanitarian Aid Analysis
# ============================================================================
#
# WHAT THIS ANALYSIS DOES:
# Compares conflict deaths to humanitarian aid funding for two groups:
# 1. Prominent conflicts (high media coverage): Ukraine, Gaza, Sudan, etc.
# 2. Underrated conflicts (low media coverage): Ethiopia, Mali, Somalia, etc.
#
# GOAL: Show whether humanitarian aid follows actual need or media attention
#
# AUTHOR: Phoebe Lamb
# REPO: github.com/phoebelamb411/Conflict_and_Humanitarian_aid
# ============================================================================


# ---- STEP 1: Load Required Packages ----------------------------------------

# Packages we need for this analysis
required_packages <- c("tidyverse", "readxl", "janitor", "scales", "patchwork")

# Install any missing packages
missing_packages <- required_packages[!(required_packages %in% rownames(installed.packages()))]
if (length(missing_packages) > 0) {
  install.packages(missing_packages)
}

# Load all packages
invisible(lapply(required_packages, library, character.only = TRUE))

cat("\n✓ All packages loaded successfully!\n\n")


# ---- STEP 2: Define File Paths and Country Groups -------------------------

# Data files we're using
path_ocha_funding <- "ocha_funding.csv"

path_regional_deaths <- c(
  "Africa_aggregated_data_up_to-2025-08-30.xlsx",
  "Middle-East_aggregated_data_up_to-2025-08-30.xlsx",
  "Europe-Central-Asia_aggregated_data_up_to-2025-08-23.xlsx"
)

# Verify files exist
if (!file.exists(path_ocha_funding)) {
  stop("Cannot find OCHA funding file: ", path_ocha_funding)
}

# Define our two comparison groups
prominent_conflicts <- c("Ukraine", "Russia", "Israel", "West Bank & Gaza", "Sudan", "Syria")
underrated_conflicts <- c("Ethiopia", "Cameroon", "Somalia", "Democratic Republic of the Congo", "Mali")

cat("✓ File paths and country groups defined\n")
cat("  Prominent conflicts (", length(prominent_conflicts), "): ", paste(prominent_conflicts, collapse = ", "), "\n", sep = "")
cat("  Underrated conflicts (", length(underrated_conflicts), "): ", paste(underrated_conflicts, collapse = ", "), "\n\n", sep = "")


# ---- STEP 3: Country Name Standardization Function ------------------------

# Function to standardize country names across different data sources
# (OCHA, ACLED, and World Bank all use slightly different naming conventions)

normalize_country_name <- function(country_name) {
  
  # Clean whitespace
  country_name <- str_trim(str_replace_all(country_name, "\\s+", " "))
  
  # Standardize to consistent names
  standardized_name <- case_when(
    # Ukraine variations
    country_name %in% c("Ukraine", "Ukraine (Govt)") ~ "Ukraine",
    
    # Russia variations
    country_name %in% c("Russia", "Russian Federation") ~ "Russia",
    
    # Israel variations
    country_name %in% c("Israel", "State of Israel") ~ "Israel",
    
    # Palestine/Gaza variations (many different names in data!)
    country_name %in% c(
      "Palestine", "Palestine, State of", "State of Palestine",
      "Palestinian Territory", "West Bank and Gaza", "West Bank & Gaza",
      "Gaza Strip", "occupied Palestinian territory", "OPT", "oPt",
      "oPt (occupied Palestinian territory)"
    ) ~ "West Bank & Gaza",
    
    # Sudan variations
    country_name %in% c("Sudan", "Republic of the Sudan") ~ "Sudan",
    
    # Syria variations
    country_name %in% c("Syria", "Syrian Arab Republic") ~ "Syria",
    
    # Ethiopia variations
    country_name %in% c("Ethiopia", "Federal Democratic Republic of Ethiopia") ~ "Ethiopia",
    
    # Somalia variations
    country_name %in% c("Somalia", "Federal Republic of Somalia") ~ "Somalia",
    
    # DRC variations (many different abbreviations!)
    country_name %in% c(
      "Congo, Dem. Rep.", "DR Congo", "DRC", "Congo (DRC)",
      "Democratic Republic of the Congo", "The Democratic Republic of the Congo"
    ) ~ "Democratic Republic of the Congo",
    
    # Mali variations
    country_name %in% c("Mali", "Republic of Mali") ~ "Mali",
    
    # Cameroon variations
    country_name %in% c("Cameroon", "Republic of Cameroon") ~ "Cameroon",
    
    # Default: keep original name
    TRUE ~ country_name
  )
  
  return(standardized_name)
}


# ---- STEP 4: Load and Process Death Data -----------------------------------

cat("Loading conflict death data from regional files...\n")

# Function to read and clean regional death data files
load_regional_deaths <- function(filepath) {
  
  # Check if file exists
  if (!file.exists(filepath)) {
    warning("File not found: ", filepath)
    return(tibble())
  }
  
  # Read Excel file
  raw_data <- suppressMessages(read_excel(filepath)) %>%
    clean_names()
  
  # Check for required columns
  if (!all(c("country", "fatalities") %in% names(raw_data))) {
    warning("Missing required columns in: ", filepath)
    return(tibble())
  }
  
  # Extract year from data
  # Some files have 'year' column, others have 'week' (format: YYYY-WW)
  if (!("year" %in% names(raw_data))) {
    if ("week" %in% names(raw_data)) {
      raw_data <- raw_data %>%
        mutate(
          week_string = as.character(week),
          year = as.integer(substr(week_string, 1, 4))
        )
    } else {
      warning("Cannot extract year from: ", filepath)
      return(tibble())
    }
  }
  
  # Clean and standardize
  cleaned_data <- raw_data %>%
    transmute(
      country = normalize_country_name(country),
      year = as.integer(year),
      deaths = as.numeric(fatalities)
    ) %>%
    filter(!is.na(country), !is.na(year), !is.na(deaths))
  
  return(cleaned_data)
}

# Load all regional files and combine
all_deaths_raw <- map_dfr(path_regional_deaths, load_regional_deaths)

if (nrow(all_deaths_raw) == 0) {
  stop("No death data loaded! Check file paths and formats.")
}

# Use 2024 data (most recent)
analysis_year <- 2024

deaths_by_country <- all_deaths_raw %>%
  filter(year == analysis_year) %>%
  group_by(country) %>%
  summarize(
    total_deaths = sum(deaths, na.rm = TRUE),
    .groups = "drop"
  )

cat("✓ Death data loaded for", analysis_year, "\n")
cat("  Total countries with death data:", nrow(deaths_by_country), "\n")
cat("  Total deaths across all countries:", comma(sum(deaths_by_country$total_deaths)), "\n\n")


# ---- STEP 5: Load and Process Humanitarian Aid Data -----------------------

cat("Loading humanitarian aid data from OCHA...\n")

# Function to extract country name from OCHA plan titles
# (OCHA titles include plan types and years we need to remove)
extract_country_from_plan <- function(plan_title) {
  
  # Remove text in parentheses
  cleaned <- str_squish(plan_title)
  cleaned <- str_remove_all(cleaned, "\\([^\\)]*\\)")
  
  # Remove common plan suffixes
  plan_patterns <- c(
    "Humanitarian Needs and Response Plan",
    "Humanitarian Response Plan",
    "Regional Refugee and Resilience Plan",
    "Regional Refugee Response Plan",
    "Situation Regional Refugee Response Plan",
    "Flash Appeal",
    "Joint Response Plan",
    "Response Plan",
    "Plan de Réponse Humanitaire",  # French
    "Besoins Humanitaires et Plan de Réponse",  # French
    "Plan de Réponse"  # French
  )
  
  for (pattern in plan_patterns) {
    cleaned <- str_remove(cleaned, paste0("\\b", pattern, "\\b.*$"))
  }
  
  # Remove trailing years (e.g., "2024" or "2023-2024")
  cleaned <- str_remove(cleaned, "\\b\\d{4}(\\s*-\\s*\\d{4})?$")
  
  # Translate French country names
  cleaned <- recode(cleaned,
    "République Démocratique du Congo" = "Democratic Republic of the Congo",
    "République Centrafricaine" = "Central African Republic",
    "Haïti" = "Haiti",
    "Tchad" = "Chad",
    .default = cleaned
  )
  
  return(str_squish(cleaned))
}

# Load OCHA funding data
funding_raw <- read_csv(path_ocha_funding, show_col_types = FALSE) %>%
  clean_names()

# Verify required columns exist
required_cols <- c("name", "year", "funding")
if (!all(required_cols %in% names(funding_raw))) {
  stop("OCHA funding file missing required columns: ", paste(required_cols, collapse = ", "))
}

# Process funding data
funding_by_country <- funding_raw %>%
  mutate(
    # Extract country from plan name
    country_extracted = extract_country_from_plan(name),
    country = normalize_country_name(country_extracted),
    
    # Clean funding values (some have "#VALUE!" errors)
    funding_clean = as.character(funding),
    funding_clean = if_else(str_detect(funding_clean, "^#value", negate = FALSE), NA_character_, funding_clean),
    funding_usd = parse_number(funding_clean)
  ) %>%
  filter(year == analysis_year, !is.na(funding_usd)) %>%
  group_by(country) %>%
  summarize(
    total_funding_usd = sum(funding_usd, na.rm = TRUE),
    .groups = "drop"
  )

cat("✓ Funding data loaded for", analysis_year, "\n")
cat("  Countries with funding data:", nrow(funding_by_country), "\n")
cat("  Total humanitarian funding: $", comma(sum(funding_by_country$total_funding_usd) / 1e9), "B\n\n", sep = "")


# ---- STEP 6: Merge Data and Create Analysis Tables ------------------------

cat("Merging death and funding data...\n")

# Combine deaths and funding
combined_data <- deaths_by_country %>%
  full_join(funding_by_country, by = "country") %>%
  mutate(
    # Replace missing values appropriately
    total_deaths = replace_na(total_deaths, 0),
    # Keep funding as NA if missing (we'll show this on charts)
    total_funding_usd = if_else(is.na(total_funding_usd) | total_funding_usd == 0, 
                                 NA_real_, 
                                 total_funding_usd)
  )

# Create table for prominent conflicts (in display order)
prominent_display_order <- c("Syria", "Sudan", "Israel", "West Bank & Gaza", "Russia", "Ukraine")

table_prominent <- combined_data %>%
  filter(country %in% prominent_conflicts) %>%
  mutate(country = factor(country, levels = prominent_display_order)) %>%
  arrange(country) %>%
  filter(!is.na(country))  # Remove any that didn't match

# Create table for underrated conflicts (in display order)
underrated_display_order <- c("Cameroon", "Somalia", "Ethiopia", "Mali", "Democratic Republic of the Congo")

table_underrated <- combined_data %>%
  filter(country %in% underrated_conflicts) %>%
  mutate(country = factor(country, levels = underrated_display_order)) %>%
  arrange(country) %>%
  filter(!is.na(country))

# Verify we have data
if (nrow(table_prominent) == 0 || nrow(table_underrated) == 0) {
  stop("Missing data for some country groups. Check country name matching.")
}

cat("✓ Data merged successfully\n")
cat("  Prominent conflicts with data:", nrow(table_prominent), "\n")
cat("  Underrated conflicts with data:", nrow(table_underrated), "\n\n")


# ---- STEP 7: Create Visualization Theme and Helpers -----------------------

# Colors for charts
COLOR_PROMINENT <- "#C65D5D"  # Muted red for prominent conflicts
COLOR_UNDERRATED <- "#3A3A3A"  # Charcoal for underrated conflicts

# Custom ggplot theme
theme_conflict_viz <- function() {
  theme_minimal(base_size = 12) +
    theme(
      panel.grid.major.y = element_blank(),
      axis.text.y = element_text(margin = margin(r = 6)),
      plot.title = element_text(face = "bold", size = 16, margin = margin(b = 6)),
      plot.caption = element_text(size = 9, color = "grey40")
    )
}

# Display labels for charts (shorter versions for readability)
format_country_label_prominent <- function(country_name) {
  recode(as.character(country_name),
    "West Bank & Gaza" = "W.B. & Gaza",
    .default = country_name
  )
}

format_country_label_underrated <- function(country_name) {
  recode(as.character(country_name),
    "Cameroon" = "Cameroon / Lake Chad belt",
    "Somalia" = "Somalia (Al-Shabab insurgency)",
    "Ethiopia" = "Ethiopia (post-Tigray & regional)",
    "Mali" = "Mali (insurgency)",
    "Democratic Republic of the Congo" = "DRC (beyond M23)",
    .default = country_name
  )
}


# ---- STEP 8: Create Death Charts -------------------------------------------

cat("Creating visualizations...\n")

# Deaths chart for prominent conflicts
chart_deaths_prominent <- table_prominent %>%
  mutate(country_label = format_country_label_prominent(country)) %>%
  ggplot(aes(x = total_deaths, y = country_label)) +
  geom_col(width = 0.45, fill = COLOR_PROMINENT) +
  scale_x_continuous(labels = label_number(scale_cut = cut_short_scale())) +
  labs(
    title = "Deaths (2024)",
    x = NULL,
    y = NULL
  ) +
  theme_conflict_viz()

# Deaths chart for underrated conflicts
chart_deaths_underrated <- table_underrated %>%
  mutate(country_label = format_country_label_underrated(country)) %>%
  ggplot(aes(x = total_deaths, y = country_label)) +
  geom_col(width = 0.45, fill = COLOR_UNDERRATED) +
  scale_x_continuous(labels = label_number(scale_cut = cut_short_scale())) +
  labs(
    title = "Conflict deaths (2024)",
    x = NULL,
    y = NULL
  ) +
  theme_conflict_viz()


# ---- STEP 9: Create Funding Charts -----------------------------------------

# Funding chart for prominent conflicts
# (Shows "N/A" where funding data is missing)
chart_funding_prominent <- table_prominent %>%
  mutate(
    country_label = format_country_label_prominent(country),
    funding_for_plot = replace_na(total_funding_usd, 0),
    is_missing = is.na(total_funding_usd)
  ) %>%
  ggplot(aes(x = funding_for_plot, y = country_label)) +
  geom_col(width = 0.45, fill = COLOR_PROMINENT) +
  geom_text(
    data = . %>% filter(is_missing),
    aes(x = 0, label = "N/A"),
    hjust = -0.15,
    size = 3.4,
    color = "grey40"
  ) +
  scale_x_continuous(
    labels = label_dollar(scale = 1e-9, suffix = "B", accuracy = 0.1),
    expand = expansion(mult = c(0.02, 0))
  ) +
  labs(
    title = "Humanitarian aid (USD, 2024)",
    x = NULL,
    y = NULL
  ) +
  theme_conflict_viz()

# Funding chart for underrated conflicts
chart_funding_underrated <- table_underrated %>%
  mutate(
    country_label = format_country_label_underrated(country),
    funding_for_plot = replace_na(total_funding_usd, 0)
  ) %>%
  ggplot(aes(x = funding_for_plot, y = country_label)) +
  geom_col(width = 0.45, fill = COLOR_UNDERRATED) +
  scale_x_continuous(
    labels = label_dollar(scale = 1e-9, suffix = "B", accuracy = 0.1)
  ) +
  labs(
    title = "Humanitarian aid (USD, 2024)",
    x = NULL,
    y = NULL
  ) +
  theme_conflict_viz()


# ---- STEP 10: Combine and Save Charts --------------------------------------

cat("Saving visualizations...\n")

# Create chart caption
chart_caption <- paste0(
  "Sources: UCDP/ACLED regional aggregates (conflict deaths); OCHA Financial Tracking Service (humanitarian funding). ",
  "Data year: ", analysis_year, ". ",
  "'N/A' indicates no plan-level funding data reported."
)

# Combine prominent conflict charts
final_chart_prominent <- chart_deaths_prominent + chart_funding_prominent +
  plot_annotation(
    title = paste0("Prominent Conflicts: Deaths vs Humanitarian Funding (", analysis_year, ")"),
    caption = chart_caption,
    theme = theme_conflict_viz()
  )

# Save prominent conflicts chart
ggsave(
  filename = "prominent_conflicts.png",
  plot = final_chart_prominent,
  width = 13,
  height = 8,
  dpi = 300,
  bg = "white"
)

# Combine underrated conflict charts
final_chart_underrated <- chart_deaths_underrated + chart_funding_underrated +
  plot_annotation(
    title = paste0("Underrated Conflicts: Deaths vs Humanitarian Aid (", analysis_year, ")"),
    subtitle = "Ethiopia, Cameroon, Somalia, DRC, Mali",
    caption = chart_caption,
    theme = theme_conflict_viz()
  )

# Save underrated conflicts chart
ggsave(
  filename = "underrated_conflicts.png",
  plot = final_chart_underrated,
  width = 13,
  height = 8,
  dpi = 300,
  bg = "white"
)

cat("✓ Charts saved:\n")
cat("  - prominent_conflicts.png\n")
cat("  - underrated_conflicts.png\n\n")


# ---- STEP 11: Print Summary Statistics -------------------------------------

cat("="*70, "\n", sep = "")
cat("ANALYSIS COMPLETE!\n")
cat("="*70, "\n\n", sep = "")

cat("📊 PROMINENT CONFLICTS (High Media Coverage):\n")
cat("  Total deaths:", comma(sum(table_prominent$total_deaths, na.rm = TRUE)), "\n")
cat("  Total funding: $", comma(sum(table_prominent$total_funding_usd, na.rm = TRUE) / 1e9), "B\n", sep = "")
cat("  Funding per death: $", comma(sum(table_prominent$total_funding_usd, na.rm = TRUE) / sum(table_prominent$total_deaths, na.rm = TRUE)), "\n\n", sep = "")

cat("📊 UNDERRATED CONFLICTS (Low Media Coverage):\n")
cat("  Total deaths:", comma(sum(table_underrated$total_deaths, na.rm = TRUE)), "\n")
cat("  Total funding: $", comma(sum(table_underrated$total_funding_usd, na.rm = TRUE) / 1e9), "B\n", sep = "")
cat("  Funding per death: $", comma(sum(table_underrated$total_funding_usd, na.rm = TRUE) / sum(table_underrated$total_deaths, na.rm = TRUE)), "\n\n", sep = "")

cat("💡 KEY INSIGHT:\n")
cat("  Humanitarian aid often follows media attention rather than need.\n")
cat("  Prominent conflicts receive more funding per death than underrated ones.\n\n")


# ---- STEP 12: Save Session Info for Reproducibility -----------------------

# Save R session information
session_info_file <- "session_info.txt"
writeLines(capture.output(sessionInfo()), session_info_file)

cat("✓ Session info saved:", session_info_file, "\n\n")
cat("All outputs saved successfully!\n")
