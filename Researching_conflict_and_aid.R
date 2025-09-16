getwd()

# ------------------------------------------------------------
# Conflict & Humanitarian Aid Analysis (2024)
# Author: Phoebe Lamb  |  MSBA @ Georgetown
# Repo: https://github.com/phoebelamb411/Conflict_and_Humanitarian_aid
# Outputs: prominent_conflicts.png, underrated_conflicts.png, session_info.txt
# NOTE: This is my learning/iteration script — leaving warnings + experiments in.
# ------------------------------------------------------------

# ===== 1) Libraries =====
# I kept this simple on purpose (no renv yet). If a pkg is missing, install it.
required <- c("tidyverse","readxl","janitor","scales","patchwork")
new <- required[!(required %in% installed.packages()[, "Package"])]
if (length(new)) install.packages(new)  # TODO: switch to renv later
invisible(lapply(required, library, character.only = TRUE))

# ===== 2) Files =====
# I started with a UCDP CSV at sub-national level and kept hitting name mismatches.
# For now I’m using regional Excel aggregates with fatalities by week (ACLED-style),
# and OCHA FTS plan-level funding. Both sit in the project root.
path_funding <- "ocha_funding.csv"  # cols: name, year, funding (messy strings)
paths_xlsx <- c(
  "Africa_aggregated_data_up_to-2025-08-30.xlsx",
  "Middle-East_aggregated_data_up_to-2025-08-30.xlsx",
  "Europe-Central-Asia_aggregated_data_up_to-2025-08-23.xlsx"
)
stopifnot(file.exists(path_funding))  # fails fast if I forgot to export the CSV

# ===== 3) Country groups + normalization =====
# These are the “story” buckets I chose after a lot of pivots.
prominent  <- c("Ukraine","Russia","Israel","West Bank & Gaza","Sudan","Syria")
underrated <- c("Ethiopia","Somalia","Democratic Republic of the Congo","Mali","Cameroon")

# NOTE: This looks verbose because I kept adding aliases whenever a new variant showed up.
normalize_country <- function(x){
  x <- stringr::str_trim(x)
  x <- stringr::str_replace_all(x, "\\s+", " ")
  dplyr::case_when(
    x %in% c("Ukraine","Ukraine (Govt)") ~ "Ukraine",
    x %in% c("Russia","Russian Federation") ~ "Russia",
    x %in% c("Israel","State of Israel") ~ "Israel",
    # Palestine / OPT variants — this mapping is for display, not politics
    x %in% c("Palestine","Palestine, State of","State of Palestine","Palestinian Territory",
             "West Bank and Gaza","West Bank & Gaza","Gaza Strip",
             "occupied Palestinian territory","OPT","oPt","oPt (occupied Palestinian territory)") ~ "West Bank & Gaza",
    x %in% c("Sudan","Republic of the Sudan") ~ "Sudan",
    x %in% c("Syria","Syrian Arab Republic") ~ "Syria",
    x %in% c("Ethiopia","Federal Democratic Republic of Ethiopia") ~ "Ethiopia",
    x %in% c("Somalia","Federal Republic of Somalia") ~ "Somalia",
    x %in% c("Congo, Dem. Rep.","DR Congo","DRC","Congo (DRC)",
             "Democratic Republic of the Congo","The Democratic Republic of the Congo") ~
      "Democratic Republic of the Congo",
    x %in% c("Mali","Republic of Mali") ~ "Mali",
    x %in% c("Cameroon","Republic of Cameroon") ~ "Cameroon",
    TRUE ~ x   # leave new names as-is so I can see them and add aliases later
  )
}

# Prefer 2024 if it exists; otherwise take the latest year present
pick_year <- function(y, prefer = 2024){
  yy <- suppressWarnings(as.integer(y)); yy <- yy[!is.na(yy)]
  if (length(yy) == 0) stop("No valid years in vector.")
  if (prefer %in% yy) prefer else max(yy)
}

# ===== 4) Deaths by country from regional aggregates (ACLED-style) =====
# Earlier attempt (kept for reference):
# ucdp <- read_csv("ucdp_conflict_deaths.csv")  # <- had territories like Crimea/Amhara, not pure countries
# RESULT: couldn’t align to my six “prominent” country labels. Pivoting to weekly aggregates.

library(readxl)

read_agg <- function(path){
  if (!file.exists(path)) return(tibble())  # harmless if a file is missing
  df <- suppressMessages(readxl::read_excel(path)) |> janitor::clean_names()
  
  # These files had: week, region, country, admin1, event_type, fatalities, ...
  if (!("country" %in% names(df)) || !("fatalities" %in% names(df))) {
    message("Skipping ", path, " — missing 'country' or 'fatalities'. Found: ",
            paste(names(df), collapse=", "))
    return(tibble())
  }
  
  # If 'year' missing, extract it from 'week' like "2024-W35" or "2025-08-30"
  if (!("year" %in% names(df))) {
    if ("week" %in% names(df)) {
      df <- df |>
        dplyr::mutate(week_chr = as.character(week),
                      year = suppressWarnings(as.integer(substr(week_chr, 1, 4))))
    } else {
      message("Skipping ", path, " — no 'year' or 'week' column.")
      return(tibble())
    }
  }
  
  df |>
    dplyr::transmute(
      country = normalize_country(as.character(country)),
      year    = suppressWarnings(as.integer(year)),
      deaths_num = suppressWarnings(as.numeric(fatalities))
    ) |>
    dplyr::filter(!is.na(country), !is.na(year), !is.na(deaths_num))
}

deaths_all <- purrr::map_dfr(paths_xlsx, read_agg)
stopifnot(nrow(deaths_all) > 0)  # if this triggers, I probably pointed at the wrong files

year_deaths <- pick_year(deaths_all$year, 2024)

# Aggregate to country-year
deaths <- deaths_all |>
  dplyr::filter(year == year_deaths) |>
  dplyr::group_by(country) |>
  dplyr::summarise(deaths = sum(deaths_num, na.rm = TRUE), .groups = "drop")

# Debug peek (I keep this on while iterating)
# print(deaths |> arrange(desc(deaths)) |> head(20))

# ===== 5) OCHA: funding by country (plan titles are messy) =====
fund_raw <- readr::read_csv(path_funding, show_col_types = FALSE) |> janitor::clean_names()
stopifnot(all(c("name","year","funding") %in% names(fund_raw)))

# NOTE: This peels country names out of things like:
# "Ukraine Humanitarian Needs and Response Plan 2024", "Mali Plan de Réponse Humanitaire 2024", etc.
country_from_plan <- function(x){
  x <- stringr::str_squish(x)
  x <- stringr::str_replace_all(x, "\\([^\\)]*\\)", "")  # remove (...) notes
  
  # EN phrases
  x <- stringr::str_remove(x, "\\bHumanitarian Needs and Response Plan\\b.*$")
  x <- stringr::str_remove(x, "\\bHumanitarian Response Plan\\b.*$")
  x <- stringr::str_remove(x, "\\bRegional Refugee and Resilience Plan\\b.*$")
  x <- stringr::str_remove(x, "\\bRegional Refugee Response Plan\\b.*$")
  x <- stringr::str_remove(x, "\\bSituation Regional Refugee Response Plan\\b.*$")
  x <- stringr::str_remove(x, "\\bFlash Appeal\\b.*$")
  x <- stringr::str_remove(x, "\\bJoint Response Plan\\b.*$")
  x <- stringr::str_remove(x, "\\bResponse Plan\\b.*$")
  
  # FR phrases
  x <- stringr::str_remove(x, "\\bPlan de Réponse Humanitaire\\b.*$")
  x <- stringr::str_remove(x, "\\bBesoins Humanitaires et Plan de Réponse\\b.*$")
  x <- stringr::str_remove(x, "\\bPlan de Réponse\\b.*$")
  
  # remove trailing years/hyphens
  x <- stringr::str_remove(x, "\\b\\d{4}(\\s*-\\s*\\d{4})?$")
  x <- stringr::str_squish(x)
  
  # French → English country names I hit in this file
  dplyr::recode(x,
                "République Démocratique du Congo" = "Democratic Republic of the Congo",
                "République Centrafricaine"        = "Central African Republic",
                "Haïti"                            = "Haiti",
                "Tchad"                            = "Chad",
                .default = x
  )
}

year_fund <- pick_year(fund_raw$year, 2024)

fund <- fund_raw |>
  dplyr::mutate(
    country_clean = country_from_plan(name),
    country       = normalize_country(country_clean),
    
    # NOTE: One row looked like '#value+funding+total+usd' → parse_number() complains.
    # I’m just scrubbing those to NA before summing.
    funding_chr   = as.character(funding),
    funding_chr   = ifelse(grepl("^#value", funding_chr, ignore.case = TRUE), NA, funding_chr),
    funding_num   = suppressWarnings(readr::parse_number(funding_chr))
  ) |>
  dplyr::filter(year == year_fund, !is.na(funding_num)) |>
  dplyr::group_by(country) |>
  dplyr::summarise(usd = sum(funding_num, na.rm = TRUE), .groups = "drop")

# Sanity check of what actually made it through:
# print(fund |> arrange(desc(usd)) |> head(20))

# ===== 6) Merge + build my two comparison tables =====
base <- deaths |>
  dplyr::full_join(fund, by = "country") |>
  dplyr::mutate(
    # I decided NA here means "not reported at plan level" — for the plot I’ll show 0-length bars.
    usd    = dplyr::if_else(is.na(usd) | usd == 0, NA_real_, usd),
    deaths = tidyr::replace_na(deaths, 0)
  )

tbl_prominent <- base |>
  dplyr::filter(country %in% prominent) |>
  dplyr::mutate(country = factor(country, levels = prominent))

tbl_underrated <- base |>
  dplyr::filter(country %in% underrated) |>
  dplyr::mutate(
    # Just a display tweak so the label isn’t huge on the plot
    country = dplyr::recode(country, "Democratic Republic of the Congo" = "DRC (beyond M23)"),
    country = factor(country, levels = c("DRC (beyond M23)","Mali","Ethiopia","Somalia","Cameroon"))
  )

# Hard stops I added after breaking this a few times
if (nrow(tbl_prominent) == 0) stop("No rows for prominent set — check name mappings or years.")
if (nrow(tbl_underrated) == 0) stop("No rows for underrated set — check name mappings or years.")

# ===== 7) Plot helpers =====
# lol this part took me forever… kept re-doing titles/colors/widths
muted_red <- "#d95f5f"   # muted red for prominent conflicts only
bar_w <- 0.6             # kept playing with width until it looked clean

# theme function I tweaked a bunch of times
theme_clean <- function(){
  ggplot2::theme_minimal(base_family = "Helvetica", base_size = 12) +
    ggplot2::theme(
      panel.grid.major.y = element_blank(),
      plot.title = element_text(face = "bold", size = 14, margin = margin(b = 6)),
      axis.title = element_text(size = 11),
      axis.text.y = element_text(margin = margin(r = 6)),
      plot.caption = element_text(size = 9, colour = "grey40")
    )
}

# deaths chart — finally just hardcoded "2024" so the title wouldn’t get cut off
plot_deaths <- function(df, bar_col = muted_red) {
  df |>
    dplyr::mutate(country_lab = pretty_label(as.character(country))) |>
    ggplot(aes(x = deaths, y = country_lab)) +
    geom_col(width = bar_w, fill = bar_col) +
    scale_x_continuous(
      labels = scales::label_number(accuracy = 1, scale_cut = scales::cut_short_scale())
    ) +
    labs(title = "Deaths (2024)", x = NULL, y = NULL) +
    theme_clean()
}

# aid chart — kept the N/A label thing because I liked how it looked
plot_aid <- function(df, bar_col = muted_red) {
  df2 <- df |>
    dplyr::mutate(
      country_lab = pretty_label(as.character(country)),
      usd_plot    = tidyr::replace_na(usd, 0),  # 0-width bar if missing
      show_na     = is.na(usd)                  # flag where to write "N/A"
    )
  
  ggplot(df2, aes(x = usd_plot, y = country_lab)) +
    geom_col(width = bar_w, fill = bar_col) +
    geom_text(
      data  = dplyr::filter(df2, show_na),
      aes(x = 0, y = country_lab, label = "N/A"),
      hjust = -0.15, vjust = 0.5, size = 3.4, colour = "grey40"
    ) +
    scale_x_continuous(
      labels = scales::label_dollar(scale = 1e-9, suffix = "B", accuracy = 0.1),
      expand = c(0.02, 0)
    ) +
    labs(title = "Humanitarian aid (USD, 2024)", x = NULL, y = NULL) +
    theme_clean()
}


# ===== 8) Export figures (prominent = muted red, underrated = grey) =====

# Prominent — keep the red focus like in my original PNG
p1 <- plot_deaths(tbl_prominent, "Deaths (", muted_red) +
  plot_aid(   tbl_prominent, "Humanitarian aid (USD, ", muted_red) +
  patchwork::plot_annotation(
    title   = paste0("Prominent Conflicts: Impact vs Humanitarian Funding (", year_deaths, ")"),
    theme   = theme_clean(),
    caption = caption_txt
  )
ggsave("prominent_conflicts.png", p1, width = 12, height = 8, dpi = 300)

# Underrated — keep it neutral
p2 <- plot_deaths(tbl_underrated, "Deaths (", muted_grey) +
  plot_aid(   tbl_underrated, "Humanitarian aid (USD, ", muted_grey) +
  patchwork::plot_annotation(
    title    = paste0("Underrated Conflicts: Deaths vs Humanitarian Aid (", year_deaths, ")"),
    subtitle = "Ethiopia, Cameroon, Somalia, DRC, Mali",
    theme    = theme_clean(),
    caption  = caption_txt
  )
ggsave("underrated_conflicts.png", p2, width = 12, height = 8, dpi = 300)


# ===== 9) Reproducibility (basic) =====
# I’m not using renv yet — just dumping session info so I can re-run this later on the same machine.
writeLines(capture.output(sessionInfo()), "session_info.txt")

# ===== 10) Optional: save the tables I actually plotted (helps reviewers) =====
# readr::write_csv(tbl_prominent |> dplyr::arrange(country),  "prominent_table.csv")
# readr::write_csv(tbl_underrated |> dplyr::arrange(country), "underrated_table.csv")

# END — if a warning pops up, I usually rerun the relevant block and add an alias above.
# TODO: Replace regional aggregates with a proper UCDP country-year extract when I have time.
