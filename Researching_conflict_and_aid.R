getwd()

# ------------------------------------------------------------
# Conflict & Humanitarian Aid (2024)
# Phoebe Lamb  |  MSBA @ Georgetown
# Repo: https://github.com/phoebelamb411/Conflict_and_Humanitarian_aid
# Exports: prominent_conflicts.png, underrated_conflicts.png, session_info.txt
# ------------------------------------------------------------

# 0) libraries ------------------------------------------------
need <- c("tidyverse","readxl","janitor","scales","patchwork")
new  <- need[!(need %in% rownames(installed.packages()))]
if (length(new)) install.packages(new)
invisible(lapply(need, library, character.only = TRUE))

# 1) data files I’m using ------------------------------------
# OCHA FTS export
path_funding <- "ocha_funding.csv"

# ACLED-style weekly aggregates I used to approximate deaths by country/year
paths_xlsx <- c(
  "Africa_aggregated_data_up_to-2025-08-30.xlsx",
  "Middle-East_aggregated_data_up_to-2025-08-30.xlsx",
  "Europe-Central-Asia_aggregated_data_up_to-2025-08-23.xlsx"
)

stopifnot(file.exists(path_funding))

# 2) groups & name normalizer -------------------------------
prominent  <- c("Ukraine","Russia","Israel","West Bank & Gaza","Sudan","Syria")
underrated <- c("Ethiopia","Cameroon","Somalia","Democratic Republic of the Congo","Mali")

normalize_country <- function(x){
  x <- as.character(x)
  x <- stringr::str_trim(stringr::str_replace_all(x, "\\s+", " "))
  dplyr::case_when(
    x %in% c("Ukraine","Ukraine (Govt)") ~ "Ukraine",
    x %in% c("Russia","Russian Federation") ~ "Russia",
    x %in% c("Israel","State of Israel") ~ "Israel",
    x %in% c("Palestine","Palestine, State of","State of Palestine",
             "Palestinian Territory","West Bank and Gaza","West Bank & Gaza",
             "Gaza Strip","occupied Palestinian territory","OPT","oPt",
             "oPt (occupied Palestinian territory)") ~ "West Bank & Gaza",
    x %in% c("Sudan","Republic of the Sudan") ~ "Sudan",
    x %in% c("Syria","Syrian Arab Republic") ~ "Syria",
    x %in% c("Ethiopia","Federal Democratic Republic of Ethiopia") ~ "Ethiopia",
    x %in% c("Somalia","Federal Republic of Somalia") ~ "Somalia",
    x %in% c("Congo, Dem. Rep.","DR Congo","DRC","Congo (DRC)",
             "Democratic Republic of the Congo","The Democratic Republic of the Congo") ~
      "Democratic Republic of the Congo",
    x %in% c("Mali","Republic of Mali") ~ "Mali",
    x %in% c("Cameroon","Republic of Cameroon") ~ "Cameroon",
    TRUE ~ x
  )
}

pick_year <- function(y, prefer=2024){
  yy <- suppressWarnings(as.integer(y)); yy <- yy[!is.na(yy)]
  if (!length(yy)) stop("No valid years found.")
  if (prefer %in% yy) prefer else max(yy)
}

# 3) deaths from regional XLSX (ACLED-style) -----------------
read_agg <- function(path){
  if (!file.exists(path)) return(tibble())
  df <- suppressMessages(readxl::read_excel(path)) |> janitor::clean_names()
  
  # I only need country, year (or week), fatalities
  if (!("country" %in% names(df)) || !("fatalities" %in% names(df))) return(tibble())
  
  if (!("year" %in% names(df))) {
    if ("week" %in% names(df)) {
      df <- df |>
        dplyr::mutate(week_chr = as.character(week),
                      year = suppressWarnings(as.integer(substr(week_chr,1,4))))
    } else {
      return(tibble())
    }
  }
  
  df |>
    dplyr::transmute(
      country    = normalize_country(country),
      year       = suppressWarnings(as.integer(year)),
      deaths_num = suppressWarnings(as.numeric(fatalities))
    ) |>
    dplyr::filter(!is.na(country), !is.na(year), !is.na(deaths_num))
}

deaths_all <- purrr::map_dfr(paths_xlsx, read_agg)
stopifnot(nrow(deaths_all) > 0)

year_deaths <- pick_year(deaths_all$year, 2024)

deaths <- deaths_all |>
  dplyr::filter(year == year_deaths) |>
  dplyr::group_by(country) |>
  dplyr::summarise(deaths = sum(deaths_num, na.rm = TRUE), .groups="drop")

# 4) OCHA funding (country pulled from plan titles) ----------
country_from_plan <- function(x){
  x <- stringr::str_squish(as.character(x))
  x <- stringr::str_replace_all(x, "\\([^\\)]*\\)", "")
  x <- stringr::str_remove(x, "\\bHumanitarian Needs and Response Plan\\b.*$")
  x <- stringr::str_remove(x, "\\bHumanitarian Response Plan\\b.*$")
  x <- stringr::str_remove(x, "\\bRegional Refugee and Resilience Plan\\b.*$")
  x <- stringr::str_remove(x, "\\bRegional Refugee Response Plan\\b.*$")
  x <- stringr::str_remove(x, "\\bSituation Regional Refugee Response Plan\\b.*$")
  x <- stringr::str_remove(x, "\\bFlash Appeal\\b.*$")
  x <- stringr::str_remove(x, "\\bJoint Response Plan\\b.*$")
  x <- stringr::str_remove(x, "\\bResponse Plan\\b.*$")
  x <- stringr::str_remove(x, "\\bPlan de Réponse Humanitaire\\b.*$")
  x <- stringr::str_remove(x, "\\bBesoins Humanitaires et Plan de Réponse\\b.*$")
  x <- stringr::str_remove(x, "\\bPlan de Réponse\\b.*$")
  x <- stringr::str_remove(x, "\\b\\d{4}(\\s*-\\s*\\d{4})?$")
  x <- dplyr::recode(x,
                     "République Démocratique du Congo" = "Democratic Republic of the Congo",
                     "République Centrafricaine"        = "Central African Republic",
                     "Haïti"                            = "Haiti",
                     "Tchad"                            = "Chad",
                     .default = x)
  x
}

fund_raw <- readr::read_csv(path_funding, show_col_types = FALSE) |> janitor::clean_names()
stopifnot(all(c("name","year","funding") %in% names(fund_raw)))

year_fund <- pick_year(fund_raw$year, 2024)

fund <- fund_raw |>
  dplyr::mutate(
    country_clean = country_from_plan(name),
    country       = normalize_country(country_clean),
    funding_chr   = as.character(funding),
    funding_chr   = ifelse(grepl("^#value", funding_chr, ignore.case=TRUE), NA, funding_chr),
    funding_num   = suppressWarnings(readr::parse_number(funding_chr))
  ) |>
  dplyr::filter(year == year_fund, !is.na(funding_num)) |>
  dplyr::group_by(country) |>
  dplyr::summarise(usd = sum(funding_num, na.rm = TRUE), .groups="drop")

# 5) join + tables in the order I presented ------------------
base <- deaths |>
  dplyr::full_join(fund, by="country") |>
  dplyr::mutate(usd = dplyr::if_else(is.na(usd) | usd == 0, NA_real_, usd),
                deaths = tidyr::replace_na(deaths, 0))

# order in the final plot (top→bottom) to match my original images
prom_order <- c("Syria","Sudan","Israel","West Bank & Gaza","Russia","Ukraine")
und_order  <- c("Cameroon","Somalia","Ethiopia","Mali","Democratic Republic of the Congo")

tbl_prominent <- base |>
  dplyr::filter(country %in% prominent) |>
  dplyr::mutate(country = factor(country, levels = prom_order)) |>
  dplyr::arrange(country)

tbl_underrated <- base |>
  dplyr::filter(country %in% underrated) |>
  dplyr::mutate(country = factor(country, levels = und_order)) |>
  dplyr::arrange(country)

stopifnot(nrow(tbl_prominent)>0, nrow(tbl_underrated)>0)

# 6) plotting helpers (kept these tiny & opinionated) --------
muted_red <- "#C65D5D"      # hard-code so it never turns orange
charcoal  <- "#3A3A3A"
bar_w     <- 0.45

theme_clean <- function(){
  ggplot2::theme_minimal(base_size = 12) +
    ggplot2::theme(
      panel.grid.major.y = element_blank(),
      axis.text.y  = element_text(margin = margin(r=6)),
      plot.title   = element_text(face="bold", size=16, margin=margin(b=6)),
      plot.caption = element_text(size=9, colour="grey40")
    )
}

# the labels I actually showed
pretty_label_prom <- function(x){
  x <- as.character(x)
  dplyr::recode(x,
                "West Bank & Gaza" = "W.B. & Gaza",
                .default = x
  )
}

pretty_label_und <- function(x){
  x <- as.character(x)
  dplyr::recode(x,
                "Cameroon"                            = "Cameroon / Lake Chad belt",
                "Somalia"                             = "Somalia (Al-Shabab insurgency)",
                "Ethiopia"                            = "Ethiopia (post-Tigray & regional)",
                "Mali"                                = "Mali (insurgency)",
                "Democratic Republic of the Congo"    = "DRC (beyond M23)",
                .default = x
  )
}

# deaths (prom & und share same code except labels & color)
plot_deaths_prom <- function(df){
  df |>
    dplyr::mutate(country_lab = pretty_label_prom(country)) |>
    ggplot(aes(x = deaths, y = country_lab)) +
    geom_col(width = bar_w, fill = muted_red) +
    scale_x_continuous(labels = label_number(accuracy = 1, scale_cut = cut_short_scale())) +
    labs(title = "Deaths (2024)", x = NULL, y = NULL) +
    theme_clean()
}

plot_deaths_und <- function(df){
  df |>
    dplyr::mutate(country_lab = pretty_label_und(country)) |>
    ggplot(aes(x = deaths, y = country_lab)) +
    geom_col(width = bar_w, fill = charcoal) +
    scale_x_continuous(labels = label_number(accuracy = 1, scale_cut = cut_short_scale())) +
    labs(title = "Conflict deaths (2024)", x = NULL, y = NULL) +
    theme_clean()
}

# aid charts (show "N/A" where usd is missing)
plot_aid_prom <- function(df){
  df2 <- df |>
    dplyr::mutate(
      country_lab = pretty_label_prom(country),
      usd_plot    = tidyr::replace_na(usd, 0),
      show_na     = is.na(usd)
    )
  ggplot(df2, aes(x = usd_plot, y = country_lab)) +
    geom_col(width = bar_w, fill = muted_red) +
    geom_text(
      data  = dplyr::filter(df2, show_na),
      aes(x = 0, y = country_lab, label = "N/A"),
      hjust = -0.15, vjust = 0.5, size = 3.4, colour = "grey40"
    ) +
    scale_x_continuous(labels = label_dollar(scale = 1e-9, suffix = "B", accuracy = 0.1),
                       expand = c(0.02, 0)) +
    labs(title = "Humanitarian aid (USD, 2024)", x = NULL, y = NULL) +
    theme_clean()
}

plot_aid_und <- function(df){
  df |>
    dplyr::mutate(
      country_lab = pretty_label_und(country),
      usd_plot    = tidyr::replace_na(usd, 0)
    ) |>
    ggplot(aes(x = usd_plot, y = country_lab)) +
    geom_col(width = bar_w, fill = charcoal) +
    scale_x_continuous(labels = label_dollar(scale = 1e-9, suffix = "B", accuracy = 0.1)) +
    labs(title = "Humanitarian aid (USD, 2024)", x = NULL, y = NULL) +
    theme_clean()
}

caption_txt <- paste0(
  "Sources: UCDP/ACLED-style regional aggregates (fatalities by country-year); ",
  "OCHA FTS (country plans). Years — deaths: ", year_deaths, "; funding: ", year_fund,
  ". ‘N/A’ shown where plan-level funding wasn’t reported."
)

# 7) export pngs (same sizes I used before) ------------------
p1 <- plot_deaths_prom(tbl_prominent) + plot_aid_prom(tbl_prominent) +
  patchwork::plot_annotation(
    title   = paste0("Prominent Conflicts: Impact vs Humanitarian Funding (", year_deaths, "/", year_fund, ")"),
    theme   = theme_clean(),
    caption = caption_txt
  )
ggsave("prominent_conflicts.png", p1, width = 13, height = 8, dpi = 300)

p2 <- plot_deaths_und(tbl_underrated) + plot_aid_und(tbl_underrated) +
  patchwork::plot_annotation(
    title    = paste0("Underrated Conflicts: Deaths vs Humanitarian Aid (", year_deaths, "/", year_fund, ")"),
    subtitle = "Ethiopia, Cameroon, Somalia, DRC, Mali",
    theme    = theme_clean(),
    caption  = caption_txt
  )
ggsave("underrated_conflicts.png", p2, width = 13, height = 8, dpi = 300)

# 8) reproducibility -----------------------------------------
writeLines(capture.output(sessionInfo()), "session_info.txt")
