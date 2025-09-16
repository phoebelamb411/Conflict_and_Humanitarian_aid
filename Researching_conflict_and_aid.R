getwd()

# ------------------------------------------------------------
# Conflict & Humanitarian Aid (recreate LinkedIn charts 1:1)
# Author: Phoebe Lamb  |  MSBA @ Georgetown
# Repo: https://github.com/phoebelamb411/Conflict_and_Humanitarian_aid
# Outputs: prominent_conflicts.png, underrated_conflicts.png, session_info.txt
# Notes: keep it simple (CSV -> clean -> plot). trial & error left in comments.
# ------------------------------------------------------------

# ===== 1) Libraries (install if missing) =====
required <- c("tidyverse", "janitor", "scales", "patchwork")
new <- required[!(required %in% installed.packages()[, "Package"])]
if (length(new)) install.packages(new)
invisible(lapply(required, library, character.only = TRUE))

# ===== 2) File paths (CSV only to match the public charts) =====
path_deaths  <- "ucdp_conflict_deaths.csv"  # has territory_name, year, bd_best
path_funding <- "ocha_funding.csv"          # has name, year, funding
stopifnot(file.exists(path_deaths), file.exists(path_funding))

# ===== 3) Countries & order exactly like the LinkedIn version =====
prominent <- c("Syria", "Sudan", "Israel", "West Bank & Gaza", "Russia", "Ukraine")
underrated <- c(
  "DRC (beyond M23)",
  "Mali (insurgency)",
  "Ethiopia (post-Tigray & regional)",
  "Somalia (Al-Shabab insurgency)",
  "Cameroon / Lake Chad belt"
)

# ===== 4) Name helpers (I tried a few mappings, kept the ones that mattered) =====
normalize_country <- function(x){
  x <- stringr::str_trim(as.character(x))
  x <- stringr::str_replace_all(x, "\\s+", " ")
  dplyr::case_when(
    x %in% c("Ukraine") ~ "Ukraine",
    x %in% c("Russia", "Russian Federation") ~ "Russia",
    x %in% c("Israel", "State of Israel") ~ "Israel",
    x %in% c("Palestine", "Palestine, State of", "State of Palestine",
             "Palestinian Territory", "West Bank and Gaza", "West Bank & Gaza",
             "Gaza Strip", "occupied Palestinian territory", "OPT", "oPt") ~ "West Bank & Gaza",
    x %in% c("Sudan","Republic of the Sudan") ~ "Sudan",
    x %in% c("Syria","Syrian Arab Republic") ~ "Syria",
    TRUE ~ x
  )
}

# pretty axis labels I used in the LinkedIn plots for the underrated set
pretty_label_underrated <- function(x){
  x <- as.character(x)
  dplyr::recode(
    x,
    "Democratic Republic of the Congo" = "DRC (beyond M23)",
    "Mali"      = "Mali (insurgency)",
    "Ethiopia"  = "Ethiopia (post-Tigray & regional)",
    "Somalia"   = "Somalia (Al-Shabab insurgency)",
    "Cameroon"  = "Cameroon / Lake Chad belt",
    .default = x
  )
}

# ===== 5) Pick year (prefer 2024) =====
pick_year <- function(y, prefer = 2024){
  yy <- suppressWarnings(as.integer(y)); yy <- yy[!is.na(yy)]
  if (length(yy) == 0) stop("No valid years found.")
  if (prefer %in% yy) prefer else max(yy)
}

# ===== 6) UCDP deaths (CSV) =====
deaths_raw <- readr::read_csv(path_deaths, show_col_types = FALSE) |> janitor::clean_names()
stopifnot(all(c("territory_name","year","bd_best") %in% names(deaths_raw)))

year_deaths <- pick_year(deaths_raw$year, 2024)

deaths <- deaths_raw |>
  dplyr::transmute(
    country = normalize_country(territory_name),
    year = year,
    deaths = suppressWarnings(as.numeric(bd_best))
  ) |>
  dplyr::filter(year == year_deaths, !is.na(country)) |>
  dplyr::group_by(country) |>
  dplyr::summarise(deaths = sum(deaths, na.rm = TRUE), .groups = "drop")

# ===== 7) OCHA funding (CSV) =====
fund_raw <- readr::read_csv(path_funding, show_col_types = FALSE) |> janitor::clean_names()
stopifnot(all(c("name","year","funding") %in% names(fund_raw)))

# strip plan text to get a country-ish string, then normalize
country_from_plan <- function(x){
  x <- stringr::str_squish(as.character(x))
  x <- stringr::str_replace_all(x, "\\([^\\)]*\\)", "") # remove (..)
  # EN
  x <- stringr::str_remove(x, "\\bHumanitarian Needs and Response Plan\\b.*$")
  x <- stringr::str_remove(x, "\\bHumanitarian Response Plan\\b.*$")
  x <- stringr::str_remove(x, "\\bRegional Refugee and Resilience Plan\\b.*$")
  x <- stringr::str_remove(x, "\\bRegional Refugee Response Plan\\b.*$")
  x <- stringr::str_remove(x, "\\bFlash Appeal\\b.*$")
  x <- stringr::str_remove(x, "\\bJoint Response Plan\\b.*$")
  x <- stringr::str_remove(x, "\\bResponse Plan\\b.*$")
  # FR
  x <- stringr::str_remove(x, "\\bPlan de Réponse Humanitaire\\b.*$")
  x <- stringr::str_remove(x, "\\bBesoins Humanitaires et Plan de Réponse\\b.*$")
  x <- stringr::str_remove(x, "\\bPlan de Réponse\\b.*$")
  # year tails
  x <- stringr::str_remove(x, "\\b\\d{4}(\\s*-\\s*\\d{4})?$")
  x <- stringr::str_squish(x)
  # French->English country name tweaks (just the ones I hit)
  dplyr::recode(
    x,
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
    country = normalize_country(country_clean),
    funding_chr = as.character(funding),
    funding_chr = ifelse(grepl("^#value", funding_chr, ignore.case = TRUE), NA, funding_chr),
    usd = suppressWarnings(readr::parse_number(funding_chr))
  ) |>
  dplyr::filter(year == year_fund) |>
  dplyr::group_by(country) |>
  dplyr::summarise(usd = sum(usd, na.rm = TRUE), .groups = "drop") |>
  dplyr::mutate(usd = dplyr::if_else(usd == 0, NA_real_, usd)) # keep NA to show "N/A" text

# ===== 8) Merge for the two views =====
base <- deaths |>
  dplyr::full_join(fund, by = "country") |>
  dplyr::mutate(deaths = tidyr::replace_na(deaths, 0))

# Prominent (fixed order)
tbl_prominent <- base |>
  dplyr::filter(country %in% prominent) |>
  dplyr::mutate(country = factor(country, levels = prominent))

# Underrated (map labels + fixed order)
tbl_underrated <- base |>
  dplyr::filter(country %in% c("Democratic Republic of the Congo","Mali","Ethiopia","Somalia","Cameroon")) |>
  dplyr::mutate(
    country_lab = pretty_label_underrated(country),
    country_lab = factor(country_lab, levels = underrated)
  )

# ===== 9) Plot helpers (kept the “muted red, narrow bars, N/A text” look) =====
muted_red <- "#d95f02"  # the soft red you used publicly
bar_w     <- 0.5        # narrower bars like your screenshot

theme_clean <- function(){
  ggplot2::theme_minimal(base_family = "Helvetica", base_size = 12) +
    ggplot2::theme(
      panel.grid.major.y = element_blank(),
      axis.title = element_text(size = 11),
      axis.text.y = element_text(margin = margin(r = 6))
    )
}

# deaths (color configurable so we can reuse for prominent/underrated)
plot_deaths <- function(df, y_var, title_txt, fill_col){
  ggplot(df, aes(x = deaths, y = !!y_var)) +
    geom_col(width = bar_w, fill = fill_col) +
    scale_x_continuous(labels = label_number(accuracy = 1, scale_cut = cut_short_scale())) +
    labs(title = title_txt, x = NULL, y = NULL) +
    theme_clean()
}

# aid (shows N/A text for missing bars)
plot_aid <- function(df, y_var, title_txt, fill_col){
  df2 <- df |>
    dplyr::mutate(usd_plot = tidyr::replace_na(usd, 0),
                  show_na = is.na(usd))
  ggplot(df2, aes(x = usd_plot, y = !!y_var)) +
    geom_col(width = bar_w, fill = fill_col) +
    geom_text(
      data = dplyr::filter(df2, show_na),
      aes(x = 0, y = !!y_var, label = "N/A"),
      hjust = -0.15, vjust = 0.5, size = 3.4, colour = "grey40"
    ) +
    scale_x_continuous(
      labels = label_dollar(scale = 1e-9, suffix = "B", accuracy = 0.1),
      expand = c(0.04, 0)  # small left pad so "N/A" isn’t clipped
    ) +
    labs(title = title_txt, x = NULL, y = NULL) +
    theme_clean()
}

caption_txt <- "Sources: UCDP (bd_best), OCHA FTS (country plans). Years — deaths: 2024; funding: 2024. ‘N/A’ shown where plan-level funding wasn’t reported."

# ===== 10) Prominent figure (muted red) =====
p1_left  <- plot_deaths(tbl_prominent, rlang::sym("country"), "Deaths (2024)", muted_red)
p1_right <- plot_aid(   tbl_prominent, rlang::sym("country"), "Humanitarian aid (USD, 2024)", muted_red)

p1 <- p1_left + p1_right +
  patchwork::plot_annotation(
    title = "Prominent Conflicts: Impact vs Humanitarian Funding (2024/2024)",
    theme = ggplot2::theme(plot.title = element_text(face = "bold", size = 14)),
    caption = caption_txt
  )
ggsave("prominent_conflicts.png", p1, width = 13, height = 8, dpi = 300)

# ===== 11) Underrated figure (neutral grey) =====
grey_bar <- "grey25"

p2_left  <- plot_deaths(tbl_underrated, rlang::sym("country_lab"), "Conflict deaths (2024)", grey_bar)
p2_right <- plot_aid(   tbl_underrated, rlang::sym("country_lab"), "Humanitarian aid (USD, 2024)", grey_bar)

p2 <- p2_left + p2_right +
  patchwork::plot_annotation(
    title = "Underrated Conflicts: Deaths vs Humanitarian Aid (2024)",
    subtitle = "Ethiopia, Cameroon, Somalia, DRC, Mali",
    theme = ggplot2::theme(plot.title = element_text(face = "bold", size = 14)),
    caption = caption_txt
  )
ggsave("underrated_conflicts.png", p2, width = 13, height = 8, dpi = 300)

# ===== 12) Reproducibility =====
writeLines(capture.output(sessionInfo()), "session_info.txt")
