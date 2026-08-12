
# =============================================================================
# global.R
#
# Part of the global.R / ui.R / server.R Shiny app split (previously a single
# app.R). global.R is sourced once per R process at app startup, before both
# ui.R and server.R, and its top-level objects are visible to both. It holds:
#   - package loads
#   - DB configuration + connection helpers
#   - DB reads and data wrangling (db_ithamaps_entries, db_hcp_per_region)
#   - dropdown option lookups (Resolution, Country, Measure, Cause, Metric, ...)
#   - spatial layer caching (adm0/1/2, simplified display geometry)
#   - prediction-mode raster assets
#   - query parsing/filtering/aggregation helpers (Parse, Extract, Search,
#     build_query_bundle, compute_outlier_aware_metric, etc.), which are called
#     per-session from server.R
#
# ui.R and server.R contain no shinyApp() call themselves; when a directory has
# global.R + ui.R + server.R (and no app.R), shiny::runApp() sources global.R,
# then ui.R (expects a `ui` object) and server.R (expects a `server` function),
# and wires them together automatically.
# =============================================================================

library(DT)
library(sf)
library(dplyr)
library(tidyr)
library(bslib)
library(shiny)
library(readxl)
library(stringr)
library(metafor)
library(leaflet)
library(leaflet.extras)
library(viridis)
library(RMariaDB)
library(ggplot2)
library(cowplot)
library(webshot2)
library(shinycssloaders)

# ---------------------------------------------------------------------------
# pick_configuration(): select row from User_Configuration.xlsx
#   - env var ITHAMAPS_MACHINE selects the row; falls back to first row
#   - env vars DB_USER / DB_PASSWORD / ITHAMAPS_DB_HOST / ITHAMAPS_DB_PORT
#     override xlsx values when set
#   - env vars DB_USER_FILE / DB_PASSWORD_FILE can point to mounted secret files
#     and take precedence over plain env vars
# ---------------------------------------------------------------------------
read_secret_or_env = function(value_key, file_key) {
  file_path = Sys.getenv(file_key, unset = "")
  if (nchar(file_path) > 0 && file.exists(file_path)) {
    value = readLines(file_path, warn = FALSE, n = 1)
    if (length(value) > 0 && nchar(value[1]) > 0) {
      return(trimws(value[1]))
    }
  }
  Sys.getenv(value_key, unset = "")
}

pick_configuration = function() {
  cfg = read_xlsx("User_Configuration.xlsx")
  machine_env = Sys.getenv("ITHAMAPS_MACHINE", unset = "")
  if (nchar(machine_env) > 0 && machine_env %in% cfg$machine) {
    row = cfg %>%
      filter(machine == machine_env) %>%
      slice(1)
  } else {
    row = cfg %>% slice(1)
  }
  host_env = Sys.getenv("ITHAMAPS_DB_HOST", unset = "")
  if (nchar(host_env) > 0) row$host = host_env
  port_env = Sys.getenv("ITHAMAPS_DB_PORT", unset = "")
  if (nchar(port_env) > 0) row$port = as.integer(port_env)
  user_env = read_secret_or_env("DB_USER", "DB_USER_FILE")
  if (nchar(user_env) > 0) row$username = user_env
  pass_env = read_secret_or_env("DB_PASSWORD", "DB_PASSWORD_FILE")
  if (nchar(pass_env) > 0) row$password = pass_env
  row
}

scalar_text = function(value, field_name) {
  out = as.character(value[[1]])
  out = trimws(out)
  if (length(out) != 1 || is.na(out) || nchar(out) == 0) {
    stop(
      paste0(
        "Invalid DB configuration field: ", field_name,
        ". Provide it in User_Configuration.xlsx or override via env/secrets."
      ),
      call. = FALSE
    )
  }
  out
}

scalar_port = function(value, field_name = "port") {
  out = suppressWarnings(as.integer(value[[1]]))
  if (length(out) != 1 || is.na(out) || out <= 0) {
    stop(
      paste0(
        "Invalid DB configuration field: ", field_name,
        ". Must be a positive integer."
      ),
      call. = FALSE
    )
  }
  out
}

Configuration = pick_configuration()

read_db_prefixes = function(path = "secrets/db_prefix") {
  defaults = list(
    ithabase_prefix = "_live",
    joomla_prefix = "_live"
  )

  if (!file.exists(path)) {
    return(defaults)
  }

  lines = readLines(path, warn = FALSE)
  lines = trimws(lines)
  lines = lines[nzchar(lines)]
  lines = lines[!startsWith(lines, "#")]

  Reduce(function(acc, line) {
    parts = strsplit(line, "=", fixed = TRUE)[[1]]
    if (length(parts) < 2) {
      return(acc)
    }
    key = trimws(parts[1])
    value = trimws(paste(parts[-1], collapse = "="))
    value = gsub('^"|"$', "", value)
    value = gsub("^'|'$", "", value)
    if (key %in% names(acc) && nzchar(value)) {
      acc[[key]] = value
    }
    acc
  }, lines, init = defaults)
}

db_prefixes = read_db_prefixes()
ithanet_site_root = Sys.getenv("ITHAMAPS_ITHA_ROOT", unset = "http://localhost/live-ithanet-j4")
ithanet_dbname = paste0("ithabase", db_prefixes$ithabase_prefix)
joomla_dbname = paste0("joomla", db_prefixes$joomla_prefix)

# ---------------------------------------------------------------------------
# open_mariadb_connection(): connect using correct RMariaDB argument names
# ---------------------------------------------------------------------------
open_mariadb_connection = function(dbname, cfg) {
  user_val = scalar_text(cfg$username, "username")
  pass_val = scalar_text(cfg$password, "password")
  host_val = scalar_text(cfg$host, "host")
  port_val = scalar_port(cfg$port, "port")
  dbConnect(RMariaDB::MariaDB(),
    dbname   = dbname,
    user     = user_val,
    password = pass_val,
    host     = host_val,
    port     = port_val
  )
}

parse_set_column_levels = function(column_type) {
  # Parse MariaDB/MySQL COLUMN_TYPE strings like: set('A','B','C')
  matches = regmatches(
    column_type,
    gregexpr("'((?:''|[^'])*)'", column_type, perl = TRUE)
  )[[1]]
  if (length(matches) == 0 || (length(matches) == 1 && identical(matches[[1]], ""))) {
    return(character())
  }
  levels = gsub("^'|'$", "", matches)
  gsub("''", "'", levels, fixed = TRUE)
}

get_set_column_options = function(con, dbname, table_names) {
  if (length(table_names) == 0) {
    return(list())
  }

  quoted_tables = paste(DBI::dbQuoteString(con, table_names), collapse = ",")
  quoted_db = DBI::dbQuoteString(con, dbname)
  sql = paste0(
    "SELECT TABLE_NAME, COLUMN_NAME, COLUMN_TYPE ",
    "FROM information_schema.COLUMNS ",
    "WHERE TABLE_SCHEMA = ", quoted_db, " ",
    "AND TABLE_NAME IN (", quoted_tables, ") ",
    "AND DATA_TYPE = 'set'"
  )

  info = DBI::dbGetQuery(con, sql)
  if (nrow(info) == 0) {
    return(list())
  }

  names(info) = tolower(names(info))
  by_table = split(info, info$table_name)
  lapply(by_table, function(tbl) {
    stats::setNames(
      lapply(tbl$column_type, parse_set_column_levels),
      tbl$column_name
    )
  })
}

apply_set_factors = function(df, table_options) {
  out = df

  for (col_name in names(table_options)) {
    if (!(col_name %in% names(out))) {
      next
    }
    levels = table_options[[col_name]]
    values = as.character(out[[col_name]])
    if (any(!is.na(values) & values == "")) {
      levels = unique(c(levels, ""))
    }
    # Preserve unexpected concrete values (e.g. combined SET payloads) rather
    # than coercing them to NA; schema-declared levels remain first.
    observed = unique(values[!is.na(values)])
    extras = setdiff(observed, levels)
    if (length(extras) > 0) {
      levels = c(levels, extras)
    }
    out[[col_name]] = factor(values, levels = levels)
  }

  out
}

add_explicit_na_level = function(x, na_level = "Unspecified") {
  if (!is.factor(x)) {
    return(x)
  }
  lvls = levels(x)
  if (!(na_level %in% lvls)) {
    levels(x) = c(lvls, na_level)
  }
  x[is.na(x)] = na_level
  x
}

read_table_with_set_factors = function(con, table_name, set_options_by_table) {
  out = dbReadTable(con, table_name) %>% as_tibble()
  table_options = set_options_by_table[[table_name]]
  if (is.null(table_options) || length(table_options) == 0) {
    return(out)
  }

  # Convert SET columns post-read, so table ingestion remains dataframe-first.
  apply_set_factors(out, table_options)
}

# Connection to ITHANET
Ithanet = open_mariadb_connection(ithanet_dbname, Configuration)

Datatables = dbListTables(Ithanet)
Datatables = Datatables[Datatables %in% c(
  "country",
  "locus",
  "globin_phenotypes",
  "ithamaps_accumulated_sources",
  "regions",
  "measure",
  "metric",
  "sex_groups",
  "age_groups",
  "ethnicities",
  "religions",
  "ithamaps_cohort",
  "cause",
  "hc_policies",
  "hcp_per_region",
  "ithamaps_entries",
  "ithamaps_log",
  "criteria",
  "criteria_responses",
  "j_criteria",
  "ithagenes_common",
  "ithagenes_globin_phen"
)]

set_options = get_set_column_options(Ithanet, ithanet_dbname, Datatables)

list2env(
  setNames(
    lapply(Datatables, function(Data) read_table_with_set_factors(Ithanet, Data, set_options)),
    paste0("db_", Datatables)
  ),
  envir = .GlobalEnv
)

dbDisconnect(Ithanet)

rm(Ithanet, Datatables, set_options)

# Connection to joomla
Joomla = open_mariadb_connection(joomla_dbname, Configuration)

Datatables = dbListTables(Joomla)
Datatables = Datatables[Datatables %in% c("itha_experts")]

set_options = get_set_column_options(Joomla, joomla_dbname, Datatables)

list2env(
  setNames(
    lapply(Datatables, function(Data) read_table_with_set_factors(Joomla, Data, set_options)),
    paste0("db_", Datatables)
  ),
  envir = .GlobalEnv
)

dbDisconnect(Joomla)

rm(Joomla, Datatables, set_options)

# ---------------------------------------------------------------------------
# Join diagnostics: export and silence expected many-to-many joins
#   - writes summary and key-level diagnostics for detected many-to-many joins
#   - uses relationship='many-to-many' only when detected to avoid noisy warnings
# ---------------------------------------------------------------------------
join_log_dir = Sys.getenv("ITHAMAPS_JOIN_LOG_DIR", unset = "logs")
dir.create(join_log_dir, recursive = TRUE, showWarnings = FALSE)
join_log_summary_path = file.path(join_log_dir, "many_to_many_join_summary.csv")
join_log_keys_path = file.path(join_log_dir, "many_to_many_join_keys.csv")

append_csv_row = function(path, df_row) {
  needs_header = !file.exists(path)
  utils::write.table(
    df_row,
    file = path,
    sep = ",",
    row.names = FALSE,
    col.names = needs_header,
    append = !needs_header,
    qmethod = "double"
  )
}

log_many_to_many_join = function(x, y, by, join_name) {
  by_cols = if (is.character(by)) by else names(by)

  left_dups = x %>%
    dplyr::count(dplyr::across(dplyr::all_of(by_cols)), name = "left_n") %>%
    dplyr::filter(left_n > 1)

  right_dups = y %>%
    dplyr::count(dplyr::across(dplyr::all_of(by_cols)), name = "right_n") %>%
    dplyr::filter(right_n > 1)

  m2m_keys = dplyr::inner_join(left_dups, right_dups, by = by_cols)
  has_m2m = nrow(m2m_keys) > 0

  if (has_m2m) {
    timestamp = format(Sys.time(), "%Y-%m-%d %H:%M:%S")
    summary_row = data.frame(
      timestamp = timestamp,
      join_name = join_name,
      by_columns = paste(by_cols, collapse = "|"),
      left_rows = nrow(x),
      right_rows = nrow(y),
      left_duplicated_keys = nrow(left_dups),
      right_duplicated_keys = nrow(right_dups),
      many_to_many_keys = nrow(m2m_keys),
      stringsAsFactors = FALSE
    )
    append_csv_row(join_log_summary_path, summary_row)

    key_rows = m2m_keys %>%
      dplyr::mutate(
        timestamp = timestamp,
        join_name = join_name,
        .before = 1
      )
    append_csv_row(join_log_keys_path, key_rows)
  }

  has_m2m
}

left_join_logged = function(x, y, by, join_name) {
  if (log_many_to_many_join(x, y, by = by, join_name = join_name)) {
    dplyr::left_join(x, y, by = by, relationship = "many-to-many")
  } else {
    dplyr::left_join(x, y, by = by)
  }
}

# Fetch data
Note_function = function(end_year_assumed, region_comment, nationality_comment, comments, sample_size_comment) {
  parts = c(
    if (!is.na(end_year_assumed)) end_year_assumed,
    if (!is.na(region_comment)) region_comment,
    if (!is.na(nationality_comment)) nationality_comment,
    if (!is.na(comments)) comments,
    if (!is.na(sample_size_comment)) sample_size_comment
  )
  if (length(parts) > 0) paste(parts, collapse = ", ") else "None"
}

db_ithamaps_entries = db_ithamaps_entries %>%
  select(-starts_with("v_")) %>%
  rename("phen_id" = globin_phenotype) %>%
  left_join(db_globin_phenotypes %>%
    rename(
      "phen_id" = id,
      "globin_phenotype" = name
    ) %>%
    select(phen_id, globin_phenotype), by = "phen_id") %>%
  left_join(db_regions, by = "regions_id") %>%
  left_join(db_measure, by = "measure_id") %>%
  left_join(db_cause, by = "cause_id") %>%
  # metric_id is a FK into the `metric` table, which records the UNIT the value
  # is stored in (e.g. percent, per 100000). Rename it to `metric_unit` so it is
  # never confused with the Joomla "Metric" summary-statistic parameter/column.
  left_join(db_metric %>% rename(metric_unit = metric_name), by = "metric_id") %>%
  left_join(db_ithamaps_cohort %>%
    select(-starts_with("v_")) %>%
    rename(
      "cohort_id" = cid,
      "source_id" = source,
      "age_group_id" = age_group,
      "sex_group_id" = sex_group,
      "cohort_name" = name
    ), by = "cohort_id") %>%
  left_join(db_age_groups, by = "age_group_id") %>%
  left_join(db_sex_groups, by = "sex_group_id") %>%
  left_join(db_ethnicities, by = "ethnicity_id") %>%
  left_join(db_religions, by = "religion_id") %>%
  left_join(db_locus %>%
    rename(
      "locus_id" = id,
      "locus" = name
    ), by = "locus_id") %>%
  left_join(db_ithamaps_accumulated_sources %>%
    rename("ihme_id" = expert), by = "source_id") %>%
  left_join(db_ithamaps_log %>%
    rename("expert_id" = curated_by) %>%
    select(entry_id, expert_id), by = "entry_id") %>%
  left_join(db_itha_experts %>%
    rename("expert_id" = id) %>%
    mutate(curated_by = paste0(name, " ", surname)) %>%
    select(expert_id, curated_by), by = "expert_id") %>%
  # Ported join diagnostics: if this join is many-to-many, export keys and
  # suppress warning noise by marking the relationship explicitly.
  left_join_logged(db_j_criteria, by = "entry_id", join_name = "db_ithamaps_entries__db_j_criteria__entry_id") %>%
  left_join(db_itha_experts %>%
    rename("ihme_id" = id) %>%
    mutate(expert = name) %>%
    select(ihme_id, expert), by = "ihme_id") %>%
  left_join(db_country %>%
    rename("country_id" = idCountry) %>%
    select(country_id, countryName, continentName), by = "country_id") %>%
  rename("Country" = countryName) %>%
  left_join(db_country %>%
    rename("nationality" = idCountry) %>%
    select(nationality, countryName), by = "nationality") %>%
  left_join(db_ithagenes_globin_phen %>%
    rename("identifier" = phen_id) %>%
    left_join(db_globin_phenotypes %>%
      rename("identifier" = id, "phenotype" = name) %>%
      select(identifier, phenotype) %>%
      mutate(phenotype = case_when(
        # id = 1 => -- α0
        identifier %in% c(1) ~ "α0",
        # id = 2 => -- α+, id = 3 => -- α+/α0
        identifier %in% c(2, 3) ~ "α+",
        # id = 4 => -- β0
        identifier %in% c(4) ~ "β0",
        # id = 5 => -- β+, id = 7 => -- β++, id = 8 => -- β++ (silent), id = 9 => -- β0 / β+
        identifier %in% c(5, 7, 8, 9) ~ "β+",
        # id = 10 => -- δ0
        identifier %in% c(10) ~ "δ0",
        # id = 11 => -- δ+
        identifier %in% c(11) ~ "δ+",
        TRUE ~ NA_character_
      )), by = "identifier") %>%
    filter(!is.na(phenotype)) %>%
    select(-identifier) %>%
    group_by(ithaID) %>%
    mutate(count = n()) %>%
    ungroup() %>%
    mutate(phenotype = if_else(count >= 2, NA_character_, phenotype)) %>%
    filter(!is.na(phenotype)) %>%
    select(-count) %>%
    distinct(), by = "ithaID") %>%
  # Keep the original metric FK for diagnostics (distinct from the Joomla
  # "Metric" summary-statistic parameter).
  mutate(metric_fk_id = metric_id) %>%
  select(
    -phen_id, -regions_id, -metric_id, -cause_id, -measure_id,
    -primary_ontology, -secondary_ontology, -cohort_id, -cohort_name,
    -sex_group_id, -age_group_id, -ethnicity_id, -religion_id,
    -chr_id, -build, -refseq, -version, -locus_id, -expert_id,
    -j_criteria_id, -criterium_id, -response_id, -created.x,
    -updated.x, -created.y, -updated.y, -created.x.x, -locus,
    -updated.x.x, -created.y.y, -updated.y.y, -ihme_id, -country_id, -nationality
  ) %>%
  rename("nationality" = countryName) %>%
  mutate(
    longitude = as.character(longitude),
    latitude = as.character(latitude)
  ) %>%
  mutate(
    end_year_assumed = ifelse(end_year_assumed == 1, "End year of study period based on study's publication year", NA),
    timeframe = ifelse(!is.na(start_year) & !is.na(end_year), paste0(start_year, "-", end_year),
      ifelse(is.na(start_year) & !is.na(end_year), paste0("up to ", end_year),
        ifelse(!is.na(start_year) & is.na(end_year), paste0("from ", start_year), "Unspecified")
      )
    ),
    nationality_comment = ifelse(nationality_comment == "assumed", "Nationality based on study's location", NA),
    age = ifelse(!is.na(age_group_name) & !is.na(min_age) & !is.na(max_age), paste0(age_group_name, " (", min_age, "-", max_age, ")"),
      ifelse(!is.na(age_group_name) & !is.na(min_age) & is.na(max_age), paste0(age_group_name, " (", min_age, ")"),
        ifelse(!is.na(age_group_name) & is.na(min_age) & !is.na(max_age), paste0(age_group_name, " (", max_age, ")"),
          ifelse(!is.na(age_group_name) & is.na(min_age) & is.na(max_age), age_group_name,
            ifelse(is.na(age_group_name) & !is.na(min_age) & !is.na(max_age), paste0("(", min_age, "-", max_age, ")"),
              ifelse(is.na(age_group_name) & !is.na(min_age) & is.na(max_age), paste0("(", min_age, ")"),
                ifelse(is.na(age_group_name) & is.na(min_age) & !is.na(max_age), paste0("(", max_age, ")"), "Unspecified")
              )
            )
          )
        )
      )
    ),
    sex = ifelse(sex_group_name == "Both" & is.na(n_female) & is.na(n_male), "Both",
      ifelse(sex_group_name == "Both" & !is.na(n_female) & is.na(n_male), paste0("Both (Female: ", n_female, ")"),
        ifelse(sex_group_name == "Both" & is.na(n_female) & !is.na(n_male), paste0("Both (Male: ", n_male, ")"),
          ifelse(sex_group_name == "Both" & !is.na(n_female) & !is.na(n_male), paste0("Both (Female: ", n_female, ", Male: ", n_male, ")"),
            ifelse(sex_group_name == "Female" & !is.na(n_female) & is.na(n_male), paste0("Female (n: ", n_female, ")"),
              ifelse(sex_group_name == "Male" & is.na(n_female) & !is.na(n_male), paste0("Male (n: ", n_male, ")"), "Unspecified")
            )
          )
        )
      )
    ),
    status_group = ifelse(status_group == "Both", "Carriers and patients", status_group),
    nationality = ifelse(is.na(nationality), "Unspecified", nationality),
    ethnicity_name = ifelse(is.na(ethnicity_name), "Unspecified", ethnicity_name),
    race = ifelse(is.na(race), "Unspecified", race),
    religion_name = ifelse(is.na(religion_name), "Unspecified", religion_name),
    consaguinity = ifelse(is.na(consaguinity), "Unspecified", consaguinity),
    recruitment_site = ifelse(is.na(recruitment_site), "Unspecified", recruitment_site),
    globin_phenotype = ifelse(measure_name %in% c("Carrier prevalence", "Allele frequency", "Prevalence") & is.na(globin_phenotype), "Unspecified",
      ifelse(!(measure_name %in% c("Carrier prevalence", "Allele frequency", "Prevalence")), "Not applicable", globin_phenotype)
    ),
    phenotype = ifelse(measure_name == "Relative allele frequency" & is.na(phenotype), "Other", phenotype),
    ithaID = ifelse(is.na(ithaID), "Not applicable", ithaID),
    # count / sample_size are nullable in the DB; suppressWarnings() silences the
    # expected "NAs introduced by coercion" when a non-numeric/empty value
    # (e.g. NULL, "") is coerced to NA rather than a real parse failure.
    count = suppressWarnings(as.integer(formatC(as.numeric(count), format = "f", digits = 0))),
    sample_size = suppressWarnings(as.integer(formatC(as.numeric(sample_size), format = "f", digits = 0)))
  ) %>%
  rowwise() %>%
  mutate(note = Note_function(end_year_assumed, region_comment, nationality_comment, comments, sample_size_comment)) %>%
  ungroup() %>%
  select(
    -start_year, -score, -recomputed, -comment, -curated_by, -expert, -source_id, -pmid, -report,
    -doi, -end_year_assumed, -region_comment, -nationality_comment, -comments, -sample_size_comment, -min_age,
    -max_age, -age_group_name, -n_female, -n_male, -sex_group_name, -admin0, -admin1, -admin2, -admin3, -hc_key, -entry_id
  ) %>%
  distinct()

Note_function2 = function(end_year_assumed, region_comment, compensation_comment) {
  parts = c(
    if (!is.na(end_year_assumed)) end_year_assumed,
    if (!is.na(region_comment)) region_comment,
    if (!is.na(compensation_comment)) compensation_comment
  )
  if (length(parts) > 0) paste(parts, collapse = ", ") else "None"
}

hcp_per_region_vanilla = db_hcp_per_region
availability_levels = expand_grid(
  l1 = str_to_lower(levels(hcp_per_region_vanilla$eligibility)),
  l2 = str_to_lower(levels(hcp_per_region_vanilla$coverage))
) %>%
  mutate(
    l1 = case_when(l1 == "unavailable" ~ l1,
                   l1 == "unclear" ~ "unavailable",
      TRUE ~"available"),
    # Optional: adjust factor levels if "universal" should also be "available" in level definitions
    l2 = case_when(l2 == "universal" ~"national", 
                   l2 == "unclear" ~ NA,
                   TRUE ~l2),
    combo_level = str_to_title(str_c(l1, " (", l2, ")"))
  ) %>%
  distinct(combo_level) %>%
  pull(combo_level)

# check in sql:
# SELECT * FROM `hcp_per_region` AS h LEFT JOIN regions AS r ON r.regions_id = h.regions_id LEFT JOIN country AS c ON r.country_id = c.idCountry WHERE h.hcp_id =14 AND c.continentName = "Europe" AND h.cause_id = 10
# ORDER BY `h`.`regions_id` ASC;
db_hcp_per_region = db_hcp_per_region %>%
  rename("source_id" = source) %>%
  left_join(db_regions %>% select(-c(created, updated)), by = "regions_id") %>%
  left_join(db_cause, by = "cause_id") %>%
  left_join(db_ithamaps_accumulated_sources %>%
            mutate(source_link = case_when(!is.na(pmid) ~str_c("https://pubmed.ncbi.nlm.nih.gov/", pmid),
                                           is.na(pmid) & !is.na(doi) ~ str_c("https://doi.org/", doi),
                                           is.na(pmid) & is.na(doi) & !is.na(expert) ~ str_c(ithanet_site_root, "/community/experts?expID=", expert), is.na(pmid) & is.na(doi) & is.na(expert) & !is.na(citation_str) ~ citation_str,
                                          TRUE ~ report)) %>%
            rename("ihme_id" = expert) %>%
            select(-c(created, updated)),
            by = "source_id") %>%
  left_join(db_ithamaps_log %>%
    filter(!is.na(hcp_entry_id)) %>%
    rename("expert_id" = curated_by) %>%
    left_join(db_itha_experts %>%
      rename("expert_id" = id) %>%
      mutate(curated_by = paste0(name, " ", surname)) %>%
      select(expert_id, curated_by), by = "expert_id") %>%
    # this could be a source of duplicate values as in the log table there are
    # entries for add and edit on the hcp entries
    group_by(hcp_entry_id) %>%
    mutate(curated_by = replace_na(''), 
           curated_by = str_c(unique(curated_by), collapse =",")) %>% 
    # handling the case where we might have added data programmatically and did 
    # not add curator so the NA values are reated as distinct and introduce dups
    filter(!is.na(expert_id)) %>%
    distinct() %>%
    select(hcp_entry_id, expert_id, curated_by), by = "hcp_entry_id") %>%
  left_join(db_itha_experts %>%
    rename("ihme_id" = id) %>%
    mutate(expert = name) %>%
    select(ihme_id, expert), by = "ihme_id") %>%
  left_join(db_hc_policies %>%
    select(hcp_id, hcp_name, ancestor0) %>%
    rename("ancestor0_hcp_id" = ancestor0), by = "hcp_id") %>%
  left_join(db_hc_policies %>%
    filter(is.na(ancestor0)) %>%
    select(hcp_id, hcp_name) %>%
    rename(ancestor0_hcp_id = hcp_id, 
           hcp_name_ancestor = hcp_name), 
    by = "ancestor0_hcp_id") %>%
  left_join(db_country %>%
    rename("country_id" = idCountry) %>%
    select(country_id, countryName, continentName), by = "country_id") %>%
  rename("Country" = countryName) %>%
  select(
    -regions_id, -cause_id, -primary_ontology, -secondary_ontology,
    -expert_id, -hcp_id, -ihme_id, -country_id
  ) %>%
  mutate(
    longitude = as.character(longitude),
    latitude = as.character(latitude)
  ) %>%
  # Some DB SET columns are factors; make NA explicit first so downstream
  # replacements do not accidentally coerce factor labels to integer codes.
  mutate(across(where(is.factor), ~ add_explicit_na_level(.x, "Unspecified"))) %>%
  mutate(
    end_year_assumed = ifelse(end_year_assumed == 1, "End year of study period based on study's publication year", NA),
    timeframe = case_when(!is.na(start_year) & !is.na(end_year) ~str_c(start_year, "-", end_year), 
                          is.na(start_year) & !is.na(end_year) ~str_c("up to ", end_year), 
                          !is.na(start_year) & is.na(end_year) ~str_c("from ", start_year),
                          TRUE ~"Unspecified"),
    uptake = as.character(uptake),
    uptake = replace_na(uptake, "Unspecified"),
    #implementation = str_to_title(implementation),
    diagnostic_method = case_when(
      # these are entries on newborn screening, prevention strategy and prenatal genetic diagnosis, for which diagnostic method is applicable but have no data, so we set it to "Unspecified"
      ancestor0_hcp_id %in% c(1,2,3) & is.na(diagnostic_method) ~ "Unspecified",
      # even if there are data for diagnostic method by mistake in curation, if the entry is not on newborn screening, prevention strategy or prenatal genetic diagnosis, we set it to "Not applicable"
      !ancestor0_hcp_id %in% c(1,2,3) & is.na(diagnostic_method) ~ "Not applicable",
      TRUE ~ diagnostic_method
    )
  ) %>%
  rowwise() %>%
  mutate(note = Note_function2(end_year_assumed, region_comment, compensation_comment)) %>%
  ungroup() %>%
  # Ported from IthaMaps-shinyapp/app.R lines 270-278: synthesize
  # healthcare availability and coverage labels before grouped harmonization.
  #mutate(
  #  coverage = ifelse(coverage == "Universal", "National",
  #    ifelse(coverage == "Unclear", "NULL", coverage)
  #  )
  #) %>%
  #commenting out as this is potentially a residue from curation pipeline and not needed for the shiny app
  #mutate(across(everything(), ~ {
  #  value_chr = as.character(.)
  #  ifelse(value_chr == ":", "NULL", value_chr)
  #})) %>%
  #mutate(across(everything(), ~ {
  #  value_chr = as.character(.)
  #  ifelse(is.na(value_chr), "NULL", value_chr)
  #})) %>%
  mutate(
    availability = dplyr::case_when(
      eligibility == "Unavailable" ~ "Unavailable",
      eligibility == "NULL" | is.na(coverage) | coverage == "Unclear" ~ NA_character_,
      coverage == "Regional" ~ "Available (Regionally)",
      coverage == "Universal" ~ "Available (Nationally)",
      TRUE ~ NA_character_
    ),
    availability = factor(
      availability,
      levels = c("Available (Nationally)", "Available (Regionally)", "Unavailable")
    )
  ) %>%
  filter(!is.na(availability)) %>%
  mutate(
    compensation = gsub("and", "&", compensation),
    compensation = gsub(",", " &", compensation),
    compensation = ifelse(compensation_comment != "NULL", paste0(compensation, " (", compensation_comment, ")"), compensation)
  ) %>%
  mutate(
    source_label = dplyr::case_when(
      !is.na(pmid) ~ str_c("PMID: ", pmid),
      is.na(pmid) & !is.na(doi) ~ str_c("DOI: ", doi),
      is.na(pmid) & is.na(doi) & !is.na(expert) ~ str_c("expert: ", expert),
      !is.na(citation_str) ~ citation_str,
      TRUE ~ "Source"
    )
  ) %>%
  select(-any_of(c(
    "comments", "curated_by", "expert", "source_id", "pmid", "report", "doi", "hc_key",
    "end_year_assumed", "region_comment", "admin0", "admin1", "admin2", "admin3"
  ))) %>%
  distinct()

# Preserve healthcare availability options as factor levels from the full
# source data so downstream subsets/legends can show all options even when one
# option has zero rows for a given query.
hcp_availability_levels = if (is.factor(db_hcp_per_region$availability)) {
  levels(db_hcp_per_region$availability)
} else {
  db_hcp_per_region %>%
    dplyr::pull(availability) %>%
    as.character() %>%
    unique() %>%
    stats::na.omit() %>%
    as.character()
}

rm(Note_function, Note_function2)

# User input options
Resolution = data.frame(
  ID = c(1, 2, 3),
  Option = c("Global-level", "Continent-level", "Country-level")
)

Continent = data.frame(
  ID = c(1, 2, 3, 4, 5, 6, 7),
  Option = c(unique(db_country$continentName))
)

Country = data.frame(
  ID = db_country$idCountry,
  Option = db_country$countryName
)

# ---------------------------------------------------------------------------
# Allowed Measure / Cause options and their permitted combinations are derived
# from the data itself rather than from hard-coded label lists: curated measures
# and their causes come from ithamaps_entries, and the synthetic "Healthcare
# availability" measure (id 21) takes its causes from hcp_per_region. A measure
# is offered only if it occurs in the data, and a (measure, cause) pair is
# accepted only if that exact pair occurs in the data.
# ---------------------------------------------------------------------------
measure_cause_allowed = db_ithamaps_entries %>%
  dplyr::distinct(measure_name, cause_name) %>%
  dplyr::filter(!is.na(measure_name), !is.na(cause_name)) %>%
  dplyr::bind_rows(
    db_hcp_per_region %>%
      dplyr::distinct(cause_name) %>%
      dplyr::filter(!is.na(cause_name)) %>%
      dplyr::mutate(measure_name = "Healthcare availability") %>%
      dplyr::select(measure_name, cause_name)
  ) %>%
  dplyr::distinct()

# Measures present in the curated data (independent of cause, so allele-frequency
# measures that carry no cause are still offered).
measures_in_data = db_ithamaps_entries %>%
  dplyr::distinct(measure_name) %>%
  dplyr::filter(!is.na(measure_name)) %>%
  dplyr::pull(measure_name)

Measure = data.frame(
  ID = db_measure$measure_id,
  Option = db_measure$measure_name,
  stringsAsFactors = FALSE
) %>%
  filter(Option %in% measures_in_data) %>%
  rbind(data.frame(
    ID = 21,
    Option = "Healthcare availability",
    stringsAsFactors = FALSE
  ))

Cause = data.frame(
  ID = db_cause$cause_id,
  Option = db_cause$cause_name,
  stringsAsFactors = FALSE
) %>%
  filter(Option %in% unique(measure_cause_allowed$cause_name))

measure_mode = function(measure_label) {
  if (is.null(measure_label) || length(measure_label) == 0 || is.na(measure_label[[1]])) {
    return(NULL)
  }

  measure_lower = tolower(as.character(measure_label[[1]]))
  if (measure_lower == "healthcare availability") {
    return("healthcare")
  }
  if (grepl("carrier", measure_lower)) {
    return("carrier")
  }
  if (measure_lower == "allele frequency") {
    return("allele_frequency")
  }
  if (measure_lower == "relative allele frequency") {
    return("relative_allele_frequency")
  }
  "phenotype"
}

validate_measure_cause_combination = function(measure_id, cause_id, measure_label, cause_label) {
  errors = character()

  if (is.null(measure_id) || is.null(cause_id)) {
    return(errors)
  }

  mode = measure_mode(measure_label)
  if (is.null(mode)) {
    return(errors)
  }

  # Allele-frequency measures do not use a cause at all, so any supplied cause
  # is invalid regardless of what occurs in the data.
  if (mode %in% c("allele_frequency", "relative_allele_frequency")) {
    return(unique(c(
      errors,
      sprintf(
        "Cause '%s' is not allowed when Measure is '%s'.",
        as.character(cause_label %||% cause_id),
        as.character(measure_label %||% measure_id)
      )
    )))
  }

  # Otherwise the (measure, cause) pair must actually occur in the data.
  measure_label_chr = as.character(measure_label %||% "")
  cause_label_chr = as.character(cause_label %||% "")
  pair_ok = any(
    measure_cause_allowed$measure_name == measure_label_chr &
      measure_cause_allowed$cause_name == cause_label_chr
  )
  if (!pair_ok) {
    errors = c(
      errors,
      sprintf(
        "Cause '%s' is not allowed for Measure '%s'.",
        as.character(cause_label %||% cause_id),
        as.character(measure_label %||% measure_id)
      )
    )
  }

  unique(errors)
}

Healthcare = data.frame(
  ID = db_hc_policies$hcp_id,
  Option = db_hc_policies$hcp_name,
  Extra = db_hc_policies$ancestor0
) %>%
  filter(is.na(Extra)) %>%
  select(-Extra)

list2env(
  setNames(
    lapply(1:13, function(x) {
      db_hc_policies %>%
        filter(ancestor0 == x) %>%
        select(ID = hcp_id, Option = hcp_name)
    }),
    paste0("HealthcareS", 1:13)
  ),
  envir = .GlobalEnv
)

GlobinPheAF = data.frame(
  ID = db_globin_phenotypes$id,
  Option = db_globin_phenotypes$name
) %>%
  filter(Option %in% c("α0", "α⁺", "α-thalassaemia modifier", "non-deletional α+")) %>%
  mutate(Option = ifelse(Option == "α⁺", "α+", Option))

VariantC = data.frame(
  ID = c(1, 2),
  Option = c("Individual variants", "Grouped variants by globin phenotype")
)

IthaID = data.frame(
  ID = c(db_ithagenes_common$ithaID),
  Option = c(db_ithagenes_common$ithaID)
)

GlobinPheRAF = data.frame(
  ID = db_globin_phenotypes$id,
  Option = db_globin_phenotypes$name
) %>%
  filter(Option %in% c("α0", "α⁺", "β0", "β+", "δ0", "δ+")) %>%
  mutate(Option = ifelse(Option == "α⁺", "α+", Option)) %>%
  rbind(data.frame(ID = 38, Option = "Other"))

Metric = data.frame(
  ID = c(1, 2, 3, 4, 5, 6, 7),
  Option = c("Weighted mean", "Mean", "Median", "Highest value", "Lowest value", "Most recent value", "Value from largest surveyed population"),
  Extra = c("Wmean", "Mean", "Median", "Max", "Min", "Latest", "Largest")
)

Aggregation = data.frame(
  ID = c(1, 2, 3),
  Option = c("Country-level", "Province-level", "District-level")
)

# Ported from IthaMaps-shinyapp/app.R lines 345-359: add the DataType query
# dimension so the target app can route between curated and prediction modes.
DataType = data.frame(
  ID = c(1, 2),
  Option = c("Curated data", "Prediction data")
)

rm(Configuration, list = setdiff(ls(pattern = "^db_"), c("db_hcp_per_region", "db_ithamaps_entries")))

# ---------------------------------------------------------------------------
# Cache spatial files once at startup (re-used per session in server)
# ---------------------------------------------------------------------------
read_sf_sources = function(specs) {
  # Reuse the generic non-cache parallel controls.
  # ITHAMAPS_PARALLEL_CORES is an optional override: when unset (NA) the
  # candidates list filters it out and parallel::detectCores() drives the choice.
  parallel_flag = tolower(trimws(Sys.getenv("ITHAMAPS_PARALLEL", unset = "false")))
  parallel_enabled = parallel_flag %in% c("1", "true", "yes", "on")
  requested_workers = suppressWarnings(as.integer(Sys.getenv("ITHAMAPS_PARALLEL_CORES", unset = NA_character_)))
  detected_workers = parallel::detectCores(logical = TRUE)

  # Memory-aware cap for large geometry reads. Defaults are intentionally
  # conservative for this workload (0.5-1 GB source files).
  mem_reserve_mb = suppressWarnings(as.integer(Sys.getenv("ITHAMAPS_PARALLEL_MEM_RESERVE_MB", unset = "4096")))
  per_worker_mb = suppressWarnings(as.integer(Sys.getenv("ITHAMAPS_SF_READ_WORKER_MB", unset = "2200")))
  max_workers_cap = suppressWarnings(as.integer(Sys.getenv("ITHAMAPS_SF_READ_MAX_WORKERS", unset = "2")))

  # reads system memory info from /proc/meminfo (Linux only) and returns the available memory in MB
  mem_available_mb = NA_real_
  mem_line = tryCatch(
    readLines("/proc/meminfo", warn = FALSE),
    error = function(e) character(0)
  )
  mem_avail_raw = mem_line[grepl("^MemAvailable:", mem_line)]
  if (length(mem_avail_raw) > 0) {
    mem_available_mb = as.numeric(gsub("[^0-9]", "", mem_avail_raw[[1]])) / 1024
  }

  cpu_candidates = c(requested_workers, detected_workers, 1L)
  cpu_candidates = cpu_candidates[!is.na(cpu_candidates) & cpu_candidates > 0]
  cpu_workers = cpu_candidates[[1]]

  memory_workers = if (!is.na(mem_available_mb) && !is.na(mem_reserve_mb) && !is.na(per_worker_mb) && per_worker_mb > 0) {
    max(1L, floor((mem_available_mb - mem_reserve_mb) / per_worker_mb))
  } else {
    1L
  }

  workers = min(length(specs), cpu_workers, memory_workers)
  if (!is.na(max_workers_cap) && max_workers_cap > 0) {
    workers = min(workers, max_workers_cap)
  }
  workers = max(1L, workers)

  message(sprintf(
    "[ithamaps] source sf read mode=%s workers=%d (cpu=%s mem_available_mb=%s mem_cap=%d)",
    if (parallel_enabled) "parallel" else "sequential",
    if (parallel_enabled) workers else 1L,
    ifelse(is.na(detected_workers), "NA", as.character(detected_workers)),
    ifelse(is.na(mem_available_mb), "NA", as.character(round(mem_available_mb, 0))),
    memory_workers
  ))

  read_one = function(spec) {
    list(name = spec$name, value = read_sf(spec$path))
  }

  results = if (parallel_enabled && workers > 1L) {
    parallel::mclapply(specs, read_one, mc.cores = workers)
  } else {
    lapply(specs, read_one)
  }

  setNames(lapply(results, `[[`, "value"), vapply(results, `[[`, character(1), "name"))
}

sf_sources = read_sf_sources(list(
  list(name = "adm0_sf", path = "ADM0.gpkg"),
  list(name = "adm1_sf", path = "ADM1.gpkg"),
  list(name = "adm2_sf", path = "ADM2.gpkg")
))

adm0_sf = sf_sources[["adm0_sf"]]
adm1_sf = sf_sources[["adm1_sf"]]
adm2_sf = sf_sources[["adm2_sf"]]
rm(sf_sources)

adm0_sel = adm0_sf %>%
  dplyr::select(geo_admin0, name, geom) %>%
  rename("Region" = name)
adm1_sel = adm1_sf %>%
  dplyr::select(geo_admin1, name, geom) %>%
  rename("Region1" = name)
adm2_sel = adm2_sf %>%
  dplyr::select(geo_admin2, name, geom) %>%
  rename("Region2" = name)
adm0_lookup = adm0_sel %>% st_drop_geometry()
adm1_lookup = adm1_sel %>% st_drop_geometry()
adm2_lookup = adm2_sel %>% st_drop_geometry()

# ---------------------------------------------------------------------------
# Pre-simplified display geometry (performance).
# The ADM0/1/2 boundaries are full resolution (gpkg are 0.5-1 GB), so a global
# query selecting many polygons produces very large GeoJSON that dominates
# websocket transfer + client-side render and can crash the browser. The vertex
# detail is invisible at choropleth zoom.
#
# (a) Aggressive fixed tolerances (in degrees) reduce vertex counts massively.
# (b) The simplification is done ONCE at startup and persisted to .rds, so it is
#     never recomputed per render (and restarts skip the work entirely). The
#     render path swaps the cached simplified geometry in by key, which is a
#     cheap match() + geometry assignment.
# Row order and attributes are preserved so layerIds / selection indices stay
# aligned with the unsimplified SubsetG (which is kept full-resolution for
# shape-click matching and PNG export fidelity).
# ---------------------------------------------------------------------------
build_simplified_layer = function(sf_layer, tol, cache_file, source_file) {
  layer_geom_mb = function(x) round(as.numeric(object.size(sf::st_geometry(x))) / 1024^2, 1)
  if (file.exists(cache_file) && file.exists(source_file) &&
      file.info(cache_file)$mtime >= file.info(source_file)$mtime) {
    cached = tryCatch(readRDS(cache_file), error = function(e) NULL)
    if (!is.null(cached) && nrow(cached) == nrow(sf_layer)) {
      message(sprintf("[ithamaps] simplify %s: cache hit (%s, %.1f MB geom)",
                      cache_file, nrow(cached), layer_geom_mb(cached)))
      return(cached)
    }
  }
  before_mb = layer_geom_mb(sf_layer)
  t0 = proc.time()[["elapsed"]]
  # Use planar GEOS (not s2) for simplification: s2 rejects self-intersecting
  # loops ("Loop N is not valid: Edge ... crosses edge ..."), which are common
  # in coarse admin boundaries; GEOS simplifies/repairs them fine for display.
  old_s2 = sf::sf_use_s2()
  suppressMessages(sf::sf_use_s2(FALSE))
  on.exit(suppressMessages(sf::sf_use_s2(old_s2)), add = TRUE)
  simplify_once = function(layer) {
    suppressWarnings(sf::st_simplify(layer, dTolerance = tol, preserveTopology = TRUE))
  }
  simplified = tryCatch(
    simplify_once(sf_layer),
    error = function(e) {
      message(sprintf("[ithamaps] simplify %s: st_simplify error: %s; retrying after st_make_valid",
                      cache_file, conditionMessage(e)))
      tryCatch({
        valid_layer = sf::st_make_valid(sf_layer)
        simplify_once(valid_layer)
      }, error = function(e2) {
        message(sprintf("[ithamaps] simplify %s: retry failed: %s", cache_file, conditionMessage(e2)))
        NULL
      })
    }
  )
  if (is.null(simplified)) {
    message(sprintf("[ithamaps] simplify %s: FAILED, using full-resolution geometry (%.1f MB)",
                    cache_file, before_mb))
    return(sf_layer)
  }
  # preserveTopology should prevent empties, but guard: restore the original
  # geometry for any feature that simplified away to empty.
  empty = sf::st_is_empty(sf::st_geometry(simplified))
  if (any(empty)) {
    sf::st_geometry(simplified)[empty] = sf::st_geometry(sf_layer)[empty]
  }
  tryCatch(saveRDS(simplified, cache_file), error = function(e) NULL)
  message(sprintf("[ithamaps] simplify %s: %.1f -> %.1f MB geom (tol=%g) in %.1fs, cached",
                  cache_file, before_mb, layer_geom_mb(simplified), tol,
                  proc.time()[["elapsed"]] - t0))
  simplified
}

# Larger admin units can tolerate a coarser tolerance; ADM2 districts are small
# so they get a finer one. ADM0 is the detailed world coastline, so it needs the
# coarsest tolerance (0.1 deg ~ 11 km) to shrink its payload meaningfully.
cache_specs = list(
  list(name = "adm0_sel_disp", layer = adm0_sel, tol = 0.02, cache = "cache_adm0_disp.rds", source = "ADM0.gpkg"),
  list(name = "adm1_sel_disp", layer = adm1_sel, tol = 0.02, cache = "cache_adm1_disp.rds", source = "ADM1.gpkg"),
  list(name = "adm2_sel_disp", layer = adm2_sel, tol = 0.01, cache = "cache_adm2_disp.rds", source = "ADM2.gpkg")
)

# Optional startup parallelism for cache (re)generation when cache files are
# missing/stale/invalid. Cache-hit paths remain fast and mostly unaffected.
# ITHAMAPS_CACHE_BUILD_CORES is an optional override: when unset (NA) the
# candidates list filters it out and parallel::detectCores() drives the choice.
parallel_cache_flag = tolower(trimws(Sys.getenv("ITHAMAPS_PARALLEL_CACHE_BUILD", unset = "false")))
parallel_cache_enabled = parallel_cache_flag %in% c("1", "true", "yes", "on")
requested_cache_workers = suppressWarnings(as.integer(Sys.getenv("ITHAMAPS_CACHE_BUILD_CORES", unset = NA_character_)))
detected_cache_workers = parallel::detectCores(logical = TRUE)
cache_worker_candidates = c(requested_cache_workers, detected_cache_workers, 1L)
cache_worker_candidates = cache_worker_candidates[!is.na(cache_worker_candidates) & cache_worker_candidates > 0]
# Only three independent layers exist, so cap workers at 3 to avoid excess
# memory pressure with no throughput gain.
cache_workers = min(length(cache_specs), cache_worker_candidates[[1]])

cache_build_message = sprintf(
  "[ithamaps] startup cache build mode=%s workers=%d (detected_cores=%s)",
  if (parallel_cache_enabled) "parallel" else "sequential",
  if (parallel_cache_enabled) cache_workers else 1L,
  ifelse(is.na(detected_cache_workers), "NA", as.character(detected_cache_workers))
)
message(cache_build_message)

build_spec = function(spec) {
  build_simplified_layer(spec$layer, spec$tol, spec$cache, spec$source)
}

# Parallel branch is Linux/container-only here via mclapply (forking), which
# matches this deployment target.
cache_results = if (parallel_cache_enabled && cache_workers > 1L) {
  parallel::mclapply(cache_specs, build_spec, mc.cores = cache_workers)
} else {
  lapply(cache_specs, build_spec)
}

names(cache_results) = vapply(cache_specs, function(spec) spec$name, character(1))
adm0_sel_disp = cache_results[["adm0_sel_disp"]]
adm1_sel_disp = cache_results[["adm1_sel_disp"]]
adm2_sel_disp = cache_results[["adm2_sel_disp"]]

rm(cache_specs, cache_results, parallel_cache_flag, parallel_cache_enabled,
  requested_cache_workers, detected_cache_workers, cache_worker_candidates,
  cache_workers, cache_build_message, build_spec)

# attach_display_geometry(): replace an sf object's geometry with the cached,
# pre-simplified geometry, matched by the finest available admin key. Cheap
# (no simplification at render time). Falls back to the original geometry for
# any unmatched feature.
attach_display_geometry = function(sf_obj) {
  if (is.null(sf_obj) || nrow(sf_obj) == 0) {
    return(sf_obj)
  }
  cols = names(sf_obj)
  if ("geo_admin2" %in% cols && any(!is.na(sf_obj$geo_admin2))) {
    idx = match(sf_obj$geo_admin2, adm2_sel_disp$geo_admin2)
    src = sf::st_geometry(adm2_sel_disp)
  } else if ("geo_admin1" %in% cols && any(!is.na(sf_obj$geo_admin1))) {
    idx = match(sf_obj$geo_admin1, adm1_sel_disp$geo_admin1)
    src = sf::st_geometry(adm1_sel_disp)
  } else {
    idx = match(sf_obj$geo_admin0, adm0_sel_disp$geo_admin0)
    src = sf::st_geometry(adm0_sel_disp)
  }
  new_geom = sf::st_geometry(sf_obj)
  ok = !is.na(idx)
  if (any(ok)) {
    new_geom[ok] = src[idx[ok]]
    sf::st_geometry(sf_obj) = new_geom
  }
  sf_obj
}

# Ported from IthaMaps-shinyapp/app.R lines 418-426: load prediction rasters,
# priority sites, and admin lookups once so prediction mode can reuse them.
load_prediction_assets = function() {
  # raster::stack() objects use external C pointers that do not survive
  # mclapply's fork+exec model. Rasters must always be loaded sequentially in
  # the parent process. Only plain-R objects (CSVs) are safe to parallelize.
  # ITHAMAPS_PARALLEL_CORES is an optional override: when unset (NA) the
  # candidates list filters it out and parallel::detectCores() drives the choice.
  parallel_assets_flag = tolower(trimws(Sys.getenv("ITHAMAPS_PARALLEL", unset = "false")))
  parallel_assets_enabled = parallel_assets_flag %in% c("1", "true", "yes", "on")
  requested_asset_workers = suppressWarnings(as.integer(Sys.getenv("ITHAMAPS_PARALLEL_CORES", unset = NA_character_)))
  detected_asset_workers = parallel::detectCores(logical = TRUE)
  asset_worker_candidates = c(requested_asset_workers, detected_asset_workers, 1L)
  asset_worker_candidates = asset_worker_candidates[!is.na(asset_worker_candidates) & asset_worker_candidates > 0]
  asset_workers = min(2L, asset_worker_candidates[[1]])

  # Rasters: always sequential (C pointer / fork safety).
  mean_admin = raster::stack(file.path("Predictions", "Mean_with_admin.tif"))
  ci95_admin  = raster::stack(file.path("Predictions", "CI95_with_admin.tif"))
  burden_admin = raster::stack(file.path("Predictions", "Burden_with_admin.tif"))

  # CSVs: safe to parallelize via mclapply (plain-R data frames).
  csv_specs = list(
    list(name = "Selected_sites", path = file.path("Predictions", "Selected-sites_with_admin.csv")),
    list(name = "ADM0_lookup",    path = file.path("Predictions", "ADM0_lookup.csv")),
    list(name = "ADM1_lookup",    path = file.path("Predictions", "ADM1_lookup.csv")),
    list(name = "ADM2_lookup",    path = file.path("Predictions", "ADM2_lookup.csv"))
  )

  read_csv_spec = function(spec) list(name = spec$name, value = read.csv(spec$path))

  message(sprintf(
    "[ithamaps] prediction asset load rasters=sequential csv_mode=%s workers=%d (detected_cores=%s)",
    if (parallel_assets_enabled) "parallel" else "sequential",
    if (parallel_assets_enabled) asset_workers else 1L,
    ifelse(is.na(detected_asset_workers), "NA", as.character(detected_asset_workers))
  ))

  csv_results = if (parallel_assets_enabled && asset_workers > 1L) {
    parallel::mclapply(csv_specs, read_csv_spec, mc.cores = asset_workers)
  } else {
    lapply(csv_specs, read_csv_spec)
  }

  assets = setNames(lapply(csv_results, `[[`, "value"), vapply(csv_results, `[[`, character(1), "name"))

  mean_raster = mean_admin[["Mean"]]
  ci95_raster = ci95_admin[["CI95"]]
  burden_raster = burden_admin[["Burden"]]

  mean_colours = viridis::viridis(10, option = "F", end = 0.9)
  ci95_colours = viridis::viridis(10, option = "G", end = 0.9)
  burden_colours = viridis::viridis(10, option = "F", end = 0.9)

  # Pre-extract raster values at each priority site once at startup so map
  # renders can use popup_html directly without per-render extraction.
  selected_sites_df = assets[["Selected_sites"]] %>%
    dplyr::mutate(lon = as.numeric(lon), lat = as.numeric(lat)) %>%
    dplyr::filter(!is.na(lon), !is.na(lat))

  if (nrow(selected_sites_df) > 0) {
    site_coords = cbind(selected_sites_df$lon, selected_sites_df$lat)
    selected_sites_df$mean_pred   = raster::extract(mean_raster,   site_coords)
    selected_sites_df$ci95_pred   = raster::extract(ci95_raster,   site_coords)
    selected_sites_df$burden_pred = raster::extract(burden_raster, site_coords)
    selected_sites_df$popup_html  = paste0(
      "<strong>ADM0:</strong> ",             selected_sites_df$ADM0, "<br>",
      "<strong>ADM1:</strong> ",             selected_sites_df$ADM1, "<br>",
      "<strong>ADM2:</strong> ",             selected_sites_df$ADM2, "<br>",
      "<strong>Lon / Lat:</strong> ",        round(selected_sites_df$lon, 4), ", ", round(selected_sites_df$lat, 4), "<br>",
      "<strong>Mean prevalence:</strong> ",  ifelse(is.na(selected_sites_df$mean_pred),   "No data", round(selected_sites_df$mean_pred,   4)), "<br>",
      "<strong>Uncertainty (95% CI):</strong> ", ifelse(is.na(selected_sites_df$ci95_pred),   "No data", round(selected_sites_df$ci95_pred,   4)), "<br>",
      "<strong>Est. carriers:</strong> ",    ifelse(is.na(selected_sites_df$burden_pred), "No data", round(selected_sites_df$burden_pred, 4))
    )
  }

  list(
    Mean_admin = mean_admin,
    CI95_admin = ci95_admin,
    Burden_admin = burden_admin,
    Mean = mean_raster,
    CI95 = ci95_raster,
    Burden = burden_raster,
    Selected_sites = selected_sites_df,
    ADM0_lookup = assets[["ADM0_lookup"]],
    ADM1_lookup = assets[["ADM1_lookup"]],
    ADM2_lookup = assets[["ADM2_lookup"]],
    Mean_min = min(mean_raster[], na.rm = TRUE),
    Mean_max = max(mean_raster[], na.rm = TRUE),
    CI95_min = min(ci95_raster[], na.rm = TRUE),
    CI95_max = max(ci95_raster[], na.rm = TRUE),
    Burden_min = min(burden_raster[], na.rm = TRUE),
    Burden_max = max(burden_raster[], na.rm = TRUE),
    Mean_colours = mean_colours,
    CI95_colours = ci95_colours,
    Burden_colours = burden_colours,
    Mean_palette = colorNumeric(
      reverse = TRUE,
      na.color = "transparent",
      palette = mean_colours,
      domain = c(min(mean_raster[], na.rm = TRUE), max(mean_raster[], na.rm = TRUE))
    ),
    CI95_palette = colorNumeric(
      reverse = TRUE,
      na.color = "transparent",
      palette = ci95_colours,
      domain = c(min(ci95_raster[], na.rm = TRUE), max(ci95_raster[], na.rm = TRUE))
    ),
    Burden_palette = colorNumeric(
      reverse = TRUE,
      na.color = "transparent",
      palette = burden_colours,
      domain = c(min(burden_raster[], na.rm = TRUE), max(burden_raster[], na.rm = TRUE))
    )
  )
}

prediction_assets = load_prediction_assets()

# ---------------------------------------------------------------------------
# Query helpers (called per-session inside server)
# ---------------------------------------------------------------------------
Parse = function(Query) {
  Info = list()
  if (is.null(Query) || length(Query) == 0 || is.na(Query[[1]])) {
    return(Info)
  }

  Query = as.character(Query[[1]])
  Query = trimws(Query)
  qs = sub("^\\?", "", Query)
  if (nchar(qs) == 0) {
    return(Info)
  }

  canonicalize_key = function(raw_key) {
    key_lower = tolower(raw_key)
    if (key_lower == "datatype") {
      return("DataType")
    }
    if (key_lower == "country") {
      return("Country")
    }
    if (key_lower == "resolution") {
      return("Resolution")
    }
    if (key_lower == "continent") {
      return("Continent")
    }
    if (key_lower %in% c("measure", "parameter")) {
      return("Measure")
    }
    if (key_lower == "cause") {
      return("Cause")
    }
    if (key_lower == "healthcare") {
      return("Healthcare")
    }
    if (key_lower == "healthcaredetail") {
      return("HealthcareDetail")
    }
    if (grepl("^healthcares[0-9]+$", key_lower)) {
      idx = suppressWarnings(as.integer(sub("^healthcares([0-9]+)$", "\\1", key_lower)))
      if (!is.na(idx) && idx >= 1 && idx <= 13) {
        return(paste0("HealthcareS", idx))
      }
    }
    if (key_lower == "globinpheaf") {
      return("GlobinPheAF")
    }
    if (key_lower == "variantc") {
      return("VariantC")
    }
    if (key_lower == "globinpheraf") {
      return("GlobinPheRAF")
    }
    if (key_lower == "ithaid") {
      return("IthaID")
    }
    if (key_lower == "metric") {
      return("Metric")
    }
    if (key_lower == "aggregation") {
      return("Aggregation")
    }
    raw_key
  }

  parts = strsplit(qs, "&", fixed = TRUE)[[1]]
  items = Filter(Negate(is.null), lapply(parts, function(x) {
    Item = strsplit(x, "=", fixed = TRUE)[[1]]
    if (length(Item) == 2) {
      list(key = canonicalize_key(Item[1]), value = Item[2])
    }
  }))
  Reduce(function(acc, item) {
    acc[[item$key]] = item$value
    acc
  }, items, init = Info)
}

Extract = function(Query) {
  Info = list()

  parse_int = function(v) {
    if (is.null(v)) {
      return(NULL)
    }
    out = suppressWarnings(as.integer(v))
    if (length(out) == 0 || is.na(out[1])) {
      return(NULL)
    }
    out[1]
  }

  expected_keys = c(
    "DataType", "Resolution", "Continent", "Country", "Measure", "Cause",
    "Healthcare", "GlobinPheAF", "VariantC",
    "GlobinPheRAF", "IthaID", "Metric", "Aggregation"
  )
  expected_keys = c(expected_keys, paste0("HealthcareS", 1:13))

  parsed_values = lapply(expected_keys, function(x) parse_int(Query[[x]]))
  names(parsed_values) = expected_keys
  Info = Filter(Negate(is.null), parsed_values)

  if (is.null(Info[["Cause"]])) {
    legacy_values = Filter(
      Negate(is.null),
      lapply(c("HemoglobinopathyH", "HemoglobinopathyC", "HemoglobinopathyP"), function(legacy_key) {
        parse_int(Query[[legacy_key]])
      })
    )
    if (length(legacy_values) > 0) {
      Info[["Cause"]] = legacy_values[[1]]
    }
  }

  # Backward-compatible fallback: if HealthcareDetail is provided,
  # map it to the expected HealthcareS<Healthcare> key.
  healthcare_detail = parse_int(Query[["HealthcareDetail"]])
  healthcare_parent = parse_int(Info[["Healthcare"]])
  if (!is.null(healthcare_detail) && !is.null(healthcare_parent) && healthcare_parent >= 1 && healthcare_parent <= 13) {
    subkey = paste0("HealthcareS", healthcare_parent)
    if (is.null(Info[[subkey]])) {
      Info[[subkey]] = healthcare_detail
    }
  }

  Info
}

Search = function(Item, Identifier, Data) {
  if (is.null(Identifier)) {
    return(NA)
  }
  Outcome = Data %>%
    filter(ID == Identifier) %>%
    pull(Option)
  if (length(Outcome) == 0) {
    return(NA)
  }
  Outcome
}

# (Parse, Extract, Search are defined above and used per-session in server)

query_bundle_cache = new.env(parent = emptyenv())
query_bundle_cache_version = "timings_v4_links"

ithamaps_debug_mode = tolower(trimws(Sys.getenv("ITHAMAPS_DEBUG_MODE", unset = "false"))) %in% c("1", "true", "yes", "on")
query_bundle_debug_store = new.env(parent = emptyenv())
query_bundle_debug_counter = 0L

debug_bundle_enabled = function() {
  isTRUE(ithamaps_debug_mode)
}

normalize_query_string = function(raw_qs) {
  if (is.null(raw_qs) || is.na(raw_qs) || nchar(raw_qs) == 0) {
    return("")
  }
  sub("^\\?", "", raw_qs)
}

entries_to_point_sf = function(data) {
  if (is.null(data) || nrow(data) == 0) {
    return(NULL)
  }

  data %>%
    mutate(
      longitude = suppressWarnings(as.numeric(longitude)),
      latitude = suppressWarnings(as.numeric(latitude))
    ) %>%
    filter(!is.na(longitude), !is.na(latitude)) %>%
    st_as_sf(coords = c("longitude", "latitude"), crs = 4326, remove = FALSE)
}

timed_call = function(timing_env, label, fn) {
  start = proc.time()[["elapsed"]]
  value = fn()
  timing_env[[label]] = round(proc.time()[["elapsed"]] - start, 3)
  value
}

timing_list = function(timing_env) {
  as.list.environment(timing_env, all.names = TRUE)
}

# Human-friendly rendering of the data unit stored in the `metric` table
# (the `metric_unit` column). Kept deliberately separate from the Joomla
# "Metric" summary statistic to avoid confusion. Returns NA when no usable unit.
format_metric_unit = function(unit) {
  if (is.null(unit) || length(unit) == 0 || is.na(unit[[1]]) || !nzchar(as.character(unit[[1]]))) {
    return(NA_character_)
  }
  u = as.character(unit[[1]])
  if (identical(tolower(u), "percent")) "%" else u
}

# Compose a legend/label title that appends the data unit (if any) to the
# summary-statistic name, e.g. "Mean (%)" or "Median (per 100000)".
metric_title_with_unit = function(metric_name, unit) {
  base = if (is.null(metric_name) || length(metric_name) == 0 || is.na(metric_name[[1]])) "Value" else as.character(metric_name[[1]])
  pretty_unit = format_metric_unit(unit)
  if (is.na(pretty_unit)) base else paste0(base, " (", pretty_unit, ")")
}

# Ported from IthaMaps-shinyapp/app.R lines 1022-1063: apply the curated-data
# outlier filters and guarded weighted-mean path before aggregated metrics.
compute_outlier_aware_metric = function(data, group_col, metric_key) {
  names_before = colnames(data)

  filtered_data = data %>%
    filter(!is.na(.data[[group_col]])) %>%
    mutate(Exclude = ifelse(grepl("duplicated", note), TRUE, FALSE)) %>%
    group_by(.data[[group_col]]) %>%
    distinct(sample_size, count, value, .keep_all = TRUE) %>%
    mutate(
      entries = sum(!Exclude),
      Median = ifelse(entries > 1, median(value[!Exclude], na.rm = TRUE), NA_real_),
      Mean = ifelse(entries > 1, mean(value[!Exclude], na.rm = TRUE), NA_real_),
      lowquantile = ifelse(entries > 1, quantile(value[!Exclude], probs = 0.25, na.rm = TRUE), NA_real_),
      upperquantile = ifelse(entries > 1, quantile(value[!Exclude], probs = 0.75, na.rm = TRUE), NA_real_),
      SD = ifelse(entries > 1, sd(value[!Exclude], na.rm = TRUE), NA_real_),
      Mean_plus_sd = ifelse(entries > 1, Mean + SD, NA_real_),
      Mean_minus_sd = ifelse(entries > 1, Mean - SD, NA_real_),
      mad_denominator = ifelse(entries > 1, mad(value[!Exclude], na.rm = TRUE), NA_real_),
      Mad = ifelse(entries > 1 & !Exclude & !is.na(mad_denominator) & mad_denominator != 0,
        0.6745 * (value - Median) / mad_denominator,
        NA_real_
      )
    ) %>%
    ungroup() %>%
    mutate(
      MAD_flag = ifelse(entries >= 5 & Mad != 0 & abs(Mad) > 3.5, "OUTLIER", NA_character_),
      SD_flag = ifelse(entries >= 5 & (value < Mean_minus_sd | value > Mean_plus_sd), "OUTLIER", NA_character_),
      IQR_flag = ifelse(
        entries >= 5 & (
          value < (upperquantile - 1.5 * (upperquantile - lowquantile)) |
            value > (upperquantile + 1.5 * (upperquantile - lowquantile))
        ),
        "OUTLIER",
        "NULL"
      ),
      Outlier = ifelse(MAD_flag == "OUTLIER" & SD_flag == "OUTLIER" & IQR_flag == "OUTLIER", "OUTLIER", NA_character_)
    ) %>%
    filter(is.na(Outlier)) %>%
    group_by(.data[[group_col]]) %>%
    distinct(sample_size, count, value, .keep_all = TRUE) %>%
    mutate(
      Entries = n(),
      Max = max(value, na.rm = TRUE),
      Min = min(value, na.rm = TRUE),
      Largest = ifelse(all(is.na(sample_size)), NA_real_, value[which.max(sample_size)]),
      Latest = if (all(is.na(end_year))) NA_real_ else value[which.max(end_year)],
      Median = median(value, na.rm = TRUE),
      Mean = mean(value, na.rm = TRUE)
    ) %>%
    ungroup() %>%
    mutate(
      Exclude = ifelse(is.na(count) | is.na(sample_size) | Entries == 0 | count == 0, TRUE, FALSE),
      Scale = ifelse(metric_unit == "percent", 100, 100000)
    ) %>%
    group_by(.data[[group_col]]) %>%
    mutate(
      valid_n = sum(!Exclude),
      Wmean = case_when(
        valid_n == 0 ~ NA_real_,
        valid_n == 1 ~ value[which(!Exclude)[1]],
        valid_n >= 2 ~ tryCatch(
          {
            (sin(predict(rma(
              yi,
              vi,
              data = escalc(
                xi = count,
                ni = sample_size,
                data = pick() %>% filter(Exclude == FALSE),
                measure = "PFT",
                add = 0
              ),
              method = "REML",
              level = 95
            ))$pred / 2))^2 * Scale[which(!Exclude)[1]]
          },
          error = function(e) NA_real_
        ),
        TRUE ~ NA_real_
      )
    ) %>%
    ungroup() %>%
    dplyr::select(
      -Exclude, -Scale, -valid_n, -entries, -lowquantile, -upperquantile,
      -SD, -Mean_plus_sd, -Mean_minus_sd, -mad_denominator, -Mad,
      -MAD_flag, -SD_flag, -IQR_flag, -Outlier
    )

  if (is.null(metric_key) || length(metric_key) == 0 || is.na(metric_key[[1]])) {
    return(filtered_data %>%
      dplyr::select(all_of(names_before)) %>%
      mutate(Metric = round(value, 2)))
  }

  filtered_data %>%
    dplyr::select(all_of(names_before), Metric = all_of(metric_key[[1]])) %>%
    filter(!is.na(Metric)) %>%
    mutate(Metric = round(Metric, 2))
}

# Ported from IthaMaps-shinyapp/app.R lines 1430-1504: harmonize healthcare
# availability outputs, timeframe labels, application mode, and references.
harmonize_healthcare_subset = function(data, debug_mode = FALSE) {
  debug_steps = list()
  debug_capture = function(label, value) {
    if (!isTRUE(debug_mode)) {
      return(invisible(NULL))
    }
    debug_steps[[length(debug_steps) + 1L]] <<- list(label = label, value = value)
    invisible(NULL)
  }

  if (is.null(data) || nrow(data) == 0) {
    debug_capture("input_empty", data)
    if (isTRUE(debug_mode)) {
      return(list(result = data, steps = debug_steps))
    }
    return(data)
  }

  debug_capture("input", data)

  compensation_sources = function(comment) {
    comment = gsub("\\s*\\(.*?\\)$", "", comment)
    comment = gsub("^Mixed\\s+", "", comment)
    comment = gsub("\\s*&\\s*|\\s*,\\s*|\\s+and\\s+", " & ", comment)
    strsplit(comment, "\\s*&\\s*")[[1]]
  }

  harmonised_data = data %>%
    group_by(geo_admin0) %>%
    group_modify(function(.x, .y) {
      # Build a country-level availability label with priority rules:
      # National > Regional > Unavailable > Unspecified. Original values stay
      # untouched; the harmonized result is written to a new factor column.
      harmonise_availability_value = function(v) {
        value = trimws(as.character(v))
        if (is.na(value) || !nzchar(value) || value %in% c("NULL", "Unspecified")) {
          return("Unspecified")
        }
        value
      }

      raw_levels = if (is.factor(.x$availability)) levels(.x$availability) else unique(as.character(.x$availability))
      harmonised_levels = unique(vapply(raw_levels, harmonise_availability_value, character(1)))
      harmonised_levels = unique(c(harmonised_levels, "Unspecified"))

      availability_values = vapply(.x$availability, harmonise_availability_value, character(1))
      availability_set = unique(availability_values[availability_values != "Unspecified"])

      if (any(availability_set == "Available (Nationally)")) {
        label = "Available (Nationally)"
      } else if (any(availability_set == "Available (Regionally)")) {
        label = "Available (Regionally)"
      } else if (any(availability_set == "Unavailable")) {
        label = "Unavailable"
      } else if (length(availability_set) == 1) {
        label = availability_set[[1]]
      } else {
        label = "Unspecified"
      }

      .x$availability_harmonised = factor(rep(label, nrow(.x)), levels = harmonised_levels)

      # Per country group, build one diagnostic-method label from all unique
      # non-Unspecified values and apply it to the whole group.
      diag_set = unique(as.character(.x$diagnostic_method))
      diag_set = trimws(diag_set)
      diag_set = diag_set[
        !is.na(diag_set) &
          nzchar(diag_set) &
          tolower(diag_set) != "unspecified"
      ]

      if (length(diag_set) > 0) {
        .x$diagnostic_method_harmonised = paste(sort(diag_set), collapse = "/")
      } else {
        .x$diagnostic_method_harmonised = "Unspecified"
      }
      .x
    }) %>%
    group_modify(function(.x, .y) {
      comp_set = unique(.x$compensation)
      comp_set = comp_set[!grepl("^Unspecified", comp_set)]
      all_sources = sort(unique(unlist(lapply(comp_set, compensation_sources))))
      if (length(all_sources) > 1) {
        label = paste("Mixed", paste(all_sources, collapse = " & "))
      } else if (length(all_sources) == 1) {
        label = all_sources
      } else {
        label = "Unspecified"
      }
      years = str_extract(.x$compensation, "(?<=Compensation since )\\d{4}")
      years = as.numeric(na.omit(years))
      if (length(years) > 0) {
        label = paste0(label, " (Compensation since ", min(years), ")")
      }
      .x$compensation_harmonised = label
      .x
    }) %>%
    group_modify(function(.x, .y) {
      # Recode levels first (Targeted + On request -> Targeted/On request),
      # then prioritize group entries without mutating the original column.
      harmonise_eligibility_value = function(v) {
        value = trimws(as.character(v))
        if (is.na(value) || !nzchar(value) || value %in% c("NULL", "Unspecified")) {
          return("Unspecified")
        }
        if (value %in% c("Targeted", "On request")) {
          return("Targeted/On request")
        }
        value
      }

      raw_levels = if (is.factor(.x$eligibility)) levels(.x$eligibility) else unique(as.character(.x$eligibility))
      harmonised_levels = unique(vapply(raw_levels, harmonise_eligibility_value, character(1)))
      harmonised_levels = unique(c(harmonised_levels, "Unspecified"))

      elig_values = vapply(.x$eligibility, harmonise_eligibility_value, character(1))
      elig_set = unique(elig_values[elig_values != "Unspecified"])

      if ("Universal" %in% elig_set) {
        label = "Universal"
      } else if (length(elig_set) == 1) {
        label = elig_set[[1]]
      } else {
        label = "Unspecified"
      }

      .x$eligibility_harmonised = factor(rep(label, nrow(.x)), levels = harmonised_levels)
      .x
    }) %>%
    group_modify(function(.x, .y) {
      # Keep original application untouched; write prioritized group label to
      # a harmonized factor column.
      harmonise_application_value = function(v) {
        value = trimws(as.character(v))
        if (is.na(value) || !nzchar(value) || value %in% c("NULL", "Unspecified")) {
          return("Unspecified")
        }
        value
      }

      raw_levels = if (is.factor(.x$application)) levels(.x$application) else unique(as.character(.x$application))
      harmonised_levels = unique(vapply(raw_levels, harmonise_application_value, character(1)))
      harmonised_levels = unique(c(harmonised_levels, "Unspecified"))

      app_values = vapply(.x$application, harmonise_application_value, character(1))
      app_set = unique(app_values[app_values != "Unspecified"])

      if ("Mandatory" %in% app_set) {
        label = "Mandatory"
      } else if ("Voluntary" %in% app_set) {
        label = "Voluntary"
      } else {
        label = "Unspecified"
      }

      .x$application_harmonised = factor(rep(label, nrow(.x)), levels = harmonised_levels)
      .x
    }) %>%
    group_modify(function(.x, .y) {
      # Keep original implementation untouched; write grouped harmonized value
      # to a dedicated factor column with Unspecified as the placeholder.
      harmonise_implementation_value = function(v) {
        value = trimws(as.character(v))
        if (is.na(value) || !nzchar(value) || value %in% c("NULL", "Unspecified")) {
          return("Unspecified")
        }
        tools::toTitleCase(tolower(value))
      }

      raw_levels = if (is.factor(.x$implementation)) levels(.x$implementation) else unique(as.character(.x$implementation))
      harmonised_levels = unique(vapply(raw_levels, harmonise_implementation_value, character(1)))
      harmonised_levels = unique(c(harmonised_levels, "Unspecified"))

      impl_values = vapply(.x$implementation, harmonise_implementation_value, character(1))
      impl_set = unique(impl_values[impl_values != "Unspecified"])

      if (length(impl_set) == 0) {
        label = "Unspecified"
      } else {
        label = paste(sort(impl_set), collapse = "/")
      }

      .x$implementation_harmonised = factor(rep(label, nrow(.x)), levels = harmonised_levels)
      .x
    }) %>%
    group_modify(function(.x, .y) {
      start_vals = suppressWarnings(as.numeric(setdiff(.x$start_year, NA)))
      end_vals = suppressWarnings(as.numeric(setdiff(.x$end_year, NA)))
      start_min = if (length(start_vals) > 0) min(start_vals, na.rm = TRUE) else NA_real_
      end_max = if (length(end_vals) > 0) max(end_vals, na.rm = TRUE) else NA_real_
      .x$known_implementation_period_harmonised = dplyr::case_when(
        !is.na(start_min) & !is.na(end_max) & start_min == end_max ~ as.character(start_min),
        !is.na(start_min) & !is.na(end_max) ~ paste0(start_min, "-", end_max),
        !is.na(start_min) & is.na(end_max) ~ paste0("Since ", start_min),
        is.na(start_min) & !is.na(end_max) ~ paste0("Until ", end_max),
        TRUE ~ "Unknown"
      )
      .x
    }) %>%
    group_modify(function(.x, .y) {
      collapse_field = function(vec) {
        out = unique(vec[vec != "NULL"])
        if (length(out) == 0) {
          return("NULL")
        }
        paste(sort(out), collapse = " | ")
      }
      .x$citation_str_harmonised = collapse_field(.x$citation_str)
      pairs = unique(data.frame(
        label = as.character(if ("source_label" %in% names(.x)) .x$source_label else .x$citation_str),
        url   = as.character(if ("source_link"  %in% names(.x)) .x$source_link  else NA_character_),
        stringsAsFactors = FALSE
      ))
      pairs = pairs[!is.na(pairs$label) & pairs$label != "NULL" & nzchar(pairs$label), ]
      .x$citation_link_harmonised = if (nrow(pairs) == 0) "None" else {
        lnks = vapply(seq_len(nrow(pairs)), function(i) {
          lbl = htmltools::htmlEscape(pairs$label[i])
          url = pairs$url[i]
          if (!is.na(url) && grepl("^https?://", url, ignore.case = TRUE))
            paste0('<a href="', htmltools::htmlEscape(url), '" target="_blank" rel="noopener noreferrer">', lbl, '</a>')
          else lbl
        }, character(1))
        paste(sort(unique(lnks)), collapse = " | ")
      }
      .x
    }) %>%
    group_modify(function(.x, .y) {
      collapse_non_missing = function(vec, default = "Unspecified") {
        vals = trimws(as.character(vec))
        vals = vals[
          !is.na(vals) &
            nzchar(vals) &
            !(vals %in% c("NULL", "Unspecified", "Not applicable"))
        ]
        vals = sort(unique(vals))
        if (length(vals) == 0) {
          return(default)
        }
        paste(vals, collapse = " | ")
      }

      .x$uptake_harmonised = collapse_non_missing(.x$uptake)
      .x$recruitment_site_harmonised = collapse_non_missing(.x$recruitment_site)
      .x$note_harmonised = collapse_non_missing(.x$note, default = "None")
      .x
    }) 
  out = harmonised_data %>%
    ungroup() %>%
    distinct(geo_admin0, across(ends_with("harmonised")))
    #mutate(availability = factor(as.character(Availability), levels = availability_levels))

  debug_capture("output", out)
  if (isTRUE(debug_mode)) {
    return(list(result = out, steps = debug_steps))
  }
  out
}

# ---------------------------------------------------------------------------
# build_query_bundle(): run per session from URL query string.
# Returns list(SubsetE, SubsetG, MetricN).  All NULL when no valid query.
# ---------------------------------------------------------------------------
build_query_bundle = function(raw_qs) {
  timing_env = new.env(parent = emptyenv())
  total_start = proc.time()[["elapsed"]]
  debug_mode = debug_bundle_enabled()
  debug_steps = list()
  debug_capture = function(label, value) {
    if (!isTRUE(debug_mode)) {
      return(invisible(NULL))
    }
    debug_steps[[length(debug_steps) + 1L]] <<- list(label = label, value = value)
    invisible(NULL)
  }

  Query = timed_call(timing_env, "parse_extract", function() {
    Extract(Parse(raw_qs))
  })
  debug_capture("query_parsed", Query)

  # Joomla iframe currently forwards only country; default to Country-level resolution.
  if (!is.null(Query$Country) && is.null(Query$Resolution)) {
    Query$Resolution = 3L
  }

  lookup = list(
    DataType = DataType, Resolution = Resolution, Continent = Continent, Country = Country,
    Measure = Measure, Cause = Cause,
    Healthcare = Healthcare,
    HealthcareS1 = HealthcareS1, HealthcareS2 = HealthcareS2, HealthcareS3 = HealthcareS3,
    HealthcareS4 = HealthcareS4, HealthcareS5 = HealthcareS5, HealthcareS6 = HealthcareS6,
    HealthcareS7 = HealthcareS7, HealthcareS8 = HealthcareS8, HealthcareS9 = HealthcareS9,
    HealthcareS10 = HealthcareS10, HealthcareS11 = HealthcareS11, HealthcareS12 = HealthcareS12,
    HealthcareS13 = HealthcareS13, GlobinPheAF = GlobinPheAF, VariantC = VariantC,
    GlobinPheRAF = GlobinPheRAF, IthaID = IthaID, Metric = Metric, Aggregation = Aggregation
  )

  Info = Reduce(function(acc, x) {
    if (x %in% names(lookup)) {
      acc[[x]] = Search(x, Query[[x]], lookup[[x]])
    }
    acc
  }, names(Query), init = list())
  debug_capture("query_info", Info)

  if (is.null(Info$DataType) || is.na(Info$DataType)) {
    Info$DataType = "Curated data"
  }

  validation_errors = validate_measure_cause_combination(
    measure_id = Query$Measure,
    cause_id = Query$Cause,
    measure_label = Info$Measure,
    cause_label = Info$Cause
  )

  # A Measure ID that was supplied but did not resolve to a supported label
  # (e.g. a measure_name that is filtered out of the Measure lookup, such as
  # "Carrier incidence") must be rejected explicitly. Otherwise Info$Measure is
  # NA, the parameter filter early-returns without clearing SubsetHCP, and the
  # leftover global healthcare subset is rendered as if Healthcare availability
  # had been requested.
  if (!is.null(Query$Measure) && (is.null(Info$Measure) || is.na(Info$Measure))) {
    validation_errors = c(
      validation_errors,
      sprintf("Measure ID '%s' is not supported.", as.character(Query$Measure))
    )
  }

  if (length(validation_errors) > 0) {
    timing_env[["total_query_bundle"]] = round(proc.time()[["elapsed"]] - total_start, 3)
    bundle = list(
      DataType = Info$DataType,
      query_info = Info,
      prediction = NULL,
      SubsetE = NULL,
      SubsetHCP = NULL,
      SubsetG = NULL,
      MetricN = NULL,
      MetricUnit = NULL,
      validation_errors = validation_errors,
      timings = timing_list(timing_env)
    )
    if (isTRUE(debug_mode)) {
      debug_capture("validation_errors", validation_errors)
      bundle$debug = list(
        enabled = TRUE,
        query = Query,
        info = Info,
        steps = debug_steps
      )
    }
    return(bundle)
  }

  if (identical(Info$DataType, "Prediction data")) {
    timing_env[["prediction_assets"]] = 0
    timing_env[["total_query_bundle"]] = round(proc.time()[["elapsed"]] - total_start, 3)
    bundle = list(
      DataType = Info$DataType,
      query_info = Info,
      prediction = prediction_assets,
      SubsetE = NULL,
      SubsetHCP = NULL,
      SubsetG = NULL,
      MetricN = NULL,
      MetricUnit = NULL,
      validation_errors = character(),
      timings = timing_list(timing_env)
    )
    if (isTRUE(debug_mode)) {
      debug_capture("prediction_mode", TRUE)
      bundle$debug = list(
        enabled = TRUE,
        query = Query,
        info = Info,
        steps = debug_steps
      )
    }
    return(bundle)
  }

  SubsetE = NULL
  SubsetHCP = NULL
  SubsetHCPRaw = NULL
  SubsetG = NULL
  MetricN = NULL
  MetricUnit = NULL

  # --- Resolution & Region ---
  resolution_result = timed_call(timing_env, "resolution_filter", function() {
    result = list(SubsetE = SubsetE, SubsetHCP = SubsetHCP)

    if (!("Resolution" %in% names(Info)) || is.na(Info$Resolution)) {
      return(result)
    }

    Field = Resolution[Resolution$Option == Info$Resolution, "Option"]
    if (length(Field) > 0 && Field == "Global-level") {
      result$SubsetHCP = db_hcp_per_region %>% select(-Country, -continentName)
      result$SubsetE = db_ithamaps_entries %>% select(-Country, -continentName)
    }
    if (length(Field) > 0 && Field == "Continent-level") {
      if ("Continent" %in% names(Info) && !is.na(Info$Continent)) {
        Field = Continent[Continent$Option == Info$Continent, "Option"]
        result$SubsetHCP = db_hcp_per_region %>%
          filter(continentName == Field) %>%
          select(-Country, -continentName)
        result$SubsetE = db_ithamaps_entries %>%
          filter(continentName == Field) %>%
          select(-Country, -continentName)
      }
    }
    if (length(Field) > 0 && Field == "Country-level") {
      if ("Country" %in% names(Info) && !is.na(Info$Country)) {
        Field = Country[Country$Option == Info$Country, "Option"]
        result$SubsetHCP = db_hcp_per_region %>%
          filter(Country == Field) %>%
          select(-Country, -continentName)
        result$SubsetE = db_ithamaps_entries %>%
          filter(Country == Field) %>%
          select(-Country, -continentName)
      }
    }
    result
  })
  SubsetE = resolution_result$SubsetE
  SubsetHCP = resolution_result$SubsetHCP
  debug_capture("after_resolution_SubsetE", SubsetE)
  debug_capture("after_resolution_SubsetHCP", SubsetHCP)

  # --- Measure, Cause, Globin phenotype, IthaID, Healthcare ---
  parameter_result = timed_call(timing_env, "parameter_filter", function() {
    result = list(SubsetE = SubsetE, SubsetHCP = SubsetHCP)

    if (is.null(SubsetE) || !("Measure" %in% names(Info)) || is.na(Info$Measure)) {
      # Measure missing or unresolved: do not leave a populated SubsetHCP behind,
      # otherwise a non-healthcare query would be mis-rendered as healthcare.
      result$SubsetHCP = NULL
      return(result)
    }

    Field = Measure[Measure$Option == Info$Measure, "Option"]
    if (length(Field) > 0 && Field == "Healthcare availability") {
      result$SubsetE = NULL
      if ("Cause" %in% names(Info) && !is.na(Info$Cause)) {
        Field = Cause[Cause$Option == Info$Cause, "Option"]
        result$SubsetHCP = result$SubsetHCP %>%
          filter(cause_name == Field) %>%
          select(-cause_name)
        if ("Healthcare" %in% names(Info) && !is.na(Info$Healthcare)) {
          Field = Healthcare[Healthcare$Option == Info$Healthcare, "Option"]
          result$SubsetHCP = result$SubsetHCP %>%
            filter(hcp_name_ancestor == Field) %>%
            select(-hcp_name_ancestor)
          healthcare_keys = paste0("HealthcareS", 1:13)
          matched_key = healthcare_keys[
            vapply(healthcare_keys, function(key) key %in% names(Info) && !is.na(Info[[key]]), logical(1))
          ]
          if (length(matched_key) > 0) {
            key = matched_key[[1]]
            HCS = lookup[[key]]
            Field = HCS[HCS$Option == Info[[key]], "Option"]
            result$SubsetHCP = result$SubsetHCP %>%
              filter(hcp_name == Field) %>%
              select(-hcp_name)
          }
        }
      }
    } else if (length(Field) > 0) {
      result$SubsetHCP = NULL
      result$SubsetE = result$SubsetE %>%
        filter(measure_name == Field) %>%
        select(-measure_name)
      if (Field == "Allele frequency") {
        # Allele frequency does not use Cause; keep `ithaID` in SubsetE because
        # downstream outputs (markers/details/exports) reference it.
        result$SubsetE = result$SubsetE %>% select(-cause_name)
        if ("GlobinPheAF" %in% names(Info) && !is.na(Info$GlobinPheAF)) {
          Field = GlobinPheAF[GlobinPheAF$Option == Info$GlobinPheAF, "Option"]
          result$SubsetE = result$SubsetE %>%
            filter(globin_phenotype == Field) %>%
            select(-phenotype)
        }
      }
      if (Field == "Relative allele frequency") {
        result$SubsetE = result$SubsetE %>% select(-cause_name)
        if ("VariantC" %in% names(Info) && !is.na(Info$VariantC)) {
          vField = VariantC[VariantC$Option == Info$VariantC, "Option"]
          if (vField == "Grouped variants by globin phenotype" &&
            "GlobinPheRAF" %in% names(Info) && !is.na(Info$GlobinPheRAF)) {
            Field = GlobinPheRAF[GlobinPheRAF$Option == Info$GlobinPheRAF, "Option"]
            result$SubsetE = result$SubsetE %>%
              filter(phenotype == Field) %>%
              select(-phenotype)
          }
          if (vField == "Individual variants" &&
            "IthaID" %in% names(Info) && !is.na(Info$IthaID)) {
            Field = IthaID[IthaID$Option == Info$IthaID, "Option"]
            result$SubsetE = result$SubsetE %>%
              filter(ithaID == Field) %>%
              select(-phenotype)
          }
        }
      }
      if ("Cause" %in% names(Info) && !is.na(Info$Cause)) {
        Field = Cause[Cause$Option == Info$Cause, "Option"]
        result$SubsetE = result$SubsetE %>%
          filter(cause_name == Field) %>%
          select(-cause_name, -phenotype)
      }
    }
    result
  })
  SubsetE = parameter_result$SubsetE
  SubsetHCP = parameter_result$SubsetHCP
  SubsetHCPRaw = SubsetHCP
  debug_capture("after_parameter_SubsetE", SubsetE)
  debug_capture("after_parameter_SubsetHCP", SubsetHCP)

  # --- Metric & Aggregation ---
  if (!is.null(SubsetE)) {
    agg_level = if ("Aggregation" %in% names(Info) && !is.na(Info$Aggregation)) {
      Aggregation[Aggregation$Option == Info$Aggregation, "Option"]
    } else {
      "Country-level"
    }
    mField = NULL
    MetricN = "Value"
    if ("Metric" %in% names(Info) && !is.na(Info$Metric)) {
      mField = Metric[Metric$Option == Info$Metric, "Extra"]
      MetricN = Metric %>%
        filter(Extra == mField) %>%
        pull(Option)
    }
    # Capture the data unit (from the metric table) so it can be shown in the
    # legend title and the summary table. Distinct from MetricN (the Joomla
    # summary statistic). Only used when the whole subset shares one unit.
    if ("metric_unit" %in% names(SubsetE)) {
      unit_raw = as.character(SubsetE$metric_unit)
      unit_raw[is.na(unit_raw) | !nzchar(trimws(unit_raw))] = "<missing>"
      unit_counts = sort(table(unit_raw), decreasing = TRUE)

      unit_vals = setdiff(names(unit_counts), "<missing>")
      if (length(unit_vals) == 1) {
        MetricUnit = unit_vals[[1]]
      }

      # DEBUG: flag queries where units are mixed/missing so we can explain why
      # a unit is not displayed alongside the selected summary metric.
      has_missing_units = "<missing>" %in% names(unit_counts)
      has_mixed_units = length(unit_vals) > 1
      has_no_units = length(unit_vals) == 0
      if (has_missing_units || has_mixed_units || has_no_units) {
        format_top_counts = function(x, n = 8L) {
          if (length(x) == 0) {
            return("none")
          }
          shown = head(x, n)
          paste(paste0(names(shown), "=", as.integer(shown)), collapse = ", ")
        }

        metric_fk_counts = NULL
        if ("metric_fk_id" %in% names(SubsetE)) {
          metric_fk_raw = as.character(SubsetE$metric_fk_id)
          metric_fk_raw[is.na(metric_fk_raw) | !nzchar(trimws(metric_fk_raw))] = "<missing>"
          metric_fk_counts = sort(table(metric_fk_raw), decreasing = TRUE)
        }

        flags = c(
          if (has_mixed_units) "mixed_units" else NULL,
          if (has_missing_units) "missing_unit_values" else NULL,
          if (has_no_units) "no_non_missing_unit" else NULL
        )

        cat(
          "[ithamaps-debug][metric-unit]",
          "qs='", normalize_query_string(raw_qs), "'",
          " rows=", nrow(SubsetE),
          " measure='", as.character(Info$Measure %||% ""), "'",
          " cause='", as.character(Info$Cause %||% ""), "'",
          " summary_metric='", as.character(Info$Metric %||% ""), "'",
          " units={", format_top_counts(unit_counts), "}",
          " metric_fk={", format_top_counts(metric_fk_counts), "}",
          " FLAG=", paste(flags, collapse = ","),
          "\n",
          sep = ""
        )
        flush.console()
      }
    }
    group_col = switch(agg_level,
      "Country-level"  = "geo_admin0",
      "Province-level" = "geo_admin1",
      "District-level" = "geo_admin2"
    )
    timing_env[["group_prepare"]] = 0
    SubsetE = timed_call(timing_env, "metric_compute", function() {
      compute_outlier_aware_metric(SubsetE, group_col, mField)
    })
    timing_env[["metric_finalize"]] = 0

    SubsetE = timed_call(timing_env, "geometry_finalize", function() {
      idx0 = match(SubsetE$geo_admin0, adm0_lookup$geo_admin0)
      idx1 = match(SubsetE$geo_admin1, adm1_lookup$geo_admin1)
      idx2 = match(SubsetE$geo_admin2, adm2_lookup$geo_admin2)

      SubsetE %>%
        mutate(
          Region = adm0_lookup$Region[idx0],
          Region1 = ifelse(is.na(adm1_lookup$Region1[idx1]), "Not applicable", adm1_lookup$Region1[idx1]),
          Region2 = ifelse(is.na(adm2_lookup$Region2[idx2]), "Not applicable", adm2_lookup$Region2[idx2])
        )
    })

    polygon_keys = timed_call(timing_env, "polygon_subset", function() {
      if (agg_level == "Country-level") {
        SubsetE %>%
          dplyr::select(geo_admin0, Metric) %>%
          distinct()
      } else if (agg_level == "Province-level") {
        SubsetE %>%
          dplyr::select(geo_admin0, geo_admin1, Metric) %>%
          distinct()
      } else {
        SubsetE %>%
          dplyr::select(geo_admin0, geo_admin1, geo_admin2, Metric) %>%
          distinct()
      }
    })

    SubsetG = timed_call(timing_env, "geometry_join", function() {
      if (agg_level == "Country-level") {
        idx0 = match(polygon_keys$geo_admin0, adm0_sel$geo_admin0)
        st_as_sf(
          polygon_keys %>% mutate(
            Region = adm0_lookup$Region[idx0],
            Region1 = "Not applicable",
            Region2 = "Not applicable",
            geom = st_geometry(adm0_sel)[idx0]
          ),
          sf_column_name = "geom"
        )
      } else if (agg_level == "Province-level") {
        idx0 = match(polygon_keys$geo_admin0, adm0_lookup$geo_admin0)
        idx1 = match(polygon_keys$geo_admin1, adm1_sel$geo_admin1)
        st_as_sf(
          polygon_keys %>% mutate(
            Region = adm0_lookup$Region[idx0],
            Region1 = adm1_lookup$Region1[idx1],
            Region2 = "Not applicable",
            geom = st_geometry(adm1_sel)[idx1]
          ),
          sf_column_name = "geom"
        )
      } else {
        idx0 = match(polygon_keys$geo_admin0, adm0_lookup$geo_admin0)
        idx1 = match(polygon_keys$geo_admin1, adm1_lookup$geo_admin1)
        idx2 = match(polygon_keys$geo_admin2, adm2_sel$geo_admin2)
        st_as_sf(
          polygon_keys %>% mutate(
            Region = adm0_lookup$Region[idx0],
            Region1 = adm1_lookup$Region1[idx1],
            Region2 = adm2_lookup$Region2[idx2],
            geom = st_geometry(adm2_sel)[idx2]
          ),
          sf_column_name = "geom"
        )
      }
    })
  }

  if (!is.null(SubsetHCP) && nrow(SubsetHCP) > 0) {
    debug_capture("before_healthcare_harmonize", SubsetHCP)
    SubsetHCP = timed_call(timing_env, "healthcare_harmonize", function() {
      harmonized = harmonize_healthcare_subset(SubsetHCP, debug_mode = debug_mode)
      if (isTRUE(debug_mode)) {
        debug_capture("healthcare_harmonize_steps", harmonized$steps)
        return(harmonized$result)
      }
      harmonized
    })
    debug_capture("after_healthcare_harmonize", SubsetHCP)
  }

  timing_env[["total_query_bundle"]] = round(proc.time()[["elapsed"]] - total_start, 3)

  bundle = list(DataType = Info$DataType, query_info = Info, SubsetE = SubsetE, SubsetHCP = SubsetHCP, SubsetG = SubsetG, MetricN = MetricN, MetricUnit = MetricUnit, validation_errors = character(), timings = timing_list(timing_env))
  bundle$SubsetHCPRaw = SubsetHCPRaw

  if (isTRUE(debug_mode)) {
    query_bundle_debug_counter <<- query_bundle_debug_counter + 1L
    debug_name = sprintf("bundle_debug_%04d", query_bundle_debug_counter)
    bundle$debug = list(
      enabled = TRUE,
      name = debug_name,
      query = Query,
      info = Info,
      steps = debug_steps
    )
    assign(debug_name, bundle$debug, envir = query_bundle_debug_store)
    assign("ithamaps_last_bundle_debug", bundle$debug, envir = .GlobalEnv)
    assign("ithamaps_last_bundle_debug_name", debug_name, envir = .GlobalEnv)
  }

  bundle
}

build_query_bundle_cached = function(raw_qs) {
  cache_key = paste(
    query_bundle_cache_version,
    ifelse(debug_bundle_enabled(), "debug_on", "debug_off"),
    normalize_query_string(raw_qs),
    sep = "::"
  )

  bundle_has_timings = function(bundle) {
    is.list(bundle) && is.list(bundle$timings) && length(bundle$timings) > 0
  }

  if (exists(cache_key, envir = query_bundle_cache, inherits = FALSE)) {
    bundle = get(cache_key, envir = query_bundle_cache, inherits = FALSE)
    if (!bundle_has_timings(bundle)) {
      rm(list = cache_key, envir = query_bundle_cache)
    } else {
      bundle$timings$cache_hit = TRUE
      bundle$timings$cache_lookup = 0
      return(bundle)
    }
  }

  cache_start = proc.time()[["elapsed"]]
  bundle = build_query_bundle(raw_qs)

  # Leaflet polygon layers require sf/spatial input. Ensure the aggregated
  # polygon payload preserves sf class before caching.
  if (!is.null(bundle$SubsetG) && !inherits(bundle$SubsetG, "sf") && "geom" %in% names(bundle$SubsetG)) {
    bundle$SubsetG = st_as_sf(bundle$SubsetG)
  }

  bundle$timings$cache_hit = FALSE
  bundle$timings$cache_lookup = round(proc.time()[["elapsed"]] - cache_start, 3)

  assign(cache_key, bundle, envir = query_bundle_cache)
  bundle
}

