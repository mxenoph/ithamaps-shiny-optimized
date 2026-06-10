library(DT)
library(sf)
library(dplyr)
library(bslib)
library(shiny)
library(readxl)
library(stringr)
library(metafor)
library(leaflet)
library(viridis)
library(RMariaDB)
library(ggplot2)
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
read_secret_or_env <- function(value_key, file_key) {
  file_path <- Sys.getenv(file_key, unset = "")
  if (nchar(file_path) > 0 && file.exists(file_path)) {
    value <- readLines(file_path, warn = FALSE, n = 1)
    if (length(value) > 0 && nchar(value[1]) > 0) {
      return(trimws(value[1]))
    }
  }
  Sys.getenv(value_key, unset = "")
}

pick_configuration <- function() {
  cfg <- read_xlsx("User_Configuration.xlsx")
  machine_env <- Sys.getenv("ITHAMAPS_MACHINE", unset = "")
  if (nchar(machine_env) > 0 && machine_env %in% cfg$machine) {
    row <- cfg %>%
      filter(machine == machine_env) %>%
      slice(1)
  } else {
    row <- cfg %>% slice(1)
  }
  host_env <- Sys.getenv("ITHAMAPS_DB_HOST", unset = "")
  if (nchar(host_env) > 0) row$host <- host_env
  port_env <- Sys.getenv("ITHAMAPS_DB_PORT", unset = "")
  if (nchar(port_env) > 0) row$port <- as.integer(port_env)
  user_env <- read_secret_or_env("DB_USER", "DB_USER_FILE")
  if (nchar(user_env) > 0) row$username <- user_env
  pass_env <- read_secret_or_env("DB_PASSWORD", "DB_PASSWORD_FILE")
  if (nchar(pass_env) > 0) row$password <- pass_env
  row
}

scalar_text <- function(value, field_name) {
  out <- as.character(value[[1]])
  out <- trimws(out)
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

scalar_port <- function(value, field_name = "port") {
  out <- suppressWarnings(as.integer(value[[1]]))
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

Configuration <- pick_configuration()

# ---------------------------------------------------------------------------
# open_mariadb_connection(): connect using correct RMariaDB argument names
# ---------------------------------------------------------------------------
open_mariadb_connection <- function(dbname, cfg) {
  user_val <- scalar_text(cfg$username, "username")
  pass_val <- scalar_text(cfg$password, "password")
  host_val <- scalar_text(cfg$host, "host")
  port_val <- scalar_port(cfg$port, "port")
  dbConnect(RMariaDB::MariaDB(),
    dbname   = dbname,
    user     = user_val,
    password = pass_val,
    host     = host_val,
    port     = port_val
  )
}

# Connection to ITHANET
Ithanet <- open_mariadb_connection("ithabase_mk", Configuration)

Datatables <- dbListTables(Ithanet)
Datatables <- Datatables[Datatables %in% c(
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

for (Data in Datatables) {
  assign(
    paste("db_", Data, sep = ""),
    (dbReadTable(Ithanet, Data) %>% as_tibble())
  )
}

dbDisconnect(Ithanet)

rm(Ithanet, Data, Datatables)

# Connection to joomla
Joomla <- open_mariadb_connection("joomla_live", Configuration)

Datatables <- dbListTables(Joomla)
Datatables <- Datatables[Datatables %in% c("itha_experts")]

for (Data in Datatables) {
  assign(
    paste("db_", Data, sep = ""),
    (dbReadTable(Joomla, Data) %>% as_tibble())
  )
}

dbDisconnect(Joomla)

rm(Joomla, Data, Datatables)

# ---------------------------------------------------------------------------
# Join diagnostics: export and silence expected many-to-many joins
#   - writes summary and key-level diagnostics for detected many-to-many joins
#   - uses relationship='many-to-many' only when detected to avoid noisy warnings
# ---------------------------------------------------------------------------
join_log_dir <- Sys.getenv("ITHAMAPS_JOIN_LOG_DIR", unset = "logs")
dir.create(join_log_dir, recursive = TRUE, showWarnings = FALSE)
join_log_summary_path <- file.path(join_log_dir, "many_to_many_join_summary.csv")
join_log_keys_path <- file.path(join_log_dir, "many_to_many_join_keys.csv")

append_csv_row <- function(path, df_row) {
  needs_header <- !file.exists(path)
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

log_many_to_many_join <- function(x, y, by, join_name) {
  by_cols <- if (is.character(by)) by else names(by)

  left_dups <- x %>%
    dplyr::count(dplyr::across(dplyr::all_of(by_cols)), name = "left_n") %>%
    dplyr::filter(left_n > 1)

  right_dups <- y %>%
    dplyr::count(dplyr::across(dplyr::all_of(by_cols)), name = "right_n") %>%
    dplyr::filter(right_n > 1)

  m2m_keys <- dplyr::inner_join(left_dups, right_dups, by = by_cols)
  has_m2m <- nrow(m2m_keys) > 0

  if (has_m2m) {
    timestamp <- format(Sys.time(), "%Y-%m-%d %H:%M:%S")
    summary_row <- data.frame(
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

    key_rows <- m2m_keys %>%
      dplyr::mutate(
        timestamp = timestamp,
        join_name = join_name,
        .before = 1
      )
    append_csv_row(join_log_keys_path, key_rows)
  }

  has_m2m
}

left_join_logged <- function(x, y, by, join_name) {
  if (log_many_to_many_join(x, y, by = by, join_name = join_name)) {
    dplyr::left_join(x, y, by = by, relationship = "many-to-many")
  } else {
    dplyr::left_join(x, y, by = by)
  }
}

# Fetch data
Note_function <- function(end_year_assumed, region_comment, nationality_comment, comments, sample_size_comment) {
  parts <- c(
    if (!is.na(end_year_assumed)) end_year_assumed,
    if (!is.na(region_comment)) region_comment,
    if (!is.na(nationality_comment)) nationality_comment,
    if (!is.na(comments)) comments,
    if (!is.na(sample_size_comment)) sample_size_comment
  )
  if (length(parts) > 0) paste(parts, collapse = ", ") else "None"
}

db_ithamaps_entries <- db_ithamaps_entries %>%
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
  left_join(db_metric, by = "metric_id") %>%
  left_join(db_ithamaps_cohort %>%
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
        phenotype %in% c("α0") ~ "α0",
        phenotype %in% c("α⁺", "α+/α0") ~ "α+",
        phenotype %in% c("β0") ~ "β0",
        phenotype %in% c("β+", "β++", "β++ (silent)", "β0 / β+") ~ "β+",
        phenotype %in% c("δ0") ~ "δ0",
        phenotype %in% c("δ+") ~ "δ+",
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
    count = as.integer(formatC(as.numeric(count), format = "f", digits = 0)),
    sample_size = as.integer(formatC(as.numeric(sample_size), format = "f", digits = 0))
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

Note_function2 <- function(end_year_assumed, region_comment, compensation_comment) {
  parts <- c(
    if (!is.na(end_year_assumed)) end_year_assumed,
    if (!is.na(region_comment)) region_comment,
    if (!is.na(compensation_comment)) compensation_comment
  )
  if (length(parts) > 0) paste(parts, collapse = ", ") else "None"
}

db_hcp_per_region <- db_hcp_per_region %>%
  rename("source_id" = source) %>%
  left_join(db_regions, by = "regions_id") %>%
  left_join(db_cause, by = "cause_id") %>%
  left_join(db_ithamaps_accumulated_sources %>%
    rename("ihme_id" = expert), by = "source_id") %>%
  left_join(db_ithamaps_log %>%
    rename("expert_id" = curated_by) %>%
    select(hcp_entry_id, expert_id), by = "hcp_entry_id") %>%
  left_join(db_itha_experts %>%
    rename("expert_id" = id) %>%
    mutate(curated_by = paste0(name, " ", surname)) %>%
    select(expert_id, curated_by), by = "expert_id") %>%
  left_join(db_itha_experts %>%
    rename("ihme_id" = id) %>%
    mutate(expert = name) %>%
    select(ihme_id, expert), by = "ihme_id") %>%
  left_join(db_hc_policies %>%
    select(hcp_id, hcp_name, ancestor0) %>%
    rename("hcp_id0" = ancestor0), by = "hcp_id") %>%
  left_join(db_hc_policies %>%
    rename(
      "hcp_id0" = hcp_id,
      "hcp_name_ancestor" = hcp_name
    ) %>%
    select(hcp_id0, hcp_name_ancestor), by = "hcp_id0") %>%
  left_join(db_country %>%
    rename("country_id" = idCountry) %>%
    select(country_id, countryName, continentName), by = "country_id") %>%
  rename("Country" = countryName) %>%
  select(
    -regions_id, -cause_id, -primary_ontology, -secondary_ontology,
    -expert_id, -hcp_id, -hcp_id0, -created.x, -updated.x,
    -created.y, -updated.y, -ihme_id, -country_id
  ) %>%
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
    eligibility = ifelse(is.na(eligibility), "Unspecified", eligibility),
    eligibility_comment = ifelse(is.na(eligibility_comment), "Unspecified", eligibility_comment),
    recruitment_site = ifelse(is.na(recruitment_site), "Unspecified", recruitment_site),
    uptake = ifelse(is.na(uptake), "Unspecified", uptake),
    implementation = ifelse(is.na(implementation), "Unspecified",
      ifelse(implementation == "policy", "Policy",
        ifelse(implementation == "pilot", "Pilot",
          ifelse(implementation == "service", "Service", "Unspecified")
        )
      )
    ),
    diagnostic_method = ifelse(hcp_name_ancestor %in% c(
      "Newborn screening (aims to establish disease in a baby shortly after birth)",
      "Prevention strategy (aims to reduce birth of new affected individuals)",
      "Prenatal genetic diagnosis (aims to establish the presence of disease in a fetus)"
    ) & is.na(diagnostic_method), "Unspecified",
    ifelse(!(hcp_name_ancestor %in% c(
      "Newborn screening (aims to establish disease in a baby shortly after birth)",
      "Prevention strategy (aims to reduce birth of new affected individuals)",
      "Prenatal genetic diagnosis (aims to establish the presence of disease in a fetus)"
    )) & is.na(diagnostic_method), "Not applicable",
    ifelse(!(hcp_name_ancestor %in% c(
      "Newborn screening (aims to establish disease in a baby shortly after birth)",
      "Prevention strategy (aims to reduce birth of new affected individuals)",
      "Prenatal genetic diagnosis (aims to establish the presence of disease in a fetus)"
    )) & !is.na(diagnostic_method), "Not applicable", diagnostic_method)
    )
    )
  ) %>%
  rowwise() %>%
  mutate(note = Note_function2(end_year_assumed, region_comment, compensation_comment)) %>%
  ungroup() %>%
  # Ported from IthaMaps-shinyapp/app.R lines 270-278: synthesize
  # healthcare availability and coverage labels before grouped harmonization.
  mutate(
    coverage = ifelse(coverage == "Universal", "National",
      ifelse(coverage == "Unclear", "NULL", coverage)
    )
  ) %>%
  mutate(across(everything(), ~ {
    value_chr <- as.character(.)
    ifelse(value_chr == ":", "NULL", value_chr)
  })) %>%
  mutate(across(everything(), ~ {
    value_chr <- as.character(.)
    ifelse(is.na(value_chr), "NULL", value_chr)
  })) %>%
  mutate(
    availability = ifelse(eligibility == "Unavailable", "Unavailable",
      ifelse(eligibility == "NULL", "NULL", "Available")
    ),
    eligibility = ifelse(eligibility == "Unavailable", "NULL", eligibility)
  ) %>%
  filter(availability != "NULL") %>%
  filter(coverage != "NULL") %>%
  mutate(
    Availability = ifelse(availability == "Available" & coverage == "Regional", "Available (Regionally)",
      ifelse(availability == "Available" & coverage == "National", "Available (Nationally)",
        ifelse(availability == "Unavailable", "Unavailable", "NULL")
      )
    )
  ) %>%
  mutate(
    compensation = gsub("and", "&", compensation),
    compensation = gsub(",", " &", compensation),
    compensation = ifelse(compensation_comment != "NULL", paste0(compensation, " (", compensation_comment, ")"), compensation)
  ) %>%
  select(
    -comments, -curated_by, -expert, -source_id, -pmid, -report, -doi, -hc_key,
    -end_year_assumed, -region_comment, -admin0, -admin1, -admin2, -admin3
  ) %>%
  distinct()

rm(Note_function, Note_function2)

# User input options
Resolution <- data.frame(
  ID = c(1, 2, 3),
  Option = c("Global-level", "Continent-level", "Country-level")
)

Continent <- data.frame(
  ID = c(1, 2, 3, 4, 5, 6, 7),
  Option = c(unique(db_country$continentName))
)

Country <- data.frame(
  ID = db_country$idCountry,
  Option = db_country$countryName
)

Parameter <- data.frame(
  ID = db_measure$measure_id,
  Option = db_measure$measure_name
) %>%
  filter(Option %in% c(
    "Prevalence", "Carrier prevalence",
    "Incidence", "Allele frequency", "Prenatal prevalence",
    "Prenatal carrier prevalence", "Relative allele frequency",
    "Preimplantation carrier prevalence", "Preimplantation prevalence"
  )) %>%
  rbind(data.frame(
    ID = 21,
    Option = "Healthcare availability"
  ))

HemoglobinopathyH <- data.frame(
  ID = db_cause$cause_id,
  Option = db_cause$cause_name
) %>%
  filter(Option %in% c("Thalassaemia", "Hemoglobinopathy", "Sickle Cell Disease"))

HemoglobinopathyP <- data.frame(
  ID = db_cause$cause_id,
  Option = db_cause$cause_name
) %>%
  filter(Option %in% c(
    "Beta Thalassaemia", "Alpha Thalassaemia", "Sickle Cell Disease", "Hemoglobin E Disease",
    "Hemoglobin C Disease", "Thalassaemia", "Delta Thalassaemia", "Sickle Cell Disease-SC",
    "Sickle Cell Disease-SE", "Sickle Beta Thalassaemia", "Hemoglobin C/Beta Thalassaemia Disease",
    "Hemoglobin E/Beta Thalassaemia Disease", "Delta Beta Thalassaemia", "Thalassaemia Intermedia",
    "Thalassaemia Major", "Hemoglobin H Disease", "Hydrops Fetalis", "Hemoglobin Barts", "Sickle Cell Disease-SS"
  ))

HemoglobinopathyC <- data.frame(
  ID = db_cause$cause_id,
  Option = db_cause$cause_name
) %>%
  filter(Option %in% c(
    "Beta Thalassaemia", "Alpha Thalassaemia", "Sickle Cell Disease", "Hemoglobin E Disease",
    "Hemoglobin C Disease", "Thalassaemia", "Delta Thalassaemia", "Sickle Cell Disease-SS"
  ))

Healthcare <- data.frame(
  ID = db_hc_policies$hcp_id,
  Option = db_hc_policies$hcp_name,
  Extra = db_hc_policies$ancestor0
) %>%
  filter(is.na(Extra)) %>%
  select(-Extra)

for (x in 1:13) {
  assign(
    paste0("HealthcareS", x),
    db_hc_policies %>%
      filter(ancestor0 == x) %>%
      select(ID = hcp_id, Option = hcp_name)
  )
}

GlobinPheAF <- data.frame(
  ID = db_globin_phenotypes$id,
  Option = db_globin_phenotypes$name
) %>%
  filter(Option %in% c("α0", "α⁺", "α-thalassaemia modifier", "non-deletional α+")) %>%
  mutate(Option = ifelse(Option == "α⁺", "α+", Option))

VariantC <- data.frame(
  ID = c(1, 2),
  Option = c("Individual variants", "Grouped variants by globin phenotype")
)

IthaID <- data.frame(
  ID = c(db_ithagenes_common$ithaID),
  Option = c(db_ithagenes_common$ithaID)
)

GlobinPheRAF <- data.frame(
  ID = db_globin_phenotypes$id,
  Option = db_globin_phenotypes$name
) %>%
  filter(Option %in% c("α0", "α⁺", "β0", "β+", "δ0", "δ+")) %>%
  mutate(Option = ifelse(Option == "α⁺", "α+", Option)) %>%
  rbind(data.frame(ID = 38, Option = "Other"))

Metric <- data.frame(
  ID = c(1, 2, 3, 4, 5, 6, 7),
  Option = c("Weighted mean", "Mean", "Median", "Highest value", "Lowest value", "Most recent value", "Value from largest surveyed population"),
  Extra = c("Wmean", "Mean", "Median", "Max", "Min", "Latest", "Largest")
)

Aggregation <- data.frame(
  ID = c(1, 2, 3),
  Option = c("Country-level", "Province-level", "District-level")
)

# Ported from IthaMaps-shinyapp/app.R lines 345-359: add the DataType query
# dimension so the target app can route between curated and prediction modes.
DataType <- data.frame(
  ID = c(1, 2),
  Option = c("Curated data", "Prediction data")
)

rm(x, Configuration, list = setdiff(ls(pattern = "^db_"), c("db_hcp_per_region", "db_ithamaps_entries")))

# ---------------------------------------------------------------------------
# Cache spatial files once at startup (re-used per session in server)
# ---------------------------------------------------------------------------
adm0_sf <- read_sf("ADM0.gpkg")
adm1_sf <- read_sf("ADM1.gpkg")
adm2_sf <- read_sf("ADM2.gpkg")

adm0_sel <- adm0_sf %>%
  dplyr::select(geo_admin0, name, geom) %>%
  rename("Region" = name)
adm1_sel <- adm1_sf %>%
  dplyr::select(geo_admin1, name, geom) %>%
  rename("Region1" = name)
adm2_sel <- adm2_sf %>%
  dplyr::select(geo_admin2, name, geom) %>%
  rename("Region2" = name)
adm0_lookup <- adm0_sel %>% st_drop_geometry()
adm1_lookup <- adm1_sel %>% st_drop_geometry()
adm2_lookup <- adm2_sel %>% st_drop_geometry()

# Ported from IthaMaps-shinyapp/app.R lines 418-426: load prediction rasters,
# priority sites, and admin lookups once so prediction mode can reuse them.
load_prediction_assets <- function() {
  mean_admin <- raster::stack(file.path("Predictions", "Mean_with_admin.tif"))
  ci95_admin <- raster::stack(file.path("Predictions", "CI95_with_admin.tif"))
  burden_admin <- raster::stack(file.path("Predictions", "Burden_with_admin.tif"))

  mean_raster <- mean_admin[["Mean"]]
  ci95_raster <- ci95_admin[["CI95"]]
  burden_raster <- burden_admin[["Burden"]]

  mean_colours <- viridis::viridis(10, option = "F", end = 0.9)
  ci95_colours <- viridis::viridis(10, option = "G", end = 0.9)
  burden_colours <- viridis::viridis(10, option = "F", end = 0.9)

  list(
    Mean_admin = mean_admin,
    CI95_admin = ci95_admin,
    Burden_admin = burden_admin,
    Mean = mean_raster,
    CI95 = ci95_raster,
    Burden = burden_raster,
    Selected_sites = read.csv(file.path("Predictions", "Selected-sites_with_admin.csv")) %>%
      dplyr::mutate(
        lon = as.numeric(lon),
        lat = as.numeric(lat)
      ) %>%
      dplyr::filter(!is.na(lon), !is.na(lat)),
    ADM0_lookup = read.csv(file.path("Predictions", "ADM0_lookup.csv")),
    ADM1_lookup = read.csv(file.path("Predictions", "ADM1_lookup.csv")),
    ADM2_lookup = read.csv(file.path("Predictions", "ADM2_lookup.csv")),
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

prediction_assets <- load_prediction_assets()

# ---------------------------------------------------------------------------
# Query helpers (called per-session inside server)
# ---------------------------------------------------------------------------
Parse <- function(Query) {
  Info <- list()
  if (is.null(Query) || length(Query) == 0 || is.na(Query[[1]])) {
    return(Info)
  }

  Query <- as.character(Query[[1]])
  Query <- trimws(Query)
  qs <- sub("^\\?", "", Query)
  if (nchar(qs) == 0) {
    return(Info)
  }

  canonicalize_key <- function(raw_key) {
    key_lower <- tolower(raw_key)
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
    if (key_lower == "parameter") {
      return("Parameter")
    }
    if (key_lower == "hemoglobinopathyh") {
      return("HemoglobinopathyH")
    }
    if (key_lower == "hemoglobinopathyc") {
      return("HemoglobinopathyC")
    }
    if (key_lower == "hemoglobinopathyp") {
      return("HemoglobinopathyP")
    }
    if (key_lower == "healthcare") {
      return("Healthcare")
    }
    if (key_lower == "healthcaredetail") {
      return("HealthcareDetail")
    }
    if (grepl("^healthcares[0-9]+$", key_lower)) {
      idx <- suppressWarnings(as.integer(sub("^healthcares([0-9]+)$", "\\1", key_lower)))
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

  for (x in strsplit(qs, "&")[[1]]) {
    Item <- strsplit(x, "=")[[1]]
    if (length(Item) == 2) {
      key <- canonicalize_key(Item[1])
      Info[[key]] <- Item[2]
    }
  }
  Info
}

Extract <- function(Query) {
  Info <- list()

  parse_int <- function(v) {
    if (is.null(v)) {
      return(NULL)
    }
    out <- suppressWarnings(as.integer(v))
    if (length(out) == 0 || is.na(out[1])) {
      return(NULL)
    }
    out[1]
  }

  expected_keys <- c(
    "DataType", "Resolution", "Continent", "Country", "Parameter",
    "HemoglobinopathyH", "HemoglobinopathyC", "HemoglobinopathyP",
    "Healthcare", "GlobinPheAF", "VariantC",
    "GlobinPheRAF", "IthaID", "Metric", "Aggregation"
  )
  expected_keys <- c(expected_keys, paste0("HealthcareS", 1:13))

  for (x in expected_keys) {
    parsed <- parse_int(Query[[x]])
    if (!is.null(parsed)) Info[[x]] <- parsed
  }

  # Backward-compatible fallback: if HealthcareDetail is provided,
  # map it to the expected HealthcareS<Healthcare> key.
  healthcare_detail <- parse_int(Query[["HealthcareDetail"]])
  healthcare_parent <- parse_int(Info[["Healthcare"]])
  if (!is.null(healthcare_detail) && !is.null(healthcare_parent) && healthcare_parent >= 1 && healthcare_parent <= 13) {
    subkey <- paste0("HealthcareS", healthcare_parent)
    if (is.null(Info[[subkey]])) {
      Info[[subkey]] <- healthcare_detail
    }
  }

  Info
}

Search <- function(Item, Identifier, Data) {
  if (is.null(Identifier)) {
    return(NA)
  }
  Outcome <- Data %>%
    filter(ID == Identifier) %>%
    pull(Option)
  if (length(Outcome) == 0) {
    return(NA)
  }
  Outcome
}

# (Parse, Extract, Search are defined above and used per-session in server)

query_bundle_cache <- new.env(parent = emptyenv())
query_bundle_cache_version <- "timings_v2"

normalize_query_string <- function(raw_qs) {
  if (is.null(raw_qs) || is.na(raw_qs) || nchar(raw_qs) == 0) {
    return("")
  }
  sub("^\\?", "", raw_qs)
}

entries_to_point_sf <- function(data) {
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

timed_call <- function(timing_env, label, fn) {
  start <- proc.time()[["elapsed"]]
  value <- fn()
  timing_env[[label]] <- round(proc.time()[["elapsed"]] - start, 3)
  value
}

timing_list <- function(timing_env) {
  as.list.environment(timing_env, all.names = TRUE)
}

# Ported from IthaMaps-shinyapp/app.R lines 1022-1063: apply the curated-data
# outlier filters and guarded weighted-mean path before aggregated metrics.
compute_outlier_aware_metric <- function(data, group_col, metric_key) {
  names_before <- colnames(data)

  filtered_data <- data %>%
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
      Scale = ifelse(metric_name == "percent", 100, 100000)
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
                data = cur_data() %>% filter(Exclude == FALSE),
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
      mutate(Metric = round(value, 2))
    )
  }

  filtered_data %>%
    dplyr::select(all_of(names_before), Metric = all_of(metric_key[[1]])) %>%
    filter(!is.na(Metric)) %>%
    mutate(Metric = round(Metric, 2))
}

# Ported from IthaMaps-shinyapp/app.R lines 1430-1504: harmonize healthcare
# availability outputs, timeframe labels, application mode, and references.
harmonize_healthcare_subset <- function(data) {
  if (is.null(data) || nrow(data) == 0) {
    return(data)
  }

  compensation_sources <- function(comment) {
    comment <- gsub("\\s*\\(.*?\\)$", "", comment)
    comment <- gsub("^Mixed\\s+", "", comment)
    comment <- gsub("\\s*&\\s*|\\s*,\\s*|\\s+and\\s+", " & ", comment)
    strsplit(comment, "\\s*&\\s*")[[1]]
  }

  data %>%
    group_by(geo_admin0, Availability) %>%
    group_modify(function(.x, .y) {
      diag_set <- setdiff(unique(.x$diagnostic_method), "NULL")
      if (length(diag_set) == 2) {
        .x$diagnostic_method <- "Biochemical/Hematological/Molecular Diagnosis"
      } else if (length(diag_set) == 1) {
        .x$diagnostic_method[.x$diagnostic_method == "NULL"] <- diag_set
      }
      .x
    }) %>%
    group_modify(function(.x, .y) {
      comp_set <- unique(.x$compensation)
      comp_set <- comp_set[!grepl("^Unspecified", comp_set)]
      all_sources <- sort(unique(unlist(lapply(comp_set, compensation_sources))))
      if (length(all_sources) > 1) {
        label <- paste("Mixed", paste(all_sources, collapse = " & "))
      } else if (length(all_sources) == 1) {
        label <- all_sources
      } else {
        label <- "Unspecified"
      }
      years <- str_extract(.x$compensation, "(?<=Compensation since )\\d{4}")
      years <- as.numeric(na.omit(years))
      if (length(years) > 0) {
        label <- paste0(label, " (Compensation since ", min(years), ")")
      }
      .x$compensation <- label
      .x
    }) %>%
    group_modify(function(.x, .y) {
      elig <- setdiff(unique(.x$eligibility), "NULL")
      if ("Universal" %in% elig) {
        label <- "Universal"
      } else if (all(c("Targeted", "On request") %in% elig)) {
        label <- "Targeted/On request"
      } else if (length(elig) == 1) {
        label <- elig
      } else {
        label <- "Unspecified"
      }
      .x$eligibility <- label
      .x
    }) %>%
    group_modify(function(.x, .y) {
      app_vals <- unique(.x$application)
      if ("Mandatory" %in% app_vals) {
        label <- "Mandatory"
      } else if ("Voluntary" %in% app_vals) {
        label <- "Voluntary"
      } else {
        label <- "Unspecified"
      }
      .x$application <- label
      .x
    }) %>%
    group_modify(function(.x, .y) {
      impl_vals <- setdiff(unique(.x$implementation), "NULL")
      if (length(impl_vals) == 0) {
        label <- "NULL"
      } else {
        impl_vals <- tools::toTitleCase(tolower(impl_vals))
        label <- paste(sort(impl_vals), collapse = "/")
      }
      .x$implementation <- label
      .x
    }) %>%
    group_modify(function(.x, .y) {
      start_vals <- suppressWarnings(as.numeric(setdiff(.x$start_year, "NULL")))
      end_vals <- suppressWarnings(as.numeric(setdiff(.x$end_year, "NULL")))
      start_min <- if (length(start_vals) > 0) min(start_vals, na.rm = TRUE) else NA_real_
      end_max <- if (length(end_vals) > 0) max(end_vals, na.rm = TRUE) else NA_real_
      .x$known_implementation_period <- dplyr::case_when(
        !is.na(start_min) & !is.na(end_max) & start_min == end_max ~ as.character(start_min),
        !is.na(start_min) & !is.na(end_max) ~ paste0(start_min, "-", end_max),
        !is.na(start_min) & is.na(end_max) ~ paste0("Since ", start_min),
        is.na(start_min) & !is.na(end_max) ~ paste0("Until ", end_max),
        TRUE ~ "Unknown"
      )
      .x
    }) %>%
    group_modify(function(.x, .y) {
      collapse_field <- function(vec) {
        out <- unique(vec[vec != "NULL"])
        if (length(out) == 0) {
          return("NULL")
        }
        paste(sort(out), collapse = " | ")
      }
      .x$citation_str <- collapse_field(.x$citation_str)
      .x
    }) %>%
    ungroup() %>%
    distinct()
}

# ---------------------------------------------------------------------------
# build_query_bundle(): run per session from URL query string.
# Returns list(SubsetE, SubsetG, MetricN).  All NULL when no valid query.
# ---------------------------------------------------------------------------
build_query_bundle <- function(raw_qs) {
  timing_env <- new.env(parent = emptyenv())
  total_start <- proc.time()[["elapsed"]]

  Query <- timed_call(timing_env, "parse_extract", function() {
    Extract(Parse(raw_qs))
  })

  # Joomla iframe currently forwards only country; default to Country-level resolution.
  if (!is.null(Query$Country) && is.null(Query$Resolution)) {
    Query$Resolution <- 3L
  }

  lookup <- list(
    DataType = DataType, Resolution = Resolution, Continent = Continent, Country = Country,
    Parameter = Parameter, HemoglobinopathyH = HemoglobinopathyH,
    HemoglobinopathyC = HemoglobinopathyC, HemoglobinopathyP = HemoglobinopathyP,
    Healthcare = Healthcare,
    HealthcareS1 = HealthcareS1, HealthcareS2 = HealthcareS2, HealthcareS3 = HealthcareS3,
    HealthcareS4 = HealthcareS4, HealthcareS5 = HealthcareS5, HealthcareS6 = HealthcareS6,
    HealthcareS7 = HealthcareS7, HealthcareS8 = HealthcareS8, HealthcareS9 = HealthcareS9,
    HealthcareS10 = HealthcareS10, HealthcareS11 = HealthcareS11, HealthcareS12 = HealthcareS12,
    HealthcareS13 = HealthcareS13, GlobinPheAF = GlobinPheAF, VariantC = VariantC,
    GlobinPheRAF = GlobinPheRAF, IthaID = IthaID, Metric = Metric, Aggregation = Aggregation
  )

  Info <- list()
  for (x in names(Query)) {
    if (x %in% names(lookup)) Info[[x]] <- Search(x, Query[[x]], lookup[[x]])
  }

  if (is.null(Info$DataType) || is.na(Info$DataType)) {
    Info$DataType <- "Curated data"
  }

  if (identical(Info$DataType, "Prediction data")) {
    timing_env[["prediction_assets"]] <- 0
    timing_env[["total_query_bundle"]] <- round(proc.time()[["elapsed"]] - total_start, 3)
    return(list(
      DataType = Info$DataType,
      query_info = Info,
      prediction = prediction_assets,
      SubsetE = NULL,
      SubsetHCP = NULL,
      SubsetG = NULL,
      MetricN = NULL,
      timings = timing_list(timing_env)
    ))
  }

  SubsetE <- NULL
  SubsetHCP <- NULL
  SubsetG <- NULL
  MetricN <- NULL

  # --- Resolution & Region ---
  resolution_result <- timed_call(timing_env, "resolution_filter", function() {
    result <- list(SubsetE = SubsetE, SubsetHCP = SubsetHCP)

    if (!("Resolution" %in% names(Info)) || is.na(Info$Resolution)) {
      return(result)
    }

    Field <- Resolution[Resolution$Option == Info$Resolution, "Option"]
    if (length(Field) > 0 && Field == "Global-level") {
      result$SubsetHCP <- db_hcp_per_region %>% select(-Country, -continentName)
      result$SubsetE <- db_ithamaps_entries %>% select(-Country, -continentName)
    }
    if (length(Field) > 0 && Field == "Continent-level") {
      if ("Continent" %in% names(Info) && !is.na(Info$Continent)) {
        Field <- Continent[Continent$Option == Info$Continent, "Option"]
        result$SubsetHCP <- db_hcp_per_region %>%
          filter(continentName == Field) %>%
          select(-Country, -continentName)
        result$SubsetE <- db_ithamaps_entries %>%
          filter(continentName == Field) %>%
          select(-Country, -continentName)
      }
    }
    if (length(Field) > 0 && Field == "Country-level") {
      if ("Country" %in% names(Info) && !is.na(Info$Country)) {
        Field <- Country[Country$Option == Info$Country, "Option"]
        result$SubsetHCP <- db_hcp_per_region %>%
          filter(Country == Field) %>%
          select(-Country, -continentName)
        result$SubsetE <- db_ithamaps_entries %>%
          filter(Country == Field) %>%
          select(-Country, -continentName)
      }
    }
    result
  })
  SubsetE <- resolution_result$SubsetE
  SubsetHCP <- resolution_result$SubsetHCP

  # --- Parameter, Hemoglobinopathy, Globin phenotype, IthaID, Healthcare ---
  parameter_result <- timed_call(timing_env, "parameter_filter", function() {
    result <- list(SubsetE = SubsetE, SubsetHCP = SubsetHCP)

    if (is.null(SubsetE) || !("Parameter" %in% names(Info)) || is.na(Info$Parameter)) {
      return(result)
    }

    Field <- Parameter[Parameter$Option == Info$Parameter, "Option"]
    if (length(Field) > 0 && Field == "Healthcare availability") {
      result$SubsetE <- NULL
      if ("HemoglobinopathyH" %in% names(Info) && !is.na(Info$HemoglobinopathyH)) {
        Field <- HemoglobinopathyH[HemoglobinopathyH$Option == Info$HemoglobinopathyH, "Option"]
        result$SubsetHCP <- result$SubsetHCP %>%
          filter(cause_name == Field) %>%
          select(-cause_name)
        if ("Healthcare" %in% names(Info) && !is.na(Info$Healthcare)) {
          Field <- Healthcare[Healthcare$Option == Info$Healthcare, "Option"]
          result$SubsetHCP <- result$SubsetHCP %>%
            filter(hcp_name_ancestor == Field) %>%
            select(-hcp_name_ancestor)
          for (sn in 1:13) {
            key <- paste0("HealthcareS", sn)
            if (key %in% names(Info) && !is.na(Info[[key]])) {
              HCS <- lookup[[key]]
              Field <- HCS[HCS$Option == Info[[key]], "Option"]
              result$SubsetHCP <- result$SubsetHCP %>%
                filter(hcp_name == Field) %>%
                select(-hcp_name)
              break
            }
          }
        }
      }
    } else if (length(Field) > 0) {
      result$SubsetHCP <- NULL
      result$SubsetE <- result$SubsetE %>%
        filter(measure_name == Field) %>%
        select(-measure_name)
      if (Field == "Allele frequency") {
        result$SubsetE <- result$SubsetE %>% select(-cause_name)
        if ("GlobinPheAF" %in% names(Info) && !is.na(Info$GlobinPheAF)) {
          Field <- GlobinPheAF[GlobinPheAF$Option == Info$GlobinPheAF, "Option"]
          result$SubsetE <- result$SubsetE %>%
            filter(globin_phenotype == Field) %>%
            select(-phenotype)
        }
      }
      if (Field == "Relative allele frequency") {
        result$SubsetE <- result$SubsetE %>% select(-cause_name)
        if ("VariantC" %in% names(Info) && !is.na(Info$VariantC)) {
          vField <- VariantC[VariantC$Option == Info$VariantC, "Option"]
          if (vField == "Grouped variants by globin phenotype" &&
            "GlobinPheRAF" %in% names(Info) && !is.na(Info$GlobinPheRAF)) {
            Field <- GlobinPheRAF[GlobinPheRAF$Option == Info$GlobinPheRAF, "Option"]
            result$SubsetE <- result$SubsetE %>%
              filter(phenotype == Field) %>%
              select(-phenotype)
          }
          if (vField == "Individual variants" &&
            "IthaID" %in% names(Info) && !is.na(Info$IthaID)) {
            Field <- IthaID[IthaID$Option == Info$IthaID, "Option"]
            result$SubsetE <- result$SubsetE %>%
              filter(ithaID == Field) %>%
              select(-phenotype)
          }
        }
      }
      if ("HemoglobinopathyC" %in% names(Info) && !is.na(Info$HemoglobinopathyC)) {
        Field <- HemoglobinopathyC[HemoglobinopathyC$Option == Info$HemoglobinopathyC, "Option"]
        result$SubsetE <- result$SubsetE %>%
          filter(cause_name == Field) %>%
          select(-cause_name, -phenotype)
      }
      if ("HemoglobinopathyP" %in% names(Info) && !is.na(Info$HemoglobinopathyP)) {
        Field <- HemoglobinopathyP[HemoglobinopathyP$Option == Info$HemoglobinopathyP, "Option"]
        result$SubsetE <- result$SubsetE %>%
          filter(cause_name == Field) %>%
          select(-cause_name, -phenotype)
      }
    }
    result
  })
  SubsetE <- parameter_result$SubsetE
  SubsetHCP <- parameter_result$SubsetHCP

  # --- Metric & Aggregation ---
  if (!is.null(SubsetE)) {
    agg_level <- if ("Aggregation" %in% names(Info) && !is.na(Info$Aggregation)) {
      Aggregation[Aggregation$Option == Info$Aggregation, "Option"]
    } else {
      "Country-level"
    }
    mField <- NULL
    MetricN <- "Value"
    if ("Metric" %in% names(Info) && !is.na(Info$Metric)) {
      mField <- Metric[Metric$Option == Info$Metric, "Extra"]
      MetricN <- Metric %>%
        filter(Extra == mField) %>%
        pull(Option)
    }
    group_col <- switch(agg_level,
      "Country-level"  = "geo_admin0",
      "Province-level" = "geo_admin1",
      "District-level" = "geo_admin2"
    )
    timing_env[["group_prepare"]] <- 0
    SubsetE <- timed_call(timing_env, "metric_compute", function() {
      compute_outlier_aware_metric(SubsetE, group_col, mField)
    })
    timing_env[["metric_finalize"]] <- 0

    SubsetE <- timed_call(timing_env, "geometry_finalize", function() {
      idx0 <- match(SubsetE$geo_admin0, adm0_lookup$geo_admin0)
      idx1 <- match(SubsetE$geo_admin1, adm1_lookup$geo_admin1)
      idx2 <- match(SubsetE$geo_admin2, adm2_lookup$geo_admin2)

      SubsetE %>%
        mutate(
          Region = adm0_lookup$Region[idx0],
          Region1 = ifelse(is.na(adm1_lookup$Region1[idx1]), "Not applicable", adm1_lookup$Region1[idx1]),
          Region2 = ifelse(is.na(adm2_lookup$Region2[idx2]), "Not applicable", adm2_lookup$Region2[idx2])
        )
    })

    polygon_keys <- timed_call(timing_env, "polygon_subset", function() {
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

    SubsetG <- timed_call(timing_env, "geometry_join", function() {
      if (agg_level == "Country-level") {
        idx0 <- match(polygon_keys$geo_admin0, adm0_sel$geo_admin0)
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
        idx0 <- match(polygon_keys$geo_admin0, adm0_lookup$geo_admin0)
        idx1 <- match(polygon_keys$geo_admin1, adm1_sel$geo_admin1)
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
        idx0 <- match(polygon_keys$geo_admin0, adm0_lookup$geo_admin0)
        idx1 <- match(polygon_keys$geo_admin1, adm1_lookup$geo_admin1)
        idx2 <- match(polygon_keys$geo_admin2, adm2_sel$geo_admin2)
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
    SubsetHCP <- timed_call(timing_env, "healthcare_harmonize", function() {
      harmonize_healthcare_subset(SubsetHCP)
    })
  }

  timing_env[["total_query_bundle"]] <- round(proc.time()[["elapsed"]] - total_start, 3)

  list(DataType = Info$DataType, query_info = Info, SubsetE = SubsetE, SubsetHCP = SubsetHCP, SubsetG = SubsetG, MetricN = MetricN, timings = timing_list(timing_env))
}

build_query_bundle_cached <- function(raw_qs) {
  cache_key <- paste(query_bundle_cache_version, normalize_query_string(raw_qs), sep = "::")

  bundle_has_timings <- function(bundle) {
    is.list(bundle) && is.list(bundle$timings) && length(bundle$timings) > 0
  }

  if (exists(cache_key, envir = query_bundle_cache, inherits = FALSE)) {
    bundle <- get(cache_key, envir = query_bundle_cache, inherits = FALSE)
    if (!bundle_has_timings(bundle)) {
      rm(list = cache_key, envir = query_bundle_cache)
    } else {
      bundle$timings$cache_hit <- TRUE
      bundle$timings$cache_lookup <- 0
      return(bundle)
    }
  }

  cache_start <- proc.time()[["elapsed"]]
  bundle <- build_query_bundle(raw_qs)

  # Leaflet polygon layers require sf/spatial input. Ensure the aggregated
  # polygon payload preserves sf class before caching.
  if (!is.null(bundle$SubsetG) && !inherits(bundle$SubsetG, "sf") && "geom" %in% names(bundle$SubsetG)) {
    bundle$SubsetG <- st_as_sf(bundle$SubsetG)
  }

  bundle$timings$cache_hit <- FALSE
  bundle$timings$cache_lookup <- round(proc.time()[["elapsed"]] - cache_start, 3)

  assign(cache_key, bundle, envir = query_bundle_cache)
  bundle
}

# Generate shiny app
ui <- fluidPage(
  theme = bs_theme(version = 5, bootswatch = "litera"),
  tags$style(HTML(".dataTables_wrapper .dataTables_filter,
                                 .dataTables_wrapper .dataTables_length,
                                 .dataTables_wrapper .dataTables_info,
                                 .dataTables_wrapper .dataTables_paginate {font-size: 0.75rem; margin: 0.25rem 0;}

                                 table.dataTable td, table.dataTable th {font-size: 0.75rem;}
                                 .dataTables_wrapper .dataTables_filter input,
                                 .dataTables_wrapper .dataTables_length select { font-size: 0.75rem; padding: 2px 4px; height: 1.5rem; line-height: 1; border-radius: 0.2rem;}
                                 .dataTables_wrapper .dataTables_paginate .paginate_button {font-size: 0.75rem; padding: 0.2rem 0.5rem; min-width: 1.5rem; margin: 0 0.1rem;}

                                 .dataTables_wrapper .dataTables_paginate {padding: 0.25rem 0;}
                                 .dataTables_wrapper .dataTables_info {padding: 0.25rem 0;}
                                 .dataTables_wrapper .dataTables_filter input,
                                 .dataTables_wrapper .dataTables_length select,
                                 table.dataTable thead .form-control {font-size: 0.75rem; padding: 2px 4px; height: 1.5rem; line-height: 1; border-radius: 0.2rem;}

                                 .dataTables_wrapper .dataTables_paginate ul.pagination li.page-item .page-link {font-size: 0.7rem !important; padding: 0.05rem 0.3rem !important; min-width: 1.1rem !important; height: 1.2rem !important;}
                                 .perf-panel {font-size: 0.82rem; margin-bottom: 1rem;}
                                 .perf-panel table {margin-bottom: 0;}
                                 .perf-panel td, .perf-panel th {padding: 0.25rem 0.5rem;}
                                 .map-title {font-size: 1rem; font-weight: 600; text-align: center; margin-bottom: 0.5rem;}
                                 .map-card {border: 1px solid #ccc; border-radius: 0.4rem; box-shadow: 0 0.125rem 0.25rem rgba(0,0,0,0.075); padding: 0.75rem; background-color: white;}
                                 .leaflet-container {background: #f8f9fa;}
                                 .info-card {border: 1px solid #ccc; border-radius: 0.4rem; box-shadow: 0 0.125rem 0.25rem rgba(0,0,0,0.075); padding: 0.75rem; background-color: #f8f9fa; font-size: 0.85rem;}
                                 .value-table {width: 100%; border-collapse: collapse;}
                                 .value-table td {border: 1px solid #ddd; padding: 4px 6px;}
                                 .value-table td:first-child {font-weight: 600; background-color: #f1f1f1; width: 40%;}
                                 .raster-legend {margin-top: 0.75rem; padding: 0.5rem 0.25rem 0.25rem 0.25rem; font-size: 0.8rem;}
                                 .raster-legend-title {font-weight: 600; margin-bottom: 0.35rem; text-align: center;}
                                 .raster-legend-bar {height: 14px; border-radius: 4px; border: 1px solid #bbb;}
                                 .raster-legend-labels {display: flex; justify-content: space-between; margin-top: 0.25rem; font-size: 0.75rem;}
                                 .download-row {margin-top: 1rem; margin-bottom: 1rem; display: flex; flex-wrap: wrap; gap: 0.5rem; justify-content: center;}")),
  uiOutput("no_data_notification"),
  uiOutput("timing_panel"),
  uiOutput("main_content")
)


server <- function(input, output, session) {
  perf_state <- reactiveValues(map_render_secs = NULL, table_render_secs = NULL)

  # Build data bundle from the current URL query string
  query_bundle <- reactive({
    raw_qs <- session$clientData$url_search %||% ""
    fetch_start <- proc.time()[["elapsed"]]
    bundle <- build_query_bundle_cached(raw_qs)
    bundle$timings <- bundle$timings %||% list()
    bundle$timings$bundle_fetch <- round(proc.time()[["elapsed"]] - fetch_start, 3)
    bundle$timings$query_string <- normalize_query_string(raw_qs)
    bundle
  })

  SubsetE_r <- reactive({
    query_bundle()$SubsetE
  })
  SubsetHCP_r <- reactive({
    query_bundle()$SubsetHCP
  })
  DataType_r <- reactive({
    query_bundle()$DataType %||% "Curated data"
  })
  is_prediction_mode <- reactive({
    identical(DataType_r(), "Prediction data")
  })
  prediction_data_r <- reactive({
    query_bundle()$prediction
  })
  is_hcp_mode <- reactive({
    hcp <- SubsetHCP_r()
    !is.null(hcp) && nrow(hcp) > 0
  })
  SubsetG_r <- reactive({
    b <- query_bundle()
    if (!is.null(b$SubsetG) && !inherits(b$SubsetG, "sf") && "geom" %in% names(b$SubsetG)) {
      st_as_sf(b$SubsetG)
    } else {
      b$SubsetG
    }
  })
  MetricN_r <- reactive({
    query_bundle()$MetricN
  })
  timing_info_r <- reactive({
    query_bundle()$timings %||% list()
  })

  data_available <- reactive({
    if (is_prediction_mode()) {
      return(TRUE)
    }
    se <- SubsetE_r()
    hcp <- SubsetHCP_r()
    (!is.null(se) && nrow(se) > 0) || (!is.null(hcp) && nrow(hcp) > 0)
  })

  filtered_data <- reactive({
    req(!is_prediction_mode())
    req(data_available())
    if (is_hcp_mode()) {
      SubsetHCP <- SubsetHCP_r()
      if (!is.null(input$data_table_rows_all)) SubsetHCP[input$data_table_rows_all, ] else SubsetHCP
    } else {
      SubsetE <- SubsetE_r()
      if (!is.null(input$data_table_rows_all)) SubsetE[input$data_table_rows_all, ] else SubsetE
    }
  })

  selected_marker_idx <- reactiveVal(NULL)
  selected_shape_idx <- reactiveVal(NULL)
  selected_row <- reactiveVal(NULL)
  selected_prediction_point <- reactiveVal(NULL)

  output$main_content <- renderUI({
    if (is_prediction_mode()) {
      # Ported from IthaMaps-shinyapp/app.R lines 489-513 and 757-851:
      # render the dedicated prediction-mode four-map layout and export actions.
      return(div(
        class = "container-fluid py-4 px-4",
        div(
          class = "row g-3 mb-3",
          div(
            class = "col-12",
            div(class = "info-card", uiOutput("selected_prediction_values"))
          )
        ),
        div(
          class = "row g-3",
          div(
            class = "col-md-6",
            div(
              class = "map-card",
              div(class = "map-title", "Predicted mean carrier prevalence"),
              withSpinner(leafletOutput("map_mean", height = "500px"), type = 3, color = "#0000CC", color.background = "white", caption = "Loading mean predicted carrier prevalence raster..."),
              uiOutput("mean_legend")
            )
          ),
          div(
            class = "col-md-6",
            div(
              class = "map-card",
              div(class = "map-title", "Prediction uncertainty (95% CI)"),
              withSpinner(leafletOutput("map_ci95", height = "500px"), type = 3, color = "#0000CC", color.background = "white", caption = "Loading prediction uncertainty raster..."),
              uiOutput("ci95_legend")
            )
          ),
          div(
            class = "col-md-6",
            div(
              class = "map-card",
              div(class = "map-title", "Estimated number of carriers"),
              withSpinner(leafletOutput("map_burden", height = "500px"), type = 3, color = "#0000CC", color.background = "white", caption = "Loading estimated number of carriers raster..."),
              uiOutput("burden_legend")
            )
          ),
          div(
            class = "col-md-6",
            div(
              class = "map-card",
              div(class = "map-title", "Prediction uncertainty (95% CI) with priority sites for future epidemiological studies"),
              withSpinner(leafletOutput("map_ci95_2", height = "500px"), type = 3, color = "#0000CC", color.background = "white", caption = "Loading prediction uncertainty raster with priority sites..."),
              uiOutput("ci95_legend_2")
            )
          )
        ),
        div(
          class = "download-row",
          downloadButton("download_tif", "Export as .tif", icon = icon("file-arrow-down"), class = "btn btn-secondary btn-sm"),
          downloadButton("download_png", "Export as .png", icon = icon("file-image"), class = "btn btn-secondary btn-sm"),
          downloadButton("download_csv", "Export as .csv", icon = icon("file-csv"), class = "btn btn-secondary btn-sm")
        )
      ))
    }

    div(
      class = "container-fluid py-4 px-4",
      div(
        class = "d-flex mb-4 shadow-sm rounded border",
        div(
          style = "width: 30%; max-height: 500px; overflow-y: auto; padding: 10px; border-right: 1px solid #ccc; background-color: #f8f9fa;",
          uiOutput("custom_popup")
        ),
        div(
          style = "flex-grow: 1;",
          withSpinner(leafletOutput("map", height = "500px"), type = 3, color = "#0000CC", color.background = "white", caption = "Retrieving requested data. This may take a moment.")
        )
      ),
      div(
        class = "mb-4 d-flex flex-wrap gap-2 justify-content-center",
        downloadButton("download_png", "Export as .png", icon = icon("file-image"), class = "btn btn-secondary btn-sm"),
        downloadButton("download_csv", "Export as .csv", icon = icon("file-arrow-down"), class = "btn btn-secondary btn-sm"),
        downloadButton("download_gpkg", "Export as .gpkg", icon = icon("file-arrow-down"), class = "btn btn-secondary btn-sm"),
        downloadButton("download_geojson", "Export as .geojson", icon = icon("file-arrow-down"), class = "btn btn-secondary btn-sm")
      ),
      div(
        class = "table-responsive shadow-sm rounded border",
        style = "max-height: 500px; overflow-y: auto;",
        DTOutput("data_table")
      )
    )
  })

  selected_parameters_r <- reactive({
    raw_qs <- session$clientData$url_search %||% ""
    Query <- Extract(Parse(raw_qs))

    if (length(Query) == 0) {
      return(data.frame(Parameter = character(), Selection = character(), stringsAsFactors = FALSE))
    }

    lookup <- list(
      DataType = DataType, Resolution = Resolution, Continent = Continent, Country = Country,
      Parameter = Parameter, HemoglobinopathyH = HemoglobinopathyH,
      HemoglobinopathyC = HemoglobinopathyC, HemoglobinopathyP = HemoglobinopathyP,
      Healthcare = Healthcare,
      HealthcareS1 = HealthcareS1, HealthcareS2 = HealthcareS2, HealthcareS3 = HealthcareS3,
      HealthcareS4 = HealthcareS4, HealthcareS5 = HealthcareS5, HealthcareS6 = HealthcareS6,
      HealthcareS7 = HealthcareS7, HealthcareS8 = HealthcareS8, HealthcareS9 = HealthcareS9,
      HealthcareS10 = HealthcareS10, HealthcareS11 = HealthcareS11, HealthcareS12 = HealthcareS12,
      HealthcareS13 = HealthcareS13, GlobinPheAF = GlobinPheAF, VariantC = VariantC,
      GlobinPheRAF = GlobinPheRAF, IthaID = IthaID, Metric = Metric, Aggregation = Aggregation
    )

    preferred_order <- c(
      "DataType", "Resolution", "Continent", "Country", "Parameter",
      "HemoglobinopathyH", "HemoglobinopathyC", "HemoglobinopathyP",
      "Healthcare", paste0("HealthcareS", 1:13),
      "GlobinPheAF", "VariantC", "GlobinPheRAF", "IthaID", "Metric", "Aggregation"
    )
    keys <- preferred_order[preferred_order %in% names(Query)]
    extra_keys <- setdiff(names(Query), keys)
    keys <- c(keys, extra_keys)

    rows <- lapply(keys, function(key) {
      id_val <- Query[[key]]
      selection <- as.character(id_val)
      if (key %in% names(lookup)) {
        opt <- suppressWarnings(Search(key, id_val, lookup[[key]]))
        if (!is.na(opt)[1]) {
          selection <- as.character(opt[[1]])
        }
      }
      data.frame(Parameter = key, Selection = selection, stringsAsFactors = FALSE)
    })

    bind_rows(rows)
  })

  summary_panel_r <- reactive({
    params_df <- selected_parameters_r()

    get_param_selection <- function(name) {
      hit <- params_df$Selection[params_df$Parameter == name]
      if (length(hit) == 0) {
        return(NA_character_)
      }
      as.character(hit[[1]])
    }

    before_count <- if (is_hcp_mode()) {
      hcp <- SubsetHCP_r()
      if (is.null(hcp)) 0 else nrow(hcp)
    } else if (is_prediction_mode()) {
      assets <- prediction_data_r()
      if (is.null(assets)) 0 else raster::ncell(assets$Mean)
    } else {
      se <- SubsetE_r()
      if (is.null(se)) 0 else nrow(se)
    }
    after_count <- if (is_prediction_mode()) {
      assets <- prediction_data_r()
      if (is.null(assets)) 0 else nrow(assets$Selected_sites)
    } else if (isTRUE(data_available())) {
      nrow(filtered_data())
    } else {
      0
    }
    mode_label <- if (is_prediction_mode()) {
      "Prediction data"
    } else if (is_hcp_mode()) {
      "Healthcare availability"
    } else {
      "Curated data"
    }

    summary_df <- bind_rows(
      data.frame(
        Parameter = c("Data mode", "Total records", "Shown records"),
        Selection = c(mode_label, as.character(before_count), as.character(after_count)),
        stringsAsFactors = FALSE
      ),
      params_df
    )

    metric_table_html <- ""
    resolution_sel <- get_param_selection("Resolution")
    aggregation_sel <- get_param_selection("Aggregation")
    metric_name_sel <- get_param_selection("Metric")
    if (!is_hcp_mode() && identical(resolution_sel, "Country-level") && identical(aggregation_sel, "Country-level")) {
      sg <- SubsetG_r()
      metric_values <- if (is.null(sg) || !("Metric" %in% names(sg))) numeric(0) else as.numeric(sg$Metric)
      metric_values <- metric_values[!is.na(metric_values)]
      metric_value_label <- if (length(metric_values) == 0) {
        "N/A"
      } else if (length(unique(metric_values)) == 1) {
        as.character(round(unique(metric_values)[1], 2))
      } else {
        paste0("Multiple (", length(unique(metric_values)), ")")
      }
      metric_name_label <- if (is.na(metric_name_sel) || !nzchar(metric_name_sel)) "Metric" else metric_name_sel
      metric_table_html <- paste0(
        "<h5 style='margin:8px 0 4px 0;'>Metric calculation</h5>",
        "<table style='width:100%; border-collapse:collapse; border: 1px solid #ddd;'>",
        "<tr><td style='padding:2px 4px; vertical-align:top; background:#f9f9f9; color:#333; font-weight:600; width:42%; white-space:nowrap; border: 1px solid #ddd;'>Metric</td>",
        sprintf("<td style='padding:2px 4px; vertical-align:top; background:#ffffff; color:#000; border: 1px solid #ddd;'>%s</td></tr>", metric_name_label),
        "<tr><td style='padding:2px 4px; vertical-align:top; background:#f9f9f9; color:#333; font-weight:600; width:42%; white-space:nowrap; border: 1px solid #ddd;'>Value</td>",
        sprintf("<td style='padding:2px 4px; vertical-align:top; background:#ffffff; color:#000; border: 1px solid #ddd;'>%s</td></tr>", metric_value_label),
        "</table>"
      )
    }

    table_html <- paste0(
      "<div style='font-family:sans-serif; font-size:0.75em; max-width:600px;'>",
      "<h4 style='margin-bottom:6px;'>Selected query summary</h4>",
      "<table style='width:100%; border-collapse:collapse; border: 1px solid #ddd;'>",
      paste(apply(summary_df, 1, function(row) {
        sprintf(
          "<tr><td style='padding:2px 4px; vertical-align:top; background:#f9f9f9; color:#333; font-weight:600; width:42%%; white-space:nowrap; border: 1px solid #ddd;'>%s</td><td style='padding:2px 4px; vertical-align:top; background:#ffffff; color:#000; border: 1px solid #ddd;'>%s</td></tr>",
          row[1], row[2]
        )
      }), collapse = ""),
      "</table>",
      metric_table_html,
      "</div>"
    )
    HTML(table_html)
  })

  observeEvent(input$data_table_rows_selected, {
    selected_row(input$data_table_rows_selected)
  })

  observeEvent(filtered_data(),
    {
      selected_marker_idx(NULL)
      selected_shape_idx(NULL)
    },
    ignoreInit = TRUE
  )

  observe({
    req(!is_prediction_mode())
    req(selected_row())
    proxy <- leafletProxy("map", data = filtered_data())
    proxy %>% clearGroup("highlight")
    if (!is.null(selected_row())) {
      data <- filtered_data()[selected_row(), ]
      proxy %>%
        addCircleMarkers(
          data = data,
          lat = ~ as.numeric(latitude),
          lng = ~ as.numeric(longitude),
          color = "#0000CC",
          fillColor = "#0000CC",
          weight = 2,
          radius = 12,
          fillOpacity = 1,
          group = "highlight"
        )
    }
  })

  popup_contentA_r <- reactive({
    req(!is_prediction_mode())
    req(data_available())
    SubsetG <- SubsetG_r()
    if (is.null(SubsetG)) {
      return(list())
    }
    lapply(1:nrow(SubsetG), function(i) {
      fields <- c("Country", "Province", "District", "Value")
      values <- c(
        SubsetG$Region[i],
        SubsetG$Region1[i],
        SubsetG$Region2[i],
        SubsetG$Metric[i]
      )
      if (length(values) == 0) {
        df <- data.frame(Field = character(), Value = character())
      } else {
        df <- data.frame(Field = fields, Value = values, stringsAsFactors = FALSE)
        df <- df[df$Value != "" & !is.na(df$Value), , drop = FALSE]
      }
      if (nrow(df) > 0) {
        table_html <- paste0(
          "<div style='font-family:sans-serif; font-size:0.75em; max-width:600px;'>",
          "<h4 style='margin-bottom:6px;'>Aggregated value details</h4>",
          "<table style='width:100%; border-collapse:collapse; border: 1px solid #ddd;'>",
          paste(apply(df, 1, function(row) {
            sprintf(
              "<tr><td style='padding:2px 4px; vertical-align:top; background:#f9f9f9; color:#333; font-weight:600; width:35%%; white-space:nowrap; border: 1px solid #ddd;'>%s</td><td style='padding:2px 4px; vertical-align:top; background:#ffffff; color:#000; border: 1px solid #ddd;'>%s</td></tr>",
              row[1], row[2]
            )
          }), collapse = ""),
          "</table></div>"
        )
        HTML(table_html)
      } else {
        HTML("<div>No data available</div>")
      }
    })
  })

  popup_content_r <- reactive({
    req(!is_prediction_mode())
    req(data_available())
    if (is_hcp_mode()) {
      SubsetHCP <- SubsetHCP_r()
      idx0 <- match(SubsetHCP$geo_admin0, adm0_lookup$geo_admin0)
      country_names <- adm0_lookup$Region[idx0]
      lapply(seq_len(nrow(SubsetHCP)), function(i) {
        fields <- c(
          "Country", "Availability", "Study period", "Known implementation timeframe",
          "Eligibility", "Implementation", "Application", "Compensation",
          "Diagnostic method", "Uptake", "Recruitment site", "Notes", "Source"
        )
        values <- c(
          country_names[i],
          SubsetHCP$Availability[i],
          SubsetHCP$timeframe[i],
          SubsetHCP$known_implementation_period[i],
          SubsetHCP$eligibility[i],
          SubsetHCP$implementation[i],
          SubsetHCP$application[i],
          SubsetHCP$compensation[i],
          SubsetHCP$diagnostic_method[i],
          SubsetHCP$uptake[i],
          SubsetHCP$recruitment_site[i],
          SubsetHCP$note[i],
          SubsetHCP$citation_str[i]
        )
        df <- data.frame(Field = fields, Value = values, stringsAsFactors = FALSE)
        df <- df[df$Value != "" & !is.na(df$Value) & df$Value != "Unspecified" & df$Value != "Not applicable", ]
        table_html <- paste0(
          "<div style='font-family:sans-serif; font-size:0.75em; max-width:600px;'>",
          "<h4 style='margin-bottom:6px;'>Healthcare policy details</h4>",
          "<table style='width:100%; border-collapse:collapse; border: 1px solid #ddd;'>",
          paste(apply(df, 1, function(row) {
            sprintf(
              "<tr><td style='padding:2px 4px; background:#f9f9f9; color:#333; font-weight:600; width:35%%; white-space:nowrap; border: 1px solid #ddd;'>%s</td><td style='padding:2px 4px; background:#fff; color:#000; border: 1px solid #ddd;'>%s</td></tr>",
              row[1], row[2]
            )
          }), collapse = ""),
          "</table></div>"
        )
        HTML(table_html)
      })
    } else {
      SubsetE <- SubsetE_r()
      lapply(1:nrow(SubsetE), function(i) {
        fields <- c(
          "Country", "Province", "District", "Value", "Study period", "Risk of bias", "Globin phenotype", "IthaID",
          "Sample size", "Population tested positive", "Cohort", "Nationality", "Ethnicity", "Race", "Religion",
          "Sex", "Age", "Consanguinity", "Diagnostic method", "Recruitment site", "Coordinates", "Notes", "Source"
        )
        values <- c(
          SubsetE$Region[i], SubsetE$Region1[i], SubsetE$Region2[i], SubsetE$value[i], SubsetE$timeframe[i],
          SubsetE$bias_flag[i], SubsetE$globin_phenotype[i], SubsetE$ithaID[i], SubsetE$sample_size[i],
          SubsetE$count[i], SubsetE$status_group[i], SubsetE$nationality[i], SubsetE$ethnicity_name[i],
          SubsetE$race[i], SubsetE$religion_name[i], SubsetE$sex[i], SubsetE$age[i], SubsetE$consaguinity[i],
          SubsetE$diagnostic_method[i], SubsetE$recruitment_site[i],
          paste0("(", round(as.numeric(SubsetE$latitude[i]), 4), ", ", round(as.numeric(SubsetE$longitude[i]), 4), ")"),
          SubsetE$note[i], SubsetE$citation_str[i]
        )
        df <- data.frame(Field = fields, Value = values, stringsAsFactors = FALSE)
        df <- df[df$Value != "" & !is.na(df$Value), ]
        table_html <- paste0(
          "<div style='font-family:sans-serif; font-size:0.75em; max-width:600px;'>",
          "<h4 style='margin-bottom:6px;'>Study details</h4>",
          "<table style='width:100%; border-collapse:collapse; border: 1px solid #ddd;'>",
          paste(apply(df, 1, function(row) {
            sprintf(
              "<tr><td style='padding:2px 4px; background:#f9f9f9; color:#333; font-weight:600; width:35%%; white-space:nowrap; border: 1px solid #ddd;'>%s</td><td style='padding:2px 4px; background:#fff; color:#000; border: 1px solid #ddd;'>%s</td></tr>",
              row[1], row[2]
            )
          }), collapse = ""),
          "</table></div>"
        )
        HTML(table_html)
      })
    }
  })

  pal_metric_r <- reactive({
    req(!is_prediction_mode())
    req(data_available())
    if (is_hcp_mode()) {
      return(NULL)
    }
    SubsetG <- SubsetG_r()
    viridis_palette <- viridis::viridis(81, option = "F", begin = 0, end = 0.7, direction = -1)
    metric_values <- SubsetG$Metric
    unique_vals <- unique(metric_values)
    if (length(unique_vals) == 1) {
      # Expand domain so the legend can render a continuous scale.
      # Use 0 as lower bound (natural for prevalence/frequency data);
      # fall back to a unit interval when the single value is itself 0.
      lower <- if (unique_vals[1] > 0) 0 else -1
      expanded <- c(lower, unique_vals[1])
      list(
        pal         = colorNumeric(palette = viridis_palette, domain = expanded),
        legend_vals = expanded
      )
    } else {
      list(
        pal         = colorNumeric(palette = viridis_palette, domain = metric_values),
        legend_vals = metric_values
      )
    }
  })

  # Ported from IthaMaps-shinyapp/app.R lines 513-722: build prediction-mode
  # legends, synchronized map behaviour, and click-based raster interrogation.
  sync_js <- "function(el, x) {if (!window.syncedLeafletMaps) {window.syncedLeafletMaps = {};} var map = this; window.syncedLeafletMaps[el.id] = map; function initialiseSync() {var mapIds = ['map_mean', 'map_ci95', 'map_burden', 'map_ci95_2']; var maps = mapIds.map(function(id) {return window.syncedLeafletMaps[id];}); if (maps.some(function(m) {return !m;})) {setTimeout(initialiseSync, 250); return;} if (window.allMapsSyncReady) {return;} window.allMapsSyncReady = true; var syncing = false; function syncAll(source) {if (syncing) return; syncing = true; maps.forEach(function(target) {if (target !== source) {target.setView(source.getCenter(), source.getZoom(), {animate: false, reset: true});}}); syncing = false;} maps.forEach(function(m) {m.on('moveend zoomend', function() {syncAll(m);});});} initialiseSync();}"

  prediction_legend_bar <- function(palette_values, title, min_value, max_value) {
    gradient <- paste0(palette_values, collapse = ", ")
    HTML(paste0(
      "<div class='raster-legend'><div class='raster-legend-title'>", title, "</div>",
      "<div class='raster-legend-bar' style='background: linear-gradient(to right, ", gradient, ");'></div>",
      "<div class='raster-legend-labels'><span>", round(min_value, 2), "</span><span>", round(max_value, 2), "</span></div></div>"
    ))
  }

  lookup_prediction_admin <- function(id, lookup_table, id_col, name_col) {
    if (is.null(id) || is.na(id)) {
      return("No data")
    }
    matched_name <- lookup_table[[name_col]][match(id, lookup_table[[id_col]])]
    if (length(matched_name) == 0 || is.na(matched_name)) {
      return("No data")
    }
    matched_name
  }

  extract_prediction_values <- function(lng, lat) {
    assets <- prediction_data_r()
    req(!is.null(assets))

    point_sf <- sf::st_as_sf(
      data.frame(Longitude = lng, Latitude = lat),
      coords = c("Longitude", "Latitude"),
      crs = 4326,
      remove = FALSE
    )
    point_for_raster <- point_sf %>% sf::st_transform(raster::crs(assets$Mean))
    point_sp <- as(point_for_raster, "Spatial")

    clicked_mean <- raster::extract(assets$Mean_admin, point_sp)
    clicked_ci95 <- raster::extract(assets$CI95, point_sp)
    clicked_burden <- raster::extract(assets$Burden, point_sp)
    clicked_admin <- clicked_mean[, c("geo_admin0_raster", "geo_admin1_raster", "geo_admin2_raster"), drop = FALSE]

    geo_admin0_raster <- clicked_admin[, "geo_admin0_raster"]
    geo_admin1_raster <- clicked_admin[, "geo_admin1_raster"]
    geo_admin2_raster <- clicked_admin[, "geo_admin2_raster"]

    data.frame(
      Longitude = lng,
      Latitude = lat,
      geo_admin0_raster = geo_admin0_raster,
      geo_admin1_raster = geo_admin1_raster,
      geo_admin2_raster = geo_admin2_raster,
      ADM0 = lookup_prediction_admin(geo_admin0_raster, assets$ADM0_lookup, "geo_admin0_raster", "ADM0"),
      ADM1 = lookup_prediction_admin(geo_admin1_raster, assets$ADM1_lookup, "geo_admin1_raster", "ADM1"),
      ADM2 = lookup_prediction_admin(geo_admin2_raster, assets$ADM2_lookup, "geo_admin2_raster", "ADM2"),
      Mean = clicked_mean[, "Mean"],
      CI95 = clicked_ci95,
      Burden = clicked_burden
    )
  }

  update_selected_prediction_point <- function(click) {
    req(click$lng, click$lat)
    values <- extract_prediction_values(click$lng, click$lat)
    selected_prediction_point(values)

    for (map_id in c("map_mean", "map_ci95", "map_burden", "map_ci95_2")) {
      leafletProxy(map_id) %>%
        clearGroup("selected_point") %>%
        addCircleMarkers(
          lng = click$lng,
          lat = click$lat,
          radius = 7,
          color = "#0000CC",
          fillColor = "#0000CC",
          fillOpacity = 1,
          weight = 2,
          group = "selected_point"
        )
    }
  }

  current_prediction_extent <- reactive({
    assets <- prediction_data_r()
    req(!is.null(assets))
    bounds <- input$map_mean_bounds
    if (is.null(bounds)) {
      return(raster::extent(assets$Mean))
    }
    raster::extent(bounds$west, bounds$east, bounds$south, bounds$north)
  })

  output$mean_legend <- renderUI({
    req(is_prediction_mode())
    assets <- prediction_data_r()
    prediction_legend_bar(rev(assets$Mean_colours), "Predicted mean carrier prevalence (%)", assets$Mean_min, assets$Mean_max)
  })

  output$ci95_legend <- renderUI({
    req(is_prediction_mode())
    assets <- prediction_data_r()
    prediction_legend_bar(rev(assets$CI95_colours), "Prediction uncertainty (95% Credible Interval)", assets$CI95_min, assets$CI95_max)
  })

  output$burden_legend <- renderUI({
    req(is_prediction_mode())
    assets <- prediction_data_r()
    prediction_legend_bar(rev(assets$Burden_colours), "Estimated number of carriers", assets$Burden_min, assets$Burden_max)
  })

  output$ci95_legend_2 <- renderUI({
    req(is_prediction_mode())
    assets <- prediction_data_r()
    prediction_legend_bar(rev(assets$CI95_colours), "Prediction uncertainty (95% Credible Interval)", assets$CI95_min, assets$CI95_max)
  })

  output$selected_prediction_values <- renderUI({
    req(is_prediction_mode())
    values <- selected_prediction_point()
    if (is.null(values)) {
      return(HTML("<strong>Tip:</strong> Click any map to display the mean predicted carrier prevalence, prediction uncertainty, and estimated number of carriers at the selected location."))
    }
    mean_value <- ifelse(is.na(values$Mean), "No data", round(values$Mean, 4))
    ci95_value <- ifelse(is.na(values$CI95), "No data", round(values$CI95, 4))
    burden_value <- ifelse(is.na(values$Burden), "No data", round(values$Burden, 4))
    HTML(paste0(
      "<h5 style='margin-bottom: 8px;'>Data at selected coordinates</h5><table class='value-table'>",
      "<tr><td>Longitude</td><td>", round(values$Longitude, 5), "</td></tr>",
      "<tr><td>Latitude</td><td>", round(values$Latitude, 5), "</td></tr>",
      "<tr><td>ADM0</td><td>", values$ADM0, "</td></tr>",
      "<tr><td>ADM1</td><td>", values$ADM1, "</td></tr>",
      "<tr><td>ADM2</td><td>", values$ADM2, "</td></tr>",
      "<tr><td>Mean predicted carrier prevalence</td><td>", mean_value, "</td></tr>",
      "<tr><td>Prediction uncertainty (95% CI)</td><td>", ci95_value, "</td></tr>",
      "<tr><td>Estimated number of carriers</td><td>", burden_value, "</td></tr></table>"
    ))
  })

  output$map_mean <- renderLeaflet({
    req(is_prediction_mode())
    assets <- prediction_data_r()
    leaflet(options = leafletOptions(worldCopyJump = FALSE)) %>%
      # options no_wrap stops conntinuous raster images from wrapping around the globe
      addProviderTiles("CartoDB.Positron", options = providerTileOptions(noWrap = TRUE)) %>%
      addScaleBar(position = "bottomleft") %>%
      setView(lng = 115, lat = 30, zoom = 2) %>%
      addRasterImage(assets$Mean, colors = assets$Mean_palette, opacity = 0.8, project = TRUE) %>%
      htmlwidgets::onRender(sync_js)
  })

  output$map_ci95 <- renderLeaflet({
    req(is_prediction_mode())
    assets <- prediction_data_r()
    leaflet(options = leafletOptions(worldCopyJump = FALSE)) %>%
      # options no_wrap stops conntinuous raster images from wrapping around the globe
      addProviderTiles("CartoDB.Positron", options = providerTileOptions(noWrap = TRUE)) %>%
      addScaleBar(position = "bottomleft") %>%
      setView(lng = 115, lat = 30, zoom = 2) %>%
      addRasterImage(assets$CI95, colors = assets$CI95_palette, opacity = 0.8, project = TRUE) %>%
      htmlwidgets::onRender(sync_js)
  })

  output$map_burden <- renderLeaflet({
    req(is_prediction_mode())
    assets <- prediction_data_r()
    leaflet(options = leafletOptions(worldCopyJump = FALSE)) %>%
      # options no_wrap stops conntinuous raster images from wrapping around the globe
      addProviderTiles("CartoDB.Positron", options = providerTileOptions(noWrap = TRUE)) %>%
      addScaleBar(position = "bottomleft") %>%
      setView(lng = 115, lat = 30, zoom = 2) %>%
      addRasterImage(assets$Burden, colors = assets$Burden_palette, opacity = 0.8, project = TRUE) %>%
      htmlwidgets::onRender(sync_js)
  })

  output$map_ci95_2 <- renderLeaflet({
    req(is_prediction_mode())
    assets <- prediction_data_r()
    # options worldCopyJump = FALSE prevents the map from creating a duplicate set of tiles when the user pans across the antimeridian, which would cause confusion when interpreting the raster and clicking to interrogate values.
    leaflet(options = leafletOptions(worldCopyJump = FALSE)) %>%
      # options no_wrap stops conntinuous raster images from wrapping around the globe
      addProviderTiles("CartoDB.Positron", options = providerTileOptions(noWrap = TRUE)) %>%
      addScaleBar(position = "bottomleft") %>%
      setView(lng = 115, lat = 30, zoom = 2) %>%
      addRasterImage(assets$CI95, colors = assets$CI95_palette, opacity = 0.8, project = TRUE) %>%
      addCircleMarkers(
        data = assets$Selected_sites,
        lng = ~lon,
        lat = ~lat,
        radius = 5,
        color = "white",
        fillColor = "black",
        fillOpacity = 0.9,
        weight = 1.5,
        group = "Priority sites",
        popup = ~paste0(
          "<strong>ADM0:</strong> ", ADM0, "<br>",
          "<strong>ADM1:</strong> ", ADM1, "<br>",
          "<strong>ADM2:</strong> ", ADM2, "<br>",
          "<strong>Longitude:</strong> ", lon, "<br>",
          "<strong>Latitude:</strong> ", lat
        )
      ) %>%
      addLayersControl(overlayGroups = c("Priority sites"), options = layersControlOptions(collapsed = FALSE)) %>%
      htmlwidgets::onRender(sync_js)
  })

  observeEvent(input$map_mean_click, {
    req(is_prediction_mode())
    update_selected_prediction_point(input$map_mean_click)
  })
  observeEvent(input$map_ci95_click, {
    req(is_prediction_mode())
    update_selected_prediction_point(input$map_ci95_click)
  })
  observeEvent(input$map_burden_click, {
    req(is_prediction_mode())
    update_selected_prediction_point(input$map_burden_click)
  })
  observeEvent(input$map_ci95_2_click, {
    req(is_prediction_mode())
    update_selected_prediction_point(input$map_ci95_2_click)
  })
  observeEvent(input$map_ci95_2_marker_click, {
    req(is_prediction_mode())
    update_selected_prediction_point(input$map_ci95_2_marker_click)
  })

  output$map <- renderLeaflet({
    req(!is_prediction_mode())
    req(data_available())
    render_start <- proc.time()[["elapsed"]]

    if (is_hcp_mode()) {
      data <- SubsetHCP_r()
      map_widget <- leaflet() %>%
        addProviderTiles("CartoDB.Positron") %>%
        addScaleBar(position = "bottomleft") %>%
        addCircleMarkers(
          data = data,
          lat = ~ as.numeric(latitude),
          lng = ~ as.numeric(longitude),
          stroke = TRUE,
          color = "white",
          weight = 1,
          fillColor = "steelblue",
          fillOpacity = 1,
          radius = 7,
          clusterOptions = markerClusterOptions(
            spiderfyDistanceMultiplier = 1,
            animate = TRUE,
            animateAddingMarkers = TRUE,
            spiderfyOnMaxZoom = TRUE,
            zoomToBoundsOnClick = TRUE,
            showCoverageOnHover = TRUE,
            maxClusterRadius = 4
          )
        ) %>%
        htmlwidgets::onRender("function(el, x) {var map = this;

                                                                                     // Style clusters
                                                                                     map.on('layeradd', function(e) {var layer = e.layer; if (layer.getChildCount && layer._icon) {var count = layer.getChildCount(); var color = 'black'; var icon = L.divIcon({html: '<div style=\"background-color:' + color + '; color:white; border-radius:50%; width:20px; height:20px; display:flex; align-items:center; justify-content:center; font-weight:bold; font-size:12px;\">' + count + '</div>', className: '', iconSize: new L.Point(20, 20)}); layer.setIcon(icon);}});

                                                                                     // Highlight on hover
                                                                                     map.on('layeradd', function(e) {var layer = e.layer; if (layer instanceof L.CircleMarker && !layer.getChildCount) {layer.on('mouseover', function() {this.setStyle({radius: 10, weight: 2, color: '#0000CC', fillColor: '#0000CC'}); this.bringToFront();}); layer.on('mouseout', function() {this.setStyle({radius: 7, weight: 1, color: 'steelblue', fillColor: 'steelblue'});});}});}")
      perf_state$map_render_secs <- round(proc.time()[["elapsed"]] - render_start, 3)
      return(map_widget)
    }

    SubsetG <- SubsetG_r()
    MetricN <- MetricN_r()
    pal_metric_obj <- pal_metric_r()
    pal_metric <- pal_metric_obj$pal
    legend_vals <- pal_metric_obj$legend_vals
    data <- filtered_data()

    map_widget <- leaflet(data) %>%
      addProviderTiles("CartoDB.Positron") %>%
      addScaleBar(position = "bottomleft") %>%
      addCircleMarkers(
        lat = ~ as.numeric(latitude),
        lng = ~ as.numeric(longitude),
        stroke = TRUE,
        color = "white",
        weight = 1,
        fillColor = "black",
        fillOpacity = 1,
        radius = 7,
        clusterOptions = markerClusterOptions(
          spiderfyDistanceMultiplier = 1,
          animate = TRUE,
          animateAddingMarkers = TRUE,
          spiderfyOnMaxZoom = TRUE,
          zoomToBoundsOnClick = TRUE,
          showCoverageOnHover = TRUE,
          maxClusterRadius = 4
        )
      ) %>%
      addPolygons(
        data = SubsetG,
        weight = 0.3,
        opacity = 1,
        color = "black",
        fillOpacity = 0.8,
        smoothFactor = 0.5,
        highlightOptions = highlightOptions(
          weight = 1.4,
          color = "#0000CC",
          fillOpacity = 0.5,
          fillColor = "#0000CC",
          bringToFront = FALSE
        ),
        fillColor = ~ pal_metric(Metric)
      ) %>%
      addLegend(
        pal = pal_metric,
        values = legend_vals,
        title = MetricN,
        opacity = 1,
        position = "bottomright"
      ) %>%
      htmlwidgets::onRender("function(el, x) {var map = this;

                                                                                     // Style clusters
                                                                                     map.on('layeradd', function(e) {var layer = e.layer; if (layer.getChildCount && layer._icon) {var count = layer.getChildCount(); var color = 'black'; var icon = L.divIcon({html: '<div style=\"background-color:' + color + '; color:white; border-radius:50%; width:20px; height:20px; display:flex; align-items:center; justify-content:center; font-weight:bold; font-size:12px;\">' + count + '</div>', className: '', iconSize: new L.Point(20, 20)}); layer.setIcon(icon);}});

                                                                                     // Highlight on hover
                                                                                     map.on('layeradd', function(e) {var layer = e.layer; if (layer instanceof L.CircleMarker && !layer.getChildCount) {layer.on('mouseover', function() {this.setStyle({radius: 10, weight: 2, color: '#0000CC', fillColor: '#0000CC'}); this.bringToFront();}); layer.on('mouseout', function() {this.setStyle({radius: 7, weight: 1, color: 'white', fillColor: 'black'});});}});}")

    perf_state$map_render_secs <- round(proc.time()[["elapsed"]] - render_start, 3)
    map_widget
  })

  output$data_table <- renderDT({
    req(!is_prediction_mode())
    req(data_available())
    render_start <- proc.time()[["elapsed"]]

    if (is_hcp_mode()) {
      SubsetHCP <- SubsetHCP_r()
      idx0 <- match(SubsetHCP$geo_admin0, adm0_lookup$geo_admin0)
      df <- SubsetHCP %>%
        mutate(Country = adm0_lookup$Region[idx0]) %>%
        dplyr::select(any_of(c(
          "hcp_entry_id", "Country", "Availability", "timeframe", "known_implementation_period",
          "eligibility", "implementation", "application", "compensation", "diagnostic_method", "uptake",
          "recruitment_site", "note", "citation_str"
        ))) %>%
        dplyr::rename(any_of(c(
          "HCP Entry ID" = "hcp_entry_id",
          "Availability" = "Availability",
          "Study period" = "timeframe",
          "Known implementation timeframe" = "known_implementation_period",
          "Eligibility" = "eligibility",
          "Implementation" = "implementation",
          "Application" = "application",
          "Compensation" = "compensation",
          "Diagnostic method" = "diagnostic_method",
          "Uptake" = "uptake",
          "Recruitment site" = "recruitment_site",
          "Notes" = "note",
          "Source" = "citation_str"
        )))
      table_widget <- datatable(df,
        selection = "single",
        filter = "top",
        options = list(
          pageLength = 25,
          scrollX = TRUE,
          rowCallback = JS("function(row, data) {", "$(row).css('min-height', '30px');", "}"),
          columnDefs = list(list(visible = FALSE, targets = which(names(df) %in% c("Notes", "Source"))))
        ),
        class = "stripe hover cell-border"
      )
      perf_state$table_render_secs <- round(proc.time()[["elapsed"]] - render_start, 3)
      return(table_widget)
    }

    SubsetE <- SubsetE_r()
    df <- SubsetE %>%
      rename(
        "Country" = Region,
        "Province" = Region1,
        "District" = Region2,
        "Recruitment site" = recruitment_site,
        "Latitude" = latitude,
        "Longitude" = longitude,
        "Study period" = timeframe,
        "Risk of bias" = bias_flag,
        "Globin phenotype" = globin_phenotype,
        "IthaID" = ithaID,
        "Sample size" = sample_size,
        "Population tested positive" = count,
        "Value" = value,
        "Cohort" = status_group,
        "Nationality" = nationality,
        "Ethnicity" = ethnicity_name,
        "Race" = race,
        "Religion" = religion_name,
        "Sex" = sex,
        "Age" = age,
        "Consanguinity" = consaguinity,
        "Diagnostic method" = diagnostic_method,
        "Notes" = note,
        "Source" = citation_str
      ) %>%
      dplyr::select(
        "Country", "Province", "District", "Recruitment site", "Latitude", "Longitude",
        "Study period", "Risk of bias", "Globin phenotype", "IthaID", "Sample size",
        "Population tested positive", "Value", "Cohort", "Nationality", "Ethnicity",
        "Race", "Religion", "Sex", "Age", "Consanguinity", "Diagnostic method", "Notes", "Source"
      )
    table_widget <- datatable(df,
      selection = "single",
      filter = "top",
      options = list(
        pageLength = 25,
        scrollX = TRUE,
        rowCallback = JS("function(row, data) {", "$(row).css('min-height', '30px');", "}"),
        columnDefs = list(list(visible = FALSE, targets = which(names(df) %in% c("Notes", "Source"))))
      ),
      class = "stripe hover cell-border"
    )
    perf_state$table_render_secs <- round(proc.time()[["elapsed"]] - render_start, 3)
    table_widget
  })

  # Ported from IthaMaps-shinyapp/app.R lines 757-851: export prediction-mode
  # rasters, cropped figure, and CSV packages from the current prediction view.
  output$download_tif <- downloadHandler(
    filename = function() {
      paste0("IthaMaps_", Sys.Date(), ".zip")
    },
    content = function(file) {
      req(is_prediction_mode())
      assets <- prediction_data_r()
      Notification <- showNotification("Export as .zip in progress... Please wait until export completes before adjusting map options.", type = "message", duration = NULL)
      on.exit(removeNotification(Notification), add = TRUE)

      export_dir <- tempfile("IthaMaps_")
      dir.create(export_dir, recursive = TRUE, showWarnings = FALSE)
      export_folder_name <- paste0("IthaMaps_", Sys.Date())
      export_folder <- file.path(export_dir, export_folder_name)
      dir.create(export_folder, recursive = TRUE, showWarnings = FALSE)

      mean_export <- file.path(export_folder, "Predicted-carrier-prevalence.tif")
      ci95_export <- file.path(export_folder, "Prediction-uncertainty.tif")
      burden_export <- file.path(export_folder, "Estimated-carriers.tif")
      sites_export <- file.path(export_folder, "Priority-sites.csv")

      raster::writeRaster(assets$Mean, filename = mean_export, format = "GTiff", overwrite = TRUE)
      raster::writeRaster(assets$CI95, filename = ci95_export, format = "GTiff", overwrite = TRUE)
      raster::writeRaster(assets$Burden, filename = burden_export, format = "GTiff", overwrite = TRUE)
      write.csv(assets$Selected_sites, file = sites_export, row.names = FALSE)

      zip::zipr(zipfile = file, files = list.files(export_folder, full.names = TRUE), root = export_dir)
    }
  )

  output$download_png <- downloadHandler(
    contentType = "image/png",
    filename = function() {
      paste0("IthaMaps_", Sys.Date(), ".png")
    },
    content = function(file) {
      if (is_prediction_mode()) {
        assets <- prediction_data_r()
        Notification <- showNotification("Export as .png in progress... Please wait until export completes before adjusting map options.", type = "message", duration = NULL)
        on.exit(removeNotification(Notification), add = TRUE)

        ext <- current_prediction_extent()
        mean_crop <- tryCatch(raster::crop(assets$Mean, ext), error = function(e) assets$Mean)
        ci95_crop <- tryCatch(raster::crop(assets$CI95, ext), error = function(e) assets$CI95)
        burden_crop <- tryCatch(raster::crop(assets$Burden, ext), error = function(e) assets$Burden)

        if (is.null(mean_crop) || all(is.na(mean_crop[]))) mean_crop <- assets$Mean
        if (is.null(ci95_crop) || all(is.na(ci95_crop[]))) ci95_crop <- assets$CI95
        if (is.null(burden_crop) || all(is.na(burden_crop[]))) burden_crop <- assets$Burden

        png(filename = file, width = 1800, height = 1800, res = 150)
        par(mfrow = c(2, 2), mar = c(4, 4, 4, 5))
        raster::plot(mean_crop, col = rev(assets$Mean_colours), main = "Predicted carrier prevalence (%)", axes = TRUE, box = TRUE)
        raster::plot(ci95_crop, col = rev(assets$CI95_colours), main = "Prediction uncertainty (95% Credible Interval)", axes = TRUE, box = TRUE)
        raster::plot(burden_crop, col = rev(assets$Burden_colours), main = "Estimated number of carriers", axes = TRUE, box = TRUE)
        raster::plot(ci95_crop, col = rev(assets$CI95_colours), main = "Prediction uncertainty with priority sites", axes = TRUE, box = TRUE)

        selected_sites_export <- assets$Selected_sites %>%
          dplyr::filter(
            lon >= raster::xmin(ci95_crop),
            lon <= raster::xmax(ci95_crop),
            lat >= raster::ymin(ci95_crop),
            lat <= raster::ymax(ci95_crop)
          )
        points(selected_sites_export$lon, selected_sites_export$lat, pch = 21, bg = "black", col = "white", cex = 0.8)
        dev.off()
        return(invisible(NULL))
      }

      if (is_hcp_mode()) {
        showNotification("PNG export is not available for Healthcare availability data.", type = "warning", duration = 4)
        return(invisible(NULL))
      }
      Notification <- showNotification("Export as .png in progress...",
        type = "message", duration = NULL
      )
      on.exit(removeNotification(Notification), add = TRUE)

      SubsetG <- SubsetG_r()
      MetricN <- MetricN_r()
      pal_metric_obj <- pal_metric_r()
      legend_vals <- pal_metric_obj$legend_vals
      pts <- filtered_data() %>%
        mutate(
          longitude = suppressWarnings(as.numeric(longitude)),
          latitude  = suppressWarnings(as.numeric(latitude))
        ) %>%
        filter(!is.na(longitude), !is.na(latitude))

      # Honour the user's current map viewport if available.
      bounds <- input$map_bounds # list(north, south, east, west) or NULL
      xlim <- if (!is.null(bounds)) c(bounds$west, bounds$east) else NULL
      ylim <- if (!is.null(bounds)) c(bounds$south, bounds$north) else NULL

      # Build a continuous viridis fill scale matching the interactive map.
      fill_scale <- scale_fill_gradientn(
        colours  = viridis::viridis(81, option = "F", begin = 0, end = 0.7, direction = -1),
        limits   = range(legend_vals, na.rm = TRUE),
        na.value = "grey80",
        name     = MetricN
      )

      p <- ggplot2::ggplot() +
        ggplot2::geom_sf(
          data = SubsetG,
          ggplot2::aes(fill = Metric),
          colour = "black",
          linewidth = 0.2,
          alpha = 0.8
        ) +
        fill_scale +
        ggplot2::geom_point(
          data = pts,
          ggplot2::aes(x = longitude, y = latitude),
          colour = "black", fill = "black",
          shape = 21, size = 1.8, stroke = 0.4
        ) +
        ggplot2::coord_sf(xlim = xlim, ylim = ylim, expand = FALSE) +
        ggplot2::theme_minimal(base_size = 11) +
        ggplot2::theme(
          panel.grid = ggplot2::element_line(colour = "grey90"),
          legend.position = "right"
        )

      tryCatch(
        {
          ggplot2::ggsave(file, plot = p, width = 12, height = 8, dpi = 150, device = "png")
        },
        error = function(e) {
          showNotification(paste("PNG export failed:", conditionMessage(e)), type = "error", duration = 8)
          stop(e)
        }
      )
    }
  )

  output$download_csv <- downloadHandler(
    filename = function() {
      if (is_prediction_mode()) {
        paste0("IthaMaps_", Sys.Date(), "_csv.zip")
      } else {
        paste0("IthaMaps_", Sys.Date(), ".csv")
      }
    },
    content = function(file) {
      if (is_prediction_mode()) {
        assets <- prediction_data_r()
        Notification <- showNotification("Export as .csv in progress... Please wait until export completes before adjusting map options.", type = "message", duration = NULL)
        on.exit(removeNotification(Notification), add = TRUE)

        export_dir <- tempfile("IthaMaps_csv_")
        dir.create(export_dir, recursive = TRUE, showWarnings = FALSE)
        raster_csv <- file.path(export_dir, "Data from rasters.csv")
        sites_csv <- file.path(export_dir, "Coordinates of priority-sites.csv")
        prediction_stack <- raster::stack(assets$Mean, assets$CI95, assets$Burden)
        names(prediction_stack) <- c("Mean", "CI95", "Burden")
        prediction_values <- raster::rasterToPoints(prediction_stack) %>% as.data.frame()
        names(prediction_values) <- c("Longitude", "Latitude", "Mean", "CI95", "Burden")
        write.csv(prediction_values, file = raster_csv, row.names = FALSE)
        write.csv(assets$Selected_sites, file = sites_csv, row.names = FALSE)
        zip::zipr(zipfile = file, files = list.files(export_dir, full.names = TRUE), root = export_dir)
        return(invisible(NULL))
      }

      Notification <- showNotification("Export as .csv in progress... Please wait until export completes before adjusting filter options.",
        type = "message", duration = NULL
      )
      if (is_hcp_mode()) {
        SubsetHCP <- SubsetHCP_r()
        idx0 <- match(SubsetHCP$geo_admin0, adm0_lookup$geo_admin0)
        df <- SubsetHCP %>%
          mutate(Country = adm0_lookup$Region[idx0]) %>%
          dplyr::select(any_of(c(
            "Country", "Availability", "timeframe", "known_implementation_period",
            "eligibility", "implementation", "application", "compensation", "diagnostic_method", "uptake",
            "recruitment_site", "note", "citation_str"
          ))) %>%
          dplyr::rename(any_of(c(
            "Availability" = "Availability",
            "Study period" = "timeframe", "Eligibility" = "eligibility",
            "Known implementation timeframe" = "known_implementation_period",
            "Implementation" = "implementation",
            "Application" = "application",
            "Compensation" = "compensation",
            "Diagnostic method" = "diagnostic_method", "Uptake" = "uptake",
            "Recruitment site" = "recruitment_site",
            "Notes" = "note", "Source" = "citation_str"
          )))
        write.csv(df, file, row.names = FALSE)
        removeNotification(Notification)
        return(invisible(NULL))
      }
      SubsetE <- SubsetE_r()
      df <- SubsetE %>%
        rename(
          "Country" = Region, "Province" = Region1, "District" = Region2,
          "Recruitment site" = recruitment_site, "Latitude" = latitude,
          "Longitude" = longitude, "Study period" = timeframe,
          "Risk of bias" = bias_flag, "Globin phenotype" = globin_phenotype,
          "IthaID" = ithaID, "Sample size" = sample_size,
          "Population tested positive" = count, "Value" = value,
          "Cohort" = status_group, "Nationality" = nationality,
          "Ethnicity" = ethnicity_name, "Race" = race,
          "Religion" = religion_name, "Sex" = sex, "Age" = age,
          "Consanguinity" = consaguinity,
          "Diagnostic method" = diagnostic_method,
          "Notes" = note, "Source" = citation_str
        ) %>%
        dplyr::select(
          "Country", "Province", "District", "Recruitment site",
          "Latitude", "Longitude", "Study period", "Risk of bias",
          "Globin phenotype", "IthaID", "Sample size",
          "Population tested positive", "Value", "Cohort",
          "Nationality", "Ethnicity", "Race", "Religion",
          "Sex", "Age", "Consanguinity", "Diagnostic method",
          "Notes", "Source"
        )
      write.csv(df, file, row.names = FALSE)
      removeNotification(Notification)
    }
  )

  output$download_geojson <- downloadHandler(
    filename = function() {
      paste0("IthaMaps_", Sys.Date(), ".geojson")
    },
    content = function(file) {
      if (is_prediction_mode()) {
        showNotification("GeoJSON export is not available for prediction raster data.", type = "warning", duration = 4)
        return(invisible(NULL))
      }
      if (is_hcp_mode()) {
        showNotification("GeoJSON export is not available for Healthcare availability data.", type = "warning", duration = 4)
        return(invisible(NULL))
      }
      Notification <- showNotification("Export as .geojson in progress... Please wait until export completes before adjusting filter options.",
        type = "message", duration = NULL
      )
      SubsetE <- SubsetE_r()
      sf_df <- entries_to_point_sf(SubsetE)
      if (is.null(sf_df)) stop("No point coordinates available for GeoJSON export.")
      sf_df <- sf_df %>%
        rename(
          "Country" = Region, "Province" = Region1, "District" = Region2,
          "Recruitment site" = recruitment_site, "Latitude" = latitude,
          "Longitude" = longitude, "Study period" = timeframe,
          "Risk of bias" = bias_flag, "Globin phenotype" = globin_phenotype,
          "IthaID" = ithaID, "Sample size" = sample_size,
          "Population tested positive" = count, "Value" = value,
          "Cohort" = status_group, "Nationality" = nationality,
          "Ethnicity" = ethnicity_name, "Race" = race,
          "Religion" = religion_name, "Sex" = sex, "Age" = age,
          "Consanguinity" = consaguinity,
          "Diagnostic method" = diagnostic_method,
          "Notes" = note, "Source" = citation_str
        ) %>%
        dplyr::select(
          "Country", "Province", "District", "Recruitment site",
          "Latitude", "Longitude", "Study period", "Risk of bias",
          "Globin phenotype", "IthaID", "Sample size",
          "Population tested positive", "Value", "Cohort",
          "Nationality", "Ethnicity", "Race", "Religion",
          "Sex", "Age", "Consanguinity", "Diagnostic method",
          "Notes", "Source"
        )
      st_write(sf_df, file, driver = "GeoJSON", delete_dsn = TRUE)
      removeNotification(Notification)
    }
  )

  output$download_gpkg <- downloadHandler(
    filename = function() {
      paste0("IthaMaps_", Sys.Date(), ".gpkg")
    },
    content = function(file) {
      if (is_prediction_mode()) {
        showNotification("GPKG export is not available for prediction raster data.", type = "warning", duration = 4)
        return(invisible(NULL))
      }
      if (is_hcp_mode()) {
        showNotification("GPKG export is not available for Healthcare availability data.", type = "warning", duration = 4)
        return(invisible(NULL))
      }
      Notification <- showNotification("Export as .gpkg in progress... Please wait until export completes before adjusting filter options.",
        type = "message", duration = NULL
      )
      SubsetE <- SubsetE_r()
      sf_df <- entries_to_point_sf(SubsetE)
      if (is.null(sf_df)) stop("No point coordinates available for GPKG export.")
      sf_df <- sf_df %>%
        rename(
          "Country" = Region, "Province" = Region1, "District" = Region2,
          "Recruitment site" = recruitment_site, "Latitude" = latitude,
          "Longitude" = longitude, "Study period" = timeframe,
          "Risk of bias" = bias_flag, "Globin phenotype" = globin_phenotype,
          "IthaID" = ithaID, "Sample size" = sample_size,
          "Population tested positive" = count, "Value" = value,
          "Cohort" = status_group, "Nationality" = nationality,
          "Ethnicity" = ethnicity_name, "Race" = race,
          "Religion" = religion_name, "Sex" = sex, "Age" = age,
          "Consanguinity" = consaguinity,
          "Diagnostic method" = diagnostic_method,
          "Notes" = note, "Source" = citation_str
        ) %>%
        dplyr::select(
          "Country", "Province", "District", "Recruitment site",
          "Latitude", "Longitude", "Study period", "Risk of bias",
          "Globin phenotype", "IthaID", "Sample size",
          "Population tested positive", "Value", "Cohort",
          "Nationality", "Ethnicity", "Race", "Religion",
          "Sex", "Age", "Consanguinity", "Diagnostic method",
          "Notes", "Source"
        )
      st_write(sf_df, dsn = file, driver = "GPKG", delete_dsn = TRUE)
      removeNotification(Notification)
    }
  )

  observeEvent(input$map_marker_click, {
    req(!is_prediction_mode())
    req(data_available())
    click <- input$map_marker_click
    data <- filtered_data()
    if (!is.null(click)) {
      lng <- suppressWarnings(as.numeric(data$longitude))
      lat <- suppressWarnings(as.numeric(data$latitude))
      dists <- (lng - click$lng)^2 + (lat - click$lat)^2
      dists[is.na(dists)] <- Inf
      nearest_idx <- which.min(dists)
      selected_marker_idx(nearest_idx)
      selected_shape_idx(NULL)
    }
  })

  observeEvent(input$map_shape_click, {
    req(!is_prediction_mode())
    req(data_available())
    if (is_hcp_mode()) {
      return(invisible(NULL))
    }
    click <- input$map_shape_click
    SubsetG <- SubsetG_r()
    if (!is.null(click) && !is.null(SubsetG)) {
      clicked_shape <- st_sfc(st_point(c(click$lng, click$lat)), crs = st_crs(SubsetG))
      dists <- st_distance(clicked_shape, st_centroid(SubsetG))
      nearest_idx <- which.min(dists)
      selected_shape_idx(nearest_idx)
      selected_marker_idx(NULL)
    }
  })

  output$custom_popup <- renderUI({
    if (is_prediction_mode()) {
      return(summary_panel_r())
    }

    marker_idx <- selected_marker_idx()
    shape_idx <- selected_shape_idx()

    if (!is.null(marker_idx)) {
      popup_content <- popup_content_r()
      if (marker_idx >= 1 && marker_idx <= length(popup_content)) {
        return(popup_content[[marker_idx]])
      }
    }

    if (!is.null(shape_idx) && !is_hcp_mode()) {
      popup_contentA <- popup_contentA_r()
      if (shape_idx >= 1 && shape_idx <= length(popup_contentA)) {
        return(popup_contentA[[shape_idx]])
      }
    }

    summary_panel_r()
  })

  output$timing_panel <- renderUI({
    timings <- timing_info_r()
    if (length(timings) == 0) {
      raw_qs <- session$clientData$url_search %||% ""
      return(div(
        class = "alert alert-secondary perf-panel",
        strong("Performance timings"),
        tags$p(
          class = "mb-0",
          if (nchar(normalize_query_string(raw_qs)) == 0) {
            "No timing data available yet. The app has not received a URL query string in this session."
          } else {
            "No timing data is available for the current query."
          }
        )
      ))
    }

    timing_rows <- c(
      "Cache hit" = if (isTRUE(timings$cache_hit)) "yes" else "no",
      "Cache lookup" = if (!is.null(timings$cache_lookup)) sprintf("%.3fs", timings$cache_lookup) else NA_character_,
      "Bundle fetch" = if (!is.null(timings$bundle_fetch)) sprintf("%.3fs", timings$bundle_fetch) else NA_character_,
      "Parse + extract" = if (!is.null(timings$parse_extract)) sprintf("%.3fs", timings$parse_extract) else NA_character_,
      "Resolution filter" = if (!is.null(timings$resolution_filter)) sprintf("%.3fs", timings$resolution_filter) else NA_character_,
      "Parameter filter" = if (!is.null(timings$parameter_filter)) sprintf("%.3fs", timings$parameter_filter) else NA_character_,
      "Group prepare" = if (!is.null(timings$group_prepare)) sprintf("%.3fs", timings$group_prepare) else NA_character_,
      "Metric compute" = if (!is.null(timings$metric_compute)) sprintf("%.3fs", timings$metric_compute) else NA_character_,
      "Metric finalize" = if (!is.null(timings$metric_finalize)) sprintf("%.3fs", timings$metric_finalize) else NA_character_,
      "Geometry join" = if (!is.null(timings$geometry_join)) sprintf("%.3fs", timings$geometry_join) else NA_character_,
      "Geometry finalize" = if (!is.null(timings$geometry_finalize)) sprintf("%.3fs", timings$geometry_finalize) else NA_character_,
      "Polygon subset" = if (!is.null(timings$polygon_subset)) sprintf("%.3fs", timings$polygon_subset) else NA_character_,
      "Query bundle total" = if (!is.null(timings$total_query_bundle)) sprintf("%.3fs", timings$total_query_bundle) else NA_character_,
      "Map render" = if (!is.null(perf_state$map_render_secs)) sprintf("%.3fs", perf_state$map_render_secs) else NA_character_,
      "Table render" = if (!is.null(perf_state$table_render_secs)) sprintf("%.3fs", perf_state$table_render_secs) else NA_character_
    )

    timing_rows <- timing_rows[!is.na(timing_rows)]

    div(
      class = "alert alert-secondary perf-panel",
      strong("Performance timings"),
      tags$p(class = "mb-2", sprintf("Timing entries: %d", length(timing_rows))),
      tags$div(
        class = "perf-rows",
        lapply(names(timing_rows), function(label) {
          tags$div(
            class = "d-flex justify-content-between align-items-start perf-row",
            tags$div(class = "perf-label", label),
            tags$div(class = "perf-value", timing_rows[[label]])
          )
        })
      )
    )
  })

  output$no_data_notification <- renderUI({
    if (is_prediction_mode()) {
      return(NULL)
    }
    if (!data_available()) {
      div(class = "alert alert-warning", "No data is available for the selected parameter combination.")
    }
  })
}

shinyApp(ui = ui, server = server)
