
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
library(mapview)
library(RMariaDB)
library(webshot2)

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
    if (length(value) > 0 && nchar(value[1]) > 0) return(trimws(value[1]))
  }
  Sys.getenv(value_key, unset = "")
}

pick_configuration <- function() {
  cfg <- read_xlsx("User_Configuration.xlsx")
  machine_env <- Sys.getenv("ITHAMAPS_MACHINE", unset = "")
  if (nchar(machine_env) > 0 && machine_env %in% cfg$machine) {
    row <- cfg %>% filter(machine == machine_env) %>% slice(1)
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
    stop(paste0("Invalid DB configuration field: ", field_name,
                ". Provide it in User_Configuration.xlsx or override via env/secrets."),
         call. = FALSE)
  }
  out
}

scalar_port <- function(value, field_name = "port") {
  out <- suppressWarnings(as.integer(value[[1]]))
  if (length(out) != 1 || is.na(out) || out <= 0) {
    stop(paste0("Invalid DB configuration field: ", field_name,
                ". Must be a positive integer."),
         call. = FALSE)
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
            port     = port_val)
}

# Connection to ITHANET
Ithanet <- open_mariadb_connection("ithabase_mk", Configuration)

Datatables <- dbListTables(Ithanet)
Datatables <- Datatables[Datatables %in% c("country",
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
                                           "ithagenes_globin_phen")] 

for (Data in Datatables) {assign(paste("db_", Data, sep = ""), 
                                 (dbReadTable(Ithanet, Data) %>% as_tibble()))}

dbDisconnect(Ithanet)

rm(Ithanet, Data, Datatables)

# Connection to joomla
Joomla <- open_mariadb_connection("joomla_live", Configuration)

Datatables <- dbListTables(Joomla)
Datatables <- Datatables[Datatables %in% c("itha_experts")]

for (Data in Datatables) {assign(paste("db_", Data, sep = ""), 
                                 (dbReadTable(Joomla, Data) %>% as_tibble()))}

dbDisconnect(Joomla)

rm(Joomla, Data, Datatables)

# Fetch data
Note_function <- function(end_year_assumed, region_comment, nationality_comment, comments, sample_size_comment) {parts <- c(if (!is.na(end_year_assumed)) end_year_assumed,
                                                                                                                            if (!is.na(region_comment)) region_comment,
                                                                                                                            if (!is.na(nationality_comment)) nationality_comment,
                                                                                                                            if (!is.na(comments)) comments,
                                                                                                                            if (!is.na(sample_size_comment)) sample_size_comment)
if (length(parts) > 0) paste(parts, collapse = ", ") else "None"}

db_ithamaps_entries <- db_ithamaps_entries %>%
  rename("phen_id" = globin_phenotype) %>%
  left_join(db_globin_phenotypes %>%
              rename("phen_id" = id,
                     "globin_phenotype" = name) %>%
              select(phen_id, globin_phenotype), by = "phen_id") %>%
  left_join(db_regions, by = "regions_id") %>%
  left_join(db_measure, by = "measure_id") %>%
  left_join(db_cause, by = "cause_id") %>%
  left_join(db_metric, by = "metric_id") %>%
  left_join(db_ithamaps_cohort %>% 
              rename("cohort_id" = cid, 
                     "source_id" = source,
                     "age_group_id" = age_group,
                     "sex_group_id" = sex_group,
                     "cohort_name" = name), by = "cohort_id") %>%
  left_join(db_age_groups, by = "age_group_id") %>%
  left_join(db_sex_groups, by = "sex_group_id") %>%
  left_join(db_ethnicities, by = "ethnicity_id") %>%
  left_join(db_religions, by = "religion_id") %>%
  left_join(db_locus %>%
              rename("locus_id" = id,
                     "locus" = name), by = "locus_id") %>%
  left_join(db_ithamaps_accumulated_sources %>%
              rename("ihme_id" = expert), by = "source_id") %>%
  left_join(db_ithamaps_log %>%
              rename("expert_id" = curated_by) %>%
              select(entry_id, expert_id), by = "entry_id") %>%
  left_join(db_itha_experts %>% 
              rename("expert_id" = id) %>%
              mutate(curated_by = paste0(name, " ", surname)) %>% 
              select(expert_id, curated_by), by = "expert_id") %>%
  left_join(db_j_criteria, by = "entry_id") %>%
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
                          mutate(phenotype = case_when(phenotype %in% c("α0") ~ "α0",
                                                       phenotype %in% c("α⁺", "α+/α0") ~ "α+",
                                                       phenotype %in% c("β0") ~ "β0",
                                                       phenotype %in% c("β+", "β++", "β++ (silent)", "β0 / β+") ~ "β+",
                                                       phenotype %in% c("δ0") ~ "δ0",
                                                       phenotype %in% c("δ+") ~ "δ+",
                                                       TRUE ~ NA_character_)), by = "identifier") %>%
              filter(!is.na(phenotype)) %>%
              select(-identifier) %>%
              group_by(ithaID) %>%
              mutate(count = n()) %>%
              ungroup() %>%
              mutate(phenotype = if_else(count >= 2, NA_character_, phenotype)) %>%
              filter(!is.na(phenotype)) %>%
              select(-count) %>%
              distinct(), by = "ithaID") %>%
  select(-phen_id, -regions_id, -metric_id, -cause_id, -measure_id, 
         -primary_ontology, -secondary_ontology, -cohort_id, -cohort_name, 
         -sex_group_id, -age_group_id, -ethnicity_id, -religion_id, 
         -chr_id, -build, -refseq, -version, -locus_id, -expert_id,
         -j_criteria_id, -criterium_id, -response_id, -created.x, 
         -updated.x, -created.y, -updated.y, -created.x.x, -locus, 
         -updated.x.x, -created.y.y, -updated.y.y, -ihme_id, -country_id, -nationality) %>%
  rename("nationality" = countryName) %>%
  mutate(longitude = as.character(longitude),
         latitude = as.character(latitude)) %>%
  mutate(end_year_assumed = ifelse(end_year_assumed == 1, "End year of study period based on study's publication year", NA),
         timeframe = ifelse(!is.na(start_year) & !is.na(end_year), paste0(start_year, "-", end_year), 
                            ifelse(is.na(start_year) & !is.na(end_year), paste0("up to ", end_year) , 
                                   ifelse(!is.na(start_year) & is.na(end_year), paste0("from ", start_year), "Unspecified"))),
         nationality_comment = ifelse(nationality_comment == "assumed", "Nationality based on study's location", NA),
         age = ifelse(!is.na(age_group_name) & !is.na(min_age) & !is.na(max_age), paste0(age_group_name, " (", min_age, "-", max_age, ")"),
                      ifelse(!is.na(age_group_name) & !is.na(min_age) & is.na(max_age), paste0(age_group_name, " (", min_age, ")"),
                             ifelse(!is.na(age_group_name) & is.na(min_age) & !is.na(max_age), paste0(age_group_name, " (", max_age, ")"),
                                    ifelse(!is.na(age_group_name) & is.na(min_age) & is.na(max_age), age_group_name,
                                           ifelse(is.na(age_group_name) & !is.na(min_age) & !is.na(max_age), paste0("(", min_age, "-", max_age, ")"),
                                                  ifelse(is.na(age_group_name) & !is.na(min_age) & is.na(max_age), paste0("(", min_age, ")"),
                                                         ifelse(is.na(age_group_name) & is.na(min_age) & !is.na(max_age), paste0("(", max_age, ")"), "Unspecified"))))))),
         sex = ifelse(sex_group_name == "Both" & is.na(n_female) & is.na(n_male), "Both",
                      ifelse(sex_group_name == "Both" & !is.na(n_female) & is.na(n_male), paste0("Both (Female: ", n_female, ")"),
                             ifelse(sex_group_name == "Both" & is.na(n_female) & !is.na(n_male), paste0("Both (Male: ", n_male, ")"),
                                    ifelse(sex_group_name == "Both" & !is.na(n_female) & !is.na(n_male), paste0("Both (Female: ", n_female, ", Male: ", n_male, ")"),
                                           ifelse(sex_group_name == "Female" & !is.na(n_female) & is.na(n_male), paste0("Female (n: ", n_female, ")"),
                                                  ifelse(sex_group_name == "Male" & is.na(n_female) & !is.na(n_male), paste0("Male (n: ", n_male, ")"), "Unspecified")))))),
         status_group = ifelse(status_group == "Both", "Carriers and patients", status_group),
         nationality = ifelse(is.na(nationality), "Unspecified", nationality),
         ethnicity_name = ifelse(is.na(ethnicity_name), "Unspecified", ethnicity_name),
         race = ifelse(is.na(race), "Unspecified", race),
         religion_name = ifelse(is.na(religion_name), "Unspecified", religion_name),
         consaguinity = ifelse(is.na(consaguinity), "Unspecified", consaguinity),
         recruitment_site = ifelse(is.na(recruitment_site), "Unspecified", recruitment_site),
         globin_phenotype = ifelse(measure_name %in% c("Carrier prevalence", "Allele frequency", "Prevalence") & is.na(globin_phenotype), "Unspecified",
                                   ifelse(!(measure_name %in% c("Carrier prevalence", "Allele frequency", "Prevalence")), "Not applicable", globin_phenotype)),
         phenotype = ifelse(measure_name == "Relative allele frequency" & is.na(phenotype), "Other", phenotype),
         ithaID = ifelse(is.na(ithaID), "Not applicable", ithaID),
         count = as.integer(formatC(as.numeric(count), format = "f", digits = 0)),
         sample_size = as.integer(formatC(as.numeric(sample_size), format = "f", digits = 0))) %>%
  rowwise() %>%
  mutate(note = Note_function(end_year_assumed, region_comment, nationality_comment, comments, sample_size_comment)) %>%
  ungroup() %>% 
  select(-start_year, -score, -recomputed, -comment, -curated_by, -expert, -source_id, -pmid, -report, 
         -doi, -end_year_assumed, -region_comment, -nationality_comment, -comments, -sample_size_comment, -min_age, 
         -max_age, -age_group_name, -n_female, -n_male, -sex_group_name, -admin0, -admin1, -admin2, -admin3, -hc_key, -entry_id) %>%
  distinct()

Note_function2 <- function(end_year_assumed, region_comment, compensation_comment) {parts <- c(if (!is.na(end_year_assumed)) end_year_assumed,
                                                                                               if (!is.na(region_comment)) region_comment,
                                                                                               if (!is.na(compensation_comment)) compensation_comment)
if (length(parts) > 0) paste(parts, collapse = ", ") else "None"}

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
              rename("hcp_id0" = hcp_id,
                     "hcp_name_ancestor" = hcp_name) %>%
              select(hcp_id0, hcp_name_ancestor), by = "hcp_id0") %>%
  left_join(db_country %>%
              rename("country_id" = idCountry) %>%
              select(country_id, countryName, continentName), by = "country_id") %>%
  rename("Country" = countryName) %>%
  select(-regions_id, -cause_id, -primary_ontology, -secondary_ontology,  
         -expert_id, -hcp_id, -hcp_id0, -created.x, -updated.x, 
         -created.y, -updated.y, -ihme_id, -country_id) %>%
  mutate(longitude = as.character(longitude),
         latitude = as.character(latitude)) %>%
  mutate(end_year_assumed = ifelse(end_year_assumed == 1, "End year of study period based on study's publication year", NA),
         timeframe = ifelse(!is.na(start_year) & !is.na(end_year), paste0(start_year, "-", end_year), 
                            ifelse(is.na(start_year) & !is.na(end_year), paste0("up to ", end_year) , 
                                   ifelse(!is.na(start_year) & is.na(end_year), paste0("from ", start_year), "Unspecified"))),
         eligibility = ifelse(is.na(eligibility), "Unspecified", eligibility),
         eligibility_comment = ifelse(is.na(eligibility_comment), "Unspecified", eligibility_comment),
         recruitment_site = ifelse(is.na(recruitment_site), "Unspecified", recruitment_site),
         uptake = ifelse(is.na(uptake), "Unspecified", uptake),
         implementation = ifelse(is.na(implementation), "Unspecified",
                                 ifelse(implementation == "policy", "Policy",
                                        ifelse(implementation == "pilot", "Pilot",
                                               ifelse(implementation == "service", "Service", "Unspecified")))),
         diagnostic_method = ifelse(hcp_name_ancestor %in% c("Newborn screening (aims to establish disease in a baby shortly after birth)",
                                                             "Prevention strategy (aims to reduce birth of new affected individuals)",
                                                             "Prenatal genetic diagnosis (aims to establish the presence of disease in a fetus)") & is.na(diagnostic_method), "Unspecified",
                                    ifelse(!(hcp_name_ancestor %in% c("Newborn screening (aims to establish disease in a baby shortly after birth)",
                                                                      "Prevention strategy (aims to reduce birth of new affected individuals)",
                                                                      "Prenatal genetic diagnosis (aims to establish the presence of disease in a fetus)")) & is.na(diagnostic_method), "Not applicable",
                                           ifelse(!(hcp_name_ancestor %in% c("Newborn screening (aims to establish disease in a baby shortly after birth)",
                                                                             "Prevention strategy (aims to reduce birth of new affected individuals)",
                                                                             "Prenatal genetic diagnosis (aims to establish the presence of disease in a fetus)")) & !is.na(diagnostic_method), "Not applicable", diagnostic_method)))) %>%
  rowwise() %>%
  mutate(note = Note_function2(end_year_assumed, region_comment, compensation_comment)) %>%
  ungroup() %>% 
  select(-start_year, -end_year, -comments, -curated_by, -expert, -source_id, -pmid, -report, -doi, -hc_key,
         -end_year_assumed, -region_comment, -compensation_comment, -admin0, -admin1, -admin2, -admin3, -hcp_entry_id) %>%
  distinct()

rm(Note_function, Note_function2)

# User input options
Resolution <- data.frame(ID = c(1, 2, 3),
                         Option = c("Global-level", "Continent-level", "Country-level"))

Continent <- data.frame(ID = c(1, 2, 3, 4, 5, 6, 7),
                        Option = c(unique(db_country$continentName)))

Country <- data.frame(ID = db_country$idCountry,
                      Option = db_country$countryName)

Parameter <- data.frame(ID = db_measure$measure_id,
                        Option = db_measure$measure_name) %>%
  filter(Option %in% c("Prevalence", "Carrier prevalence", 
                       "Incidence", "Allele frequency", "Prenatal prevalence", 
                       "Prenatal carrier prevalence", "Relative allele frequency",
                       "Preimplantation carrier prevalence", "Preimplantation prevalence")) %>%
  rbind(data.frame(ID = 21,
                   Option = "Healthcare availability"))

HemoglobinopathyH <- data.frame(ID = db_cause$cause_id,
                                Option = db_cause$cause_name) %>%
  filter(Option %in% c("Thalassaemia", "Hemoglobinopathy", "Sickle Cell Disease"))

HemoglobinopathyP <- data.frame(ID = db_cause$cause_id,
                                Option = db_cause$cause_name) %>%
  filter(Option %in% c("Beta Thalassaemia", "Alpha Thalassaemia", "Sickle Cell Disease", "Hemoglobin E Disease", 
                       "Hemoglobin C Disease", "Thalassaemia", "Delta Thalassaemia", "Sickle Cell Disease-SC",
                       "Sickle Cell Disease-SE", "Sickle Beta Thalassaemia", "Hemoglobin C/Beta Thalassaemia Disease", 
                       "Hemoglobin E/Beta Thalassaemia Disease", "Delta Beta Thalassaemia", "Thalassaemia Intermedia",
                       "Thalassaemia Major", "Hemoglobin H Disease", "Hydrops Fetalis", "Hemoglobin Barts", "Sickle Cell Disease-SS"))

HemoglobinopathyC <- data.frame(ID = db_cause$cause_id,
                                Option = db_cause$cause_name) %>%
  filter(Option %in% c("Beta Thalassaemia", "Alpha Thalassaemia", "Sickle Cell Disease", "Hemoglobin E Disease", 
                       "Hemoglobin C Disease", "Thalassaemia", "Delta Thalassaemia", "Sickle Cell Disease-SS"))

Healthcare <- data.frame(ID = db_hc_policies$hcp_id,
                         Option = db_hc_policies$hcp_name,
                         Extra = db_hc_policies$ancestor0) %>% 
  filter(is.na(Extra)) %>%
  select(-Extra)

for(x in 1:13) {assign(paste0("HealthcareS", x),
                       db_hc_policies %>%
                         filter(ancestor0 == x) %>%
                         select(ID = hcp_id, Option = hcp_name))}

GlobinPheAF <- data.frame(ID = db_globin_phenotypes$id,
                          Option = db_globin_phenotypes$name) %>%
  filter(Option %in% c("α0", "α⁺", "α-thalassaemia modifier", "non-deletional α+")) %>%
  mutate(Option = ifelse(Option == "α⁺", "α+", Option))

VariantC <- data.frame(ID = c(1, 2),
                       Option = c("Individual variants", "Grouped variants by globin phenotype"))

IthaID <- data.frame(ID = c(db_ithagenes_common$ithaID),
                     Option = c(db_ithagenes_common$ithaID))

GlobinPheRAF <- data.frame(ID = db_globin_phenotypes$id,
                           Option = db_globin_phenotypes$name) %>%
  filter(Option %in% c("α0", "α⁺", "β0", "β+", "δ0", "δ+")) %>%
  mutate(Option = ifelse(Option == "α⁺", "α+", Option)) %>%
  rbind(data.frame(ID = 38, Option = "Other"))

Metric <- data.frame(ID = c(1, 2, 3, 4, 5, 6, 7),
                     Option = c("Weighted mean", "Mean", "Median", "Highest value", "Lowest value",  "Most recent value", "Value from largest surveyed population"),
                     Extra = c("Wmean", "Mean", "Median", "Max", "Min", "Latest", "Largest"))

Aggregation <- data.frame(ID = c(1, 2, 3),
                          Option = c("Country-level", "Province-level", "District-level"))

rm(x, Configuration, list = setdiff(ls(pattern = "^db_"), c("db_hcp_per_region", "db_ithamaps_entries")))

# ---------------------------------------------------------------------------
# Cache spatial files once at startup (re-used per session in server)
# ---------------------------------------------------------------------------
adm0_sf <- read_sf("ADM0.gpkg")
adm1_sf <- read_sf("ADM1.gpkg")
adm2_sf <- read_sf("ADM2.gpkg")

adm0_sel <- adm0_sf %>% dplyr::select(geo_admin0, name, geom) %>% rename("Region" = name)
adm1_sel <- adm1_sf %>% dplyr::select(geo_admin1, name, geom) %>% rename("Region1" = name)
adm2_sel <- adm2_sf %>% dplyr::select(geo_admin2, name, geom) %>% rename("Region2" = name)

# ---------------------------------------------------------------------------
# Query helpers (called per-session inside server)
# ---------------------------------------------------------------------------
Parse <- function(Query) {
  Info <- list()
  qs <- sub(".*\\?", "", Query)
  if (nchar(qs) == 0 || qs == Query) return(Info)
  for (x in strsplit(qs, "&")[[1]]) {
    Item <- strsplit(x, "=")[[1]]
    if (length(Item) == 2) {
      key <- switch(
        tolower(Item[1]),
        "country" = "Country",
        "resolution" = "Resolution",
        Item[1]
      )
      Info[[key]] <- Item[2]
    }
  }
  Info
}

Extract <- function(Query) {
  Info <- list()
  for (x in c("Resolution", "Continent", "Country", "Parameter",
              "HemoglobinopathyH", "HemoglobinopathyC", "HemoglobinopathyP",
              "Healthcare", "HealthcareS1", "HealthcareS2", "HealthcareS3",
              "HealthcareS4", "HealthcareS5", "HealthcareS6", "HealthcareS7",
              "HealthcareS8", "HealthcareS9", "HealthcareS10", "HealthcareS11",
              "HealthcareS12", "HealthcareS13", "GlobinPheAF", "VariantC",
              "GlobinPheRAF", "IthaID", "Metric", "Aggregation")) {
    if (!is.null(Query[[x]])) Info[[x]] <- as.integer(Query[[x]])
  }
  Info
}

Search <- function(Item, Identifier, Data) {
  if (is.null(Identifier)) return(NA)
  Outcome <- Data %>% filter(ID == Identifier) %>% pull(Option)
  if (length(Outcome) == 0) return(NA)
  Outcome
}

# (Parse, Extract, Search are defined above and used per-session in server)

query_bundle_cache <- new.env(parent = emptyenv())

normalize_query_string <- function(raw_qs) {
  if (is.null(raw_qs) || is.na(raw_qs) || nchar(raw_qs) == 0) {
    return("")
  }
  sub("^\\?", "", raw_qs)
}

# ---------------------------------------------------------------------------
# build_query_bundle(): run per session from URL query string.
# Returns list(SubsetE, SubsetG, MetricN).  All NULL when no valid query.
# ---------------------------------------------------------------------------
build_query_bundle <- function(raw_qs) {
  Query <- Extract(Parse(raw_qs))

  # Joomla iframe currently forwards only country; default to Country-level resolution.
  if (!is.null(Query$Country) && is.null(Query$Resolution)) {
    Query$Resolution <- 3L
  }

  lookup <- list(
    Resolution = Resolution, Continent = Continent, Country = Country,
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

  SubsetE <- NULL; SubsetHCP <- NULL; SubsetG <- NULL; MetricN <- NULL

  # --- Resolution & Region ---
  if ("Resolution" %in% names(Info) && !is.na(Info$Resolution)) {
    Field <- Resolution[Resolution$Option == Info$Resolution, "Option"]
    if (length(Field) > 0 && Field == "Global-level") {
      SubsetHCP <- db_hcp_per_region %>% select(-Country, -continentName)
      SubsetE   <- db_ithamaps_entries %>% select(-Country, -continentName)
    }
    if (length(Field) > 0 && Field == "Continent-level") {
      if ("Continent" %in% names(Info) && !is.na(Info$Continent)) {
        Field <- Continent[Continent$Option == Info$Continent, "Option"]
        SubsetHCP <- db_hcp_per_region %>% filter(continentName == Field) %>% select(-Country, -continentName)
        SubsetE   <- db_ithamaps_entries %>% filter(continentName == Field) %>% select(-Country, -continentName)
      }
    }
    if (length(Field) > 0 && Field == "Country-level") {
      if ("Country" %in% names(Info) && !is.na(Info$Country)) {
        Field <- Country[Country$Option == Info$Country, "Option"]
        SubsetHCP <- db_hcp_per_region %>% filter(Country == Field) %>% select(-Country, -continentName)
        SubsetE   <- db_ithamaps_entries %>% filter(Country == Field) %>% select(-Country, -continentName)
      }
    }
  }

  # --- Parameter, Hemoglobinopathy, Globin phenotype, IthaID, Healthcare ---
  if (!is.null(SubsetE) && "Parameter" %in% names(Info) && !is.na(Info$Parameter)) {
    Field <- Parameter[Parameter$Option == Info$Parameter, "Option"]
    if (length(Field) > 0 && Field == "Healthcare availability") {
      SubsetE <- NULL
      if ("HemoglobinopathyH" %in% names(Info) && !is.na(Info$HemoglobinopathyH)) {
        Field <- HemoglobinopathyH[HemoglobinopathyH$Option == Info$HemoglobinopathyH, "Option"]
        SubsetHCP <- SubsetHCP %>% filter(cause_name == Field) %>% select(-cause_name)
        if ("Healthcare" %in% names(Info) && !is.na(Info$Healthcare)) {
          Field <- Healthcare[Healthcare$Option == Info$Healthcare, "Option"]
          SubsetHCP <- SubsetHCP %>% filter(hcp_name_ancestor == Field) %>% select(-hcp_name_ancestor)
          for (sn in 1:13) {
            key <- paste0("HealthcareS", sn)
            if (key %in% names(Info) && !is.na(Info[[key]])) {
              HCS <- lookup[[key]]
              Field <- HCS[HCS$Option == Info[[key]], "Option"]
              SubsetHCP <- SubsetHCP %>% filter(hcp_name == Field) %>% select(-hcp_name)
              break
            }
          }
        }
      }
    } else if (length(Field) > 0) {
      SubsetHCP <- NULL
      SubsetE <- SubsetE %>% filter(measure_name == Field) %>% select(-measure_name)
      if (Field == "Allele frequency") {
        SubsetE <- SubsetE %>% select(-cause_name)
        if ("GlobinPheAF" %in% names(Info) && !is.na(Info$GlobinPheAF)) {
          Field <- GlobinPheAF[GlobinPheAF$Option == Info$GlobinPheAF, "Option"]
          SubsetE <- SubsetE %>% filter(globin_phenotype == Field) %>% select(-phenotype)
        }
      }
      if (Field == "Relative allele frequency") {
        SubsetE <- SubsetE %>% select(-cause_name)
        if ("VariantC" %in% names(Info) && !is.na(Info$VariantC)) {
          vField <- VariantC[VariantC$Option == Info$VariantC, "Option"]
          if (vField == "Grouped variants by globin phenotype" &&
              "GlobinPheRAF" %in% names(Info) && !is.na(Info$GlobinPheRAF)) {
            Field <- GlobinPheRAF[GlobinPheRAF$Option == Info$GlobinPheRAF, "Option"]
            SubsetE <- SubsetE %>% filter(phenotype == Field) %>% select(-phenotype)
          }
          if (vField == "Individual variants" &&
              "IthaID" %in% names(Info) && !is.na(Info$IthaID)) {
            Field <- IthaID[IthaID$Option == Info$IthaID, "Option"]
            SubsetE <- SubsetE %>% filter(ithaID == Field) %>% select(-phenotype)
          }
        }
      }
      if ("HemoglobinopathyC" %in% names(Info) && !is.na(Info$HemoglobinopathyC)) {
        Field <- HemoglobinopathyC[HemoglobinopathyC$Option == Info$HemoglobinopathyC, "Option"]
        SubsetE <- SubsetE %>% filter(cause_name == Field) %>% select(-cause_name, -phenotype)
      }
      if ("HemoglobinopathyP" %in% names(Info) && !is.na(Info$HemoglobinopathyP)) {
        Field <- HemoglobinopathyP[HemoglobinopathyP$Option == Info$HemoglobinopathyP, "Option"]
        SubsetE <- SubsetE %>% filter(cause_name == Field) %>% select(-cause_name, -phenotype)
      }
    }
  }

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
      MetricN <- Metric %>% filter(Extra == mField) %>% pull(Option)
    }
    group_col <- switch(agg_level,
      "Country-level"  = "geo_admin0",
      "Province-level" = "geo_admin1",
      "District-level" = "geo_admin2"
    )
    Names <- colnames(SubsetE)
    SubsetE <- SubsetE %>%
      filter(!is.na(.data[[group_col]])) %>%
      group_by(.data[[group_col]]) %>%
      distinct(value, .keep_all = TRUE)

    if (is.null(mField) || length(mField) == 0 || is.na(mField)) {
      SubsetE <- SubsetE %>% mutate(Metric = value)
    } else if (mField == "Max") {
      SubsetE <- SubsetE %>% mutate(Metric = max(value, na.rm = TRUE))
    } else if (mField == "Min") {
      SubsetE <- SubsetE %>% mutate(Metric = min(value, na.rm = TRUE))
    } else if (mField == "Largest") {
      SubsetE <- SubsetE %>% mutate(Metric = if (all(is.na(sample_size))) NA_real_ else value[which.max(sample_size)])
    } else if (mField == "Latest") {
      SubsetE <- SubsetE %>% mutate(Metric = if (all(is.na(end_year))) NA_real_ else value[which.max(end_year)])
    } else if (mField == "Median") {
      SubsetE <- SubsetE %>% mutate(Metric = median(value, na.rm = TRUE))
    } else if (mField == "Mean") {
      SubsetE <- SubsetE %>% mutate(Metric = mean(value, na.rm = TRUE))
    } else if (mField == "Wmean") {
      SubsetE <- SubsetE %>%
        mutate(Metric = tryCatch({
          metric_data <- cur_data() %>%
            filter(!is.na(count)) %>%
            filter(!is.na(sample_size)) %>%
            filter(count != 0)

          if (nrow(metric_data) == 0) {
            NA_real_
          } else {
            Model <- rma(
              yi,
              vi,
              data = escalc(xi = count, ni = sample_size, data = metric_data, measure = "PFT", add = 0),
              method = "REML",
              level = 95
            )
            (sin(predict(Model)$pred / 2))^2 * 100
          }
        }, error = function(e) NA_real_))
    }

    SubsetE <- SubsetE %>%
      ungroup() %>%
      dplyr::select(all_of(Names), Metric) %>%
      filter(!is.na(Metric)) %>%
      mutate(Metric = round(Metric, 2))

    # --- Add geometries (cached spatial files) ---
    if (agg_level == "Country-level") {
      SubsetE <- SubsetE %>%
        left_join(adm0_sel %>% st_as_sf(), by = "geo_admin0") %>%
        left_join(adm1_sel %>% st_drop_geometry(), by = "geo_admin1") %>%
        left_join(adm2_sel %>% st_drop_geometry(), by = "geo_admin2")
    } else if (agg_level == "Province-level") {
      SubsetE <- SubsetE %>%
        left_join(adm1_sel %>% st_as_sf(), by = "geo_admin1") %>%
        left_join(adm0_sel %>% st_drop_geometry(), by = "geo_admin0") %>%
        left_join(adm2_sel %>% st_drop_geometry(), by = "geo_admin2")
    } else {
      SubsetE <- SubsetE %>%
        left_join(adm2_sel %>% st_as_sf(), by = "geo_admin2") %>%
        left_join(adm0_sel %>% st_drop_geometry(), by = "geo_admin0") %>%
        left_join(adm1_sel %>% st_drop_geometry(), by = "geo_admin1")
    }
    SubsetE <- SubsetE %>%
      dplyr::select(-geoboundary_key) %>%
      mutate(
        Region1 = ifelse(is.na(Region1), "Not applicable", Region1),
        Region2 = ifelse(is.na(Region2), "Not applicable", Region2)
      ) %>%
      distinct()
    SubsetG <- SubsetE %>%
      dplyr::select(Metric, Region, Region1, Region2, geom) %>%
      distinct()
  }

  list(SubsetE = SubsetE, SubsetG = SubsetG, MetricN = MetricN)
}

build_query_bundle_cached <- function(raw_qs) {
  cache_key <- normalize_query_string(raw_qs)

  if (exists(cache_key, envir = query_bundle_cache, inherits = FALSE)) {
    return(get(cache_key, envir = query_bundle_cache, inherits = FALSE))
  }

  bundle <- build_query_bundle(raw_qs)

  # Leaflet polygon layers require sf/spatial input. Ensure cached payload
  # preserves sf class even if upstream dplyr ops returned a tibble.
  if (!is.null(bundle$SubsetE) && !inherits(bundle$SubsetE, "sf") && "geom" %in% names(bundle$SubsetE)) {
    bundle$SubsetE <- st_as_sf(bundle$SubsetE)
  }
  if (!is.null(bundle$SubsetG) && !inherits(bundle$SubsetG, "sf") && "geom" %in% names(bundle$SubsetG)) {
    bundle$SubsetG <- st_as_sf(bundle$SubsetG)
  }

  assign(cache_key, bundle, envir = query_bundle_cache)
  bundle
}

# Generate shiny app
ui <- fluidPage(theme = bs_theme(version = 5, bootswatch = "litera"),
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
                                 
                                 .dataTables_wrapper .dataTables_paginate ul.pagination li.page-item .page-link {font-size: 0.7rem !important; padding: 0.05rem 0.3rem !important; min-width: 1.1rem !important; height: 1.2rem !important;}")),
                
                uiOutput("no_data_notification"),
                
                div(class = "container-fluid py-4 px-4",
                    ## MAP AND SIDEBAR (LEFT = POPUP, RIGHT = MAP)
                    div(class = "d-flex mb-4 shadow-sm rounded border",
                        div(style = "width: 30%; max-height: 500px; overflow-y: auto; padding: 10px; border-right: 1px solid #ccc; background-color: #f8f9fa;",
                            uiOutput("custom_popup")),
                        div(style = "flex-grow: 1;",
                            leafletOutput("map", height = "500px"))),
                    
                    ## EXPORT BUTTONS
                    div(class = "mb-4 d-flex flex-wrap gap-2 justify-content-center",
                        downloadButton("download_png", "Export as .png", icon = icon("file-image"), class = "btn btn-secondary btn-sm"),
                        downloadButton("download_csv", "Export as .csv", icon = icon("file-arrow-down"), class = "btn btn-secondary btn-sm"),
                        downloadButton("download_gpkg", "Export as .gpkg", icon = icon("file-arrow-down"), class = "btn btn-secondary btn-sm"),
                        downloadButton("download_geojson", "Export as .geojson", icon = icon("file-arrow-down"), class = "btn btn-secondary btn-sm")),
                    
                    ## DATA TABLE
                    div(class = "table-responsive shadow-sm rounded border",
                        style = "max-height: 500px; overflow-y: auto;",
                        DTOutput("data_table"))))


server <- function(input, output, session) {

  # Parse URL query string once per session
  query_bundle <- reactive({
    raw_qs <- isolate(session$clientData$url_search)
    build_query_bundle_cached(raw_qs)
  })

  SubsetE_r <- reactive({
    b <- query_bundle()
    if (!is.null(b$SubsetE) && !inherits(b$SubsetE, "sf") && "geom" %in% names(b$SubsetE)) {
      st_as_sf(b$SubsetE)
    } else {
      b$SubsetE
    }
  })
  SubsetG_r <- reactive({
    b <- query_bundle()
    if (!is.null(b$SubsetG) && !inherits(b$SubsetG, "sf") && "geom" %in% names(b$SubsetG)) {
      st_as_sf(b$SubsetG)
    } else {
      b$SubsetG
    }
  })
  MetricN_r <- reactive({ query_bundle()$MetricN })

  data_available <- reactive({
    se <- SubsetE_r()
    !is.null(se) && nrow(se) > 0
  })

  filtered_data <- reactive({
    req(data_available())
    SubsetE <- SubsetE_r()
    if (!is.null(input$data_table_rows_all)) SubsetE[input$data_table_rows_all, ] else SubsetE
  })

selected_row <- reactiveVal(NULL)

observeEvent(input$data_table_rows_selected, {selected_row(input$data_table_rows_selected)})

observe({req(selected_row())
  proxy <- leafletProxy("map", data = filtered_data())
  proxy %>% clearGroup("highlight")
  if (!is.null(selected_row())) {data <- filtered_data()[selected_row(), ]
  proxy %>%
    addCircleMarkers(data = data,
                     lat = ~as.numeric(latitude),
                     lng = ~as.numeric(longitude),
                     color = "#0000CC",
                     fillColor = "#0000CC",
                     weight = 2,
                     radius = 12,
                     fillOpacity = 1,
                     group = "highlight")}})

  popup_contentA_r <- reactive({
    req(data_available())
    SubsetE <- SubsetE_r(); SubsetG <- SubsetG_r()
    lapply(1:nrow(SubsetG), function(i) {
      fields <- c("Country", "Province", "District", "Value")
      values <- c(if (i <= nrow(SubsetE)) SubsetE$Region[i] else NA,
                  if (i <= nrow(SubsetE)) SubsetE$Region1[i] else NA,
                  if (i <= nrow(SubsetE)) SubsetE$Region2[i] else NA,
                  SubsetG$Metric[i])
      if (length(values) == 0) {df <- data.frame(Field = character(), Value = character())}
      else {df <- data.frame(Field = fields, Value = values, stringsAsFactors = FALSE)
            df <- df[df$Value != "" & !is.na(df$Value), , drop = FALSE]}
      if (nrow(df) > 0) {
        table_html <- paste0("<div style='font-family:sans-serif; font-size:0.75em; max-width:600px;'>",
          "<h4 style='margin-bottom:6px;'>Aggregated value details</h4>",
          "<table style='width:100%; border-collapse:collapse; border: 1px solid #ddd;'>",
          paste(apply(df, 1, function(row) {sprintf(
            "<tr><td style='padding:2px 4px; vertical-align:top; background:#f9f9f9; color:#333; font-weight:600; width:35%%; white-space:nowrap; border: 1px solid #ddd;'>%s</td><td style='padding:2px 4px; vertical-align:top; background:#ffffff; color:#000; border: 1px solid #ddd;'>%s</td></tr>",
            row[1], row[2])}), collapse = ""),
          "</table></div>")
        HTML(table_html)
      } else {HTML("<div>No data available</div>")}
    })
  })

  popup_content_r <- reactive({
    req(data_available())
    SubsetE <- SubsetE_r()
    lapply(1:nrow(SubsetE), function(i) {
      fields <- c("Country", "Province", "District", "Value", "Study period", "Risk of bias", "Globin phenotype", "IthaID",
                  "Sample size", "Population tested positive", "Cohort", "Nationality", "Ethnicity", "Race", "Religion",
                  "Sex", "Age", "Consanguinity", "Diagnostic method", "Recruitment site", "Coordinates", "Notes", "Source")
      values <- c(SubsetE$Region[i], SubsetE$Region1[i], SubsetE$Region2[i], SubsetE$value[i], SubsetE$timeframe[i],
                  SubsetE$bias_flag[i], SubsetE$globin_phenotype[i], SubsetE$ithaID[i], SubsetE$sample_size[i],
                  SubsetE$count[i], SubsetE$status_group[i], SubsetE$nationality[i], SubsetE$ethnicity_name[i],
                  SubsetE$race[i], SubsetE$religion_name[i], SubsetE$sex[i], SubsetE$age[i], SubsetE$consaguinity[i],
                  SubsetE$diagnostic_method[i], SubsetE$recruitment_site[i],
                  paste0("(", round(as.numeric(SubsetE$latitude[i]), 4), ", ", round(as.numeric(SubsetE$longitude[i]), 4), ")"),
                  SubsetE$note[i], SubsetE$citation_str[i])
      df <- data.frame(Field = fields, Value = values, stringsAsFactors = FALSE)
      df <- df[df$Value != "" & !is.na(df$Value), ]
      table_html <- paste0("<div style='font-family:sans-serif; font-size:0.75em; max-width:600px;'>",
        "<h4 style='margin-bottom:6px;'>Study details</h4>",
        "<table style='width:100%; border-collapse:collapse; border: 1px solid #ddd;'>",
        paste(apply(df, 1, function(row) {sprintf(
          "<tr><td style='padding:2px 4px; background:#f9f9f9; color:#333; font-weight:600; width:35%%; white-space:nowrap; border: 1px solid #ddd;'>%s</td><td style='padding:2px 4px; background:#fff; color:#000; border: 1px solid #ddd;'>%s</td></tr>",
          row[1], row[2])}), collapse = ""),
        "</table></div>")
      HTML(table_html)
    })
  })

  pal_metric_r <- reactive({
    req(data_available())
    SubsetG <- SubsetG_r()
    viridis_palette <- viridis::viridis(81, option = "F", begin = 0, end = 0.7, direction = -1)
    metric_values <- SubsetG$Metric
    unique_vals <- unique(metric_values)
    if (length(unique_vals) == 1) colorNumeric(palette = viridis_palette, domain = unique_vals)
    else colorNumeric(palette = viridis_palette, domain = metric_values)
  })

output$map <- renderLeaflet({req(data_available())
  SubsetG   <- SubsetG_r()
  MetricN   <- MetricN_r()
  pal_metric <- pal_metric_r()
  metric_values <- SubsetG$Metric
  data <- filtered_data()
  
  leaflet(data) %>%
    addProviderTiles("CartoDB.Positron") %>%
    addScaleBar(position = "bottomleft") %>%
    addCircleMarkers(lat = ~as.numeric(latitude),
                     lng = ~as.numeric(longitude),
                     stroke = TRUE,
                     color = "white",
                     weight = 1,
                     fillColor = "black",
                     fillOpacity = 1,
                     radius = 7,
                     clusterOptions = markerClusterOptions(spiderfyDistanceMultiplier = 1,
                                                           animate = TRUE,
                                                           animateAddingMarkers = TRUE,
                                                           spiderfyOnMaxZoom = TRUE,
                                                           zoomToBoundsOnClick = TRUE,
                                                           showCoverageOnHover = TRUE,
                                                           maxClusterRadius = 4)) %>%
    addPolygons(data = SubsetG,
                weight = 0.3,
                opacity = 1,
                color = "black",
                fillOpacity = 0.8,
                smoothFactor = 0.5,
                highlightOptions = highlightOptions(weight = 1.4,
                                                    color = "#0000CC",
                                                    fillOpacity = 0.5,
                                                    fillColor = "#0000CC",
                                                    bringToFront = FALSE),
                fillColor = ~pal_metric(Metric)) %>%
    addLegend(pal = pal_metric,
              values = metric_values,
              title = MetricN,
              opacity = 1,
              position = "bottomright") %>%
    htmlwidgets::onRender("function(el, x) {var map = this;
                                                                                     
                                                                                     // Style clusters
                                                                                     map.on('layeradd', function(e) {var layer = e.layer; if (layer.getChildCount && layer._icon) {var count = layer.getChildCount(); var color = 'black'; var icon = L.divIcon({html: '<div style=\"background-color:' + color + '; color:white; border-radius:50%; width:20px; height:20px; display:flex; align-items:center; justify-content:center; font-weight:bold; font-size:12px;\">' + count + '</div>', className: '', iconSize: new L.Point(20, 20)}); layer.setIcon(icon);}});
                                                                                     
                                                                                     // Highlight on hover
                                                                                     map.on('layeradd', function(e) {var layer = e.layer; if (layer instanceof L.CircleMarker && !layer.getChildCount) {layer.on('mouseover', function() {this.setStyle({radius: 10, weight: 2, color: '#0000CC', fillColor: '#0000CC'}); this.bringToFront();}); layer.on('mouseout', function() {this.setStyle({radius: 7, weight: 1, color: 'white', fillColor: 'black'});});}});}")})

output$data_table <- renderDT({req(data_available())
  SubsetE <- SubsetE_r()
  df <- st_drop_geometry(SubsetE) %>%
    rename("Country" = Region,
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
           "Source" =citation_str) %>%
    dplyr::select("Country", "Province", "District", "Recruitment site", "Latitude", "Longitude", 
                  "Study period", "Risk of bias", "Globin phenotype", "IthaID", "Sample size", 
                  "Population tested positive", "Value", "Cohort", "Nationality", "Ethnicity", 
                  "Race", "Religion", "Sex", "Age", "Consanguinity", "Diagnostic method", "Notes", "Source")
  
  df2 <- SubsetE %>%
    st_as_sf() %>%
    rename("Country" = Region,
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
           "Source" =citation_str) %>%
    dplyr::select("Country", "Province", "District", "Recruitment site", "Latitude", "Longitude", 
                  "Study period", "Risk of bias", "Globin phenotype", "IthaID", "Sample size", 
                  "Population tested positive", "Value", "Cohort", "Nationality", "Ethnicity", 
                  "Race", "Religion", "Sex", "Age", "Consanguinity", "Diagnostic method", "Notes", "Source")
  
  datatable(df,
            selection = "single",
            filter = 'top',
            options = list(pageLength = 25,
                           scrollX = TRUE,
                           rowCallback = JS("function(row, data) {", "$(row).css('min-height', '30px');", "}"),
                           columnDefs = list(list(visible = FALSE, targets = which(names(df) %in% c("Notes", "Source"))))),
            class = 'stripe hover cell-border')})

output$download_png <- downloadHandler(filename = function() {paste0("IthaMaps_", Sys.Date(), ".png")},
                                       content = function(file) {
                                         Notification <- showNotification("Export as .png in progress... Please wait until export completes before adjusting filter options.",
                                                                          type = "message", duration = NULL)
                                         SubsetG    <- SubsetG_r()
                                         MetricN    <- MetricN_r()
                                         pal_metric <- pal_metric_r()
                                         map <- leaflet(filtered_data()) %>%
                                           addProviderTiles("CartoDB.Positron") %>%
                                           addPolygons(data = SubsetG,
                                                       weight = 0.3, opacity = 1, color = "black",
                                                       fillOpacity = 0.8, smoothFactor = 0.5,
                                                       fillColor = ~pal_metric(Metric)) %>%
                                           addCircleMarkers(lat = ~as.numeric(latitude), lng = ~as.numeric(longitude),
                                                            stroke = TRUE, color = "white", weight = 1,
                                                            fillColor = "black", fillOpacity = 1, radius = 7) %>%
                                           addLegend(pal = pal_metric, values = SubsetG$Metric, title = MetricN,
                                                     position = "bottomright", opacity = 1)
                                         mapview::mapshot(map, file = file)
                                         removeNotification(Notification)})

output$download_csv <- downloadHandler(filename = function() {paste0("IthaMaps_", Sys.Date(), ".csv")},
                                       content = function(file) {
                                         Notification <- showNotification("Export as .csv in progress... Please wait until export completes before adjusting filter options.",
                                                                          type = "message", duration = NULL)
                                         SubsetE <- SubsetE_r()
                                         df <- st_drop_geometry(SubsetE) %>%
                                           rename("Country" = Region, "Province" = Region1, "District" = Region2,
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
                                                  "Notes" = note, "Source" = citation_str) %>%
                                           dplyr::select("Country", "Province", "District", "Recruitment site",
                                                         "Latitude", "Longitude", "Study period", "Risk of bias",
                                                         "Globin phenotype", "IthaID", "Sample size",
                                                         "Population tested positive", "Value", "Cohort",
                                                         "Nationality", "Ethnicity", "Race", "Religion",
                                                         "Sex", "Age", "Consanguinity", "Diagnostic method",
                                                         "Notes", "Source")
                                         write.csv(df, file, row.names = FALSE)
                                         removeNotification(Notification)})

output$download_geojson <- downloadHandler(filename = function() {paste0("IthaMaps_", Sys.Date(), ".geojson")},
                                           content = function(file) {
                                             Notification <- showNotification("Export as .geojson in progress... Please wait until export completes before adjusting filter options.",
                                                                              type = "message", duration = NULL)
                                             SubsetE <- SubsetE_r()
                                             df <- st_drop_geometry(SubsetE) %>%
                                               rename("Country" = Region, "Province" = Region1, "District" = Region2,
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
                                                      "Notes" = note, "Source" = citation_str) %>%
                                               dplyr::select("Country", "Province", "District", "Recruitment site",
                                                             "Latitude", "Longitude", "Study period", "Risk of bias",
                                                             "Globin phenotype", "IthaID", "Sample size",
                                                             "Population tested positive", "Value", "Cohort",
                                                             "Nationality", "Ethnicity", "Race", "Religion",
                                                             "Sex", "Age", "Consanguinity", "Diagnostic method",
                                                             "Notes", "Source")
                                             st_write(df, file, driver = "GeoJSON", delete_dsn = TRUE)
                                             removeNotification(Notification)})

output$download_gpkg <- downloadHandler(filename = function() {paste0("IthaMaps_", Sys.Date(), ".gpkg")},
                                        content = function(file) {
                                          Notification <- showNotification("Export as .gpkg in progress... Please wait until export completes before adjusting filter options.",
                                                                           type = "message", duration = NULL)
                                          SubsetE <- SubsetE_r()
                                          df <- SubsetE %>%
                                            st_as_sf() %>%
                                            rename("Country" = Region, "Province" = Region1, "District" = Region2,
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
                                                   "Notes" = note, "Source" = citation_str) %>%
                                            dplyr::select("Country", "Province", "District", "Recruitment site",
                                                          "Latitude", "Longitude", "Study period", "Risk of bias",
                                                          "Globin phenotype", "IthaID", "Sample size",
                                                          "Population tested positive", "Value", "Cohort",
                                                          "Nationality", "Ethnicity", "Race", "Religion",
                                                          "Sex", "Age", "Consanguinity", "Diagnostic method",
                                                          "Notes", "Source")
                                          st_write(df, dsn = file, driver = "GPKG", delete_dsn = TRUE)
                                          removeNotification(Notification)})

observeEvent(input$map_marker_click, {req(data_available())
  click <- input$map_marker_click
  SubsetE <- SubsetE_r()
  if (!is.null(click)) {
    clicked_point <- st_sfc(st_point(c(click$lng, click$lat)), crs = st_crs(SubsetE))
    dists <- st_distance(clicked_point, SubsetE)
    nearest_idx <- which.min(dists)
    popup_content <- popup_content_r()
    output$custom_popup <- renderUI({popup_content[[nearest_idx]]})}})

observeEvent(input$map_shape_click, {req(data_available())
  click <- input$map_shape_click
  SubsetG <- SubsetG_r()
  if (!is.null(click)) {
    clicked_shape <- st_sfc(st_point(c(click$lng, click$lat)), crs = st_crs(SubsetG))
    dists <- st_distance(clicked_shape, st_centroid(SubsetG))
    nearest_idx <- which.min(dists)
    popup_contentA <- popup_contentA_r()
    output$custom_popup <- renderUI({popup_contentA[[nearest_idx]]})}})

output$no_data_notification <- renderUI({if (!data_available()) {div(class = "alert alert-warning", "No data is available for the selected parameter combination.")}})}

shinyApp(ui = ui, server = server)
