# IthaMaps Shiny App

## Purpose

`app.R` is a Shiny application that renders an interactive Leaflet choropleth map and data table of haemoglobinopathy epidemiology data from the ITHANET database. It is designed to be embedded as an `<iframe>` in the IthaMaps Joomla module (`mod_ithamaps2`).

The app receives all filtering parameters via the URL query string. The Joomla module builds this URL from the user's dropdown selections, the current country context, and a set of hard-wired defaults.

---

## Configuration

Connection settings are resolved in this order (first non-empty value wins):

1. Secret files: `DB_USER_FILE` / `DB_PASSWORD_FILE`, defaulting to `secrets/db_user` / `secrets/db_password` (first line of each file).
2. Environment variables (table below).
3. Built-in defaults: host `localhost`, port `3306`.

### Connection & identity

| Environment variable | Default | Purpose |
|---|---|---|
| `DB_USER_FILE` / `DB_USER` | `secrets/db_user` | Database username (secret file path / plain value) |
| `DB_PASSWORD_FILE` / `DB_PASSWORD` | `secrets/db_password` | Database password (secret file path / plain value) |
| `ITHAMAPS_DB_HOST` | `localhost` | Database host |
| `ITHAMAPS_DB_PORT` | `3306` | Database port |
| `ITHAMAPS_ITHA_ROOT` | *(from `secrets/db_prefix`)* | Overrides the ITHANET site base URL (used for back-links) |

### `secrets/db_prefix`

Optional `key = value` file (quotes and `#` comments allowed):

```ini
ithabase_prefix = "_dev"                          # DB ithabase_dev   (default _live)
joomla_prefix = "_dev"                            # DB joomla_dev     (default _live)
ithanet_site_root = "https://dev.ithanet.eu"      # default http://localhost/live-ithanet-j4
```

`secrets/` is gitignored; create it on each machine.

### Parallelism

| Environment variable | Default | Purpose |
|---|---|---|
| `ITHAMAPS_PARALLEL` | `false` | Enable parallel workers for SF geometry reads (`true`/`1`/`yes`/`on`) |
| `ITHAMAPS_PARALLEL_CORES` | *(auto)* | Number of worker cores for SF reads |
| `ITHAMAPS_PARALLEL_MEM_RESERVE_MB` | `4096` | MB to leave free when calculating max workers |
| `ITHAMAPS_SF_READ_WORKER_MB` | `2200` | Estimated MB consumed per SF-read worker |
| `ITHAMAPS_SF_READ_MAX_WORKERS` | `2` | Hard cap on SF-read workers regardless of available RAM |
| `ITHAMAPS_PARALLEL_CACHE_BUILD` | `false` | Enable parallel workers when pre-building the geometry cache |
| `ITHAMAPS_CACHE_BUILD_CORES` | *(auto)* | Number of cores for cache-build workers |

### Logging

| Environment variable | Default | Purpose |
|---|---|---|
| `ITHAMAPS_JOIN_LOG_DIR` | `logs` | Directory where join-diagnostic CSVs are written |

### Debugging

| Environment variable | Default | Purpose |
|---|---|---|
| `ITHAMAPS_DEBUG_MODE` | `false` | Enable debug mode (`true`/`1`/`yes`/`on`). Shows the **Performance timings** panel in the UI and activates internal bundle-step capture (see [Debugging](#debugging) below) |

---

## Data Sources

### ITHANET database (`ithabase_mk`)

All tables below are loaded into in-memory tibbles at startup and kept for the lifetime of the R process:

| In-memory object | Source table |
|---|---|
| `db_country` | `country` |
| `db_locus` | `locus` |
| `db_globin_phenotypes` | `globin_phenotypes` |
| `db_ithamaps_accumulated_sources` | `ithamaps_accumulated_sources` |
| `db_regions` | `regions` |
| `db_measure` | `measure` |
| `db_metric` | `metric` |
| `db_sex_groups` | `sex_groups` |
| `db_age_groups` | `age_groups` |
| `db_ethnicities` | `ethnicities` |
| `db_religions` | `religions` |
| `db_ithamaps_cohort` | `ithamaps_cohort` |
| `db_cause` | `cause` |
| `db_hc_policies` | `hc_policies` |
| `db_hcp_per_region` | `hcp_per_region` |
| `db_ithamaps_entries` | `ithamaps_entries` |
| `db_ithamaps_log` | `ithamaps_log` |
| `db_criteria` | `criteria` |
| `db_criteria_responses` | `criteria_responses` |
| `db_j_criteria` | `j_criteria` |
| `db_ithagenes_common` | `ithagenes_common` |
| `db_ithagenes_globin_phen` | `ithagenes_globin_phen` |

After all joins and enrichment steps are applied, only `db_ithamaps_entries` and `db_hcp_per_region` are retained for query-time filtering. All other `db_*` objects are removed from memory.

### Joomla database (`joomla_live`)

| In-memory object | Source table |
|---|---|
| `db_itha_experts` | `itha_experts` |

Used to resolve expert names during the `db_ithamaps_entries` enrichment join.

### Spatial files

Loaded once at startup and cached globally for reuse across all sessions:

| File | Content |
|---|---|
| `ADM0.gpkg` | Country-level polygons |
| `ADM1.gpkg` | Province / state-level polygons |
| `ADM2.gpkg` | District-level polygons |

---

## Shiny UI

The app uses a single-page `fluidPage` layout (Bootstrap 5, "litera" theme) with four main sections:

1. **Leaflet map** — choropleth map coloured by the selected metric value per region. Clicking a coloured polygon populates the popup sidebar on the left.
2. **Popup sidebar** — shows the country, province, district, and aggregated metric value for the selected polygon.
3. **Export buttons** — download the current filtered and visible data as `.png`, `.csv`, `.gpkg`, or `.geojson`.
4. **Data table** — scrollable DT table of all individual records matching the query. Selecting a table row highlights the corresponding point on the Leaflet map.

When no valid query is supplied, or no data match the filters, a notification is shown and the map and table remain empty.

---

## URL Query Parameters

All parameters are integer IDs passed in the query string. They are resolved against option lookup tables built from the database at startup.

### Full parameter reference

| Parameter | Required? | Active when | Options |
|---|---|---|---|
| `Resolution` | Yes (for any output) | Always | 1 = Global-level, 2 = Continent-level, 3 = Country-level |
| `Continent` | Only when `Resolution=2` | Continent-level | IDs from `country.continentName` (1–7) |
| `Country` | Only when `Resolution=3` | Country-level | IDs from `country.idCountry` |
| `Measure` | Yes (for meaningful output) | Always | IDs from `measure.measure_id`; additionally 21 = Healthcare availability |
| `Cause` | Optional | Healthcare, carrier, prevalence, and incidence paths only | Allowed `cause` IDs depend on `Measure`: healthcare uses the broad disease set, carrier measures use the carrier subset, prevalence/incidence paths use the phenotype subset |
| `Healthcare` | Optional | `Measure=21` | Top-level `hc_policies` rows (where `ancestor0` IS NULL) |
| `HealthcareS1`..`HealthcareS13` | Optional | `Measure=21` and `Healthcare` set | Child `hc_policies` rows where `ancestor0` equals the parent ID (1–13) |
| `GlobinPheAF` | Optional | `Measure` = Allele frequency | Subset of globin phenotype IDs: α0, α+, α-thalassaemia modifier, non-deletional α+ |
| `VariantC` | Optional | `Measure` = Relative allele frequency | 1 = Individual variants, 2 = Grouped variants by globin phenotype |
| `GlobinPheRAF` | Optional | `VariantC=2` | Globin phenotype IDs: α0, α+, β0, β+, δ0, δ+, Other (ID 38) |
| `IthaID` | Optional | `VariantC=1` | IDs from `ithagenes_common.ithaID` |
| `Metric` | Optional | All non-healthcare paths | 1 = Weighted mean, 2 = Mean, 3 = Median, 4 = Highest value, 5 = Lowest value, 6 = Most recent value, 7 = Value from largest surveyed population |
| `Aggregation` | Optional | All non-healthcare paths | 1 = Country-level, 2 = Province-level, 3 = District-level |

When `Metric` is omitted the raw `value` column is used as-is (no aggregation).  
When `Aggregation` is omitted, country-level grouping is applied by default.

---

## Query Processing Flow

Each Shiny session processes the URL query string independently via `build_query_bundle()`:

1. **Parse** (`Parse()`) — splits the query string into key/value pairs; canonicalises key names (case-insensitive, e.g. `resolution` → `Resolution`).
2. **Extract** (`Extract()`) — casts each value to integer; handles the backward-compatible `HealthcareDetail` alias (maps to the appropriate `HealthcareS<n>` key).
3. **Search** — resolves each integer ID to an option label using the pre-built lookup data frames (`Resolution`, `Country`, `Measure`, `Cause`, etc.).
4. **Resolution filter** — creates base subsets `SubsetE` (epidemiology entries from `db_ithamaps_entries`) and `SubsetHCP` (healthcare policy entries from `db_hcp_per_region`), scoped to the selected geographic level (global / continent / country).
5. **Measure filter** — routes to one of two branches:
        - *Healthcare branch* (`Measure = Healthcare availability`): `SubsetE` is cleared; `SubsetHCP` is filtered by `Cause`, parent policy (`Healthcare`), and sub-policy (`HealthcareS*`). The Leaflet map and table are rendered from `SubsetHCP`.
        - *Non-healthcare branch*: `SubsetHCP` is cleared; `SubsetE` is filtered by measure name, and optionally by `Cause` when the selected measure allows it, plus any relevant globin phenotype or variant filter (`GlobinPheAF`, `GlobinPheRAF`, `IthaID`).
6. **Metric & aggregation** — groups `SubsetE` by the chosen admin level (`geo_admin0` / `geo_admin1` / `geo_admin2`) and computes the requested summary statistic. Weighted mean uses the `metafor` random-effects meta-analysis model (`rma` with `PFT` transformation).
7. **Geometry join** — attaches polygon geometries from the cached `.gpkg` files to produce `SubsetG`, the sf object used for the choropleth.
8. **Render** — Leaflet renders the choropleth from `SubsetG`; DT renders the full record table from `SubsetE` (or `SubsetHCP` in healthcare mode).

Results are cached per unique query string in a process-level environment (`query_bundle_cache`). Subsequent sessions with an identical query string skip steps 1–7 entirely.

---

## Joomla Module Integration

The Joomla module (`mod_ithamaps2`) embeds the Shiny app as an `<iframe>`. It:

1. Reads the current country from the Joomla URL (`?country=<code>`).
2. Reads any explicit Shiny parameters already present in the URL (e.g. `Resolution`, `Measure`, `Metric`).
3. Merges those with a set of defaults (see below), giving URL parameters priority over defaults.
4. Builds the final `<iframe src>` URL and renders the panel when `shiny_enabled` is set in the module configuration.

### Joomla module defaults (`$shinyDefaults`)

When no explicit value is present in the URL the following defaults are applied before building the iframe URL:

| Parameter | Default ID | Meaning |
|---|---|---|
| `Resolution` | 3 | Country-level |
| `Measure` | 17 | Measure ID 17 from the `measure` table (verify against the live DB) |
| `GlobinPheAF` | 1 | α0 (first alpha-thalassaemia globin phenotype) |
| `Metric` | 5 | Lowest value |
| `Aggregation` | 1 | Country-level aggregation |

If a country is present in the Joomla URL context but `Country` is not yet set as a Shiny parameter, it is automatically injected (together with `Resolution=3`).

### Module configuration parameters (Joomla back-end)

| Parameter | Default | Purpose |
|---|---|---|
| `shiny_enabled` | 0 (off) | Enables the Shiny iframe panel |
| `shiny_base_url` | *(empty)* | Base URL of the Shiny server |
| `shiny_height` | 900 | iframe height in pixels |
| `shiny_lazy_load` | 1 (on) | Uses `loading="lazy"` on the iframe |

---

## `HealthcareS1`..`HealthcareS13` Explained

The 13 `HealthcareS*` selectors each hold the child rows of one top-level healthcare policy category:

- `HealthcareS<n>` = all rows from `hc_policies` where `ancestor0 = n`.

In the query the relevant `HealthcareS<n>` key is chosen based on the selected `Healthcare` parent ID. Only one `HealthcareS*` key is active at a time. Inside the app the selected option filters `SubsetHCP` on `hcp_name`.

The Joomla module also exposes a convenience `HealthcareDetail` parameter that accepts a child policy ID directly. It is automatically resolved to the correct parent `Healthcare` ID and `HealthcareS<n>` key before the query is forwarded to the Shiny app.

---

## Cause Selector

The public query contract now uses a single `Cause` key. The allowed IDs depend on the selected `Measure`:

| Measure path | Allowed causes |
|---|---|
| Healthcare availability | Thalassaemia, Hemoglobinopathy, Sickle Cell Disease |
| Carrier prevalence paths | Beta/Alpha Thalassaemia, SCD, Hemoglobin E/C Disease, Delta Thalassaemia, SCD-SS |
| Prevalence / incidence paths | All of the carrier subset plus SCD-SC/SE, Sickle Beta Thalassaemia, compound heterozygous diseases, Thalassaemia Intermedia/Major, Hemoglobin H Disease, Hydrops Fetalis, Hemoglobin Barts |
| Allele frequency / relative allele frequency | `Cause` is not allowed |

Both the Joomla module and the Shiny app validate these measure/cause combinations. Legacy URLs using `Parameter` or `HemoglobinopathyH` / `HemoglobinopathyC` / `HemoglobinopathyP` are still accepted as backward-compatible aliases and are normalized internally to `Measure` and `Cause`.
- Factor shared transformations into reusable functions.

3. Precompute grouped summaries by aggregation level.
- `group_by + mutate + rma` runs repeatedly in branch-specific blocks.
- Precompute or memoize by key combinations when possible.

4. Optimize weighted mean calculation.
- `metafor::rma` inside grouped mutate is expensive.
- Consider fallback/shortcut when sample sizes are sparse or too small.

5. Limit columns earlier.
- Trim to required columns before heavy grouping and joins.

### Code quality and maintainability

1. Replace deeply nested if-blocks with composable filter functions.
- Especially in healthcare subcategory filtering (`HealthcareS1..S13`).

2. Use a parameter schema table.
- Define accepted params and mapping datasets declaratively instead of long switch/if chains.

3. Add diagnostics logging.
- Log resolved query, branch path, and resulting row counts.

4. Add explicit empty-state handling before `st_as_sf(SubsetE)`.
- Guard against missing objects when query paths do not produce `SubsetE`.

5. Separate data prep from server rendering.
- Move data-prep logic into dedicated functions with clear inputs/outputs.

## Notes for Current Joomla Integration

Given current behavior, if Joomla passes only `country`, this app (as currently written) will not automatically use it because the active query source is hardcoded.

For this run, no app refactor was performed by request; this README only documents current behavior and improvement opportunities.

# ⚠️ REALLY IMPORTANT: port 3838 must be open on the institutional firewall

> [!IMPORTANT]
> **If the app shows nothing (blank iframe, `http://<server>:3838` does not load) while the container is running and the logs are clean, check the network firewall first.**
> The institute's IT blocks port **3838** by default. On the dev server this was the cause of the "nothing loads" problem, even though the app itself was running fine.
>
> - Quick test: open `http://<server_ip>:3838` from a machine *inside* the network, or run `curl -sI http://127.0.0.1:3838/` on the server. If it works there but not from outside, the port is blocked upstream, not by our setup.
> - Fix: ask IT to open TCP **3838** for the server, **or** serve the app only through the Apache reverse proxy (`https://<server>/shiny/`, see [In config](#in-config)), which needs only port 443.
> - Note: the app speaks plain HTTP on 3838, so `https://<server>:3838` never works, and an `http://` iframe inside the HTTPS Joomla page is blocked by browsers (mixed content).

# Docker commands

```bash
# docker build -t ithamaps-shiny:v1.0 -t ithamaps-shiny:latest . && docker image prune -f

#docker build -t ithamaps-shiny .
docker build -t ithamaps-shiny:v2 -t ithamaps-shiny:latest . && docker image prune -f


#docker run -d --name ithamaps-shiny -p 3838:3838   --add-host=host.docker.internal:host-gateway   -e ITHAMAPS_DB_HOST=host.docker.internal   -e ITHAMAPS_DB_PORT=3306   -e DB_USER_FILE=/run/secrets/db_user   -e DB_PASSWORD_FILE=/run/secrets/db_password   -v "$PWD/secrets/db_user:/run/secrets/db_user:ro"   -v "$PWD/secrets/db_password:/run/secrets/db_password:ro"   ithamaps-shiny
docker compose up -d

# save the image locally so that it can be shared to the server without  having to rebuilt it there. this will only work if db names and passwords are the same between local and server so ignore. Instead just copy the app.R and rebuild
docker save -o ithamaps-shiny.tar ithamaps-shiny:latest
gzip ithamaps-shiny.tar
scp ithamaps-shiny.tar.gz demo:~
ssh demo
# on the server
docker load < ithamaps-shiny.tar.gz
```

# Database access from the container

The Shiny container connects to MySQL/MariaDB running on the **host**. Three things have to line up:

| What | Value in our setup | Where it is set |
|---|---|---|
| Address the app dials | `host.docker.internal` → `docker0` bridge IP, normally **172.17.0.1** | `docker-compose.yml` (`extra_hosts: host.docker.internal:host-gateway`, `ITHAMAPS_DB_HOST=host.docker.internal`) |
| Addresses MySQL listens on | `127.0.0.1` **and** `172.17.0.1` | `bind-address` in `/etc/mysql/mysql.conf.d/mysqld.cnf` |
| Source IP MySQL sees | the container's own IP on the compose network, e.g. `172.18.0.2` | matched by the MySQL user host `'172.%.%.%'` |

> **Two different bridge IPs, don't mix them up.**
> `host-gateway` always resolves to the default `docker0` bridge (**172.17.0.1**), *not* to the gateway of the network Compose creates for this project (usually 172.18.0.1, which is what `docker network inspect` / `docker inspect` show).
> Bind MySQL to the `docker0` IP. `docker0` exists as soon as the Docker daemon runs and persists across `docker compose down/up`; the compose network's gateway disappears when the stack goes down, and MySQL then silently stops listening on it.

## 1. Dedicated MySQL user

MySQL sees the connection coming from the container's IP (e.g. `172.18.0.2`), not from the gateway, so the user is created for the whole `172.%.%.%` range:

```sql
CREATE USER IF NOT EXISTS 'ithamaps_shiny'@'172.%.%.%' IDENTIFIED BY '<password from secrets/db_password>';
GRANT ALL PRIVILEGES ON ithabase_dev.* TO 'ithamaps_shiny'@'172.%.%.%';
GRANT ALL PRIVILEGES ON joomla_dev.itha_experts TO 'ithamaps_shiny'@'172.%.%.%';
FLUSH PRIVILEGES;
```

Use `_live` instead of `_dev` on the live server, matching `secrets/db_prefix`. The app only reads, so `SELECT` is enough if you want to restrict it.

## 2. Find the IP the container dials

```bash
docker exec ithamaps-shiny getent hosts host.docker.internal   # e.g. 172.17.0.1  host.docker.internal
ip -4 addr show docker0 | grep inet                            # should be the same IP
```

## 3. Bind MySQL to that IP

In `/etc/mysql/mysql.conf.d/mysqld.cnf`, under `[mysqld]`:

```ini
bind-address = 127.0.0.1,172.17.0.1
```

A comma-separated list needs MySQL ≥ 8.0.13 (MariaDB ≥ 10.11); check with `mysql --version`. Make sure no later file overrides it:

```bash
sudo grep -rn "bind-address" /etc/mysql/        # the last one read wins
sudo systemctl restart mysql
```

Make MySQL start after Docker so `172.17.0.1` exists at boot (otherwise MySQL skips it and the app gets error 111 after a reboot):

```bash
sudo systemctl edit mysql
# add:
# [Unit]
# After=docker.service
# Wants=docker.service
```

## 4. Verify

```bash
# What MySQL is configured with
sudo my_print_defaults mysqld | grep bind
mysql -u root -p -e "SHOW VARIABLES LIKE 'bind_address';"

# What MySQL is actually listening on: must list BOTH 127.0.0.1:3306 and 172.17.0.1:3306
sudo ss -ltnp | grep ':3306'
# (127.0.0.1:33060 is the MySQL X Protocol port, controlled by mysqlx-bind-address; the app does not use it)

# End-to-end from inside the container
docker exec ithamaps-shiny Rscript -e 'con = DBI::dbConnect(RMariaDB::MariaDB(), host = "host.docker.internal", port = 3306, user = readLines("/run/secrets/db_user")[1], password = readLines("/run/secrets/db_password")[1]); print(DBI::dbGetQuery(con, "SELECT CURRENT_USER()"))'

# Then restart the app and watch the log
docker compose restart ithamaps-shiny
docker logs -f ithamaps-shiny
```

Troubleshooting from the connection error:

| Error | Meaning |
|---|---|
| `Can't connect ... (111)` | Connection refused: MySQL is not listening on the IP from step 2. Compare `getent` and `ss` output. |
| `Can't connect ... (110)` / timeout | A firewall is dropping the packets (check `ufw status`). |
| `Access denied for user 'ithamaps_shiny'@'172.18.0.x'` | Networking works; the user, password or grants are wrong (or the DB name suffix in `secrets/db_prefix` doesn't match the grants). |

## Alternative (not our current setup): host networking

Not how we run it now, recorded here as an option. With host networking the container shares the host's network stack, so no bridge IPs need managing and MySQL can stay on `bind-address = 127.0.0.1`:

```yaml
  ithamaps-shiny:
    network_mode: host          # remove the ports: and extra_hosts: entries
    environment:
      - ITHAMAPS_DB_HOST=127.0.0.1
```

The MySQL user must then be created for `'ithamaps_shiny'@'127.0.0.1'`, and the host firewall must block port 3838 from outside, since the app listens on all interfaces. Apache's proxy to `127.0.0.1:3838` is unchanged.

# In Joomla

Ont he itamaps module set `https://demo.ithanet.eu/shiny/` as the shiny base URL

# In config

Set the following in the `/etc/apache2/sites-available/ithanet_demo-ssl.conf` and make sure these modules are enabled:

 - proxy_module (shared)
 - proxy_http_module (shared)
 - proxy_wstunnel_module (shared)

with 

```bash
# Enable the primary proxy modules
sudo a2enmod proxy
sudo a2enmod proxy_http
sudo a2enmod proxy_wstunnel  # (Optional but recommended for Shiny websockets)

# Restart Apache to apply the module activation
sudo systemctl restart apache2
```
Ask Petros to add the following to the virtual host configuration.
```bash
        # --- SHINY APP REVERSE PROXY SETUP ---
        ProxyPreserveHost On

        # Enable the Rewrite Engine
        RewriteEngine On

        # 1. Capture WebSocket upgrade headers and map them to ws://
        RewriteCond %{HTTP:Upgrade} =websocket [NC]
        RewriteCond %{HTTP:Connection} upgrade [NC]
        RewriteRule ^/shiny/(.*) ws://127.0.0.1:3838/$1 [P,L]

        # 2. Handle standard HTTP proxy path for normal web requests
        ProxyPass /shiny/ http://127.0.0.1:3838/
        ProxyPassReverse /shiny/ http://127.0.0.1:3838/

        # 3. Handle unslashed redirect
        RedirectMatch ^/shiny$ /shiny/
        # --------------------------------------

```

# Rstudio server shipped with ithamaps shiny compose

`docker compose up -d ithamaps-rstudio && sleep 3 && curl -s -o /dev/null -w "HTTP %{http_code}\n" http://localhost:8787`

## How to use it

Open `http://localhost:8787` in your browser.
Log in — username `rstudio`, password `ithamaps` (set in docker-compose.yml; change it there if you want a different one, then `docker compose up -d --force-recreate ithamaps-rstudio`).
Your project is mounted at `~/ithamaps` (i.e. /home/rstudio/ithamaps), same files as the ithamaps-shiny service, edited live from your host.

In the RStudio console:

```r
setwd("~/ithamaps")
source("global.R")
```

Since global.R no longer contains ui/server/shinyApp() (thanks to the earlier split), sourcing it fully is safe — it just runs the DB connections, data wrangling, and defines the helper functions, then stops.
Now everything is in your environment for interactive testing:

```r
dplyr::glimpse(db_ithamaps_entries)
bundle <- build_query_bundle("DataType=1&Resolution=3&Country=7&Measure=2&Cause=3&Metric=2&Aggregation=1")
str(bundle, max.level = 1)
debugonce(build_query_bundle)   # step through line-by-line on the next call
```

---

## Debugging

Set `ITHAMAPS_DEBUG_MODE=true` (or `1` / `yes` / `on`) before starting the app. This enables two things:

- **Performance timings panel** — a collapsible panel rendered above the main content in the UI showing per-stage wall-clock times (cache lookup, bundle fetch, parse/extract, each filter step, geometry join, map/table render, PNG export, and browser-side transfer+render time).
- **Bundle step capture** — `build_query_bundle()` records intermediate `SubsetE`/`SubsetHCP` snapshots after every filter stage.

### Enabling debug mode

**Environment variable (recommended for Docker / production-like runs):**

```bash
ITHAMAPS_DEBUG_MODE=true docker compose up ithamaps-shiny
# or in docker-compose.yml:
#   environment:
#     - ITHAMAPS_DEBUG_MODE=true
```

**At the R console (interactive / RStudio session):**

```r
ithamaps_debug_mode <<- TRUE   # override the global before sourcing server.R
```

### Inspecting bundle debug steps interactively

```r
ithamaps_debug_mode <<- TRUE

source("global.R")
raw_qs <- "DataType=1&Resolution=2&Continent=1&Measure=21&Cause=10&Healthcare=1&HealthcareS1=14"
bundle <- build_query_bundle(raw_qs)

# Option A: steps carried inside the bundle
bundle$debug$steps

# Option B: global shortcut written by build_query_bundle
ithamaps_last_bundle_debug$steps

# Option C: list step labels cleanly
lapply(bundle$debug$steps, function(s) s$label)

# Access a specific step by label
step <- Filter(function(s) s$label == "after_parameter_SubsetE", bundle$debug$steps)[[1]]
step$value

# Inspect SubsetHCP before harmonization (step index may vary; check labels first)
tmp <- bundle$debug$steps[[7]]$value
result <- harmonize_healthcare_subset(tmp, debug_mode = TRUE)
result$result %>%
  select(geo_admin0, availability, diagnostic_method, compensation, eligibility,
         application, implementation, start_year, end_year,
         known_implementation_period, citation_str, ends_with("harmonised")) %>%
  select(geo_admin0, sort(names(.))) %>%
  View()

# Run harmonize_healthcare_subset directly with debug_mode = TRUE
result <- harmonize_healthcare_subset(bundle$SubsetHCP, debug_mode = TRUE)
result$steps    # list of label/value pairs
result$result   # the harmonized data frame
```

Use the Environment pane to browse data frames, and the Source/Console panes as usual.

---


 # TODO
 
  - debug weighted mean for Yemen that should show data on map but doesn't

```r
raw_qs = "ithamaps?DataType=1&Resolution=1&Measure=9&Metric=1&Aggregation=1&Cause=1&search=search"
```