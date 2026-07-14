#!/usr/bin/env Rscript
# ---------------------------------------------------------------------------
# build_caches.R
#
# Pre-build the simplified display-geometry caches that app.R reads at startup
# (cache_adm0_disp.rds / cache_adm1_disp.rds / cache_adm2_disp.rds).
#
# The ADM0/1/2 boundary files are full resolution (0.5-1 GB), so simplifying
# them on the first user request makes the first map load very slow. Running
# this script once (e.g. during the Docker image build) writes the caches so
# the running app skips the expensive st_simplify() entirely.
#
# This mirrors the layer preparation and tolerances in app.R. If you change the
# tolerances or the adm*_sel column selection there, change them here too.
#
# Usage:
#   Rscript build_caches.R          # build any missing/stale caches
#   Rscript build_caches.R --force  # rebuild all caches unconditionally
# ---------------------------------------------------------------------------

suppressPackageStartupMessages({
  library(sf)
  library(dplyr)
})

args <- commandArgs(trailingOnly = TRUE)
force <- "--force" %in% args

# Must match app.R: source files, output cache files, and tolerances (degrees).
layers <- list(
  list(source = "ADM0.gpkg", cache = "cache_adm0_disp.rds", key = "geo_admin0", region = "Region",  tol = 0.02),
  list(source = "ADM1.gpkg", cache = "cache_adm1_disp.rds", key = "geo_admin1", region = "Region1", tol = 0.02),
  list(source = "ADM2.gpkg", cache = "cache_adm2_disp.rds", key = "geo_admin2", region = "Region2", tol = 0.01)
)

geom_mb <- function(x) round(as.numeric(object.size(st_geometry(x))) / 1024^2, 1)

build_one <- function(spec) {
  if (!file.exists(spec$source)) {
    message(sprintf("[build_caches] SKIP %s: source %s not found", spec$cache, spec$source))
    return(invisible(NULL))
  }

  if (!force && file.exists(spec$cache) &&
      file.info(spec$cache)$mtime >= file.info(spec$source)$mtime) {
    message(sprintf("[build_caches] up-to-date %s (delete it or use --force to rebuild)", spec$cache))
    return(invisible(NULL))
  }

  message(sprintf("[build_caches] reading %s ...", spec$source))
  layer <- read_sf(spec$source) %>%
    dplyr::select(dplyr::all_of(spec$key), name, geom) %>%
    dplyr::rename(!!spec$region := name)

  before <- geom_mb(layer)
  t0 <- proc.time()[["elapsed"]]
  message(sprintf("[build_caches] simplifying %s (tol=%g, %.1f MB geom) ...", spec$cache, spec$tol, before))

  # Use planar GEOS (not s2) for simplification: s2 rejects self-intersecting
  # loops, common in coarse admin boundaries; GEOS simplifies/repairs them fine.
  old_s2 <- sf_use_s2()
  suppressMessages(sf_use_s2(FALSE))
  on.exit(suppressMessages(sf_use_s2(old_s2)), add = TRUE)

  simplify_once <- function(x) suppressWarnings(st_simplify(x, dTolerance = spec$tol, preserveTopology = TRUE))
  simplified <- tryCatch(
    simplify_once(layer),
    error = function(e) {
      message(sprintf("[build_caches] st_simplify failed for %s: %s; retrying after st_make_valid",
                      spec$cache, conditionMessage(e)))
      tryCatch(simplify_once(st_make_valid(layer)),
               error = function(e2) {
                 message(sprintf("[build_caches] retry failed for %s: %s", spec$cache, conditionMessage(e2)))
                 NULL
               })
    }
  )
  if (is.null(simplified)) {
    message(sprintf("[build_caches] FAILED %s — leaving no cache; app will fall back to full resolution", spec$cache))
    return(invisible(NULL))
  }

  # Restore original geometry for any feature simplified away to empty.
  empty <- st_is_empty(st_geometry(simplified))
  if (any(empty)) {
    st_geometry(simplified)[empty] <- st_geometry(layer)[empty]
  }

  saveRDS(simplified, spec$cache)
  message(sprintf("[build_caches] wrote %s: %.1f -> %.1f MB geom (%d features) in %.1fs",
                  spec$cache, before, geom_mb(simplified), nrow(simplified),
                  proc.time()[["elapsed"]] - t0))
  invisible(NULL)
}

for (spec in layers) build_one(spec)
message("[build_caches] done")
