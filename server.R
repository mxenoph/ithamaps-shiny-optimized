
# =============================================================================
# server.R
#
# Part of the global.R / ui.R / server.R Shiny app split. Defines the `server`
# function only. global.R has already run (packages, DB reads, lookups, helper
# functions such as build_query_bundle_cached, format_metric_unit,
# metric_title_with_unit, attach_display_geometry, prediction_assets, etc.) by
# the time this file is sourced, so `server` may reference anything defined
# there.
# =============================================================================

server = function(input, output, session) {
  perf_state = reactiveValues(
    map_render_secs = NULL,
    map_client_total_secs = NULL,
    map_simplify_secs = NULL,
    table_render_secs = NULL,
    png_prep_cache_hit = NULL,
    png_prep_workers = NULL,
    png_prep_secs = NULL,
    png_context_secs = NULL,
    png_data_secs = NULL,
    png_plot_build_secs = NULL,
    png_save_secs = NULL,
    png_total_secs = NULL
  )
  trace_env = new.env(parent = emptyenv())
  trace_env$bundle_builds = 0L
  trace_env$last_qs = NA_character_

  # Browser-reported total wall-clock for the main map (server compute +
  # serialization + websocket transfer + client-side leaflet render).
  observeEvent(input$map_client_total_ms, {
    ms = suppressWarnings(as.numeric(input$map_client_total_ms))
    if (!is.na(ms)) {
      perf_state$map_client_total_secs = round(ms / 1000, 3)
    }
  })

  log_trace = function(event, details = "") {
    sid = substr(session$token %||% "unknown", 1, 8)
    ts = format(Sys.time(), "%Y-%m-%d %H:%M:%S")
    prefix = paste0("[ithamaps-trace][", ts, "][sid=", sid, "][", event, "]")
    if (nchar(details) > 0) {
      cat(prefix, details, "\n")
    } else {
      cat(prefix, "\n")
    }
    flush.console()
  }

  local_logo_asset_url = function() {
    if (file.exists(file.path(getwd(), "ithanet-logo_light-background.png"))) {
      "/ithanet-logo_light-background.png"
    } else {
      ""
    }
  }

  logo_asset_url = function() {
    override = trimws(Sys.getenv("ITHAMAPS_LOGO_URL", unset = ""))
    if (!nzchar(override)) {
      override = trimws(Sys.getenv("ITHANET_LOGO_URL", unset = ""))
    }
    if (nzchar(override)) {
      return(override)
    }

    site_root = infer_ithanet_root()
    if (nzchar(site_root)) {
      site_root = sub("/+$", "", site_root)
      site_asset = paste0(site_root, "/images/logos/ithanet-logo_light-background.png")
      return(site_asset)
    }

    local_logo_asset_url()
  }

  logo_export_image = function() {
    source_url = logo_asset_url()

    # Try HTTP URL (substitute localhost/127.0.0.1 with host.docker.internal for Docker)
    if (grepl("^https?://", source_url, ignore.case = TRUE)) {
      dl_url = sub("://localhost(/|$)", "://host.docker.internal\\1", source_url)
      dl_url = sub("://127\\.0\\.0\\.1(/|$)", "://host.docker.internal\\1", dl_url)
      tmp_path = tempfile(fileext = ".png")
      ok = tryCatch({
        utils::download.file(dl_url, destfile = tmp_path, quiet = TRUE, mode = "wb")
        file.exists(tmp_path) && file.info(tmp_path)$size > 0
      }, error = function(e) FALSE)
      img = if (ok) tryCatch(png::readPNG(tmp_path), error = function(e) NULL) else NULL
      unlink(tmp_path)
      if (!is.null(img)) return(img)
    }

    # Fallback: local copy shipped in the app directory
    local_path = file.path(getwd(), "ithanet-logo_light-background.png")
    if (file.exists(local_path)) {
      tryCatch(png::readPNG(local_path), error = function(e) NULL)
    } else {
      NULL
    }
  }

  # Wraps a legend title at word boundaries so no line exceeds `width` characters.
  wrap_legend_title = function(title, width = 21) {
    words = strsplit(title, " ")[[1]]
    current = ""
    lines = character(0)
    for (w in words) {
      candidate = if (nchar(current) == 0) w else paste(current, w)
      if (nchar(candidate) <= width) {
        current = candidate
      } else {
        lines = c(lines, current)
        current = w
      }
    }
    paste(c(lines, current), collapse = "\n")
  }

  # panel_width / panel_height must match the ggsave() dimensions for this plot panel.
  add_logo_to_ggplot = function(p, xlim, ylim, data_sf = NULL, corner = NULL, logo_img = NULL,
                                 panel_width = 12, panel_height = 8) {
    img = logo_img %||% logo_export_image()
    if (is.null(img) || length(img) == 0L) return(p)

    # Determine corner: explicit override, or detect from data centroid distribution.
    if (!is.null(corner)) {
      use_right = identical(corner, "right")
    } else {
      use_right = FALSE
      if (!is.null(data_sf) && nrow(data_sf) > 0) {
        cents = tryCatch(
          sf::st_coordinates(sf::st_centroid(suppressWarnings(sf::st_geometry(data_sf)))),
          error = function(e) NULL
        )
        if (!is.null(cents) && nrow(cents) > 0) {
          use_right = any(cents[, 1] < mean(xlim) & cents[, 2] > ylim[1] + 0.65 * diff(ylim))
        }
      }
    }

    # Logo fixed at constant physical size, aspect-ratio preserved.
    # coord_sf (WGS84) renders 1° lon as cos(φ) visual units of 1° lat, so we correct.
    img_asp    = ncol(img) / nrow(img)
    center_lat = mean(ylim)
    cos_lat    = max(cos(center_lat * pi / 180), 0.1)  # floor avoids ÷0 near poles
    geo_asp    = diff(xlim) * cos_lat / diff(ylim)
    panel_asp  = panel_width / panel_height
    if (geo_asp >= panel_asp) {
      eff_W = panel_width;  eff_H = panel_width / geo_asp
    } else {
      eff_H = panel_height; eff_W = panel_height * geo_asp
    }
    logo_h_raw = (0.45 / eff_H) * diff(ylim)
    logo_w_raw = (0.45 * img_asp / eff_W) * diff(xlim) / cos_lat
    pad_x_raw  = (0.18 / eff_W) * diff(xlim) / cos_lat
    pad_y_raw  = (0.18 / eff_H) * diff(ylim)
    # Cap so the logo never dominates a very narrow or high-latitude zoomed view.
    scale = min(1, 0.22 * diff(xlim) / logo_w_raw, 0.18 * diff(ylim) / logo_h_raw)
    logo_w = logo_w_raw * scale;  logo_h = logo_h_raw * scale
    pad_x  = pad_x_raw  * scale;  pad_y  = pad_y_raw  * scale

    if (use_right) {
      xmin = xlim[2] - pad_x - logo_w;  xmax = xlim[2] - pad_x
    } else {
      xmin = xlim[1] + pad_x;           xmax = xlim[1] + pad_x + logo_w
    }
    ymax = ylim[2] - pad_y
    ymin = ymax - logo_h

    logo_box = grid::grobTree(
      grid::roundrectGrob(gp = grid::gpar(fill = "white", col = NA, alpha = 0.85), r = grid::unit(6, "pt")),
      grid::rasterGrob(img, interpolate = TRUE,
        width = grid::unit(0.80, "npc"), height = grid::unit(0.80, "npc"))
    )
    p + ggplot2::annotation_custom(logo_box, xmin = xmin, xmax = xmax, ymin = ymin, ymax = ymax)
  }

  logo_control_html = function() {
    tags$div(
      class = "ithamaps-logo-control",
      style = "background: rgba(255,255,255,0.85); border-radius: 8px; padding: 4px 6px; box-shadow: 0 1px 3px rgba(0,0,0,0.18); line-height: 0; position: relative; z-index: 1000;",
      tags$img(
        src = logo_asset_url(),
        alt = "IthaNet logo",
        style = "display: block; max-width: 126px; max-height: 42px; width: auto; height: auto; object-fit: contain;"
      )
    )
  }

  add_logo_leaflet_control = function(map_widget) {
    map_widget %>%
      addControl(
        html = logo_control_html(),
        position = "topleft"
      ) %>%
      htmlwidgets::onRender(
        "function(el, x) {
          var map = this;
          var moveLogo = function() {
            var left = map.getContainer().querySelector('.leaflet-top.leaflet-left');
            if (!left) return;
            var logo = left.querySelector('.ithamaps-logo-control');
            if (!logo) return;
            var beforeNode = left.querySelector('.leaflet-control-zoom, .leaflet-control-fullscreen');
            if (beforeNode && logo !== beforeNode && logo.nextElementSibling !== beforeNode) {
              left.insertBefore(logo, beforeNode);
            }
            if (logo) logo.style.zIndex = '1000';
          };
          setTimeout(moveLogo, 0);
          map.whenReady(moveLogo);
        }"
      )
  }

  infer_ithanet_root = function() {
    # Optional explicit override for environments where URL inference is not
    # reliable (reverse proxies, custom ports, non-standard paths).
    root_override = trimws(Sys.getenv("ITHANET_SITE_ROOT", unset = ""))
    if (nzchar(root_override)) {
      return(sub("/+$", "", root_override))
    }

    # Preferred runtime override from the iframe query string, intended to be
    # passed from Joomla as SiteRoot=<URI::root()>.
    raw_qs = normalize_query_string(session$clientData$url_search %||% "")
    if (nzchar(raw_qs)) {
      qs_parts = strsplit(raw_qs, "&", fixed = TRUE)[[1]]
      for (part in qs_parts) {
        kv = strsplit(part, "=", fixed = TRUE)[[1]]
        if (length(kv) != 2) {
          next
        }
        key = tolower(utils::URLdecode(kv[[1]]))
        if (key %in% c("siteroot", "joomlaroot", "ithanetroot")) {
          val = trimws(utils::URLdecode(kv[[2]]))
          if (nzchar(val)) {
            return(sub("/+$", "", val))
          }
        }
      }
    }

    proto_raw = as.character(session$clientData$url_protocol %||% "http:")
    proto = sub(":$", "", proto_raw)
    host = as.character(session$clientData$url_hostname %||% "")
    port = as.character(session$clientData$url_port %||% "")
    origin = if (nzchar(host)) {
      default_port = (proto == "http" && identical(port, "80")) || (proto == "https" && identical(port, "443"))
      paste0(proto, "://", host, if (nzchar(port) && !default_port) paste0(":", port) else "")
    } else {
      ""
    }

    ref = as.character(session$request$HTTP_REFERER %||% "")
    if (!nzchar(ref)) {
      # When embedded under Joomla, referrer can be suppressed by policy. Avoid
      # leaking the Shiny port into links; fall back to host origin without
      # internal app ports.
      if (identical(port, "3838")) {
        return(paste0(proto, "://", host))
      }
      return(origin)
    }

    m = regexec("^(https?)://([^/]+)(/[^?#]*)?", ref, perl = TRUE)
    g = regmatches(ref, m)[[1]]
    if (length(g) < 3) {
      return(origin)
    }

    ref_origin = paste0(g[[2]], "://", g[[3]])
    ref_path = if (length(g) >= 4 && !is.na(g[[4]])) g[[4]] else ""
    root_path = ""

    if (nzchar(ref_path)) {
      if (grepl("/index\\.php", ref_path, ignore.case = TRUE)) {
        root_path = sub("/index\\.php.*$", "", ref_path, ignore.case = TRUE)
      } else if (grepl("/db/", ref_path, ignore.case = TRUE)) {
        root_path = sub("/db/.*$", "", ref_path, ignore.case = TRUE)
      }
    }

    root_path = sub("/+$", "", root_path)
    if (identical(root_path, "/")) {
      root_path = ""
    }

    out = paste0(ref_origin, root_path)
    sub("/+$", "", out)
  }

  build_ithaid_link = function(itha_id, root) {
    id_chr = as.character(itha_id %||% "")
    if (!nzchar(id_chr) || is.na(id_chr) || identical(id_chr, "Not applicable")) {
      return(id_chr)
    }

    safe_id = htmltools::htmlEscape(id_chr)
    href = paste0(sub("/+$", "", root), "/db/ithagenes?ithaID=", utils::URLencode(id_chr, reserved = TRUE))
    paste0(
      "<a href=\"", htmltools::htmlEscape(href), "\" target=\"_blank\" rel=\"noopener noreferrer\">",
      safe_id,
      "</a>"
    )
  }

  startup_ua = substr(session$request$HTTP_USER_AGENT %||% "", 1, 140)
  startup_ref = substr(session$request$HTTP_REFERER %||% "", 1, 140)
  log_trace("session_start", paste0("ua='", startup_ua, "' ref='", startup_ref, "'"))

  session$onSessionEnded(function() {
    log_trace("session_end", paste0("bundle_builds=", trace_env$bundle_builds))
  })

  observeEvent(session$clientData$url_search,
    {
      current_qs = normalize_query_string(session$clientData$url_search %||% "")
      if (is.na(trace_env$last_qs)) {
        log_trace("url_search_init", paste0("qs='", current_qs, "'"))
        trace_env$last_qs = current_qs
        return()
      }
      if (!identical(current_qs, trace_env$last_qs)) {
        log_trace("url_search_change", paste0("from='", trace_env$last_qs, "' to='", current_qs, "'"))
        trace_env$last_qs = current_qs
      }
    },
    ignoreInit = FALSE
  )

  # Unified query-string source: prefer a query injected via postMessage from the
  # parent Joomla page (input$injected_query) over the iframe's own URL params.
  # This lets the parent update the Shiny app without reloading the iframe while
  # still supporting shared links (first load always reads from url_search).
  active_url_search_r = reactive({
    iq = input$injected_query %||% NULL
    if (!is.null(iq) && nzchar(trimws(iq))) trimws(iq)
    else (session$clientData$url_search %||% "")
  })

  # Build data bundle from the current URL query string
  query_bundle = reactive({
    raw_qs = active_url_search_r()
    fetch_start = proc.time()[["elapsed"]]
    bundle = build_query_bundle_cached(raw_qs)
    bundle$timings = bundle$timings %||% list()
    bundle$timings$bundle_fetch = round(proc.time()[["elapsed"]] - fetch_start, 3)
    bundle$timings$query_string = normalize_query_string(raw_qs)

    trace_env$bundle_builds = trace_env$bundle_builds + 1L
    if (trace_env$bundle_builds <= 5L || trace_env$bundle_builds %% 10L == 0L) {
      cache_hit = if (isTRUE(bundle$timings$cache_hit)) "yes" else "no"
      log_trace(
        "query_bundle_build",
        paste0(
          "n=", trace_env$bundle_builds,
          " cache_hit=", cache_hit,
          " qs='", bundle$timings$query_string %||% "", "'"
        )
      )
    }

    bundle
  })

  SubsetE_r = reactive({
    query_bundle()$SubsetE
  })
  SubsetHCP_r = reactive({
    hcp = query_bundle()$SubsetHCP
    if (!is.null(hcp) && !"citation_link_harmonised" %in% names(hcp)) {
      hcp[["citation_link_harmonised"]] = hcp[["citation_str_harmonised"]]
    }
    hcp
  })
  SubsetHCP_raw_r = reactive({
    query_bundle()$SubsetHCPRaw
  })
  DataType_r = reactive({
    query_bundle()$DataType %||% "Curated data"
  })
  is_prediction_mode = reactive({
    identical(DataType_r(), "Prediction data")
  })
  prediction_data_r = reactive({
    query_bundle()$prediction
  })
  is_hcp_mode = reactive({
    hcp = SubsetHCP_r()
    !is.null(hcp) && nrow(hcp) > 0
  })
  SubsetG_r = reactive({
    b = query_bundle()
    if (!is.null(b$SubsetG) && !inherits(b$SubsetG, "sf") && "geom" %in% names(b$SubsetG)) {
      st_as_sf(b$SubsetG)
    } else {
      b$SubsetG
    }
  })
  MetricN_r = reactive({
    query_bundle()$MetricN
  })
  MetricUnit_r = reactive({
    query_bundle()$MetricUnit
  })
  validation_errors_r = reactive({
    query_bundle()$validation_errors %||% character()
  })
  timing_info_r = reactive({
    query_bundle()$timings %||% list()
  })

  data_available = reactive({
    if (length(validation_errors_r()) > 0) {
      return(FALSE)
    }
    if (is_prediction_mode()) {
      return(TRUE)
    }
    se = SubsetE_r()
    hcp = SubsetHCP_r()
    (!is.null(se) && nrow(se) > 0) || (!is.null(hcp) && nrow(hcp) > 0)
  })

  filtered_data = reactive({
    req(!is_prediction_mode())
    req(data_available())
    if (is_hcp_mode()) {
      SubsetHCP = SubsetHCP_r()
      if (!is.null(input$data_table_rows_all)) SubsetHCP[input$data_table_rows_all, ] else SubsetHCP
    } else {
      SubsetE = SubsetE_r()
      if (!is.null(input$data_table_rows_all)) SubsetE[input$data_table_rows_all, ] else SubsetE
    }
  })

  # Stable view of filtered_data used only for the (expensive) full map
  # re-render. DT emits input$data_table_rows_all asynchronously after the table
  # first draws and again on every redraw. A simple debounce is not sufficient
  # because DT initialisation often takes longer than the debounce window, so two
  # separate renders fire. Instead we use a reactiveVal that is only updated when
  # the effective filter actually changes: the transition from rows_all = NULL
  # (before DT initialises) to rows_all = [1:N] (unfiltered DT) produces the
  # same data as the base subset, so we skip the update and avoid the second
  # renderLeaflet execution. Selection mapping and downloads still read the live
  # filtered_data()/rows_all, so their behaviour is unchanged.
  map_filtered_data = reactiveVal(NULL)

  observe({
    req(!is_prediction_mode())
    req(data_available())
    base = if (is_hcp_mode()) SubsetHCP_r() else SubsetE_r()
    rows_all = input$data_table_rows_all
    is_unfiltered = is.null(rows_all) || length(rows_all) == nrow(base)
    current = isolate(map_filtered_data())
    # Skip when both old and new state are "unfiltered" with the same base size:
    # this is exactly the NULL → [1:N] startup transition that caused the double render.
    if (!is.null(current) &&
        is_unfiltered &&
        isTRUE(attr(current, "ithamaps_unfiltered")) &&
        nrow(current) == nrow(base)) {
      return()
    }
    new_data = if (is_unfiltered) base else base[rows_all, ]
    attr(new_data, "ithamaps_unfiltered") = is_unfiltered
    map_filtered_data(new_data)
  })


  selected_marker_idx = reactiveVal(NULL)
  selected_shape_idx = reactiveVal(NULL)
  selected_row = reactiveVal(NULL)
  selected_marker_layer_id = reactiveVal(NULL)
  selected_prediction_point = reactiveVal(NULL)
  export_status_text = reactiveVal("")

  # Tracks the previous set of selected table rows (in original-data 1-based
  # indices, matching input$data_table_rows_selected) so a manual click can be
  # distinguished as an "add" vs "remove" and collapsed to a single row.
  previous_selection = reactiveVal(integer(0))
  # When TRUE, the next data_table_rows_selected change was triggered
  # programmatically (shape/marker/clear/collapse) and must be accepted as-is
  # without re-collapsing. The observer consumes (resets) the flag.
  suppress_selection_observer = reactiveVal(FALSE)

  local_rows_from_table_rows = function(table_rows) {
    if (is.null(table_rows) || length(table_rows) == 0) {
      return(NULL)
    }
    table_rows = as.integer(table_rows)
    rows_all = input$data_table_rows_all
    if (is.null(rows_all)) {
      return(table_rows)
    }
    local_row = match(table_rows, rows_all)
    local_row = as.integer(local_row[!is.na(local_row)])
    if (length(local_row) == 0) NULL else local_row
  }

  table_rows_from_local_rows = function(local_rows) {
    if (is.null(local_rows) || length(local_rows) == 0) {
      return(integer(0))
    }
    local_rows = as.integer(local_rows)
    rows_all = input$data_table_rows_all
    out = if (is.null(rows_all)) local_rows else rows_all[local_rows]
    out = as.integer(out[!is.na(out)])
    out
  }

  set_export_status = function(msg) {
    text = msg %||% ""
    export_status_text(text)
    session$sendCustomMessage("ithamaps-export-status", list(text = text))
  }

  clear_export_status = function() {
    export_status_text("")
    session$sendCustomMessage("ithamaps-export-status", list(text = ""))
  }

  output$export_status_inline = renderUI({
    msg = export_status_text()
    div(
      id = "ithamaps_export_status",
      class = "alert alert-info py-2 px-3 mt-2 mb-0",
      style = paste0("font-size: 0.9rem;", if (!nzchar(msg)) " display:none;" else ""),
      msg
    )
  })

  output$main_content = renderUI({
    if (length(validation_errors_r()) > 0) {
      return(div())
    }

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
          class = "pred-maps-grid",
          # Row 1 col 1: predicted mean carrier prevalence
          div(
            class = "",
            div(
              class = "map-card",
              div(class = "map-title", "Predicted mean carrier prevalence"),
              withSpinner(leafletOutput("map_mean", height = "500px"), type = 3, color = "#0000CC", color.background = "white", caption = "Loading mean predicted carrier prevalence raster..."),
              uiOutput("mean_legend")
            )
          ),
          # Row 1 col 2: prediction uncertainty
          div(
            class = "",
            div(
              class = "map-card",
              div(class = "map-title", "Prediction uncertainty (95% CI)"),
              withSpinner(leafletOutput("map_ci95_2", height = "500px"), type = 3, color = "#0000CC", color.background = "white", caption = "Loading prediction uncertainty raster..."),
              uiOutput("ci95_legend_2")
            )
          ),
          # Row 2: estimated number of carriers (full width)
          div(
            class = "",
            div(
              class = "map-card",
              div(class = "map-title", "Estimated number of carriers"),
              withSpinner(leafletOutput("map_burden", height = "500px"), type = 3, color = "#0000CC", color.background = "white", caption = "Loading estimated number of carriers raster..."),
              uiOutput("burden_legend")
            )
          )
        ),
        div(
          class = "download-row",
          downloadButton("download_tif", "Export as .tif", icon = icon("file-arrow-down"), class = "btn btn-secondary btn-sm"),
          downloadButton("download_png", "Export as .png", icon = icon("file-image"), class = "btn btn-secondary btn-sm"),
          downloadButton("download_csv", "Export as .csv", icon = icon("file-csv"), class = "btn btn-secondary btn-sm")
        ),
        uiOutput("export_status_inline")
      ))
    }

    if (!data_available()) {
      return(div(
        class = "container-fluid py-4 px-4",
        div(
          class = "row g-3 mb-3",
          div(
            class = "col-12",
            div(
              class = "info-card",
              div(
                class = "row",
                div(class = "col-12", uiOutput("current_curated_query"))
              ),
              div(
                class = "row mt-2 pt-2",
                style = "border-top: 1px solid #dee2e6;",
                div(
                  class = "col-12",
                  style = "color: red; background-color: #fff3cd; padding: 0.5rem 0.75rem;",
                  "No data is available for the selected parameter combination."
                )
              )
            )
          )
        )
      ))
    }

    div(
      class = "container-fluid py-4 px-4",
      div(
        class = "row g-3 mb-3",
        div(
          class = "col-12",
          div(
            class = "info-card",
            div(
              class = "row",
              div(class = "col-12", uiOutput("current_curated_query"))
            ),
            div(
              class = "row mt-2 pt-2",
              style = "border-top: 1px solid #dee2e6;",
              if (is_hcp_mode()) {
                tagList(
                  div(
                    class = "col-12 text-muted",
                    paste(
                      "Healthcare availability data are aggregated per country: when multiple reports exist for the same country,",
                      "they are consolidated into a single entry for each availability status. Where reports give conflicting details,",
                      "values are merged using fixed precedence rules (for example, broader eligibility such as \"Universal\" and stricter",
                      "application such as \"Mandatory\" take precedence; diagnostic methods and source references are combined; and the",
                      "implementation timeframe spans the earliest to the most recent reported years)."
                    )
                  ),
                  div(
                    class = "col-12 text-muted mt-1",
                    "Click any country polygon for the consolidated healthcare policy details. Table column filters update the displayed records only."
                  )
                )
              } else {
                tagList(
                  div(
                    class = "col-12 text-muted",
                    "Circles show unique records. Numbered black circles indicate multiple records at that location. Click any marker for more details."
                  ),
                  div(
                    class = "col-12 text-muted mt-1",
                    "Table column filters update displayed records (table rows and black circles) only. Polygon colours and metric legends are not recomputed from table-filtered subsets."
                  )
                )
              }
            )
          )
        )
      ),
      div(
        class = "d-flex mb-4 shadow-sm rounded border",
        div(
          style = "width: 30%; max-height: 500px; overflow-y: auto; padding: 10px; border-right: 1px solid #ccc; background-color: #f8f9fa;",
          uiOutput("custom_popup")
        ),
        div(
          style = "flex-grow: 1; position: relative;",
          div(
            id = "ithamaps_map_loader",
            class = "ithamaps-map-loader is-loading",
            div(class = "ithamaps-map-loader-caption", "Retrieving requested data. This may take a moment."),
            div(class = "ithamaps-progress", div(class = "ithamaps-progress-bar"))
          ),
          leafletOutput("map", height = "500px")
        )
      ),
      div(
        class = "mb-4 d-flex flex-wrap gap-2 justify-content-center",
        downloadButton("download_png", "Export as .png", icon = icon("file-image"), class = "btn btn-secondary btn-sm"),
        downloadButton("download_csv", "Export as .csv", icon = icon("file-arrow-down"), class = "btn btn-secondary btn-sm"),
        downloadButton("download_gpkg", "Export as .gpkg", icon = icon("file-arrow-down"), class = "btn btn-secondary btn-sm"),
        downloadButton("download_geojson", "Export as .geojson", icon = icon("file-arrow-down"), class = "btn btn-secondary btn-sm")
      ),
      uiOutput("export_status_inline"),
      div(
        class = "table-responsive shadow-sm rounded border",
        style = "",
        DTOutput("data_table")
      )
    )
  })

  selected_parameters_r = reactive({
    raw_qs = active_url_search_r()
    Query = Extract(Parse(raw_qs))

    if (length(Query) == 0) {
      return(data.frame(Parameter = character(), Selection = character(), stringsAsFactors = FALSE))
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

    preferred_order = c(
      "DataType", "Resolution", "Continent", "Country", "Measure", "Cause",
      "Healthcare", paste0("HealthcareS", 1:13),
      "GlobinPheAF", "VariantC", "GlobinPheRAF", "IthaID", "Metric", "Aggregation"
    )
    keys = preferred_order[preferred_order %in% names(Query)]
    extra_keys = setdiff(names(Query), keys)
    keys = c(keys, extra_keys)

    rows = lapply(keys, function(key) {
      id_val = Query[[key]]
      selection = as.character(id_val)
      if (key %in% names(lookup)) {
        opt = suppressWarnings(Search(key, id_val, lookup[[key]]))
        if (!is.na(opt)[1]) {
          selection = as.character(opt[[1]])
        }
      }
      data.frame(Parameter = key, Selection = selection, stringsAsFactors = FALSE)
    })

    bind_rows(rows)
  })

  summary_panel_r = reactive({
    params_df = selected_parameters_r()

    get_param_selection = function(name) {
      hit = params_df$Selection[params_df$Parameter == name]
      if (length(hit) == 0) {
        return(NA_character_)
      }
      as.character(hit[[1]])
    }

    before_count = if (is_hcp_mode()) {
      hcp = SubsetHCP_r()
      if (is.null(hcp)) 0 else nrow(hcp)
    } else if (is_prediction_mode()) {
      assets = prediction_data_r()
      if (is.null(assets)) 0 else raster::ncell(assets$Mean)
    } else {
      se = SubsetE_r()
      if (is.null(se)) 0 else nrow(se)
    }
    after_count = if (is_prediction_mode()) {
      assets = prediction_data_r()
      if (is.null(assets)) 0 else nrow(assets$Selected_sites)
    } else if (isTRUE(data_available())) {
      nrow(filtered_data())
    } else {
      0
    }
    mode_label = if (is_prediction_mode()) {
      "Prediction data"
    } else if (is_hcp_mode()) {
      "Healthcare availability"
    } else {
      "Curated data"
    }

    if (is_hcp_mode()) {
      hcp_raw = SubsetHCP_raw_r()
      total_raw_entries = if (is.null(hcp_raw)) 0 else nrow(hcp_raw)
      total_consolidated_records = before_count
      shown_consolidated_records = after_count

      summary_df = bind_rows(
        data.frame(
          Parameter = c("Data mode", "Total records", "Total consolidated records", "Shown consolidated records"),
          Selection = c(
            mode_label,
            as.character(total_raw_entries),
            as.character(total_consolidated_records),
            as.character(shown_consolidated_records)
          ),
          stringsAsFactors = FALSE
        ),
        params_df
      )
    } else {
      summary_df = bind_rows(
        data.frame(
          Parameter = c("Data mode", "Total records", "Shown records"),
          Selection = c(mode_label, as.character(before_count), as.character(after_count)),
          stringsAsFactors = FALSE
        ),
        params_df
      )
    }

    metric_table_html = ""
    resolution_sel = get_param_selection("Resolution")
    aggregation_sel = get_param_selection("Aggregation")
    metric_name_sel = get_param_selection("Metric")
    if (!is_hcp_mode() && identical(resolution_sel, "Country-level") && identical(aggregation_sel, "Country-level")) {
      sg = SubsetG_r()
      metric_values = if (is.null(sg) || !("Metric" %in% names(sg))) numeric(0) else as.numeric(sg$Metric)
      metric_values = metric_values[!is.na(metric_values)]
      metric_value_label = if (length(metric_values) == 0) {
        "N/A"
      } else if (length(unique(metric_values)) == 1) {
        as.character(round(unique(metric_values)[1], 2))
      } else {
        paste0("Multiple (", length(unique(metric_values)), ")")
      }
      # Append the data unit (from the metric table) to the value shown, e.g.
      # "12.5 %" or "3.2 per 100000". Kept separate from the summary-statistic
      # name (metric_name_label) below.
      metric_unit_pretty = format_metric_unit(MetricUnit_r())
      if (!is.na(metric_unit_pretty) && !identical(metric_value_label, "N/A")) {
        metric_value_label = paste0(metric_value_label, " ", metric_unit_pretty)
      }
      metric_name_label = if (is.na(metric_name_sel) || !nzchar(metric_name_sel)) "Metric" else metric_name_sel
      metric_table_html = paste0(
        "<h5 style='margin:8px 0 4px 0;'>Metric calculation</h5>",
        "<table style='width:100%; border-collapse:collapse; border: 1px solid #ddd;'>",
        "<tr><td style='padding:2px 4px; vertical-align:top; background:#f9f9f9; color:#333; font-weight:600; width:42%; white-space:nowrap; border: 1px solid #ddd;'>Metric</td>",
        sprintf("<td style='padding:2px 4px; vertical-align:top; background:#ffffff; color:#000; border: 1px solid #ddd;'>%s</td></tr>", metric_name_label),
        "<tr><td style='padding:2px 4px; vertical-align:top; background:#f9f9f9; color:#333; font-weight:600; width:42%; white-space:nowrap; border: 1px solid #ddd;'>Value</td>",
        sprintf("<td style='padding:2px 4px; vertical-align:top; background:#ffffff; color:#000; border: 1px solid #ddd;'>%s</td></tr>", metric_value_label),
        "</table>"
      )
    }

    table_html = paste0(
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

  curated_query_box_html = reactive({
    req(!is_prediction_mode())

    params_df = selected_parameters_r()
    if (nrow(params_df) == 0) {
      return(HTML(paste0(
        "<div style='padding: 8px; border: 1px solid #9ec5fe; background: #eef6ff; border-radius: 6px;'>",
        "<strong>Current curated query:</strong> No URL query parameters were provided.",
        "</div>"
      )))
    }

    label_map = c(
      Resolution = "Resolution",
      Continent = "Continent",
      Country = "Country",
      Measure = "Measure",
      Cause = "Cause",
      Healthcare = "Healthcare",
      GlobinPheAF = "Globin phenotype",
      VariantC = "Variant mode",
      GlobinPheRAF = "Grouped phenotype",
      IthaID = "IthaID",
      Metric = "Metric",
      Aggregation = "Aggregation"
    )
    healthcare_labels = stats::setNames(
      paste("Healthcare detail", 1:13),
      paste0("HealthcareS", 1:13)
    )
    label_map = c(label_map, healthcare_labels)

    preferred_order = c(
      "Resolution", "Continent", "Country", "Measure", "Cause",
      "Healthcare", paste0("HealthcareS", 1:13),
      "GlobinPheAF", "VariantC", "GlobinPheRAF", "IthaID",
      "Metric", "Aggregation"
    )

    params_df = params_df %>%
      filter(Parameter != "DataType") %>%
      mutate(
        sort_key = match(Parameter, preferred_order),
        sort_key = ifelse(is.na(sort_key), length(preferred_order) + seq_len(n()), sort_key),
        Label = dplyr::coalesce(unname(label_map[Parameter]), Parameter)
      ) %>%
      arrange(sort_key)

    detail_text = if (nrow(params_df) == 0) {
      "No curated-data filters are currently active."
    } else {
      paste(sprintf("%s = %s", params_df$Label, params_df$Selection), collapse = "; ")
    }

    HTML(paste0(
      "<div style='padding: 8px; border: 1px solid #9ec5fe; background: #eef6ff; border-radius: 6px;'>",
      "<strong>Current curated query:</strong> ",
      detail_text,
      ".</div>"
    ))
  })

  output$current_curated_query = renderUI({
    curated_query_box_html()
  })

  # Helper: apply a single-row selection (or clear) to all selection state and,
  # if the on-screen DT selection differs, push it via the proxy with the
  # suppression flag set so the resulting rows_selected event is accepted as-is.
  apply_single_selection = function(table_row, current_selection) {
    if (is.null(table_row) || length(table_row) == 0 || is.na(table_row[[1]])) {
      previous_selection(integer(0))
      selected_row(NULL)
      selected_marker_idx(NULL)
      selected_marker_layer_id(NULL)
      selected_shape_idx(NULL)
      if (length(current_selection) > 0) {
        suppress_selection_observer(TRUE)
        dataTableProxy("data_table") %>% selectRows(NULL)
      }
      return(invisible(NULL))
    }
    table_row = as.integer(table_row[[1]])
    row_local = local_rows_from_table_rows(table_row)
    if (is.null(row_local) || length(row_local) == 0) {
      return(invisible(NULL))
    }
    row_idx = as.integer(row_local[[1]])
    previous_selection(table_row)
    selected_row(row_idx)
    selected_marker_idx(row_idx)
    selected_marker_layer_id(paste0("row_", row_idx))
    selected_shape_idx(NULL)
    same = length(current_selection) == 1 && current_selection[[1]] == table_row
    if (!same) {
      suppress_selection_observer(TRUE)
      proxy = dataTableProxy("data_table")
      page_length = 10L
      target_page = ((table_row - 1L) %/% page_length) + 1L
      proxy %>% selectPage(target_page)
      proxy %>% selectRows(table_row)
    }
  }

  # The table uses "multiple" selection so a map shape click can natively
  # highlight many rows. Manual interaction, however, must behave as single
  # selection: clicking a row selects only it, and clicking the sole selected
  # row clears it. DT's default selection does not emit the Select extension's
  # "user-select" event, so this is handled entirely server-side here by diffing
  # the new selection against the previous one.
  observeEvent(input$data_table_rows_selected, {
    new_sel = as.integer(input$data_table_rows_selected %||% integer(0))
    new_sel = new_sel[!is.na(new_sel)]

    # Programmatic change (shape/marker/clear/collapse): accept as-is and sync.
    if (isTRUE(suppress_selection_observer())) {
      suppress_selection_observer(FALSE)
      previous_selection(new_sel)
      selected_row(local_rows_from_table_rows(new_sel))
      return(invisible(NULL))
    }

    prev = as.integer(previous_selection())
    prev = prev[!is.na(prev)]
    added = setdiff(new_sel, prev)
    removed = setdiff(prev, new_sel)

    if (length(added) >= 1) {
      # User clicked an unselected row: keep only the newly clicked one.
      apply_single_selection(added[[length(added)]], new_sel)
    } else if (length(removed) >= 1) {
      if (length(prev) <= 1) {
        # Toggled off the sole selected row: clear everything.
        apply_single_selection(NULL, new_sel)
      } else {
        # Clicked one member of a multi-row (shape) selection: focus just it.
        apply_single_selection(removed[[1]], new_sel)
      }
    } else {
      apply_single_selection(NULL, new_sel)
    }
  }, ignoreNULL = FALSE)

  observeEvent(filtered_data(),
    {
      selected_marker_idx(NULL)
      selected_marker_layer_id(NULL)
      selected_shape_idx(NULL)
      selected_row(NULL)
      previous_selection(integer(0))
      suppress_selection_observer(FALSE)
    },
    ignoreInit = TRUE
  )

  observe({
    req(!is_prediction_mode())
    req(data_available())
    session$sendCustomMessage("ithamaps-select-layer", list(
      mapId = "map",
      layerId = selected_marker_layer_id()
    ))
  })

  # Healthcare availability renders country polygons, so the selected row is
  # highlighted with a blue polygon overlay drawn on top via leafletProxy.
  # Guard on input$map_bounds so the proxy only runs after the map exists.
  observe({
    req(!is_prediction_mode())
    req(is_hcp_mode())
    req(!is.null(input$map_bounds))
    proxy = leafletProxy("map")
    proxy %>% clearGroup("highlight")
    row_local = selected_row()
    if (!is.null(row_local) && length(row_local) > 0) {
      row_idx = as.integer(row_local[[1]])
      data = filtered_data()
      if (!is.na(row_idx) && row_idx >= 1L && row_idx <= nrow(data)) {
        idx0 = match(data$geo_admin0[row_idx], adm0_sel$geo_admin0)
        if (!is.na(idx0)) {
          sel_sf = st_as_sf(
            data[row_idx, , drop = FALSE] %>%
              mutate(geom = st_geometry(adm0_sel)[idx0]),
            sf_column_name = "geom"
          )
          proxy %>%
            addPolygons(
              data = sel_sf,
              color = "#0000CC",
              fillColor = "#0000CC",
              weight = 2,
              fillOpacity = 1,
              group = "highlight"
            )
        }
      }
    }
  })

  # Non-HCP modes render aggregated polygons; a clicked map shape is highlighted
  # with a blue overlay drawn on top via leafletProxy (group "shape_highlight").
  observe({
    req(!is_prediction_mode())
    req(!is_hcp_mode())
    req(!is.null(input$map_bounds))
    proxy = leafletProxy("map")
    proxy %>% clearGroup("shape_highlight")
    shp_idx = selected_shape_idx()
    if (!is.null(shp_idx) && length(shp_idx) == 1 && !is.na(shp_idx)) {
      SubsetG = SubsetG_r()
      if (!is.null(SubsetG) && shp_idx >= 1L && shp_idx <= nrow(SubsetG)) {
        proxy %>%
          addPolygons(
            data = SubsetG[shp_idx, , drop = FALSE],
            color = "#0000CC",
            fillColor = "#0000CC",
            weight = 2,
            fillOpacity = 0.5,
            group = "shape_highlight"
          )
      }
    }
  })

  popup_contentA_r = reactive({
    req(!is_prediction_mode())
    req(data_available())
    SubsetG = SubsetG_r()
    if (is.null(SubsetG)) {
      return(list())
    }
    lapply(1:nrow(SubsetG), function(i) {
      fields = c("Country", "Province", "District", "Value")
      values = c(
        SubsetG$Region[i],
        SubsetG$Region1[i],
        SubsetG$Region2[i],
        SubsetG$Metric[i]
      )
      if (length(values) == 0) {
        df = data.frame(Field = character(), Value = character())
      } else {
        df = data.frame(Field = fields, Value = values, stringsAsFactors = FALSE)
        df = df[df$Value != "" & !is.na(df$Value), , drop = FALSE]
      }
      if (nrow(df) > 0) {
        table_html = paste0(
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

  popup_content_r = reactive({
    req(!is_prediction_mode())
    req(data_available())
    if (is_hcp_mode()) {
      SubsetHCP = SubsetHCP_r()
      idx0 = match(SubsetHCP$geo_admin0, adm0_lookup$geo_admin0)
      country_names = adm0_lookup$Region[idx0]
      lapply(seq_len(nrow(SubsetHCP)), function(i) {
        fields = c(
          "Country", "Availability", "Known implementation timeframe",
          "Eligibility", "Implementation", "Application",
          "Compensation", "Diagnostic method", "Uptake",
          "Recruitment site", "Notes", "Source"
        )
        values = c(
          country_names[i],
          # Use as.character() to ensure that factors are converted to strings for display.
          as.character(SubsetHCP$availability_harmonised[i]),
          SubsetHCP$known_implementation_period_harmonised[i],
          as.character(SubsetHCP$eligibility_harmonised[i]),
          as.character(SubsetHCP$implementation_harmonised[i]),
          as.character(SubsetHCP$application_harmonised[i]),
          SubsetHCP$compensation_harmonised[i],
          SubsetHCP$diagnostic_method_harmonised[i],
          SubsetHCP$uptake_harmonised[i],
          SubsetHCP$recruitment_site_harmonised[i],
          SubsetHCP$note_harmonised[i],
          SubsetHCP$citation_link_harmonised[i]
        )
        df = data.frame(Field = fields, Value = values, stringsAsFactors = FALSE)
        df = df[df$Value != "" & !is.na(df$Value) & df$Value != "Unspecified" & df$Value != "Not applicable", ]
        table_html = paste0(
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
      SubsetE = SubsetE_r()
      lapply(1:nrow(SubsetE), function(i) {
        fields = c(
          "Country", "Province", "District", "Value", "Study period", "Risk of bias", "Globin phenotype", "IthaID",
          "Sample size", "Population tested positive", "Cohort", "Nationality", "Ethnicity", "Race", "Religion",
          "Sex", "Age", "Consanguinity", "Diagnostic method", "Recruitment site", "Coordinates", "Notes", "Source"
        )
        values = c(
          SubsetE$Region[i], SubsetE$Region1[i], SubsetE$Region2[i], SubsetE$value[i], SubsetE$timeframe[i],
          SubsetE$bias_flag[i], SubsetE$globin_phenotype[i], SubsetE$ithaID[i], SubsetE$sample_size[i],
          SubsetE$count[i], SubsetE$status_group[i], SubsetE$nationality[i], SubsetE$ethnicity_name[i],
          SubsetE$race[i], SubsetE$religion_name[i], SubsetE$sex[i], SubsetE$age[i], SubsetE$consaguinity[i],
          SubsetE$diagnostic_method[i], SubsetE$recruitment_site[i],
          paste0("(", round(as.numeric(SubsetE$latitude[i]), 4), ", ", round(as.numeric(SubsetE$longitude[i]), 4), ")"),
          SubsetE$note[i], SubsetE$citation_str[i]
        )
        df = data.frame(Field = fields, Value = values, stringsAsFactors = FALSE)
        df = df[df$Value != "" & !is.na(df$Value), ]
        table_html = paste0(
          "<div style='font-family:sans-serif; font-size:0.75em; max-width:600px;'>",
          "<h4 style='margin-bottom:6px;'>Selected study details</h4>",
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

  pal_metric_r = reactive({
    req(!is_prediction_mode())
    req(data_available())
    if (is_hcp_mode()) {
      return(NULL)
    }

    lighten_colour = function(colour, fraction = 0.25) {
      rgba = grDevices::col2rgb(colour, alpha = TRUE) / 255
      lighter_rgb = rgba[1:3, , drop = FALSE] + (1 - rgba[1:3, , drop = FALSE]) * fraction
      grDevices::rgb(
        lighter_rgb[1, 1],
        lighter_rgb[2, 1],
        lighter_rgb[3, 1],
        alpha = rgba[4, 1]
      )
    }

    SubsetG = SubsetG_r()
    viridis_palette = viridis::viridis(81, option = "F", begin = 0, end = 0.7, direction = -1)
    metric_values = SubsetG$Metric
    unique_vals = unique(metric_values)
    if (length(unique_vals) == 1) {
      # Expand domain so the legend can render a continuous scale.
      # Use 0 as lower bound (natural for prevalence/frequency data);
      # fall back to a unit interval when the single value is itself 0.
      lower = if (unique_vals[1] > 0) 0 else -1
      expanded = c(lower, unique_vals[1])
      single_value = as.numeric(unique_vals[1])
      single_pal = colorNumeric(palette = viridis_palette, domain = expanded)
      single_colour = lighten_colour(single_pal(single_value), fraction = 0.25)
      list(
        pal           = function(x) {
          out = rep(single_colour, length(x))
          out[is.na(x)] = NA_character_
          out
        },
        legend_vals   = expanded,
        single_value  = single_value,
        single_colour = single_colour
      )
    } else {
      list(
        pal           = colorNumeric(palette = viridis_palette, domain = metric_values),
        legend_vals   = metric_values,
        single_value  = NULL,
        single_colour = NULL
      )
    }
  })

  # Ported from IthaMaps-shinyapp/app.R lines 513-722: build prediction-mode
  # legends, synchronized map behaviour, and click-based raster interrogation.
  # Popup pane is inside leaflet-map-pane which has a CSS transform, creating
  # its own stacking context. Controls sit at z-index 1000 *outside* that
  # context, so popup pane z-index 700 always loses regardless of its value.
  # Fix: for each map, move its popup pane to be a direct sibling of the
  # controls (child of leaflet-container) and mirror the map pane's transform
  # so popup lat/lng positions remain correct during pan/zoom.
  sync_js = "function(el, x) {if (!window.syncedLeafletMaps) {window.syncedLeafletMaps = {};} var map = this; window.syncedLeafletMaps[el.id] = map; var mapPane = map.getPane('mapPane'); var popupPane = map.getPane('popupPane'); var container = map.getContainer(); if (mapPane && popupPane && popupPane.parentNode !== container) { container.appendChild(popupPane); popupPane.style.zIndex = '1100'; var syncPopupPane = function() { var pos = L.DomUtil.getPosition(mapPane); if (pos) { L.DomUtil.setPosition(popupPane, pos); } }; map.on('move zoom viewreset', syncPopupPane); syncPopupPane(); } function initialiseSync() {var mapIds = ['map_mean', 'map_ci95_2', 'map_burden']; var maps = mapIds.map(function(id) {return window.syncedLeafletMaps[id];}); if (maps.some(function(m) {return !m;})) {setTimeout(initialiseSync, 250); return;} if (window.allMapsSyncReady) {return;} window.allMapsSyncReady = true; if (window.__ithamapsPostHeight) { window.__ithamapsPostHeight(); } var syncing = false; function syncAll(source) {if (syncing) return; syncing = true; maps.forEach(function(target) {if (target !== source) {target.setView(source.getCenter(), source.getZoom(), {animate: false, reset: true});}}); syncing = false;} maps.forEach(function(m) {m.on('moveend zoomend', function() {syncAll(m);});}); maps.forEach(function(m) { var pane = m.getPane('popupPane'); if (!pane) { return; } pane.addEventListener('click', function(e) { var el = e.target; var isClose = false; while (el && el !== pane) { if (el.classList && el.classList.contains('leaflet-popup-close-button')) { isClose = true; break; } el = el.parentNode; } if (!isClose) { return; } if (window.syncedPopupClosing) { return; } window.syncedPopupClosing = true; maps.forEach(function(other) { if (other !== m) { var toRemove = []; other.eachLayer(function(layer) { if (layer instanceof L.Popup) { toRemove.push(layer); } }); toRemove.forEach(function(p) { other.removeLayer(p); }); other.closePopup(); } }); window.syncedPopupClosing = false; if (window.Shiny) { Shiny.setInputValue('prediction_popup_closed', (new Date()).getTime(), {priority: 'event'}); } }, true); }); var syncingLayers = false; var predGroupName = 'Priority monitoring sites'; maps.forEach(function(source) { source.on('overlayadd overlayremove', function(e) { if (syncingLayers || e.name !== predGroupName) return; syncingLayers = true; var adding = (e.type === 'overlayadd'); maps.forEach(function(target) { if (target === source) return; target.getContainer().querySelectorAll('.leaflet-control-layers-overlays label').forEach(function(label) { var span = label.querySelector('span'); if (span && span.textContent.trim() === predGroupName) { var cb = label.querySelector('input[type=checkbox]'); if (cb && cb.checked !== adding) { cb.click(); } } }); }); syncingLayers = false; }); }); maps.forEach(function(source) { source.getContainer().addEventListener('click', function() { maps.forEach(function(m) { m.scrollWheelZoom.disable(); }); source.scrollWheelZoom.enable(); }); }); document.addEventListener('click', function(e) { if (!maps.some(function(m) { return m.getContainer().contains(e.target); })) { maps.forEach(function(m) { m.scrollWheelZoom.disable(); }); } });} initialiseSync();}"

  cluster_hover_js = function(default_fill, default_stroke, show_cluster_count = TRUE) {
    paste(
      "function(el, x) {",
      "  var map = this;",
      "  var showClusterCount = ", tolower(as.character(show_cluster_count)), ";",
      "  if (!document.getElementById('ithamaps-cluster-css')) {",
      "    var s = document.createElement('style'); s.id = 'ithamaps-cluster-css';",
      "    s.textContent = '.ithamaps-cluster-icon{transition:transform 0.15s ease;transform-origin:center;}' +",
      "      '.leaflet-marker-icon:hover .ithamaps-cluster-icon{transform:scale(1.6);}'; document.head.appendChild(s);",
      "  }",
      "  var selectedLayerId = null;",
      "  var markerClusterGroup = null;",
      "",
      "  function getClusterGroup() {",
      "    if (markerClusterGroup) { return markerClusterGroup; }",
      "    map.eachLayer(function(layer) {",
      "      if (!markerClusterGroup && typeof L.MarkerClusterGroup !== 'undefined' && layer instanceof L.MarkerClusterGroup) {",
      "        markerClusterGroup = layer;",
      "      }",
      "    });",
      "    return markerClusterGroup;",
      "  }",
      "",
      "  function setClusterIcon(layer, isSelected) {",
      "    if (!layer || !layer.getChildCount || !layer._icon) { return; }",
      "    var count = layer.getChildCount();",
      "    var color = isSelected ? '#0000CC' : 'black';",
      "    var size = showClusterCount ? (isSelected ? 20 : 14) : (isSelected ? 12 : 6);",
      "    var fsize = isSelected ? '11px' : '9px';",
      "    var countText = showClusterCount ? String(count) : '';",
      "    var icon = L.divIcon({",
      "      html: '<div class=\"ithamaps-cluster-icon\" style=\"background-color:' + color + '; color:white; border-radius:50%; width:' + size + 'px; height:' + size + 'px; display:flex; align-items:center; justify-content:center; font-weight:bold; font-size:' + fsize + ';\">' + countText + '</div>',",
      "      className: '',",
      "      iconSize: new L.Point(size, size)",
      "    });",
      "    layer.setIcon(icon);",
      "  }",
      "",
      "  function clusterContainsSelected(clusterLayer) {",
      "    if (!selectedLayerId || !clusterLayer || !clusterLayer.getAllChildMarkers) { return false; }",
      "    var contains = false;",
      "    try {",
      "      var kids = clusterLayer.getAllChildMarkers();",
      "      for (var i = 0; i < kids.length; i++) {",
      "        var k = kids[i];",
      "        if (k && k.options && String(k.options.layerId) === String(selectedLayerId)) { contains = true; break; }",
      "      }",
      "    } catch (err) { contains = false; }",
      "    return contains;",
      "  }",
      "",
      "  function recolorClusters() {",
      "    var group = getClusterGroup();",
      "    if (!group) { return; }",
      "    try {",
      "      map.eachLayer(function(layer) {",
      "        if (layer && layer.getChildCount && layer._icon) {",
      "          setClusterIcon(layer, clusterContainsSelected(layer));",
      "        }",
      "      });",
      "    } catch (err) {}",
      "  }",
      "",
      "  function styleSelectedMarker(layer) {",
      "    if (!layer) { return; }",
      "    layer.setStyle({radius: 6, weight: 2, color: '#0000CC', fillColor: '#0000CC', fillOpacity: 1});",
      "    if (layer.bringToFront) { try { layer.bringToFront(); } catch (e) {} }",
      "  }",
      "",
      "  function resetMarker(layer) {",
      "    if (!layer) { return; }",
      sprintf("    layer.setStyle({radius: 4, weight: 1, color: '%s', fillColor: '%s', fillOpacity: 1});", default_stroke, default_fill),
      "  }",
      "",
      "  function isSelectedMarker(layer) {",
      "    return !!(selectedLayerId && layer && layer.options && String(layer.options.layerId) === String(selectedLayerId));",
      "  }",
      "",
      "  function recolorMarkers() {",
      "    try {",
      "      map.eachLayer(function(layer) {",
      "        if (layer instanceof L.CircleMarker && !layer.getChildCount && layer.options) {",
      "          if (isSelectedMarker(layer)) {",
      "            styleSelectedMarker(layer);",
      "          } else {",
      "            resetMarker(layer);",
      "          }",
      "        }",
      "      });",
      "      var group = getClusterGroup();",
      "      if (group && group.eachLayer) {",
      "        group.eachLayer(function(layer) {",
      "          if (layer instanceof L.CircleMarker && !layer.getChildCount && layer.options) {",
      "            if (isSelectedMarker(layer)) {",
      "              styleSelectedMarker(layer);",
      "            } else {",
      "              resetMarker(layer);",
      "            }",
      "          }",
      "        });",
      "      }",
      "    } catch (err) {}",
      "  }",
      "",
      "  map.on('layeradd', function(e) {",
      "    var layer = e.layer;",
      "    if (layer && layer.getChildCount && layer._icon) {",
      "      setClusterIcon(layer, clusterContainsSelected(layer));",
      "    }",
      "  });",
      "",
      "  map.on('layeradd', function(e) {",
      "    var layer = e.layer;",
      "    if (layer instanceof L.CircleMarker && !layer.getChildCount) {",
      "      if (isSelectedMarker(layer)) {",
      "        styleSelectedMarker(layer);",
      "      }",
      "      layer.on('mouseover', function() {",
      "        if (isSelectedMarker(this)) { return; }",
      "        this.setStyle({radius: 6, weight: 2, color: '#0000CC', fillColor: '#0000CC', fillOpacity: 0.5});",
      "        this.bringToFront();",
      "      });",
      "      layer.on('mouseout', function() {",
      "        if (isSelectedMarker(this)) { styleSelectedMarker(this); return; }",
      sprintf("        this.setStyle({radius: 4, weight: 1, color: '%s', fillColor: '%s', fillOpacity: 1});", default_stroke, default_fill),
      "      });",
      "    }",
      "  });",
      "",
      "  window.__ithamapsSelectLayerAppliers = window.__ithamapsSelectLayerAppliers || {};",
      "  window.__ithamapsSelectLayerAppliers[el.id] = function(layerId) {",
      "    selectedLayerId = (layerId === null || layerId === undefined || layerId === '') ? null : String(layerId);",
      "    recolorClusters();",
      "    recolorMarkers();",
      "  };",
      "",
      "  if (!window.__ithamapsSelectLayerHandlerRegistered && typeof Shiny !== 'undefined' && Shiny.addCustomMessageHandler) {",
      "    window.__ithamapsSelectLayerHandlerRegistered = true;",
      "    Shiny.addCustomMessageHandler('ithamaps-select-layer', function(msg) {",
      "      if (!msg || !msg.mapId) { return; }",
      "      var applier = window.__ithamapsSelectLayerAppliers && window.__ithamapsSelectLayerAppliers[msg.mapId];",
      "      if (typeof applier === 'function') {",
      "        applier(msg.layerId);",
      "      }",
      "    });",
      "  }",
      "}",
      sep = "\n"
    )
  }

  # Runs in the browser after the leaflet map is actually drawn: hides the
  # progress bar and reports the browser-observed total (server compute +
  # serialization + websocket transfer + client render) back to the server so
  # the timing panel can show the real wall-clock the user waits for.
  scroll_zoom_js = "function(el, x) {
    var map = this;
    el.addEventListener('click', function() { map.scrollWheelZoom.enable(); });
    document.addEventListener('click', function(e) {
      if (!el.contains(e.target)) { map.scrollWheelZoom.disable(); }
    });
  }"

  map_ready_js = "function(el, x) {
    try {
      var loader = document.getElementById('ithamaps_map_loader');
      if (loader) { loader.classList.remove('is-loading'); }
      if (window.__ithamapsMapRecalcStart) {
        var now = (window.performance && performance.now) ? performance.now() : Date.now();
        var ms = Math.round(now - window.__ithamapsMapRecalcStart);
        window.__ithamapsMapRecalcStart = null;
        if (typeof Shiny !== 'undefined' && Shiny.setInputValue) {
          Shiny.setInputValue('map_client_total_ms', ms, {priority: 'event'});
        }
      }
    } catch (e) {}
  }"
  # Shared prediction-map options prevent extreme zoom-out tile requests that can
  # render broken-image placeholders near the map edge while keeping sync behaviour.
  prediction_leaflet_options = leafletOptions(
    worldCopyJump = FALSE,
    minZoom = 1,
    scrollWheelZoom = FALSE,
    zoomControl = TRUE
  )

  default_leaflet_options = leafletOptions(
    scrollWheelZoom = FALSE,
    zoomControl = TRUE,
    zoomSnap = 0.5,
    zoomDelta = 0.5
  )

  map_fill_opacity = 0.8

  prediction_legend_bar = function(palette_values, title, min_value, max_value) {
    legend_colours = vapply(
      palette_values,
      function(colour) grDevices::adjustcolor(colour, alpha.f = map_fill_opacity),
      character(1)
    )
    gradient = paste0(legend_colours, collapse = ", ")
    HTML(paste0(
      "<div class='raster-legend'><div class='raster-legend-title'>", title, "</div>",
      "<div class='raster-legend-bar' style='background: linear-gradient(to right, ", gradient, ");'></div>",
      "<div class='raster-legend-labels'><span>", round(min_value, 2), "</span><span>", round(max_value, 2), "</span></div></div>"
    ))
  }

  lookup_prediction_admin = function(id, lookup_table, id_col, name_col) {
    if (is.null(id) || is.na(id)) {
      return("No data")
    }
    matched_name = lookup_table[[name_col]][match(id, lookup_table[[id_col]])]
    if (length(matched_name) == 0 || is.na(matched_name)) {
      return("No data")
    }
    matched_name
  }

  extract_prediction_values = function(lng, lat) {
    assets = prediction_data_r()
    req(!is.null(assets))

    point_sf = sf::st_as_sf(
      data.frame(Longitude = lng, Latitude = lat),
      coords = c("Longitude", "Latitude"),
      crs = 4326,
      remove = FALSE
    )
    point_for_raster = point_sf %>% sf::st_transform(raster::crs(assets$Mean))
    point_sp = as(point_for_raster, "Spatial")

    clicked_mean = raster::extract(assets$Mean_admin, point_sp)
    clicked_ci95 = raster::extract(assets$CI95, point_sp)
    clicked_burden = raster::extract(assets$Burden, point_sp)
    clicked_admin = clicked_mean[, c("geo_admin0_raster", "geo_admin1_raster", "geo_admin2_raster"), drop = FALSE]

    geo_admin0_raster = clicked_admin[, "geo_admin0_raster"]
    geo_admin1_raster = clicked_admin[, "geo_admin1_raster"]
    geo_admin2_raster = clicked_admin[, "geo_admin2_raster"]

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

  update_selected_prediction_point = function(click, from_priority_site = FALSE) {
    req(click$lng, click$lat)
    selected_point_color = if (from_priority_site) "#0000CC" else "#FFD400"

    values = extract_prediction_values(click$lng, click$lat)
    selected_prediction_point(values)

    mean_value   = ifelse(is.null(values) || is.na(values$Mean),   "No data", round(values$Mean,   4))
    ci95_value   = ifelse(is.null(values) || is.na(values$CI95),   "No data", round(values$CI95,   4))
    burden_value = ifelse(is.null(values) || is.na(values$Burden), "No data", round(values$Burden, 4))

    point_popup = if (!is.null(values)) {
      paste0(
        "<table style='border-collapse:collapse; font-size:0.82rem; min-width:220px;'>",
        "<tr><td style='padding:2px 6px; font-weight:600;'>Longitude</td><td style='padding:2px 6px;'>",   round(values$Longitude, 5), "</td></tr>",
        "<tr><td style='padding:2px 6px; font-weight:600;'>Latitude</td><td style='padding:2px 6px;'>",    round(values$Latitude,  5), "</td></tr>",
        "<tr><td style='padding:2px 6px; font-weight:600;'>ADM0</td><td style='padding:2px 6px;'>",        values$ADM0,                "</td></tr>",
        "<tr><td style='padding:2px 6px; font-weight:600;'>ADM1</td><td style='padding:2px 6px;'>",        values$ADM1,                "</td></tr>",
        "<tr><td style='padding:2px 6px; font-weight:600;'>ADM2</td><td style='padding:2px 6px;'>",        values$ADM2,                "</td></tr>",
        "<tr><td style='padding:2px 6px; font-weight:600;'>Mean prevalence</td><td style='padding:2px 6px;'>",   mean_value,   "</td></tr>",
        "<tr><td style='padding:2px 6px; font-weight:600;'>Uncertainty (95% CI)</td><td style='padding:2px 6px;'>", ci95_value,   "</td></tr>",
        "<tr><td style='padding:2px 6px; font-weight:600;'>Est. carriers</td><td style='padding:2px 6px;'>",     burden_value, "</td></tr>",
        "</table>"
      )
    } else {
      NULL
    }

    invisible(lapply(c("map_mean", "map_ci95_2", "map_burden"), function(map_id) {
      leafletProxy(map_id) %>%
        clearGroup("selected_point") %>%
        clearPopups() %>%
        addCircleMarkers(
          lng = click$lng,
          lat = click$lat,
          radius = 7,
          color = selected_point_color,
          fillColor = selected_point_color,
          fillOpacity = 1,
          weight = 2,
          group = "selected_point"
        ) %>%
        addPopups(
          lng = click$lng,
          lat = click$lat,
          popup = point_popup
        )
    }))
  }

  current_prediction_extent = reactive({
    assets = prediction_data_r()
    req(!is.null(assets))
    full_ext = raster::extent(assets$Mean)
    bounds   = input$map_mean_bounds
    if (is.null(bounds)) return(full_ext)
    user_ext  = raster::extent(bounds$west, bounds$east, bounds$south, bounds$north)
    lon_full  = full_ext@xmax - full_ext@xmin
    lat_full  = full_ext@ymax - full_ext@ymin
    # Use viewport bounds only when the user has genuinely zoomed into a sub-region
    # (both dimensions cover < 60 % of the full raster extent). On large screens the
    # 3-column layout produces a narrow viewport without any zoom interaction; at the
    # initial zoom level the longitude range is typically > 60 % of the full extent,
    # so exports will default to the full raster and not the layout-constrained view.
    if (lon_full > 0 && lat_full > 0 &&
        (user_ext@xmax - user_ext@xmin) / lon_full < 0.6 &&
        (user_ext@ymax - user_ext@ymin) / lat_full < 0.6) {
      return(user_ext)
    }
    full_ext
  })

  prediction_disclaimer_html = reactive({
    req(is_prediction_mode())
    query_info = query_bundle()$query_info %||% list()

    requested_resolution = as.character(query_info$Resolution %||% "Not provided")
    requested_measure = as.character(query_info$Measure %||% "Not provided")
    requested_cause = as.character(query_info$Cause %||% "Not provided")

    HTML(paste0(
      "<div style='margin-bottom: 8px; padding: 8px; border: 1px solid #e3b341; background: #fff8e1; border-radius: 6px;'>",
      "<strong>Disclaimer:</strong> Prediction rasters are currently generated at global level only, ",
      "for high-quality carrier prevalence data in Beta Thalassaemia. ",
      "Additional parameter combinations may become available as new validated datasets are incorporated.",
      "<br><span style='font-size: 0.82rem;'><strong>Current prediction query:</strong> ",
      "Resolution = ", requested_resolution,
      "; Measure = ", requested_measure,
      "; Cause = ", requested_cause,
      ".</span>",
      "</div>"
    ))
  })

  output$mean_legend = renderUI({
    req(is_prediction_mode())
    assets = prediction_data_r()
    prediction_legend_bar(rev(assets$Mean_colours), "Predicted mean carrier prevalence (%)", assets$Mean_min, assets$Mean_max)
  })

  output$burden_legend = renderUI({
    req(is_prediction_mode())
    assets = prediction_data_r()
    prediction_legend_bar(rev(assets$Burden_colours), "Estimated number of carriers", assets$Burden_min, assets$Burden_max)
  })

  output$ci95_legend_2 = renderUI({
    req(is_prediction_mode())
    assets = prediction_data_r()
    prediction_legend_bar(rev(assets$CI95_colours), "Prediction uncertainty (95% Credible Interval)", assets$CI95_min, assets$CI95_max)
  })

  output$selected_prediction_values = renderUI({
    req(is_prediction_mode())
    disclaimer = prediction_disclaimer_html()
    values = selected_prediction_point()
    if (is.null(values)) {
      return(tagList(
        disclaimer,
        HTML("<strong>Tip:</strong> Click any map to display the mean predicted carrier prevalence, prediction uncertainty, and estimated number of carriers at the selected location.")
      ))
    }
    tagList(
      disclaimer,
    )
  })

  output$map_mean = renderLeaflet({
    req(is_prediction_mode())
    assets = prediction_data_r()
    leaflet(options = prediction_leaflet_options) %>%
      addProviderTiles("CartoDB.Positron", options = providerTileOptions(noWrap = TRUE)) %>%
      addScaleBar(position = "bottomleft") %>%
      setView(lng = 80, lat = 30, zoom = 4) %>%
      addRasterImage(assets$Mean, colors = assets$Mean_palette, opacity = 0.8, project = TRUE) %>%
      addCircleMarkers(
        data = assets$Selected_sites,
        lng = ~lon, lat = ~lat,
        radius = 5, color = "black", fillColor = "black",
        fillOpacity = 0.9, weight = 1.5,
        group = "Priority monitoring sites"
      ) %>%
      addLayersControl(overlayGroups = c("Priority monitoring sites"), options = layersControlOptions(collapsed = FALSE)) %>%
      #  pseudoFullscreen = TRUE — this expands the map to fill the viewport using CSS (position fixed, 100% width/height) instead of calling the native API, so it works inside iframes with no policy issues
      addFullscreenControl(position = "topleft", pseudoFullscreen = TRUE) %>%
      add_logo_leaflet_control() %>%
      htmlwidgets::onRender("function(el, x) {
        var map = this;
        map.on('overlayadd', function(e) {
          if (e.name === 'Priority monitoring sites' && window.Shiny)
            Shiny.setInputValue('prediction_sites_visible', true, {priority: 'event'});
        });
        map.on('overlayremove', function(e) {
          if (e.name === 'Priority monitoring sites' && window.Shiny)
            Shiny.setInputValue('prediction_sites_visible', false, {priority: 'event'});
        });
      }") %>%
      htmlwidgets::onRender(sync_js)
  })

  output$map_burden = renderLeaflet({
    req(is_prediction_mode())
    assets = prediction_data_r()
    leaflet(options = prediction_leaflet_options) %>%
      addProviderTiles("CartoDB.Positron", options = providerTileOptions(noWrap = TRUE)) %>%
      addScaleBar(position = "bottomleft") %>%
      setView(lng = 80, lat = 30, zoom = 4) %>%
      addRasterImage(assets$Burden, colors = assets$Burden_palette, opacity = 0.8, project = TRUE) %>%
      addCircleMarkers(
        data = assets$Selected_sites,
        lng = ~lon, lat = ~lat,
        radius = 5, color = "black", fillColor = "black",
        fillOpacity = 0.9, weight = 1.5,
        group = "Priority monitoring sites"
      ) %>%
      addLayersControl(overlayGroups = c("Priority monitoring sites"), options = layersControlOptions(collapsed = FALSE)) %>%
      #  pseudoFullscreen = TRUE — this expands the map to fill the viewport using CSS (position fixed, 100% width/height) instead of calling the native API, so it works inside iframes with no policy issues
      addFullscreenControl(position = "topleft", pseudoFullscreen = TRUE) %>%
      add_logo_leaflet_control() %>%
      htmlwidgets::onRender(sync_js)
  })

  output$map_ci95_2 = renderLeaflet({
    req(is_prediction_mode())
    assets = prediction_data_r()
    # options worldCopyJump = FALSE prevents the map from creating a duplicate set of tiles when the user pans across the antimeridian, which would cause confusion when interpreting the raster and clicking to interrogate values.
    leaflet(options = prediction_leaflet_options) %>%
      # options no_wrap stops conntinuous raster images from wrapping around the globe
      addProviderTiles("CartoDB.Positron", options = providerTileOptions(noWrap = TRUE)) %>%
      addScaleBar(position = "bottomleft") %>%
      setView(lng = 80, lat = 30, zoom = 4) %>%
      addRasterImage(assets$CI95, colors = assets$CI95_palette, opacity = 0.8, project = TRUE) %>%
      addCircleMarkers(
        data = assets$Selected_sites,
        lng = ~lon,
        lat = ~lat,
        radius = 5,
        color = "black",
        fillColor = "black",
        fillOpacity = 0.9,
        weight = 1.5,
        group = "Priority monitoring sites"
      ) %>%
      addLayersControl(overlayGroups = c("Priority monitoring sites"), options = layersControlOptions(collapsed = FALSE)) %>%
      #  pseudoFullscreen = TRUE — this expands the map to fill the viewport using CSS (position fixed, 100% width/height) instead of calling the native API, so it works inside iframes with no policy issues
      addFullscreenControl(position = "topleft", pseudoFullscreen = TRUE) %>%
      add_logo_leaflet_control() %>%
      htmlwidgets::onRender(sync_js)
  })

  # Reduce marker radius by 1 on wide screens where the 3-column layout makes each map narrow.
  observe({
    req(is_prediction_mode())
    assets = prediction_data_r()
    req(!is.null(assets))
    map_px = session$clientData$output_map_mean_width
    radius = if (!is.null(map_px) && map_px < 550) 2.5 else 3
    for (map_id in c("map_mean", "map_burden", "map_ci95_2")) {
      leafletProxy(map_id) %>%
        clearGroup("Priority monitoring sites") %>%
        addCircleMarkers(
          data = assets$Selected_sites,
          lng = ~lon, lat = ~lat,
          radius = radius, color = "black", fillColor = "black",
          fillOpacity = 0.9, weight = 1.5,
          group = "Priority monitoring sites"
        )
    }
  })

  last_prediction_marker_click = reactiveVal(NULL)
  is_recent_same_prediction_marker_click = function(click, window_secs = 1, tol = 1e-7) {
    marker_click = last_prediction_marker_click()
    if (is.null(marker_click) || is.null(click$lng) || is.null(click$lat)) {
      return(FALSE)
    }
    is_recent = as.numeric(difftime(Sys.time(), marker_click$ts, units = "secs")) <= window_secs
    same_lng = abs(click$lng - marker_click$lng) <= tol
    same_lat = abs(click$lat - marker_click$lat) <= tol
    is_recent && same_lng && same_lat
  }

  handle_prediction_marker_click = function(click) {
    req(click$lng, click$lat)
    last_prediction_marker_click(list(lng = click$lng, lat = click$lat, ts = Sys.time()))
    update_selected_prediction_point(click, from_priority_site = TRUE)
  }

  observeEvent(input$map_mean_click, {
    req(is_prediction_mode())
    if (is_recent_same_prediction_marker_click(input$map_mean_click)) {
      return()
    }
    update_selected_prediction_point(input$map_mean_click, from_priority_site = FALSE)
  })
  observeEvent(input$map_mean_marker_click, {
    req(is_prediction_mode())
    handle_prediction_marker_click(input$map_mean_marker_click)
  })
  observeEvent(input$map_ci95_2_click, {
    req(is_prediction_mode())
    if (is_recent_same_prediction_marker_click(input$map_ci95_2_click)) {
      return()
    }
    update_selected_prediction_point(input$map_ci95_2_click, from_priority_site = FALSE)
  })
  observeEvent(input$map_ci95_2_marker_click, {
    req(is_prediction_mode())
    handle_prediction_marker_click(input$map_ci95_2_marker_click)
  })
  observeEvent(input$map_burden_click, {
    req(is_prediction_mode())
    if (is_recent_same_prediction_marker_click(input$map_burden_click)) {
      return()
    }
    update_selected_prediction_point(input$map_burden_click, from_priority_site = FALSE)
  })
  observeEvent(input$map_burden_marker_click, {
    req(is_prediction_mode())
    handle_prediction_marker_click(input$map_burden_marker_click)
  })

  observeEvent(input$prediction_popup_closed, {
    req(is_prediction_mode())
    selected_prediction_point(NULL)
    invisible(lapply(c("map_mean", "map_ci95_2", "map_burden"), function(map_id) {
      leafletProxy(map_id) %>% clearGroup("selected_point")
    }))
  })

  output$map = renderLeaflet({
    req(!is_prediction_mode())
    req(data_available())
    render_start = proc.time()[["elapsed"]]

    if (is_hcp_mode()) {
      data = map_filtered_data() %>%
        mutate(marker_layer_id = paste0("row_", dplyr::row_number()))
      simplify_start = proc.time()[["elapsed"]]
      idx0 = match(data$geo_admin0, adm0_sel_disp$geo_admin0)
      data = data %>%
        mutate(
          Country = adm0_lookup$Region[idx0],
          geom = st_geometry(adm0_sel_disp)[idx0]
        )
      hcp_sf = st_as_sf(data, sf_column_name = "geom")
      hcp_sf = hcp_sf[!is.na(idx0), , drop = FALSE]
      perf_state$map_simplify_secs = round(proc.time()[["elapsed"]] - simplify_start, 3)
      hcp_sf = hcp_sf %>%
        mutate(
          hover_label = paste0(
            "<strong>", Country, "</strong><br>Healthcare availability: ", availability_harmonised
          )
        )

      availability_levels = levels(hcp_sf$availability_harmonised)
      if (is.null(availability_levels) || length(availability_levels) == 0) {
        availability_levels = unique(as.character(hcp_sf$availability_harmonised))
      }
      availability_levels = availability_levels[!is.na(availability_levels) & nzchar(availability_levels)]
      hcp_palette = viridis::viridis(3, option = "F", begin = 0, end = 0.7, direction = -1)
      pal_hcp = colorFactor(palette = hcp_palette, domain = availability_levels, na.color = "#cccccc")

      map_widget = leaflet(options = default_leaflet_options) %>%
        addProviderTiles("CartoDB.Positron") %>%
        addScaleBar(position = "bottomleft") %>%
        addPolygons(
          data = hcp_sf,
          layerId = ~ marker_layer_id,
          weight = 0.3,
          opacity = 1,
          color = "black",
          fillOpacity = map_fill_opacity,
          smoothFactor = 0.5,
          highlightOptions = highlightOptions(
            weight = 1.4,
            color = "#0000CC",
            fillOpacity = 0.5,
            fillColor = "#0000CC",
            bringToFront = FALSE
          ),
          label = ~ lapply(hover_label, HTML),
          labelOptions = labelOptions(
            direction = "auto",
            textsize = "12px",
            style = list("padding" = "4px 6px")
          ),
          fillColor = ~ pal_hcp(availability_harmonised)
        ) %>%
        addLegend(
          pal = pal_hcp,
          values = availability_levels,
          title = "Healthcare availability",
          opacity = map_fill_opacity,
          position = "bottomright"
        ) %>%
        addFullscreenControl(position = "topleft", pseudoFullscreen = TRUE) %>%
        add_logo_leaflet_control() %>%
        htmlwidgets::onRender(scroll_zoom_js) %>%
        htmlwidgets::onRender(map_ready_js)

      # Centroids stay within the selected region; full polygon bbox spans overseas territories.
      cents = tryCatch(sf::st_coordinates(sf::st_centroid(suppressWarnings(sf::st_geometry(hcp_sf)))), error = function(e) NULL)
      if (!is.null(cents) && nrow(cents) > 0) {
        lng_pad = max(0.5, (max(cents[, 1]) - min(cents[, 1])) * 0.2)
        lat_pad = max(0.5, (max(cents[, 2]) - min(cents[, 2])) * 0.2)
        map_widget = map_widget %>%
          fitBounds(
            lng1 = max(-180, min(cents[, 1]) - lng_pad),
            lat1 = max(-90,  min(cents[, 2]) - lat_pad),
            lng2 = min(180,  max(cents[, 1]) + lng_pad),
            lat2 = min(90,   max(cents[, 2]) + lat_pad)
          )
      }

      perf_state$map_render_secs = round(proc.time()[["elapsed"]] - render_start, 3)
      return(map_widget)
    }

    SubsetG = SubsetG_r()
    MetricN = MetricN_r()
    legend_title = metric_title_with_unit(MetricN, MetricUnit_r())
    pal_metric_obj = pal_metric_r()
    pal_metric = pal_metric_obj$pal
    legend_vals = pal_metric_obj$legend_vals
    single_value = pal_metric_obj$single_value
    single_colour = pal_metric_obj$single_colour
    data = map_filtered_data() %>%
      mutate(marker_layer_id = paste0("row_", dplyr::row_number()))

    SubsetG = SubsetG %>%
      mutate(
        shape_layer_id = paste0("shape_", dplyr::row_number()),
        hover_region = dplyr::case_when(
          !is.na(Region2) & Region2 != "" & Region2 != "Not applicable" ~ Region2,
          !is.na(Region1) & Region1 != "" & Region1 != "Not applicable" ~ Region1,
          TRUE ~ Region
        ),
        hover_metric = ifelse(
          is.na(Metric),
          "No data",
          format(round(as.numeric(Metric), 2), nsmall = 2, trim = TRUE)
        ),
        hover_label = paste0(
          "<strong>", hover_region, "</strong><br>",
          MetricN, ": ", hover_metric
        )
      )

    simplify_start = proc.time()[["elapsed"]]
    SubsetG_display = attach_display_geometry(SubsetG)
    perf_state$map_simplify_secs = round(proc.time()[["elapsed"]] - simplify_start, 3)

    map_widget = leaflet(data, options = default_leaflet_options) %>%
      addProviderTiles("CartoDB.Positron") %>%
      addScaleBar(position = "bottomleft") %>%
      addCircleMarkers(
        data = data,
        lat = ~ as.numeric(latitude),
        lng = ~ as.numeric(longitude),
        layerId = ~ marker_layer_id,
        group = "Studies",
        stroke = TRUE,
        color = "white",
        weight = 1,
        fillColor = "black",
        fillOpacity = 1,
        radius = 3,
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
        data = SubsetG_display,
        layerId = ~ shape_layer_id,
        weight = 0.3,
        opacity = 1,
        color = "black",
        fillOpacity = map_fill_opacity,
        smoothFactor = 0.5,
        highlightOptions = highlightOptions(
          weight = 1.4,
          color = "#0000CC",
          fillOpacity = 0.5,
          fillColor = "#0000CC",
          bringToFront = FALSE
        ),
        label = ~ lapply(hover_label, HTML),
        labelOptions = labelOptions(
          direction = "auto",
          textsize = "12px",
          style = list("padding" = "4px 6px")
        ),
        fillColor = ~ pal_metric(Metric)
      )

    if (!is.null(single_value) && !is.null(single_colour)) {
      map_widget = map_widget %>%
        addLegend(
          colors = single_colour,
          labels = format(round(single_value, 2), nsmall = 2, trim = TRUE),
          title = legend_title,
          opacity = map_fill_opacity,
          position = "bottomright"
        )
    } else {
      map_widget = map_widget %>%
        addLegend(
          pal = pal_metric,
          values = legend_vals,
          title = legend_title,
          opacity = map_fill_opacity,
          position = "bottomright"
        )
    }

    map_widget = map_widget %>%
      addLayersControl(
        overlayGroups = "Studies",
        options = layersControlOptions(collapsed = FALSE)
      ) %>%
      addFullscreenControl(position = "topleft", pseudoFullscreen = TRUE) %>%
      add_logo_leaflet_control() %>%
      htmlwidgets::onRender(scroll_zoom_js) %>%
      htmlwidgets::onRender(cluster_hover_js("black", "white",
        show_cluster_count = identical(query_bundle()$query_info$Resolution %||% "", "Country-level"))) %>%
      htmlwidgets::onRender("function(el, x) {
        var map = this;
        map.on('overlayadd', function(e) {
          if (e.name === 'Studies') Shiny.setInputValue(el.id + '_studies_visible', true, {priority: 'event'});
        });
        map.on('overlayremove', function(e) {
          if (e.name === 'Studies') Shiny.setInputValue(el.id + '_studies_visible', false, {priority: 'event'});
        });
      }") %>%
      htmlwidgets::onRender(map_ready_js)

    # Observation coordinates are always within the filtered region; polygon bbox spans overseas territories.
    coords = data %>%
      dplyr::mutate(lat = suppressWarnings(as.numeric(latitude)),
                    lng = suppressWarnings(as.numeric(longitude))) %>%
      dplyr::filter(!is.na(lat), !is.na(lng))
    if (nrow(coords) > 0) {
      lng_pad = max(0.5, (max(coords$lng) - min(coords$lng)) * 0.2)
      lat_pad = max(0.5, (max(coords$lat) - min(coords$lat)) * 0.2)
      map_widget = map_widget %>%
        fitBounds(
          lng1 = max(-180, min(coords$lng) - lng_pad),
          lat1 = max(-90,  min(coords$lat) - lat_pad),
          lng2 = min(180,  max(coords$lng) + lng_pad),
          lat2 = min(90,   max(coords$lat) + lat_pad)
        )
    }

    perf_state$map_render_secs = round(proc.time()[["elapsed"]] - render_start, 3)
    message(sprintf(
      paste0("[ithamaps] map render | polygons=%d markers=%d geom=%.1fMB ",
             "geometry_prep=%.3fs render_build=%.3fs"),
      nrow(SubsetG_display), nrow(data),
      round(as.numeric(object.size(sf::st_geometry(SubsetG_display))) / 1024^2, 1),
      perf_state$map_simplify_secs, perf_state$map_render_secs
    ))
    map_widget
  })

  output$data_table = renderDT({
    req(!is_prediction_mode())
    req(data_available())
    render_start = proc.time()[["elapsed"]]

    # Single source of truth for columns hidden from view but kept in the data.
    hidden_display_cols = c("Latitude", "Longitude", "Notes", "Source")

    build_filter_meta = function(df, hidden_cols) {
      lapply(seq_along(df), function(i) {
        col_name = names(df)[i]
        col = df[[i]]

        if (col_name %in% hidden_cols) {
          return(list(type = "native", options = character(0)))
        }
        if (is.numeric(col) || is.integer(col)) {
          return(list(type = "native", options = character(0)))
        }
        vals = as.character(col)
        if (identical(col_name, "IthaID")) {
          # IthaID cells are rendered as HTML anchors; dropdown filters should
          # display only the raw ID text values.
          vals = gsub("<[^>]*>", "", vals)
        }
        vals = trimws(vals)
        vals = vals[!is.na(vals) & nzchar(vals)]
        vals = sort(unique(vals))
        if (length(vals) == 0) {
          return(list(type = "native", options = character(0)))
        }
        list(type = "select", options = I(unname(vals)))
      })
    }

    make_dropdown_filter_init = function(filter_meta_json) {
      JS(sprintf(
        "function(settings, json) {
           var api = this.api();
           var filterMeta = %s;
           if (!Array.isArray(filterMeta)) {
             filterMeta = Object.keys(filterMeta || {}).map(function(k) { return filterMeta[k]; });
           }
           var normalizeVals = function(v) {
             if (v === null || v === undefined || v === '') return [];
             return Array.isArray(v) ? v : [v];
           };
           var $container = $(api.table().container());
           var $filterCells = $('thead tr:eq(1) td, thead tr:eq(1) th', $container);
           if ($filterCells.length === 0) {
             $filterCells = $('tfoot td, tfoot th', $container);
           }
           if ($filterCells.length === 0) { return; }
           var colCount = api.columns().count();
           var colOffset = (colCount === (filterMeta.length + 1)) ? 1 : 0;

           api.columns().every(function() {
             var colIdx = this.index();
             var metaIdx = colIdx - colOffset;
             var meta = filterMeta[metaIdx] || { type: 'native', options: [] };
             if (meta.type !== 'select') { return; }

             var column = this;
             var $cell = $filterCells.eq($(this.header()).index());
             if (!$cell.length) { return; }
             var $input = $('input,select', $cell);
             if (!$input.length) { return; }

             var $select = $('<select class=\\\"form-control form-control-sm\\\"></select>');
             $select.append($('<option></option>').attr('value', '__all__').text('All'));
             $.each(meta.options || [], function(_, val) {
               $select.append($('<option></option>').attr('value', val).text(val));
             });
             $cell.empty().append($select);

             var applyFilter = function(val) {
               if (!val || val === '__all__') {
                 column.search('', true, false).draw();
                 return;
               }
               var escaped = $.fn.dataTable.util.escapeRegex(val);
               column.search('^' + escaped + '$', true, false).draw();
             };

             $select.val('__all__');
             $select.on('change', function() {
               applyFilter($(this).val());
             });
           });
         }",
        filter_meta_json
      ))
    }

    if (is_hcp_mode()) {
      SubsetHCP = SubsetHCP_r()
      SubsetHCPRaw = SubsetHCP_raw_r()
      if (is.null(SubsetHCPRaw)) {
        SubsetHCPRaw = SubsetHCP
      }
      idx0 = match(SubsetHCP$geo_admin0, adm0_lookup$geo_admin0)

      build_hcp_entry_details_html = function(country_key) {
        details_rows = SubsetHCPRaw %>%
          filter(geo_admin0 == country_key)

        detail_idx0 = match(details_rows$geo_admin0, adm0_lookup$geo_admin0)
        details_df = SubsetHCPRaw %>%
          filter(geo_admin0 == country_key) %>%
          mutate(Country = adm0_lookup$Region[detail_idx0]) %>%
          dplyr::select(any_of(c(
            "Country", "availability", "timeframe", "eligibility",
            "implementation", "application", "compensation", "diagnostic_method",
            "uptake", "recruitment_site", "note", "citation_str", "source_link"
          ))) %>%
          dplyr::rename(any_of(c(
            "Availability" = "availability",
            "Study period" = "timeframe",
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

        if (nrow(details_df) == 0) {
          return("<div class='px-2 py-1 text-muted'>No individual entries available.</div>")
        }

        details_df = details_df %>%
          mutate(across(everything(), ~ htmltools::htmlEscape(as.character(.x))))

        if ("source_link" %in% names(details_df)) {
          details_df[["Source"]] = ifelse(
            !is.na(details_df[["source_link"]]) & grepl("^https?://", details_df[["source_link"]], ignore.case = TRUE),
            paste0('<a href="', htmltools::htmlEscape(details_df[["source_link"]]), '" target="_blank" rel="noopener noreferrer">', details_df[["Source"]], '</a>'),
            details_df[["Source"]]
          )
          details_df[["source_link"]] = NULL
        }

        header_html = paste0(
          "<tr>",
          paste0("<th>", names(details_df), "</th>", collapse = ""),
          "</tr>"
        )

        rows_html = paste(
          apply(details_df, 1, function(row) {
            paste0(
              "<tr>",
              paste0("<td>", row, "</td>", collapse = ""),
              "</tr>"
            )
          }),
          collapse = ""
        )

        paste0(
          "<div class='px-2 py-2'>",
          "<div style='font-weight:600; margin-bottom:6px;'>Individual entries used in harmonisation</div>",
          "<div style='overflow-x:auto;'>",
          "<table class='table table-sm table-bordered' style='font-size:0.82rem; margin-bottom:0;'>",
          "<thead>", header_html, "</thead>",
          "<tbody>", rows_html, "</tbody>",
          "</table></div></div>"
        )
      }

      details_country_keys = unique(as.character(SubsetHCP$geo_admin0))
      details_by_country = stats::setNames(
        lapply(details_country_keys, build_hcp_entry_details_html),
        details_country_keys
      )
      details_by_country_json = jsonlite::toJSON(details_by_country, auto_unbox = TRUE)

      df = SubsetHCP %>%
        mutate(Country = adm0_lookup$Region[idx0]) %>%
        dplyr::select(any_of(c(
          "geo_admin0", "Country", "availability_harmonised", "known_implementation_period_harmonised",
          "eligibility_harmonised", "implementation_harmonised", "application_harmonised",
          "compensation_harmonised", "diagnostic_method_harmonised", "uptake_harmonised",
          "recruitment_site_harmonised", "note_harmonised", "citation_link_harmonised"
        ))) %>%
        mutate(
          Expand = "<span style='font-weight:700;'>+</span>"
        ) %>%
        dplyr::rename(any_of(c(
          "Availability" = "availability_harmonised",
          "Known implementation timeframe" = "known_implementation_period_harmonised",
          "Eligibility" = "eligibility_harmonised",
          "Implementation" = "implementation_harmonised",
          "Application" = "application_harmonised",
          "Compensation" = "compensation_harmonised",
          "Diagnostic method" = "diagnostic_method_harmonised",
          "Uptake" = "uptake_harmonised",
          "Recruitment site" = "recruitment_site_harmonised",
          "Notes" = "note_harmonised",
          "Source" = "citation_link_harmonised"
        ))) %>%
        dplyr::select(
          Expand,
          Country,
          Availability,
          `Known implementation timeframe`,
          Eligibility,
          Implementation,
          Application,
          Compensation,
          `Diagnostic method`,
          Uptake,
          `Recruitment site`,
          Notes,
          Source,
          geo_admin0
        )

      key_col_idx = which(names(df) == "geo_admin0") - 1L

      table_widget = datatable(df,
        selection = "multiple",
        filter = "top",
        escape = FALSE,
        rownames = FALSE,
        options = list(
          pageLength = 10,
          lengthChange = FALSE,
          scrollX = FALSE,
          initComplete = JS(sprintf(
            "function(settings, json) {
               var api = this.api();
               var detailsByCountry = %s;
               var keyCol = %d;
               // BUG (verified via live DOM inspection, DataTables 1.13.6):
               // row.child() inserts one wrapper <tr><td colspan=N>...</td></tr> into the
               // SAME <tbody> as normal rows, with NO distinguishing class (no 'child' class
               // exists on it). DT's own row-selection handler is a jQuery delegated listener
               // `table.on('mousedown.dt', 'tbody tr', ...)`, bound in the bubble phase on the
               // <table> element. Because jQuery delegation matches ANY 'tr' descendant of ANY
               // 'tbody' (not scoped by nesting depth), a mousedown anywhere inside our nested
               // details table also resolves to that wrapper <tr> and gets 'selected' toggled
               // onto it. Since the wrapper's only child is one large <td colspan>, the
               // DataTables CSS rule `tr.selected > *` sets color:white on it — which then
               // CASCADES (color inherits) into every nested cell, while the inset box-shadow
               // background does NOT cascade, leaving white text on a non-blue background
               // (invisible text), and a blue box only where the wrapper td shows through.
               //
               // Fix: use the public row() API (not class names, which differ across DT
               // versions) to detect real vs. injected rows — api.row(tr).length is 1 for a
               // genuine DataTables row and 0 for our injected wrapper/nested rows. Intercept
               // 'mousedown' (the event DT actually listens for) in the CAPTURE phase directly
               // on the <table> node, which is guaranteed to run before DT's bubble-phase
               // handler on that same node. stopImmediatePropagation() also blocks the
               // browser's default action for the click target, so for <a href> clicks inside
               // child rows we must re-trigger navigation manually via window.open().
               // Verified with a live Playwright session against this exact table: normal row
               // selection is unaffected, links open exactly once, and no 'selected' class or
               // white-text regression occurs on child-row content.
               var tableNode = api.table().node();
               function blockInjectedRow(e) {
                 var tr = e.target.closest('tr');
                 if (!tr || api.row(tr).length !== 0) { return; }
                 e.stopImmediatePropagation();
                 if (e.type === 'click') {
                   var link = e.target.closest('a[href]');
                   if (link) { window.open(link.href, link.target || '_blank'); }
                 }
               }
               tableNode.addEventListener('mousedown', blockInjectedRow, true);
               tableNode.addEventListener('click',     blockInjectedRow, true);
               // Expand / collapse child rows on dt-control cell click.
               api.table().container().addEventListener('click', function(evt) {
                 var cell = evt.target.closest('td.dt-control');
                 if (!cell) { return; }
                 var tr = cell.closest('tr');
                 var row = api.row(tr);
                 if (!row || !row.length) { return; }
                 if (row.child.isShown()) {
                   row.child.hide();
                   tr.classList.remove('shown');
                   cell.innerHTML = '<span style=font-weight:700;>+</span>';
                 } else {
                   var rowData = row.data();
                   var key = String(rowData[keyCol]);
                   var childHtml = detailsByCountry[key] || '<div class=\"px-2 py-1 text-muted\">No individual entries available.</div>';
                   row.child(childHtml).show();
                   tr.classList.add('shown');
                   cell.innerHTML = '<span style=font-weight:700;>-</span>';
                 }
               });
             }",
            details_by_country_json,
            key_col_idx
          )),
          rowCallback = JS("function(row, data) {", "$(row).css('min-height', '30px');", "}"),
          columnDefs = list(
            list(orderable = FALSE, className = "dt-control", targets = 0),
            list(visible = FALSE, targets = key_col_idx)
          )
        ),
        class = "stripe hover cell-border"
      )
      perf_state$table_render_secs = round(proc.time()[["elapsed"]] - render_start, 3)
      return(table_widget)
    }

    SubsetE = SubsetE_r()
    df = SubsetE %>%
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

    requested_measure = as.character((query_bundle()$query_info %||% list())$Measure %||% "")
    if (!identical(requested_measure, "Allele frequency")) {
      df = df %>% dplyr::select(-any_of("IthaID"))
    }
    if (!identical(requested_measure, "Relative allele frequency")) {
      df = df %>% dplyr::select(-any_of("Globin phenotype"))
    }

    if ("IthaID" %in% names(df)) {
      ithanet_root = infer_ithanet_root()
      df = df %>%
        mutate(IthaID = vapply(IthaID, function(x) build_ithaid_link(x, ithanet_root), character(1)))
    }

    table_widget = datatable(df,
      selection = "multiple",
      filter = "top",
      escape = setdiff(names(df), "IthaID"),
      options = list(
        pageLength = 10,
        lengthChange = FALSE,
        scrollX = FALSE,
        initComplete = make_dropdown_filter_init(jsonlite::toJSON(unname(build_filter_meta(df, hidden_display_cols)), auto_unbox = TRUE)),
        rowCallback = JS("function(row, data) {", "$(row).css('min-height', '30px');", "}"),
        # rownames=TRUE (default): row-names occupy DT index 0, so R's which() 1-based = DT column index.
        columnDefs = list(list(visible = FALSE, targets = which(names(df) %in% hidden_display_cols)))
      ),
      class = "stripe hover cell-border"
    )
    perf_state$table_render_secs = round(proc.time()[["elapsed"]] - render_start, 3)
    table_widget
  })

  png_export_cache = reactiveVal(list(key = NULL, payload = NULL))

  curated_png_payload_r = reactive({
    req(!is_prediction_mode())
    req(!is_hcp_mode())
    req(data_available())

    SubsetG = SubsetG_r()
    req(!is.null(SubsetG), nrow(SubsetG) > 0)

    query_info = query_bundle()$query_info %||% list()
    agg_level = as.character(query_info$Aggregation %||% "Country-level")
    bounds = input$map_bounds
    bounds_key = if (is.null(bounds)) "no-bounds" else paste(bounds$west, bounds$east, bounds$south, bounds$north, sep = "|")
    cache_key = paste(
      normalize_query_string(active_url_search_r()),
      agg_level,
      bounds_key,
      nrow(SubsetG),
      sep = "::"
    )

    prep_start = proc.time()[["elapsed"]]
    cached = png_export_cache()
    if (!is.null(cached$key) && identical(cached$key, cache_key) && !is.null(cached$payload)) {
      perf_state$png_prep_cache_hit = "yes"
      perf_state$png_prep_secs = round(proc.time()[["elapsed"]] - prep_start, 3)
      perf_state$png_prep_workers = 0L
      log_trace("png_prep", paste0("cache_hit=yes prep_secs=", perf_state$png_prep_secs))
      return(cached$payload)
    }

    pts = filtered_data() %>%
      mutate(
        longitude = suppressWarnings(as.numeric(longitude)),
        latitude = suppressWarnings(as.numeric(latitude))
      ) %>%
      filter(!is.na(longitude), !is.na(latitude))

    xlim = if (!is.null(bounds)) c(bounds$west, bounds$east) else NULL
    ylim = if (!is.null(bounds)) c(bounds$south, bounds$north) else NULL

    build_export_bbox = function(context_data, data_subset, map_bounds) {
      if (!is.null(map_bounds)) {
        return(sf::st_bbox(c(
          xmin = map_bounds$west,
          xmax = map_bounds$east,
          ymin = map_bounds$south,
          ymax = map_bounds$north
        ), crs = sf::st_crs(context_data)))
      }

      # Use centroids so overseas territories don't inflate the bbox.
      bbox_source = if (!is.null(data_subset) && nrow(data_subset) > 0) data_subset else context_data
      cents = tryCatch(
        sf::st_coordinates(sf::st_centroid(suppressWarnings(sf::st_geometry(bbox_source)))),
        error = function(e) NULL
      )
      bbox = if (!is.null(cents) && nrow(cents) > 0) {
        sf::st_bbox(c(xmin = min(cents[, 1]), xmax = max(cents[, 1]),
                      ymin = min(cents[, 2]), ymax = max(cents[, 2])),
                    crs = sf::st_crs(bbox_source))
      } else {
        sf::st_bbox(bbox_source)
      }
      x_pad = max((bbox$xmax - bbox$xmin) * 0.08, 0.25)
      y_pad = max((bbox$ymax - bbox$ymin) * 0.08, 0.25)
      sf::st_bbox(c(
        xmin = max(bbox$xmin - x_pad, -180),
        xmax = min(bbox$xmax + x_pad,  180),
        ymin = max(bbox$ymin - y_pad,  -90),
        ymax = min(bbox$ymax + y_pad,   90)
      ), crs = sf::st_crs(context_data))
    }

    make_plot_safe_sf = function(sf_data, bbox = NULL) {
      if (is.null(sf_data) || nrow(sf_data) == 0) {
        return(sf_data)
      }

      cropped = if (!is.null(bbox)) {
        suppressWarnings(tryCatch(
          {
            bbox_poly = sf::st_as_sfc(bbox)
            hits = suppressWarnings(sf::st_intersects(sf_data, bbox_poly, sparse = FALSE)[, 1])
            candidate = sf_data[hits, , drop = FALSE]
            if (nrow(candidate) == 0) candidate else sf::st_crop(candidate, bbox)
          },
          error = function(e) sf_data
        ))
      } else {
        sf_data
      }

      if (is.null(cropped) || nrow(cropped) == 0) {
        return(cropped)
      }

      validity = suppressWarnings(sf::st_is_valid(cropped))
      if (all(validity %in% c(TRUE, NA))) {
        safe_sf = cropped
      } else {
        safe_sf = tryCatch(
          sf::st_make_valid(cropped),
          error = function(e) suppressWarnings(sf::st_buffer(cropped, 0))
        )
      }

      safe_sf[!sf::st_is_empty(safe_sf), , drop = FALSE]
    }

    perf_state$png_prep_workers = 1L

    # s2 spherical geometry makes crop/validate/intersects on global lat-long
    # layers extremely slow; use planar GEOS for the export prep only.
    prev_s2 = sf::sf_use_s2()
    suppressMessages(sf::sf_use_s2(FALSE))
    on.exit(suppressMessages(sf::sf_use_s2(prev_s2)), add = TRUE)

    context_polygons = switch(agg_level,
      "Province-level" = adm1_sel %>% mutate(label_name = Region1),
      "District-level" = adm2_sel %>% mutate(label_name = Region2),
      adm0_sel %>% mutate(label_name = Region)
    )
    export_bbox = build_export_bbox(context_polygons, SubsetG, bounds)
    if (is.null(xlim)) xlim = c(as.numeric(export_bbox["xmin"]), as.numeric(export_bbox["xmax"]))
    if (is.null(ylim)) ylim = c(as.numeric(export_bbox["ymin"]), as.numeric(export_bbox["ymax"]))

    ctx_start = proc.time()[["elapsed"]]
    context_polygons = make_plot_safe_sf(context_polygons, export_bbox)
    if (agg_level == "Province-level") {
      context_polygons = context_polygons %>% filter(!(geo_admin1 %in% SubsetG$geo_admin1))
    } else if (agg_level == "District-level") {
      context_polygons = context_polygons %>% filter(!(geo_admin2 %in% SubsetG$geo_admin2))
    } else {
      context_polygons = context_polygons %>% filter(!(geo_admin0 %in% SubsetG$geo_admin0))
    }
    if (nrow(context_polygons) > 250) {
      context_polygons = context_polygons %>% slice_head(n = 250)
    }
    context_labels = if (nrow(context_polygons) > 0) suppressWarnings(sf::st_point_on_surface(context_polygons)) else context_polygons
    perf_state$png_context_secs = round(proc.time()[["elapsed"]] - ctx_start, 3)

    data_start = proc.time()[["elapsed"]]
    SubsetG = make_plot_safe_sf(SubsetG, export_bbox)
    if (agg_level == "Province-level") {
      SubsetG = SubsetG %>% mutate(data_label = ifelse(Region1 == "Not applicable", Region, Region1))
    } else if (agg_level == "District-level") {
      SubsetG = SubsetG %>% mutate(data_label = ifelse(Region2 == "Not applicable", Region1, Region2))
    } else {
      SubsetG = SubsetG %>% mutate(data_label = Region)
    }
    data_labels = if (nrow(SubsetG) > 0) suppressWarnings(sf::st_point_on_surface(SubsetG)) else SubsetG
    perf_state$png_data_secs = round(proc.time()[["elapsed"]] - data_start, 3)

    payload = list(
      SubsetG = SubsetG,
      context_polygons = context_polygons,
      context_labels = context_labels,
      data_labels = data_labels,
      pts = pts,
      xlim = xlim,
      ylim = ylim,
      legend_vals = pal_metric_r()$legend_vals,
      single_value = pal_metric_r()$single_value,
      single_colour = pal_metric_r()$single_colour,
      MetricN = MetricN_r(),
      MetricUnit = MetricUnit_r()
    )

    png_export_cache(list(key = cache_key, payload = payload))
    perf_state$png_prep_cache_hit = "no"
    perf_state$png_prep_secs = round(proc.time()[["elapsed"]] - prep_start, 3)
    log_trace("png_prep", paste0("cache_hit=no workers=", perf_state$png_prep_workers, " prep_secs=", perf_state$png_prep_secs, " context_secs=", perf_state$png_context_secs, " data_secs=", perf_state$png_data_secs))
    payload
  })

  # Ported from IthaMaps-shinyapp/app.R lines 757-851: export prediction-mode
  # rasters, cropped figure, and CSV packages from the current prediction view.
  output$download_tif = downloadHandler(
    filename = function() {
      paste0("IthaMaps_", Sys.Date(), ".zip")
    },
    content = function(file) {
      set_export_status("Exporting ZIP package... Please wait.")
      on.exit(clear_export_status(), add = TRUE)
      req(is_prediction_mode())
      assets = prediction_data_r()

      export_dir = tempfile("IthaMaps_")
      dir.create(export_dir, recursive = TRUE, showWarnings = FALSE)
      export_folder_name = paste0("IthaMaps_", Sys.Date())
      export_folder = file.path(export_dir, export_folder_name)
      dir.create(export_folder, recursive = TRUE, showWarnings = FALSE)

      mean_export = file.path(export_folder, "Predicted-carrier-prevalence.tif")
      ci95_export = file.path(export_folder, "Prediction-uncertainty.tif")
      burden_export = file.path(export_folder, "Estimated-carriers.tif")
      sites_export = file.path(export_folder, "Priority-sites.csv")

      raster::writeRaster(assets$Mean, filename = mean_export, format = "GTiff", overwrite = TRUE)
      raster::writeRaster(assets$CI95, filename = ci95_export, format = "GTiff", overwrite = TRUE)
      raster::writeRaster(assets$Burden, filename = burden_export, format = "GTiff", overwrite = TRUE)
      write.csv(assets$Selected_sites, file = sites_export, row.names = FALSE)

      zip::zipr(zipfile = file, files = list.files(export_folder, full.names = TRUE), root = export_dir)
    }
  )

  build_hcp_export_sf = function() {
    SubsetHCP = SubsetHCP_r()
    idx0 = match(SubsetHCP$geo_admin0, adm0_sel$geo_admin0)
    SubsetHCP %>%
      mutate(
        Country = adm0_lookup$Region[idx0],
        geom = st_geometry(adm0_sel)[idx0]
      ) %>%
      st_as_sf(sf_column_name = "geom") %>%
      filter(!is.na(idx0))
  }

  output$download_png = downloadHandler(
    contentType = "image/png",
    filename = function() {
      paste0("IthaMaps_", Sys.Date(), ".png")
    },
    content = function(file) {
      if (is_prediction_mode()) {
        set_export_status("Exporting PNG... Please wait.")
        on.exit(clear_export_status(), add = TRUE)
        assets = prediction_data_r()

        ext = current_prediction_extent()
        mean_crop = tryCatch(raster::crop(assets$Mean, ext), error = function(e) assets$Mean)
        ci95_crop = tryCatch(raster::crop(assets$CI95, ext), error = function(e) assets$CI95)
        burden_crop = tryCatch(raster::crop(assets$Burden, ext), error = function(e) assets$Burden)

        if (is.null(mean_crop) || all(is.na(mean_crop[]))) mean_crop = assets$Mean
        if (is.null(ci95_crop) || all(is.na(ci95_crop[]))) ci95_crop = assets$CI95
        if (is.null(burden_crop) || all(is.na(burden_crop[]))) burden_crop = assets$Burden

        # Priority sites filtered to current viewport extent, overlaid on all three panels.
        sites_export = assets$Selected_sites %>%
          dplyr::filter(
            lon >= raster::xmin(ci95_crop),
            lon <= raster::xmax(ci95_crop),
            lat >= raster::ymin(ci95_crop),
            lat <= raster::ymax(ci95_crop)
          )

        # Prepare grey country background for the current extent, matching the
        # curated-data export (fill = grey88, border = grey60). Countries outside
        # the prediction model (NA raster cells) will be visibly greyed out rather
        # than appearing as transparent/white.
        # Disable s2 spherical geometry for the crop (same pattern as curated export).
        prev_s2_pred = sf::sf_use_s2()
        suppressMessages(sf::sf_use_s2(FALSE))
        raster_ext = raster::extent(mean_crop)
        adm0_bg = suppressWarnings(tryCatch(
          sf::st_crop(adm0_sel, sf::st_bbox(c(
            xmin = raster_ext@xmin, xmax = raster_ext@xmax,
            ymin = raster_ext@ymin, ymax = raster_ext@ymax
          ), crs = sf::st_crs(adm0_sel))),
          error = function(e) adm0_sel
        ))
        suppressMessages(sf::sf_use_s2(prev_s2_pred))
        xlim = c(raster_ext@xmin, raster_ext@xmax)
        ylim = c(raster_ext@ymin, raster_ext@ymax)
        # NULL = never toggled (default visible); FALSE = user unchecked.
        show_sites = !isFALSE(input$prediction_sites_visible)

        # Build a ggplot2 panel matching the curated-data export style.
        make_pred_panel = function(raster_data, colours, title, logo_img = NULL) {
          rdf = raster::as.data.frame(raster_data, xy = TRUE)
          names(rdf) = c("x", "y", "value")
          rdf = rdf[!is.na(rdf$value), , drop = FALSE]

          in_topleft = rdf$x < mean(xlim) & rdf$y > ylim[1] + 0.65 * diff(ylim)
          pred_corner = if (any(in_topleft)) "right" else "left"

          p = ggplot2::ggplot() +
            ggplot2::geom_sf(
              data = adm0_bg, fill = "grey88", colour = "grey60", linewidth = 0.2, alpha = 0.95
            ) +
            ggplot2::geom_tile(
              data = rdf,
              ggplot2::aes(x = x, y = y, fill = value),
              width  = raster::xres(raster_data),
              height = raster::yres(raster_data)
            ) +
            ggplot2::scale_fill_gradientn(
              colours  = colours, limits = range(rdf$value), na.value = NA,
              guide    = ggplot2::guide_colorbar(
                title = wrap_legend_title(title), title.position = "top",
                barwidth = 0.8, barheight = 10
              )
            ) +
            ggplot2::geom_sf(data = adm0_bg, fill = NA, colour = "grey60", linewidth = 0.2) +
            ggplot2::geom_sf_text(
              data = suppressWarnings(sf::st_point_on_surface(adm0_bg)),
              ggplot2::aes(label = Region),
              colour = "grey35", size = 2.2, check_overlap = TRUE
            ) +
            ggplot2::coord_sf(xlim = xlim, ylim = ylim, expand = FALSE) +
            ggplot2::labs(x = "Longitude", y = "Latitude", title = title) +
            ggplot2::theme_minimal(base_size = 11) +
            ggplot2::theme(
              panel.grid      = ggplot2::element_line(colour = "grey90"),
              legend.position = "right",
              legend.title    = ggplot2::element_text(size = 9),
              plot.title      = ggplot2::element_text(size = 11, face = "bold")
            )

          if (!is.null(logo_img)) {
            p = add_logo_to_ggplot(p, xlim, ylim, corner = pred_corner, logo_img = logo_img, panel_height = 6)
          }
          if (show_sites && nrow(sites_export) > 0) {
            p = p + ggplot2::geom_point(
              data = sites_export,
              ggplot2::aes(x = lon, y = lat),
              pch = 21, fill = "black", colour = "black", size = 0.8
            )
          }
          p
        }

        pred_logo = logo_export_image()
        p_mean   = make_pred_panel(mean_crop,   rev(assets$Mean_colours),   "Predicted carrier prevalence (%)",                pred_logo)
        p_ci95   = make_pred_panel(ci95_crop,   rev(assets$CI95_colours),   "Prediction uncertainty (95% Credible Interval)", pred_logo)
        p_burden = make_pred_panel(burden_crop, rev(assets$Burden_colours), "Estimated number of carriers",                   pred_logo)
        combined = cowplot::plot_grid(p_mean, p_ci95, p_burden, ncol = 1, align = "v")
        ggplot2::ggsave(file, plot = combined, width = 12, height = 18, dpi = 150,
                        device = ragg::agg_png, bg = "white")
        return(invisible(NULL))
      }

      if (is_hcp_mode()) {
        set_export_status("Exporting PNG... Please wait.")
        on.exit(clear_export_status(), add = TRUE)

        hcp_sf = build_hcp_export_sf()
        if (is.null(hcp_sf) || nrow(hcp_sf) == 0) {
          stop("No healthcare polygons available for PNG export.")
        }

        availability_levels = levels(hcp_sf$availability_harmonised)
        if (is.null(availability_levels) || length(availability_levels) == 0) {
          availability_levels = unique(as.character(hcp_sf$availability_harmonised))
        }
        availability_levels = availability_levels[!is.na(availability_levels) & nzchar(availability_levels)]
        # Keep export colors aligned with the interactive Leaflet palette.
        hcp_palette = viridis::viridis(3, option = "F", begin = 0, end = 0.7, direction = -1)
        pal_hcp_export = colorFactor(
          palette = hcp_palette,
          domain = availability_levels,
          na.color = "#cccccc"
        )
        availability_palette = stats::setNames(
          unname(pal_hcp_export(availability_levels)),
          availability_levels
        )

        hcp_sf = hcp_sf %>%
          mutate(
            Availability_norm = as.character(availability_harmonised),
            Availability_norm = factor(Availability_norm, levels = availability_levels)
          )

        selected_ids = unique(hcp_sf$geo_admin0)
        context_sf = adm0_sel %>%
          filter(!(geo_admin0 %in% selected_ids))

        # Center export extent around available healthcare data with padding.
        bbox = sf::st_bbox(hcp_sf)
        x_pad = max((bbox$xmax - bbox$xmin) * 0.18, 2)
        y_pad = max((bbox$ymax - bbox$ymin) * 0.18, 2)
        xlim = c(max(-180, bbox$xmin - x_pad), min(180, bbox$xmax + x_pad))
        ylim = c(max(-85, bbox$ymin - y_pad), min(85, bbox$ymax + y_pad))

        bbox_poly = sf::st_as_sfc(sf::st_bbox(c(
          xmin = xlim[1],
          xmax = xlim[2],
          ymin = ylim[1],
          ymax = ylim[2]
        ), crs = sf::st_crs(adm0_sel)))

        context_sf = suppressWarnings(tryCatch(
          sf::st_crop(context_sf, bbox_poly),
          error = function(e) context_sf
        ))

        # Use point-on-surface labels so country names remain inside polygons
        # where possible and avoid centroid fall-out on irregular geometries.
        context_labels = if (nrow(context_sf) > 0) suppressWarnings(sf::st_point_on_surface(context_sf)) else context_sf
        hcp_labels = suppressWarnings(sf::st_point_on_surface(hcp_sf))

        p = ggplot2::ggplot() +
          ggplot2::geom_sf(
            data = context_sf,
            fill = "grey88",
            colour = "grey60",
            linewidth = 0.2,
            alpha = 0.95
          ) +
          ggplot2::geom_sf_text(
            data = context_labels,
            ggplot2::aes(label = Region),
            colour = "grey45",
            size = 2.2,
            check_overlap = TRUE
          ) +
          ggplot2::geom_sf(
            data = hcp_sf,
            ggplot2::aes(fill = Availability_norm),
            colour = "grey60",
            linewidth = 0.2,
            alpha = map_fill_opacity
          ) +
          ggplot2::geom_sf_text(
            data = hcp_labels,
            ggplot2::aes(label = Country),
            colour = "grey35",
            size = 2.6,
            check_overlap = TRUE
          ) +
          ggplot2::scale_fill_manual(
            values = availability_palette,
            breaks = availability_levels,
            limits = availability_levels,
            drop = FALSE,
            name = "Healthcare availability",
            na.translate = FALSE,
            na.value = "grey80"
          ) +
          ggplot2::guides(fill = ggplot2::guide_legend(override.aes = list(colour = "black", alpha = 1))) +
          ggplot2::coord_sf(xlim = xlim, ylim = ylim, expand = FALSE) +
          ggplot2::labs(x = "Longitude", y = "Latitude") +
          ggplot2::theme_minimal(base_size = 11) +
          ggplot2::theme(
            panel.grid = ggplot2::element_line(colour = "grey90"),
            legend.position = "right"
          )

        p = add_logo_to_ggplot(p, xlim, ylim, hcp_sf)
        ggplot2::ggsave(file, plot = p, width = 12, height = 8, dpi = 150, device = ragg::agg_png, bg = "white")
        return(invisible(NULL))
      }
      set_export_status("Exporting PNG... Please wait.")
      on.exit(clear_export_status(), add = TRUE)

      png_total_start = proc.time()[["elapsed"]]

      payload = curated_png_payload_r()
      SubsetG = payload$SubsetG
      context_polygons = payload$context_polygons
      context_labels = payload$context_labels
      data_labels = payload$data_labels
      pts = payload$pts
      xlim = payload$xlim
      ylim = payload$ylim
      legend_vals = payload$legend_vals
      single_value = payload$single_value
      single_colour = payload$single_colour
      MetricN = payload$MetricN
      legend_title = metric_title_with_unit(MetricN, payload$MetricUnit)

      plot_subsetg = SubsetG
      fill_mapping = ggplot2::aes(fill = Metric)

      if (!is.null(single_value) && !is.null(single_colour)) {
        single_label = format(round(single_value, 2), nsmall = 2, trim = TRUE)
        plot_subsetg = SubsetG %>% mutate(single_metric_label = single_label)
        fill_mapping = ggplot2::aes(fill = single_metric_label)
        single_colour_png = grDevices::adjustcolor(single_colour, alpha.f = map_fill_opacity)
        fill_scale = ggplot2::scale_fill_manual(
          values = stats::setNames(single_colour_png, single_label),
          name = legend_title,
          drop = FALSE,
          guide = ggplot2::guide_legend(
            keywidth = grid::unit(8, "mm"),
            keyheight = grid::unit(8, "mm")
          )
        )
      } else {
        png_palette = viridis::viridis(81, option = "F", begin = 0, end = 0.7, direction = -1)
        png_palette = vapply(
          png_palette,
          function(colour) grDevices::adjustcolor(colour, alpha.f = map_fill_opacity),
          character(1)
        )
        # Build a continuous viridis fill scale matching the interactive map.
        fill_scale = scale_fill_gradientn(
          colours  = png_palette,
          limits   = range(legend_vals, na.rm = TRUE),
          na.value = "grey80",
          name     = legend_title,
          guide    = ggplot2::guide_colorbar(reverse = TRUE)
        )
      }

      # NULL = never toggled (default visible); FALSE = user unchecked; TRUE = user checked.
      show_studies = !isFALSE(input$map_studies_visible)

      plot_build_start = proc.time()[["elapsed"]]
      p = ggplot2::ggplot()

      if (nrow(context_polygons) > 0) {
        p = p +
          ggplot2::geom_sf(
            data = context_polygons,
            fill = "grey88",
            colour = "grey60",
            linewidth = 0.2,
            alpha = 0.9
          ) +
          ggplot2::geom_sf_text(
            data = context_labels,
            ggplot2::aes(label = label_name),
            colour = "grey35",
            size = 2.8,
            check_overlap = TRUE
          )
      }

      p = p +
        ggplot2::geom_sf(
          data = plot_subsetg,
          fill_mapping,
          colour = "grey60",
          linewidth = 0.2,
          alpha = 0.8
        ) +
        ggplot2::geom_sf_text(
          data = data_labels,
          ggplot2::aes(label = data_label),
          colour = "grey35",
          size = 2.8,
          check_overlap = TRUE
        ) +
        fill_scale

      if (show_studies && !is.null(pts) && nrow(pts) > 0) {
        p = p + ggplot2::geom_point(
          data = pts,
          ggplot2::aes(x = longitude, y = latitude),
          colour = "black", fill = "black",
          shape = 21, size = 1.8, stroke = 0.4
        )
      }

      p = p +
        ggplot2::coord_sf(xlim = xlim, ylim = ylim, expand = FALSE) +
        ggplot2::labs(x = "Longitude", y = "Latitude") +
        ggplot2::theme_minimal(base_size = 11) +
        ggplot2::theme(
          panel.grid = ggplot2::element_line(colour = "grey90"),
          legend.position = "right"
        )
      p = add_logo_to_ggplot(p, xlim, ylim, SubsetG)
      perf_state$png_plot_build_secs = round(proc.time()[["elapsed"]] - plot_build_start, 3)

      tryCatch(
        {
          save_start = proc.time()[["elapsed"]]
          ggplot2::ggsave(file, plot = p, width = 12, height = 8, dpi = 150, device = ragg::agg_png, bg = "white")
          perf_state$png_save_secs = round(proc.time()[["elapsed"]] - save_start, 3)
          perf_state$png_total_secs = round(proc.time()[["elapsed"]] - png_total_start, 3)
          log_trace(
            "png_export",
            paste0(
              "prep_cache_hit=", perf_state$png_prep_cache_hit %||% "unknown",
              " prep_secs=", perf_state$png_prep_secs %||% NA_real_,
              " plot_build_secs=", perf_state$png_plot_build_secs %||% NA_real_,
              " save_secs=", perf_state$png_save_secs %||% NA_real_,
              " total_secs=", perf_state$png_total_secs %||% NA_real_
            )
          )
        },
        error = function(e) {
          perf_state$png_total_secs = round(proc.time()[["elapsed"]] - png_total_start, 3)
          log_trace("png_export_error", conditionMessage(e))
          showNotification(paste("PNG export failed:", conditionMessage(e)), type = "error", duration = 8)
          stop(e)
        }
      )
    }
  )

  output$download_csv = downloadHandler(
    filename = function() {
      if (is_prediction_mode()) {
        paste0("IthaMaps_", Sys.Date(), "_csv.zip")
      } else {
        paste0("IthaMaps_", Sys.Date(), ".csv")
      }
    },
    content = function(file) {
      set_export_status("Exporting CSV... Please wait.")
      on.exit(clear_export_status(), add = TRUE)
      if (is_prediction_mode()) {
        assets = prediction_data_r()

        export_dir = tempfile("IthaMaps_csv_")
        dir.create(export_dir, recursive = TRUE, showWarnings = FALSE)
        raster_csv = file.path(export_dir, "Data from rasters.csv")
        sites_csv = file.path(export_dir, "Coordinates of priority-sites.csv")
        prediction_stack = raster::stack(assets$Mean, assets$CI95, assets$Burden)
        names(prediction_stack) = c("Mean", "CI95", "Burden")
        prediction_values = raster::rasterToPoints(prediction_stack) %>% as.data.frame()
        names(prediction_values) = c("Longitude", "Latitude", "Mean", "CI95", "Burden")
        write.csv(prediction_values, file = raster_csv, row.names = FALSE)
        write.csv(assets$Selected_sites, file = sites_csv, row.names = FALSE)
        zip::zipr(zipfile = file, files = list.files(export_dir, full.names = TRUE), root = export_dir)
        return(invisible(NULL))
      }

      if (is_hcp_mode()) {
        SubsetHCP = SubsetHCP_r()
        idx0 = match(SubsetHCP$geo_admin0, adm0_lookup$geo_admin0)
        df = SubsetHCP %>%
          mutate(Country = adm0_lookup$Region[idx0]) %>%
          dplyr::select(any_of(c(
            "Country", "availability_harmonised", "known_implementation_period_harmonised",
            "eligibility_harmonised", "implementation_harmonised", "application_harmonised",
            "compensation_harmonised", "diagnostic_method_harmonised", "uptake_harmonised",
            "recruitment_site_harmonised", "note_harmonised", "citation_str_harmonised"
          ))) %>%
          dplyr::rename(any_of(c(
            "Availability (harmonised)" = "availability_harmonised",
            "Known implementation timeframe (harmonised)" = "known_implementation_period_harmonised",
            "Eligibility (harmonised)" = "eligibility_harmonised",
            "Implementation (harmonised)" = "implementation_harmonised",
            "Application (harmonised)" = "application_harmonised",
            "Compensation (harmonised)" = "compensation_harmonised",
            "Diagnostic method (harmonised)" = "diagnostic_method_harmonised",
            "Uptake (harmonised)" = "uptake_harmonised",
            "Recruitment site (harmonised)" = "recruitment_site_harmonised",
            "Notes (harmonised)" = "note_harmonised",
            "Source (harmonised)" = "citation_str_harmonised"
          )))
        write.csv(df, file, row.names = FALSE)
        return(invisible(NULL))
      }
      SubsetE = SubsetE_r()
      df = SubsetE %>%
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
      requested_measure = as.character((query_bundle()$query_info %||% list())$Measure %||% "")
      if (!identical(requested_measure, "Allele frequency")) {
        df = df %>% dplyr::select(-any_of("IthaID"))
      }
      if (!identical(requested_measure, "Relative allele frequency")) {
        df = df %>% dplyr::select(-any_of("Globin phenotype"))
      }
      write.csv(df, file, row.names = FALSE)
    }
  )

  output$download_geojson = downloadHandler(
    filename = function() {
      paste0("IthaMaps_", Sys.Date(), ".geojson")
    },
    content = function(file) {
      set_export_status("Exporting GeoJSON... Please wait.")
      on.exit(clear_export_status(), add = TRUE)
      if (is_prediction_mode()) {
        showNotification("GeoJSON export is not available for prediction raster data.", type = "warning", duration = 4)
        return(invisible(NULL))
      }
      if (is_hcp_mode()) {
        hcp_sf = build_hcp_export_sf() %>%
          dplyr::select(any_of(c(
            "Country", "availability_harmonised", "known_implementation_period_harmonised",
            "eligibility_harmonised", "implementation_harmonised", "application_harmonised",
            "compensation_harmonised", "diagnostic_method_harmonised", "uptake_harmonised",
            "recruitment_site_harmonised", "note_harmonised", "citation_str_harmonised", "geom"
          ))) %>%
          dplyr::rename(any_of(c(
            "Availability (harmonised)" = "availability_harmonised",
            "Known implementation timeframe (harmonised)" = "known_implementation_period_harmonised",
            "Eligibility (harmonised)" = "eligibility_harmonised",
            "Implementation (harmonised)" = "implementation_harmonised",
            "Application (harmonised)" = "application_harmonised",
            "Compensation (harmonised)" = "compensation_harmonised",
            "Diagnostic method (harmonised)" = "diagnostic_method_harmonised",
            "Uptake (harmonised)" = "uptake_harmonised",
            "Recruitment site (harmonised)" = "recruitment_site_harmonised",
            "Notes (harmonised)" = "note_harmonised",
            "Source (harmonised)" = "citation_str_harmonised"
          )))
        if (is.null(hcp_sf) || nrow(hcp_sf) == 0) {
          stop("No healthcare polygons available for GeoJSON export.")
        }
        st_write(hcp_sf, file, driver = "GeoJSON", delete_dsn = TRUE)
        return(invisible(NULL))
      }
      SubsetE = SubsetE_r()
      sf_df = entries_to_point_sf(SubsetE)
      if (is.null(sf_df)) stop("No point coordinates available for GeoJSON export.")
      sf_df = sf_df %>%
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
      requested_measure = as.character((query_bundle()$query_info %||% list())$Measure %||% "")
      if (!identical(requested_measure, "Allele frequency")) {
        sf_df = sf_df %>% dplyr::select(-any_of("IthaID"))
      }
      if (!identical(requested_measure, "Relative allele frequency")) {
        sf_df = sf_df %>% dplyr::select(-any_of("Globin phenotype"))
      }
      st_write(sf_df, file, driver = "GeoJSON", delete_dsn = TRUE)
    }
  )

  output$download_gpkg = downloadHandler(
    filename = function() {
      paste0("IthaMaps_", Sys.Date(), ".gpkg")
    },
    content = function(file) {
      set_export_status("Exporting GPKG... Please wait.")
      on.exit(clear_export_status(), add = TRUE)
      if (is_prediction_mode()) {
        showNotification("GPKG export is not available for prediction raster data.", type = "warning", duration = 4)
        return(invisible(NULL))
      }
      if (is_hcp_mode()) {
        hcp_sf = build_hcp_export_sf() %>%
          dplyr::select(any_of(c(
            "Country", "availability_harmonised", "known_implementation_period_harmonised",
            "eligibility_harmonised", "implementation_harmonised", "application_harmonised",
            "compensation_harmonised", "diagnostic_method_harmonised", "uptake_harmonised",
            "recruitment_site_harmonised", "note_harmonised", "citation_str_harmonised", "geom"
          ))) %>%
          dplyr::rename(any_of(c(
            "Availability (harmonised)" = "availability_harmonised",
            "Known implementation timeframe (harmonised)" = "known_implementation_period_harmonised",
            "Eligibility (harmonised)" = "eligibility_harmonised",
            "Implementation (harmonised)" = "implementation_harmonised",
            "Application (harmonised)" = "application_harmonised",
            "Compensation (harmonised)" = "compensation_harmonised",
            "Diagnostic method (harmonised)" = "diagnostic_method_harmonised",
            "Uptake (harmonised)" = "uptake_harmonised",
            "Recruitment site (harmonised)" = "recruitment_site_harmonised",
            "Notes (harmonised)" = "note_harmonised",
            "Source (harmonised)" = "citation_str_harmonised"
          )))
        if (is.null(hcp_sf) || nrow(hcp_sf) == 0) {
          stop("No healthcare polygons available for GPKG export.")
        }
        st_write(hcp_sf, dsn = file, driver = "GPKG", delete_dsn = TRUE)
        return(invisible(NULL))
      }
      SubsetE = SubsetE_r()
      sf_df = entries_to_point_sf(SubsetE)
      if (is.null(sf_df)) stop("No point coordinates available for GPKG export.")
      sf_df = sf_df %>%
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
      requested_measure = as.character((query_bundle()$query_info %||% list())$Measure %||% "")
      if (!identical(requested_measure, "Allele frequency")) {
        sf_df = sf_df %>% dplyr::select(-any_of("IthaID"))
      }
      if (!identical(requested_measure, "Relative allele frequency")) {
        sf_df = sf_df %>% dplyr::select(-any_of("Globin phenotype"))
      }
      st_write(sf_df, dsn = file, driver = "GPKG", delete_dsn = TRUE)
    }
  )

  observeEvent(input$map_marker_click, {
    req(!is_prediction_mode())
    req(data_available())
    click = input$map_marker_click
    data = filtered_data()
    if (!is.null(click)) {
      nearest_idx = NA_integer_
      click_id = click$id %||% NULL

      if (!is.null(click_id)) {
        click_id_chr = as.character(click_id)
        if (grepl("^row_[0-9]+$", click_id_chr)) {
          parsed_idx = suppressWarnings(as.integer(sub("^row_", "", click_id_chr)))
          if (!is.na(parsed_idx) && parsed_idx >= 1L && parsed_idx <= nrow(data)) {
            nearest_idx = parsed_idx
          }
        }
      }

      if (is.na(nearest_idx)) {
        lng = suppressWarnings(as.numeric(data$longitude))
        lat = suppressWarnings(as.numeric(data$latitude))
        dists = (lng - click$lng)^2 + (lat - click$lat)^2
        dists[is.na(dists)] = Inf
        nearest_idx = which.min(dists)
      }

      if (!is.finite(nearest_idx) || is.na(nearest_idx) || nearest_idx < 1L || nearest_idx > nrow(data)) {
        return(invisible(NULL))
      }

      selected_row(nearest_idx)
      selected_marker_idx(nearest_idx)
      selected_marker_layer_id(paste0("row_", nearest_idx))
      selected_shape_idx(NULL)

      table_rows = table_rows_from_local_rows(nearest_idx)
      previous_selection(table_rows)
      if (length(table_rows) > 0) {
        page_length = 10L
        target_page = ((min(table_rows) - 1L) %/% page_length) + 1L
        suppress_selection_observer(TRUE)
        proxy = dataTableProxy("data_table")
        proxy %>% selectPage(target_page)
        proxy %>% selectRows(table_rows)
      }
    }
  })

  observeEvent(input$map_shape_click, {
    req(!is_prediction_mode())
    req(data_available())
    if (is_hcp_mode()) {
      click = input$map_shape_click
      click_id = click$id %||% NULL
      if (is.null(click_id)) {
        return(invisible(NULL))
      }
      click_id_chr = as.character(click_id)
      if (!grepl("^row_[0-9]+$", click_id_chr)) {
        return(invisible(NULL))
      }
      idx = suppressWarnings(as.integer(sub("^row_", "", click_id_chr)))
      data = filtered_data()
      if (is.na(idx) || idx < 1L || idx > nrow(data)) {
        return(invisible(NULL))
      }
      # Healthcare polygons are drawn per row but a country may have several
      # rows (one per Availability status); select every row for that country.
      selected_rows = idx
      if ("geo_admin0" %in% names(data) && !is.na(data$geo_admin0[[idx]])) {
        selected_rows = which(data$geo_admin0 == data$geo_admin0[[idx]])
      }
      selected_row(selected_rows)
      selected_marker_idx(idx)
      selected_marker_layer_id(NULL)
      selected_shape_idx(NULL)
      table_rows = table_rows_from_local_rows(selected_rows)
      previous_selection(table_rows)
      if (length(table_rows) > 0) {
        page_length = 10L
        target_page = ((min(table_rows) - 1L) %/% page_length) + 1L
        suppress_selection_observer(TRUE)
        proxy = dataTableProxy("data_table")
        proxy %>% selectPage(target_page)
        proxy %>% selectRows(table_rows)
      }
      return(invisible(NULL))
    }
    click = input$map_shape_click
    SubsetG = SubsetG_r()
    if (!is.null(click) && !is.null(SubsetG)) {
      # Prefer the polygon's layerId so the clicked shape is identified exactly
      # (nearest-centroid misfires for large/elongated countries). Fall back to
      # nearest centroid only when no usable id is present.
      nearest_idx = NA_integer_
      click_id = click$id %||% NULL
      if (!is.null(click_id)) {
        click_id_chr = as.character(click_id)
        if (grepl("^shape_[0-9]+$", click_id_chr)) {
          parsed = suppressWarnings(as.integer(sub("^shape_", "", click_id_chr)))
          if (!is.na(parsed) && parsed >= 1L && parsed <= nrow(SubsetG)) {
            nearest_idx = parsed
          }
        }
      }
      if (is.na(nearest_idx)) {
        clicked_shape = st_sfc(st_point(c(click$lng, click$lat)), crs = st_crs(SubsetG))
        hits = suppressMessages(st_intersects(clicked_shape, SubsetG))
        hit_idx = if (length(hits) >= 1) hits[[1]] else integer(0)
        if (length(hit_idx) >= 1) {
          nearest_idx = as.integer(hit_idx[[1]])
        } else {
          dists = st_distance(clicked_shape, st_centroid(SubsetG))
          nearest_idx = which.min(dists)
        }
      }

      data = filtered_data()
      selected_rows = integer(0)
      selected_shape = SubsetG[nearest_idx, , drop = FALSE]

      if ("geo_admin2" %in% names(selected_shape) && "geo_admin2" %in% names(data) && !is.na(selected_shape$geo_admin2[[1]])) {
        selected_rows = which(data$geo_admin2 == selected_shape$geo_admin2[[1]])
      } else if ("geo_admin1" %in% names(selected_shape) && "geo_admin1" %in% names(data) && !is.na(selected_shape$geo_admin1[[1]])) {
        selected_rows = which(data$geo_admin1 == selected_shape$geo_admin1[[1]])
      } else if ("geo_admin0" %in% names(selected_shape) && "geo_admin0" %in% names(data) && !is.na(selected_shape$geo_admin0[[1]])) {
        selected_rows = which(data$geo_admin0 == selected_shape$geo_admin0[[1]])
      }

      # Set shape/marker state first so the programmatic selectRows() below does
      # not get treated as a manual single-row click.
      selected_shape_idx(nearest_idx)
      selected_marker_idx(NULL)
      selected_marker_layer_id(NULL)

      proxy = dataTableProxy("data_table")
      if (length(selected_rows) > 0) {
        selected_row(selected_rows)
        table_rows = table_rows_from_local_rows(selected_rows)
        previous_selection(table_rows)
        if (length(table_rows) > 0) {
          page_length = 10L
          target_page = ((min(table_rows) - 1L) %/% page_length) + 1L
          suppress_selection_observer(TRUE)
          proxy %>% selectPage(target_page)
          proxy %>% selectRows(table_rows)
        }
      } else {
        selected_row(NULL)
        previous_selection(integer(0))
        suppress_selection_observer(TRUE)
        proxy %>% selectRows(NULL)
      }
    }
  })

  observeEvent(input$clear_selection_btn, {
    selected_row(NULL)
    selected_marker_idx(NULL)
    selected_marker_layer_id(NULL)
    selected_shape_idx(NULL)
    previous_selection(integer(0))
    suppress_selection_observer(TRUE)
    dataTableProxy("data_table") %>%
      selectRows(NULL)
  })

  output$custom_popup = renderUI({
    if (is_prediction_mode()) {
      return(summary_panel_r())
    }

    marker_idx = selected_marker_idx()
    shape_idx = selected_shape_idx()

    details_ui = NULL

    if (!is.null(marker_idx)) {
      popup_content = popup_content_r()
      if (marker_idx >= 1 && marker_idx <= length(popup_content)) {
        details_ui = popup_content[[marker_idx]]
      }
    }

    if (is.null(details_ui) && !is.null(shape_idx) && !is_hcp_mode()) {
      popup_contentA = popup_contentA_r()
      if (shape_idx >= 1 && shape_idx <= length(popup_contentA)) {
        details_ui = popup_contentA[[shape_idx]]
      }
    }

    has_selection = !is.null(marker_idx) || !is.null(shape_idx) || (!is.null(selected_row()) && length(selected_row()) > 0)

    if (!is.null(details_ui)) {
      return(tagList(
        if (has_selection) {
          div(
            style = "margin-bottom: 8px;",
            actionButton("clear_selection_btn", "Clear selection", class = "btn btn-outline-secondary btn-sm")
          )
        },
        details_ui
      ))
    }

    summary_panel_r()
  })

  output$timing_panel = renderUI({
    if (!isTRUE(ithamaps_debug_mode)) return(NULL)
    timings = timing_info_r()
    if (length(timings) == 0) {
      raw_qs = active_url_search_r()
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

    timing_rows = c(
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
      "Map geometry prep" = if (!is.null(perf_state$map_simplify_secs)) sprintf("%.3fs", perf_state$map_simplify_secs) else NA_character_,
      "Map total (browser, incl. transfer+render)" = if (!is.null(perf_state$map_client_total_secs)) sprintf("%.3fs", perf_state$map_client_total_secs) else NA_character_,
      "Table render" = if (!is.null(perf_state$table_render_secs)) sprintf("%.3fs", perf_state$table_render_secs) else NA_character_,
      "PNG prep cache hit" = if (!is.null(perf_state$png_prep_cache_hit)) perf_state$png_prep_cache_hit else NA_character_,
      "PNG prep workers" = if (!is.null(perf_state$png_prep_workers)) as.character(perf_state$png_prep_workers) else NA_character_,
      "PNG prep" = if (!is.null(perf_state$png_prep_secs)) sprintf("%.3fs", perf_state$png_prep_secs) else NA_character_,
      "PNG context prep" = if (!is.null(perf_state$png_context_secs)) sprintf("%.3fs", perf_state$png_context_secs) else NA_character_,
      "PNG data prep" = if (!is.null(perf_state$png_data_secs)) sprintf("%.3fs", perf_state$png_data_secs) else NA_character_,
      "PNG plot build" = if (!is.null(perf_state$png_plot_build_secs)) sprintf("%.3fs", perf_state$png_plot_build_secs) else NA_character_,
      "PNG save" = if (!is.null(perf_state$png_save_secs)) sprintf("%.3fs", perf_state$png_save_secs) else NA_character_,
      "PNG export total" = if (!is.null(perf_state$png_total_secs)) sprintf("%.3fs", perf_state$png_total_secs) else NA_character_
    )

    timing_rows = timing_rows[!is.na(timing_rows)]

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

  output$no_data_notification = renderUI({
    validation_errors = validation_errors_r()
    if (length(validation_errors) > 0) {
      return(div(class = "alert alert-warning", paste(validation_errors, collapse = " ")))
    }
    NULL
  })
}
