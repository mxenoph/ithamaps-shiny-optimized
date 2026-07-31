
# =============================================================================
# ui.R
#
# Part of the global.R / ui.R / server.R Shiny app split. Defines the `ui`
# object only. global.R has already run (packages, DB reads, lookups, helper
# functions) by the time this file is sourced, so `ui` may reference anything
# defined there (e.g. bs_theme() from bslib, loaded in global.R).
# =============================================================================

ui = fluidPage(
  theme = bs_theme(version = 5, bootswatch = "litera"),
  tags$head(
    tags$script(HTML("(function() {
      var iframeId = 'ithamaps_shiny_iframe';
      var messageType = 'resizeIframe';
      var timer = null;
      var lastPostedHeight = 0;

      function currentHeight() {
        var body = document.body;
        var html = document.documentElement;
        return Math.max(
          body ? body.scrollHeight : 0,
          html ? html.scrollHeight : 0,
          body ? body.offsetHeight : 0,
          html ? html.offsetHeight : 0
        );
      }

      function postHeight() {
        if (!window.parent || window.parent === window) {
          return;
        }
        var nextHeight = currentHeight();
        if (!nextHeight || nextHeight < 200) {
          return;
        }
        if (Math.abs(nextHeight - lastPostedHeight) < 12) {
          return;
        }
        lastPostedHeight = nextHeight;
        window.parent.postMessage({
          type: messageType,
          iframeId: iframeId,
          height: nextHeight
        }, '*');
      }

      function schedulePostHeight() {
        clearTimeout(timer);
        timer = setTimeout(postHeight, 250);
      }

      $(document).on('shiny:connected shiny:idle', schedulePostHeight);
      $(window).on('load', schedulePostHeight);

      if (window.Shiny && Shiny.addCustomMessageHandler) {
        Shiny.addCustomMessageHandler('ithamaps-resize-iframe', function(message) {
          schedulePostHeight();
        });
        Shiny.addCustomMessageHandler('ithamaps-export-status', function(message) {
          var el = document.getElementById('ithamaps_export_status');
          if (!el) {
            return;
          }
          var text = message && message.text ? String(message.text) : '';
          if (text) {
            el.textContent = text;
            el.style.display = 'block';
          } else {
            el.textContent = '';
            el.style.display = 'none';
          }
        });
      }

      // Delay initial resize posts so Leaflet panes finish initialising
      // before the iframe height change triggers a map.getBounds() call.
      setTimeout(schedulePostHeight, 800);
      setTimeout(schedulePostHeight, 2000);
    })();")),
    tags$script(HTML("(function() {
      function loaderEl() { return document.getElementById('ithamaps_map_loader'); }
      function showLoader() {
        var el = loaderEl();
        if (el) { el.classList.add('is-loading'); }
        window.__ithamapsMapRecalcStart = (window.performance && performance.now) ? performance.now() : Date.now();
      }
      function hideLoader() {
        var el = loaderEl();
        if (el) { el.classList.remove('is-loading'); }
      }
      // The map output container has id 'map'. Show the progress bar while the
      // output is recalculating (covers server compute) and record a browser
      // timestamp; the leaflet onRender callback hides it once the map has
      // actually been drawn (covers transfer + client-side render).
      $(document).on('shiny:recalculating', function(e) {
        if (e && ((e.target && e.target.id === 'map') || e.name === 'map')) { showLoader(); }
      });
      $(document).on('shiny:error', function(e) {
        if (e && ((e.target && e.target.id === 'map') || e.name === 'map')) { hideLoader(); }
      });
      // Fallback in case onRender never runs (e.g. an empty map).
      $(document).on('shiny:value', function(e) {
        if (e && ((e.target && e.target.id === 'map') || e.name === 'map')) { setTimeout(hideLoader, 2000); }
      });
    })();"))
  ),
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
                                 .ithamaps-map-loader {position: absolute; inset: 0; z-index: 1200; display: none; flex-direction: column; align-items: center; justify-content: center; background: rgba(255,255,255,0.85); gap: 0.75rem; pointer-events: none;}
                                 .ithamaps-map-loader.is-loading {display: flex;}
                                 .ithamaps-map-loader-caption {font-size: 0.9rem; color: #333; font-weight: 600;}
                                 .ithamaps-progress {width: 60%; max-width: 360px; height: 8px; background: #e3e3e3; border-radius: 4px; overflow: hidden;}
                                 .ithamaps-progress-bar {width: 40%; height: 100%; background: #0000CC; border-radius: 4px; animation: ithamaps-indeterminate 1.1s ease-in-out infinite;}
                                 @keyframes ithamaps-indeterminate {0% {margin-left: -40%;} 100% {margin-left: 100%;}}
                                 .info-card {border: 1px solid #ccc; border-radius: 0.4rem; box-shadow: 0 0.125rem 0.25rem rgba(0,0,0,0.075); padding: 0.75rem; background-color: #f8f9fa; font-size: 0.85rem;}
                                 .value-table {width: 100%; border-collapse: collapse;}
                                 .value-table td {border: 1px solid #ddd; padding: 4px 6px;}
                                 .value-table td:first-child {font-weight: 600; background-color: #f1f1f1; width: 40%;}
                                 .raster-legend {margin-top: 0.75rem; padding: 0.5rem 0.25rem 0.25rem 0.25rem; font-size: 0.8rem;}
                                 .raster-legend-title {font-weight: 600; margin-bottom: 0.35rem; text-align: center;}
                                 .raster-legend-bar {height: 14px; border-radius: 4px; border: 1px solid #bbb;}
                                 .raster-legend-labels {display: flex; justify-content: space-between; margin-top: 0.25rem; font-size: 0.75rem;}
                                 .download-row {margin-top: 1rem; margin-bottom: 1rem; display: flex; flex-wrap: wrap; gap: 0.5rem; justify-content: center;}
                                 .pred-maps-grid {display: grid; grid-template-columns: 1fr; gap: 1rem;}
                                 @media (min-width: 1360px) { .pred-maps-grid { grid-template-columns: repeat(3, 1fr); } }")),
  uiOutput("timing_panel"),
  uiOutput("main_content")
)
