# Use the official Shiny image with tidyverse and spatial packages
FROM rocker/shiny-verse:latest

RUN sed -i 's|http://|https://|g' /etc/apt/sources.list && \
    apt-get update && \
    apt-get install -y --no-install-recommends \
        libudunits2-dev \
        libgdal-dev \
        libgeos-dev \
        libproj-dev && \
    apt-get clean && rm -rf /var/lib/apt/lists/*

# Install R packages (parallel compilation/install while keeping >=2 cores free).
RUN R -e "options(Ncpus = max(1L, parallel::detectCores(logical = TRUE) - 2L)); install.packages(c('DT', 'sf', 'dplyr', 'tidyr',  'bslib', 'shiny', 'readxl', 'stringr', 'metafor', 'leaflet', 'viridis', 'ggplot2', 'RMariaDB', 'raster', 'webshot2', 'shinycssloaders', 'zip', 'box', 'future', 'future.apply', 'ragg', 'jsonlite', 'mapview'))"

# Credential and connection env vars — override at runtime, never bake values in
# ITHAMAPS_MACHINE: row key to select from User_Configuration.xlsx (e.g. "docker")
# DB_USER_FILE / DB_PASSWORD_FILE: mounted secret-file paths (recommended)
# DB_USER / DB_PASSWORD: optional plain env overrides
# ITHAMAPS_DB_HOST / ITHAMAPS_DB_PORT: override host/port at runtime
ENV ITHAMAPS_MACHINE=docker \
    ITHAMAPS_DB_HOST="" \
    ITHAMAPS_DB_PORT=""

# Copy app files
COPY . /srv/shiny-server/

# Pre-build simplified display-geometry caches so the first map load is fast.
# Without this, st_simplify() runs on the 0.5-1 GB boundary files on the first
# map request, making the first global query very slow. Baking the .rds caches
# into the image means the running app just reads them.
RUN cd /srv/shiny-server && Rscript build_caches.R --parallel --cores=3

# Set permissions
RUN chown -R shiny:shiny /srv/shiny-server

# Set the working directory to the location of the Shiny app
WORKDIR /srv/shiny-server

# Expose port 3838 for Shiny app
EXPOSE 3838

# Start the Shiny app
CMD ["R", "-e", "shiny::runApp('.', host='0.0.0.0', port=3838)"]