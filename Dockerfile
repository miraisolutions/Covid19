FROM rocker/r-ver:4.3.2 AS builder
# We rely on version-stable Ubuntu-based r-ver as it is more convenient and compact
# than Debian-based r-base. Note that repos in any case overwitten by renv.

WORKDIR /home/app

# System dependencies for the locked packages
COPY renv.lock renv.lock
COPY ci-cd/install-sysreqs.R ci-cd/install-sysreqs.R
RUN Rscript ci-cd/install-sysreqs.R

# Copy renv configuration and restore dependencies
# - .Rprofile enables renv's autoload and bootstrapping
# - DESCRIPTION is needed here for the explicit snapshot type
# NOTE:
#   We restore the full set of locked dependencies, but we ultimately
#   want to only keep runtime dependencies (see below)
COPY renv/settings.json renv/settings.json
COPY renv/activate.R renv/activate.R
COPY .Rprofile .Rprofile
COPY DESCRIPTION DESCRIPTION
# we use a separate command to bootstrap renv, so it can be cached
RUN R -e "renv::status(dev=TRUE)"
RUN R -e "renv::restore()"

# Create an isolated library with runtime dependencies only
ENV RUNTIME_LOCKFILE=/home/build/renv.lock
ENV RUNTIME_LIBRARY=/home/build/library
# create lockfile without development dependencies
RUN R -e "renv::snapshot(dev = FALSE, lockfile='$RUNTIME_LOCKFILE')"
# restore runtime dependencies in a dedicated and isolated runtime library
# (this leverages the renv cache populated when restoring all dependencies)
RUN R -e \
  "options(renv.config.cache.symlinks = FALSE);\
  renv::restore(lockfile='$RUNTIME_LOCKFILE', library = '$RUNTIME_LIBRARY', exclude = 'renv')"

# Install app package from source in the runtime library
COPY NAMESPACE NAMESPACE
COPY R R
COPY inst inst
RUN R -e "renv::install('local::.', library = '$RUNTIME_LIBRARY', dependencies = FALSE)"


FROM rocker/r-ver:4.3.2 AS main

# System dependencies for the locked runtime packages
WORKDIR /tmp/sysreqs
COPY --from=builder /home/build/renv.lock renv.lock
COPY ci-cd/install-sysreqs.R install-sysreqs.R
RUN Rscript install-sysreqs.R && rm -rf /tmp/sysreqs
WORKDIR /

# Runtime library from builder stage (to be safe, same path as in builder)
ENV R_LIBS_USER=/home/build/library
COPY --from=builder $R_LIBS_USER $R_LIBS_USER

# Expose port
EXPOSE 80

# Run the app
CMD ["R", "-e", "options(shiny.port = 80, shiny.host = '0.0.0.0', golem.app.prod = TRUE); Covid19Mirai::run_app()"]
