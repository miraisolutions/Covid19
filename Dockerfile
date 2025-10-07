FROM rocker/r-ver:4.3.2
# We rely on version-stable Ubuntu-based r-ver as it is more convenient and compact
# than Debian-based r-base. Note that repos in any case overwitten by renv.

WORKDIR /home/app

# System dependencies for the locked packages
COPY renv.lock renv.lock
COPY deploy/install-sysreqs.R deploy/install-sysreqs.R
RUN Rscript deploy/install-sysreqs.R

# renv configuration and environment
# - .Rprofile enables renv's autoload and bootstrapping
# - DESCRIPTION is needed here for the explicit snapshot type
COPY renv/settings.json renv/settings.json
COPY renv/activate.R renv/activate.R
COPY .Rprofile .Rprofile
COPY DESCRIPTION DESCRIPTION
# we use a separate command to bootstrap renv, so it can be cached
RUN R -e "renv::status(dev=TRUE)"
RUN R -e "renv::restore()"

# Install app package from source
COPY NAMESPACE NAMESPACE
COPY R R
COPY inst inst
RUN R -e "renv::install('local::.', dependencies = FALSE)"

# Expose port
EXPOSE 80

# Run the app
CMD ["R", "-e", "options(shiny.port = 80, shiny.host = '0.0.0.0', golem.app.prod = TRUE); Covid19Mirai::run_app()"]
