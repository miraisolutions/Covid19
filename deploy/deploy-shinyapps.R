# deploy/deploy-shinyapps.R
# usethis::use_build_ignore("deploy")
rsconnect::setAccountInfo(
  Sys.getenv("SHINYAPPS_ACCOUNT"),
  Sys.getenv("SHINYAPPS_TOKEN"),
  Sys.getenv("SHINYAPPS_SECRET")
)

# to be added since rsconnect >1
options(rsconnect.packrat = TRUE)

rsconnect::deployApp(
  account = "miraisolutions",
  appName = "covid19",
  forceUpdate = TRUE # to be added since rsconnect >1
  # exclude hidden files and renv directory (if present)
  # appFiles = setdiff(list.files(), "renv")
)
