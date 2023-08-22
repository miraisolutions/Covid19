# deploy/deploy-shinyapps.R
# usethis::use_build_ignore("deploy")
rsconnect::setAccountInfo(
  Sys.getenv("SHINYAPPS_ACCOUNT"),
  Sys.getenv("SHINYAPPS_TOKEN"),
  Sys.getenv("SHINYAPPS_SECRET")
)
rsconnect::deployApp(
  account = "miraisolutions",
  appName = "Covid19",
  forceUpdate = TRUE
  # exclude hidden files and renv directory (if present)
  # appFiles = setdiff(list.files(), "renv")
)
