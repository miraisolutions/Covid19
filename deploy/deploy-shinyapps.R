# deploy/deploy-shinyapps.R
# usethis::use_build_ignore("deploy")
rsconnect::setAccountInfo(
  Sys.getenv("SHINYAPPS_ACCOUNT"),
  Sys.getenv("SHINYAPPS_TOKEN"),
  Sys.getenv("SHINYAPPS_SECRET")
)

# Add here any additional files/directories the app needs
app_files = c(
  "app.R",
  "DESCRIPTION",
  "NAMESPACE",
  "R/",
  "inst/"
)

rsconnect::deployApp(
  account = "miraisolutions",
  appName = "covid19-test",
  forceUpdate = TRUE # to be added since rsconnect >1
  # exclude hidden files and renv directory (if present)
  # appFiles = setdiff(list.files(), "renv")
)
