# We rely on pkgdepends from the library embedded in pak
install.packages("pak", repos = "https://r-lib.github.io/p/pak/stable/")
install.packages("jsonlite")
.libPaths(c(system.file("library", package = "pak"), .libPaths()))
pkgdepends::new_pkg_installation_proposal(
  names(jsonlite::read_json("renv.lock")$Packages), config = list(dependencies = FALSE)
)$solve()$install_sysreqs()
