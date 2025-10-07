tmp_lib = tempfile()
dir.create(tmp_lib)
.libPaths(tmp_lib)
on.exit(unlink(tmp_lib, recursive = TRUE))

# We rely on jsonlite and pkgdepends from the library embedded in pak
install.packages("pak", repos = "https://r-lib.github.io/p/pak/stable/")
.libPaths(system.file("library", package = "pak"))
pkgdepends::new_pkg_installation_proposal(
  names(jsonlite::read_json("renv.lock")$Packages),
  config = list(dependencies = FALSE)
)$solve()$install_sysreqs()
