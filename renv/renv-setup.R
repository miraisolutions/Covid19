# This script allows maintaining a reproducible way of tracking versions of
# package dependencies using renv, starting from all dependencies bound to a
# specific MRAN date and cherry-picking the updates.

# One-off Initialization of renv
# NOTE: you can ignore the snapshot.type warning
if (FALSE) {
  renv::init(
    # use the DESCRIPTION file to capture dependencies
    settings = list(snapshot.type = "explicit"),
    # do not install dependencies (done in a custom way)
    bare = TRUE
  )
}

# Upgrade renv, to also ensure 'explicit' snapshot type is supported (>= 0.10.0)
options(repos = "https://cloud.r-project.org")
renv::upgrade()

# Install all dependencies from a specific MRAN date repo
# options(repos = "https://mran.microsoft.com/snapshot/2020-08-03")
# options(repos = "https://mran.microsoft.com/snapshot/2020-07-16")
options(repos = "https://cran.microsoft.com/snapshot/2022-10-31")

# NOTE that renv would only enforce the given repo for the top-level
# dependencies, others seem to depend on what is found in the renv cache or was
# already installed => use remotes to get the full tree of dependencies with the
# version as in the specific repo.
# NOTE: the "recommended" CRAN packages (e.g. mgcv, nlme) are not covered by
# this process as they are normally kept bound to the version of R
renv::install("remotes")
# retrieve versioned dependencies for the specific repo
(deps <- remotes::dev_package_deps(dependencies = TRUE))
# install the specific versions if not already installed
renv::install(with(deps, sprintf("%s@%s", package[diff!=0], available[diff!=0])))

# Set the CRAN repo and cherry-pick version-constrained updates
options(repos = "https://cloud.r-project.org")
renv::install("COVID19@3.0.2")
# Alternatively, set a newer MRAN date and update from there
# options(repos = "https://mran.microsoft.com/snapshot/2020-10-12")
# renv::update("COVID19")

# Create the renv snapshot, making sure CRAN is set as repo, which will also end
# up in the lockfile. This allows restoring any version regardless of the
# corresponding MRAN date, since CRAN includes archives for older versions.
options(repos = "https://cloud.r-project.org");
renv::snapshot()
