
if (!require(renv)) {
  install.packages("renv", repos = "http://cran.us.r-project.org")
}

dirs <- dir()
dirs <- dirs[file.info(dirs)$isdir]

for (each in dirs) {
  renv::restore(each)
  renv::isolate(each)
}
