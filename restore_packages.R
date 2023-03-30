
cat("Checking for the presence of renv...\n")

if (!require(renv)) {
  cat("Install package renv...\n")
  install.packages("renv", repos = "http://cran.us.r-project.org")
}

dirs <- dir()
dirs <- dirs[file.info(dirs)$isdir]

cat("Waiting for process...\n")

for (each in dirs) {
  cat("Start process", each, "\n")
  renv::activate(each)
  renv::restore(each, comfirm = FALSE)
  renv::isolate(each)
  cat("Over process", each, "\n")
}
