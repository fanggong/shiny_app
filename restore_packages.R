
dirs <- dir()
dirs <- dirs[file.info(dirs)$isdir]

for (each in dirs) {
  renv::restore(each)
  renv::isolate(each)
}
