.onLoad <- function(libname, pkgname) {
  library.dynam("tableaux", pkgname, libname, now=TRUE)
  .C("HsStart")
  invisible()
}

.onUnLoad <- function(libpath) {
  library.dynam.unload("tableaux", libpath)
  invisible()
}
