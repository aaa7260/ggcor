.onAttach  <- function(libname, pkgname )
{
  colours <- c("#67001F", "#B2182B", "#D6604D","#F4A582", "#FDDBC7",
               "#F7F7F7", "#D1E5F0", "#92C5DE", "#4393C3", "#2166AC",
               "#053061")
  options(ggcor.fill.pal =colours )
}