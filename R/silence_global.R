# hack to keep CRAN happy and suppress NOTES about
# parts of the code that use non-standard evaluation.
# See:
# http://stackoverflow.com/questions/9439256/how-can-i-handle-r-cmd-check-no-visible-binding-for-global-variable-notes-when
# https://github.com/smbache/magrittr/issues/29
# Adapted from:
# https://github.com/cran/missCompare/blob/00339e0525029a78cb282961b81e636df6c939fd/R/silence_global.R

.onLoad <- function(libname, pkgname)
{
  # make data set names global to avoid CHECK notes
  utils::globalVariables("i")
  utils::globalVariables(".")

  invisible()
}
