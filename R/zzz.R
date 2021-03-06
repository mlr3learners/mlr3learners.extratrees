#' @import data.table
#' @import paradox
#' @import mlr3misc
#' @importFrom R6 R6Class
#' @importFrom mlr3 mlr_learners LearnerClassif LearnerRegr
"_PACKAGE"

# nocov start
register_mlr3extratrees = function(libname, pkgname) {
  # get mlr_learners dictionary from the mlr3 namespace
  x = utils::getFromNamespace("mlr_learners", ns = "mlr3")

  # add the learner to the dictionary
  x$add("classif.extratrees", LearnerClassifExtraTrees)
  x$add("regr.extratrees", LearnerRegrExtraTrees)
}

.onLoad = function(libnameb, pkgname) { # nolint
  register_mlr3extratrees()
  setHook(packageEvent("mlr3", "onLoad"), function(...) register_mlr3extratrees(),
    action = "append")
}

.onUnload = function(libpath) { # nolint
  event = packageEvent("mlr3", "onLoad")
  hooks = getHook(event)
  pkgname = vapply(hooks, function(x) environment(x)$pkgname, NA_character_)
  setHook(event, hooks[pkgname != "mlr3learners.extratrees"],
    action = "replace")
}
# nocov end
