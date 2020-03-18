#' @title Regression ExtraTrees Learner
#'
#' @aliases mlr_learners_regr.extratrees
#'
#' @description
#' A [mlr3::LearnerRegr] for a regression extratree implemented in
#' extraTree::extraTree()] in package \CRANpkg{extraTree}.
#'
#' @export
LearnerRegrExtraTrees = R6Class("LearnerRegrExtraTrees", inherit = LearnerRegr,

  public = list(

    #' @description Create a new `LearnerRegrExtraTrees` object.
    initialize = function() {
      ps = ParamSet$new(
        params = list(
          ParamInt$new(id = "ntree", default = 500L, lower = 1L,
            tags = "train"),
          ParamInt$new(id = "mtry", lower = 1L, tags = "train"),
          ParamInt$new(id = "nodesize", default = 1L, lower = 1L,
            tags = "train"),
          ParamInt$new(id = "numRandomCuts", default = 1L, tags = "train"),
          ParamLgl$new(id = "evenCuts", default = FALSE, tags = "train"),
          ParamInt$new(id = "numThreads", default = 1L, lower = 1L,
            tags = "train"),
          ParamLgl$new(id = "quantile", default = FALSE),
          ParamUty$new(id = "subsetSizes", tags = "train"),
          ParamUty$new(id = "subsetGroups", tags = "train"),
          ParamUty$new(id = "tasks", lower = 1, tags = "train"),
          ParamDbl$new(id = "probOfTaskCuts", lower = 0, upper = 1,
            tags = "train"),
          ParamInt$new(id = "numRandomTaskCuts", default = 1L, lower = 1L,
            tags = "train"),
          ParamFct$new(id = "na.action", default = "stop",
            levels = c("stop", "zero", "fuse"), tags = "train")
        )
      )

      super$initialize(
        id = "regr.extratrees",
        packages = "extraTrees",
        feature_types = c("integer", "numeric"),
        predict_types = "response",
        param_set = ps,
        properties = "weights"
      )
    }
  ),

  private = list(

    .train = function(task) {
      pars = self$param_set$get_values(tags = "train")
      data = task$data()
      x = as.matrix(data[, task$feature_names, with = FALSE])
      y = data[, task$target_names, with = FALSE][[1]]

      if ("weights" %in% task$properties) {
        pars = insert_named(pars, list(weights = task$weights$weight))
      }

      invoke(extraTrees::extraTrees, x = x, y = y, .args = pars)
    },

    .predict = function(task) {
      newdata = task$data(cols = task$feature_names)

      p = invoke(predict, self$model, newdata = newdata)
      PredictionRegr$new(task = task, response = p)
    }
  )
)
