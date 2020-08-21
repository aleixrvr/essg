evaluate_predictions <- function(model, test, outcome){
  real <- test[, outcome]
  prediction <- predict(model, test)
  caret::postResample(pred = prediction, obs = real)
}