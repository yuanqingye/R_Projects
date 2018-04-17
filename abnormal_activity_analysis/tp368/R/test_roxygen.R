#' Title
#'
#' @param original_set 
#' 
#' @return train_base
#' @export
#'
#' @examples
preprocess_train_data = function(original_set){
  train_set = original_set[,pickedColums]
  train_base = train_set[complete.cases(train_set),]
  train_base = as.data.frame(train_base)
  # x = train_base[,!(names(train_base) %in% "sign")]
  train_base$sign = as.factor(train_base$sign)
  # y = train_base$sign
  return(train_base)
}