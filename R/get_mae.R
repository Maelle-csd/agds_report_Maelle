# Function of k, returning MAE for knn model
get_mae <- function(k, df_train, df_test){
  mod_knn <- caret::train(
    pp, 
    data = df_train |> drop_na(), 
    method = "knn",
    trControl = caret::trainControl(method = "none"),
    tuneGrid = data.frame(k = k),
    metric = "RMSE"
  )
  # add predictions to the data frames
  df_train <- df_train |> 
    drop_na()
  df_train$fitted <- predict(mod_knn, newdata = df_train)
  
  df_test <- df_test |> 
    drop_na()
  df_test$fitted <- predict(mod_knn, newdata = df_test)
  
  # get metrics tables
  metrics_train <- df_train |> 
    yardstick::metrics(GPP_NT_VUT_REF, fitted)
  
  metrics_test <- df_test |> 
    yardstick::metrics(GPP_NT_VUT_REF, fitted)
  
  # extract values from metrics tables
  mae_train <- metrics_train |> 
    filter(.metric == "mae") |> 
    pull(.estimate)
  
  mae_test <- metrics_test |> 
    filter(.metric == "mae") |> 
    pull(.estimate)
  
  return(tibble::tibble(k = k, mae_train = mae_train, mae_test = mae_test))
}  