## function for tuning generalized additvie model

gam_fun <- function(data, dep, ind, folds, k_range, seed, size){
  
  smp_list <- list()
  res <- list()
  res_values <- list()
  
  for (a in seq(k_range)){
    
    for (v in 1:folds){
      set.seed(v+seed)
      smpl <- sample((nrow(df)), (nrow(df)*size))
      smp_list[[v]] <- smpl
    }
    
    values = data.frame()
    
    for (i in 1:folds){
      train <- df[smp_list[[i]],]
      test <- df[-smp_list[[i]],]
      
      if(length(ind)==1){
        formula = as.formula(paste0(dep," ~ s(",ind, ", k = k_range[a])"))
        ##}else{
        ## formula = as.formula(paste0(dep, " ~ s( ", paste0(ind,", k = k_range[a]", collapse=" + "),")"))
      }
      gammod = mgcv::gam(formula, data = train)
      
      pred = unlist(predict(gammod, test))
      obsv = unlist(test[dep])
      
      values = rbind(values, round(caret::postResample(pred,obsv),2))
      
      #values[[i]] = data.frame(RMSE = values[1], RSQ = values[2], MAE = values[3])
    }
    
    #values$knots = k_range[a]
    values$knots <- k_range[a]
    names(values) <- c("RMSE", "RSQ", "MAE", "knots")
    res[[a]] <- values
  }
  
  return(res)
}