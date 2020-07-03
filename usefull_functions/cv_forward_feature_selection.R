cvffs <- function(data, dep, vars, selected_vars = NULL, switch = c("cv", "intern"), sam = 0.8, folds, seed, size, plot = "yes"){
  if (switch == "cv"){
    cv <- function(data, dep, vars, folds, size, seed, selected_vars = NULL){
      for (v in 1:folds){
        set.seed(v+seed)
        smpl <- sample((nrow(data)), (nrow(data)*size))
        train <- data[smpl,]
        test <- data[-smpl,]
        x <- 0
        vars_org <- vars
        if (v == 1){
          results <- data.frame()
        }
        while(x < length(vars)){
          if(!is.null(selected_vars)){
            vars_org <- vars[-which(vars %in% selected_vars)]
          }
          for (i in 1:length(vars_org)){
            if(is.null(selected_vars)){
              formula <- paste(dep, " ~ ", paste(vars_org[i], collapse=" + "))
            } else {
              formula <- paste(dep, " ~ ", paste(c(selected_vars, vars_org[i]), collapse=" + "))
            }
            lmod <- lm(formula, data = train)
            pred <- predict(lmod, newdata = test)
            obsv <- test[,dep]
            resid <- obsv-pred
            ss_obsrv <- sum((obsv - mean(obsv))**2)
            ss_model <- sum((pred - mean(obsv))**2)
            ss_resid <- sum((obsv - pred)**2)
            mss_obsrv <- ss_obsrv / (length(obsv) - 1)
            mss_model <- ss_model / 1
            mss_resid <- ss_resid / (length(obsv) - 2)
            v_met <- data.frame(pred = pred,
                                obsv = obsv,
                                resid = resid,
                                ss_obsrv = ss_obsrv,
                                ss_model = ss_model,
                                ss_resid = ss_resid,
                                mss_obsrv = mss_obsrv,
                                mss_model = mss_model,
                                mss_resid = mss_resid,
                                r_squared = ss_model / ss_obsrv
            )
            ss_obsrv_zus <- sum((v_met$obsv - mean(v_met$obsv))**2)
            ss_model_zus <- sum((v_met$pred - mean(v_met$obsv))**2)
            ss_resid_zus <- sum((v_met$obsv - v_met$pred)**2)
            
            mss_obsrv_zus <- ss_obsrv / (length(v_met$obsv) - 1)
            mss_model_zus <- ss_model / 1
            mss_resid_zus <- ss_resid / (length(v_met$obsv) - 2)
            met <- data.frame(NAME = c("cross-validation F value",
                                       "linear model F value",
                                       "cross-validatino r squared",
                                       "lienar model r squared",
                                       "formula"),
                              VALUE = c(round(mss_model / mss_resid, 6),
                                        round(anova(lmod)$'F value'[1], 6),
                                        round(1 - ss_resid / ss_obsrv, 6),
                                        round(summary(lmod)$r.squared, 6),
                                        as.character(formula)))
            if (i == 1 && is.null(selected_vars)){ #v==1 eventuell #Muss für neuen Fold leer sein. + neue speicher variablen für einzelne folds
              met_all <- data.frame()
              selected_val <- c()
            }
            met_all <- rbind(met, met_all)
          }
          selrow <- which(met_all$VALUE == max(as.numeric(as.character(met_all$VALUE[met_all$NAME == "cross-validatino r squared"]))))
          selval <- max(as.numeric(as.character(met_all$VALUE[met_all$NAME == "cross-validatino r squared"])))
          if (x != 0){
            if (selval < max(selected_val)){
              print(paste("fold:",v, "max r-squared (break)"))
              break
            }
          }
          selected_val <- c(selected_val, selval)
          selected_word <- stringr::word(met_all$VALUE[selrow + 2], -1)
          selected_vars <- c(selected_vars, selected_word)
          x <- x + 1
          met_all <- data.frame()
        }
        temp <- data.frame(v, selected_vars, selected_val)
        results <- rbind(results, temp)
        selected_vars <- NULL
        
      }
      return (results)
    }
    results_cv <- cv(data=data, dep = dep, vars = vars, folds = folds, size = size, seed = seed, selected_vars = NULL)
    
    if (plot == "yes"){
      results_cv_l <- split(results_cv, f = results_cv$v)
      par_org <- par()
      par(mfrow = c(round(length(results_cv_l)/2, 0), 2))
      for (k in 1:length(results_cv_l)){
        plot(results_cv_l[[k]][[3]], type = "l", main = paste("Maximum R-Squared of CV Fold", k ), 
             ylab = "R-Squared", xlab = "Number of selected variables")
      }
      par(par_org)
    }
    
  }
  else if (switch == "intern"){
    ffs <- function(data, dep, vars, selected_vars = NULL){
      forward_feature_selection <- function(data, dep, vars, selected_vars = NULL){
        fwd_fs <- lapply(seq(length(vars)), function(v){
          if(is.null(selected_vars)){
            formula <- paste(dep, " ~ ", paste(vars[v], collapse=" + "))
          } else {
            formula <- paste(dep, " ~ ", paste(c(selected_vars, vars[v]), collapse=" + "))
          }
          lmod <- lm(formula, data = data)
          results <- data.frame(Variable = vars[v],
                                Adj_R_sqrd = round(summary(lmod)$adj.r.squared, 4),
                                AIC = round(AIC(lmod), 4))
          return(results)
        })
        fwd_fs <- do.call("rbind", fwd_fs)
        
        if(!is.null(selected_vars)){
          formula <- paste(dep, " ~ ", paste(selected_vars, collapse=" + "))
          lmod <- lm(formula, data = data)
          results_selected <- data.frame(Variable = paste0("all: ", paste(selected_vars, collapse=", ")),
                                         Adj_R_sqrd = round(summary(lmod)$adj.r.squared, 4),
                                         AIC = round(AIC(lmod), 4))
          fwd_fs <- rbind(results_selected, fwd_fs)
        }
        
        best_var <- as.character(fwd_fs$Variable[which(fwd_fs$AIC == min(fwd_fs$AIC))])
        return(fwd_fs)
      }
      act_var <- forward_feature_selection(data = data, dep = dep, vars = vars)
      if (is.null(selected_vars)){
        selected_vars <- act_var[act_var$AIC == min(act_var$AIC[2:length(act_var$AIC)]),]
      }
      
      for (i in (1:(length(vars)))){
        
        if (act_var$AIC[1] < min(act_var$AIC[2:length(act_var$AIC)])){
          selected_vars <- rbind(selected_vars, act_var[act_var$AIC == min(act_var$AIC[2:length(act_var$AIC)]),])
          act_var <- act_var[-which(as.character(act_var$Variable) %in% as.character(selected_vars$Variable)),]
          print("optimal AIC value reached")
          break
        }
        if (i != 1){
          selected_vars <- rbind(selected_vars, act_var[act_var$AIC == min(act_var$AIC[2:length(act_var$AIC)]),])
        }
        act_var <- act_var[-which(as.character(act_var$Variable) %in% as.character(selected_vars$Variable)),]
        if (exists("selected_vars") == TRUE && i == 1){
          act_var <- forward_feature_selection(data = data, dep = dep, vars = as.character(act_var$Variable),
                                               selected_vars = as.character(selected_vars$Variable))
        } else if (act_var$AIC[1] > min(act_var$AIC[2:length(act_var$AIC)]) && i > 1){
          act_var <- forward_feature_selection(data = data, dep = dep, vars = as.character(act_var$Variable[-1]),
                                               selected_vars = as.character(selected_vars$Variable))
        }
      }
      return(selected_vars)
    }
    results_int <- ffs(data = data, dep = dep, vars = vars)
    if (plot == "yes"){
      print(ggplot2::ggplot(data=results_int, aes(x=reorder(Variable,-AIC),y=AIC, group = 1))+
              geom_line(color = "blue")+
              geom_point(shape = 19, col = "blue", fill = "blue")+
              scale_color_brewer(palette = "Paired")+
              theme(plot.title = element_text(hjust=0.5, face = "bold"), axis.text = element_text(size = 10),axis.title.x = element_text(size = 14, margin = margin(t = 10, r = 0, b = 0, l = 0)), 
                    axis.title.y = element_text(size = 14, margin = margin(t = 0, r = 20, b = 0, l = 0)), title = element_text(size = 16), 
                    legend.position = "bottom", legend.background = element_rect(fill = "transparent"), legend.title = element_text(size = 22, face = "bold"), legend.text = element_text(size = 22))+
              labs(x= "Variables", title = "Forward feature selection with internal validation"))
    }
  }
  if (switch == "cv"){
    return(results_cv)
  }
  else if (switch == "intern"){
    return(results_int)
  }
}