## Leave-many-out cross-validation

cvfunction = lapply(seq(100), function(i){
  set.seed(i)
  smpl =  sample(nrow(df), nrow(df)*0.8)
  train = frame[smpl,]
  test = frame[-smpl,]
  lmod = lm(dep ~ ind)
  pred = predict(lmod, newdata = test)
  obsv = test$dep
  resid = obsv-pred
  ss_obsrv = sum((obsv - mean(obsv))**2)
  ss_model = sum((pred - mean(obsv))**2)
  ss_resid = sum((obsv - pred)**2)
  mss_obsrv = ss_obsrv / (length(obsv) - 1)
  mss_model = ss_model / 1
  mss_resid = ss_resid / (length(obsv) - 2)
  data.frame(pred = pred,
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
})