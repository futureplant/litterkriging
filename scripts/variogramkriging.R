# function that derives a variogram model and performs kriging


variogram_kriging <- function(formula, input_data, new_data, nmax = 15){
  "Returns a variogram model, a cross-validation object & the kriging output."
  
  vgm <- autofitVariogram(formula, input_data)
  
  cv <- krige.cv(formula, input_data, vgm$var_model)
  
  kriging <- krige(formula, input_data, new_data, model = vgm$var_model, nmax = nmax)
  
  outputlist <- list(vgm, cv, kriging)
  
  return(outputlist)
}