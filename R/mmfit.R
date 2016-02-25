mmfit=function(g, x, gd=NULL, start){
  
  if(class(g)=="character"){
    self = FALSE
    orig_g = g
    g = builtInDists(g)
  } else {
    self = TRUE
    if(is.null(gd))
      stop("If you use your own g, you need to provide the pmf/pdf as gd")
  }
  
  res = gmm(g=g, x=x, t0=start)
  thetahat = res$coefficients
  thetahatses = sqrt(diag(res$vcov))
  denscomp = do_denscomp(orig_g,x,thetahat, gd)

  
  if(self){
    cdfband = NULL
  } else {
    cdfband = do_cdfband(x,orig_g,thetahat)
  }
  
  obj = structure(list(thetahat = thetahat,
                       thetahatses = thetahatses,
                       denscomp = denscomp,
                       cdfband = cdfband), class="mmf")
  return(obj)
}


