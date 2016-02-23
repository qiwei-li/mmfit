mmfit=function(g, x, start){

  if(class(g)=="character"){
    self = FALSE
    orig_g = g
    g = builtInDists(g)
  } else {
    self = TRUE
  }
  
  res = gmm(g=g, x=x, t0=start)
  thetahat = res$coefficients
  thetahatses = sqrt(diag(res$vcov))
  if(self){
    denscomp = NULL
  } else {
    denscomp = do_denscomp(orig_g,x,thetahat)
  }
  cdfband = do_cdfband(x)
  
  obj = structure(list(thetahat = thetahat,
                       thetahatses = thetahatses,
                       denscomp = denscomp,
                       cdfband = cdfband), class="mmf")
  return(obj)
}


