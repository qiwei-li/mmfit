mmfit=function(g, x, start){
  g = ifelse(class(g)=="character", builtInDists(g), g)
  
  res = gmm(g=g, x=x, t0=start)
  thetahat = res$coefficients
  thetahatses = sqrt(diag(res$vcov))
  denscomp = do_denscomp(x,g,thetahat)
  cdfband = do_cdfband(x,g,thetahat)
  
  obj = structure(list(thetahat = thetahat,
                       thetahatses = thetahatses,
                       denscomp = denscomp,
                       cdfband = cdfband), class="mmfit")
  return(obj)
}


