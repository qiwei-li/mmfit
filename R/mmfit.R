mmfit=function(g, x, start){

  if(class(g)=="character"){
    orig_g = g
    g = builtInDists(g)
  }
  res = gmm(g=g, x=x, t0=start)
  thetahat = res$coefficients
  thetahatses = sqrt(diag(res$vcov))
  denscomp = do_denscomp(orig_g,x,thetahat)
  cdfband = do_cdfband(g,x,thetahat)
  
  obj = structure(list(thetahat = thetahat,
                       thetahatses = thetahatses,
                       denscomp = denscomp,
                       cdfband = cdfband), class="mmfit")
  return(obj)
}


