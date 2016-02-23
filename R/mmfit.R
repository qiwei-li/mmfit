mmfit=function(g, x, start){

  if(class(g)=="character"){
    orig_g = g
    g = builtInDists(g)
  }
  print(orig_g)
  res = gmm(g=g, x=x, t0=start)
  thetahat = res$coefficients
  thetahatses = sqrt(diag(res$vcov))
  denscomp = do_denscomp(orig_g,x,thetahat)
  cdfband = do_cdfband(x)
  
  obj = structure(list(thetahat = thetahat,
                       thetahatses = thetahatses,
                       denscomp = denscomp,
                       cdfband = cdfband), class="mmf")
  return(obj)
}


