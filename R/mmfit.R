mmfit=function(g, x, gd=NULL, start){
  
  if(class(g)=="character"){
    g = builtInDists(g)
  } else {
    if(is.null(gd))
      stop("If you use your own g, you need to provide the pmf/pdf as gd")
  }
  
  res = gmm(g=g, x=x, t0=start)
  thetahat = res$coefficients
  names(thetahat) = names(start)
  thetahatses = sqrt(diag(res$vcov))
  names(thetahatses) = names(start)
  
  denscomp = do_denscomp(g,x,thetahat, gd),
  cdfband = do_cdfband(x,g,thetahat,gd)
  
  obj = structure(list(thetahat = thetahat,
                       thetahatses = thetahatses,
                       denscomp = denscomp,
                       cdfband = cdfband), class="mmf")
  return(obj)
}


