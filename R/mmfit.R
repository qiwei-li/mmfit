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
  
  denscomp = tryCatch({do_denscomp(g,x,thetahat, gd)},
                      error = function(err){ 
                        print("Can't visualize multivariate data")
                        return(NULL)
                        })
  cdfband = tryCatch({do_cdfband(x,g,thetahat,gd)},error = function(err){ 
                        print("Can't visualize multivariate data")
                        return(NULL)})
  
  obj = structure(list(thetahat = thetahat,
                       thetahatses = thetahatses,
                       denscomp = denscomp,
                       cdfband = cdfband), class="mmf")
  return(obj)
}


