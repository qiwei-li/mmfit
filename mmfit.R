mmfit=function(g,x,start){
  if(is.null(g) || is.null(x) || is.null(start)){
    stop("ERROR: input cannot be null")
  }
  
  if(ncol(x)==length(g)){
    obj = list(g=g,x=x,start=start)
    obj = subst(obj)
  } else if (length(g)==1){
    g = as.list(rep(g, ncol(x)))
    obj = list(g=g,x=x,start=start)
    obj = subst(obj)
  } else{
    stop("Error: g is not well defined.")  
  }
  
  res = lapply(1:length(obj$g), function(i) {
    gmm(g=obj$g[[i]], x=obj$x[,i], t0=obj$start[[i]])}
  )
  class(res) = "mmfit"
  return(res)
}


