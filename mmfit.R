mmfit=function(g,x,start){
  if(is.null(g) || is.null(x) || is.null(start)){
    stop("ERROR: input cannot be null")
  }
  
  if(ncol(x)==length(g)){
    obj = structure(list(g=g,x=x,start=start), class=c("mmfit","list"))
    obj = subst(obj)
  } else if (length(g)==1){
    g = as.list(rep(g, ncol(x)))
    obj = structure(list(g=g,x=x,start=start), class=c("mmfit","list"))
    obj = subst(obj)
  } else{
    stop("Error: g is not sufficiently defined.")  
  }
  return(obj)
}


