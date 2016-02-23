mmfit=function(g, x, start){
  if(is.null(g) || is.null(x) || is.null(start))
    stop("ERROR: input cannot be null")
  
  if(class(g)!="character" || class(g)!="function")
    stop("ERROR: g needs to be a function object or a name of built in functions")
  
  if(class(x)!="integer" || class(g)!="numeric")
    stop("ERROR: x needs to be a vector")
  
  g = ifelse(class(g)=="character", builtInDists(g), g)
  
  res = gmm(g=g, x=x, t0=start)
  coef = res$coefficients
  se = sqrt(diag(a$vcov))
  d <- ggplot(data.frame(x), aes(x=x)) + geom_histogram() 
  d <- d + geom_density(data=data.frame(dbeta(x,1,2)))
  
  obj = structure(list(coef = coef,
                       se = ,
                       d = d), class="mmfit")
  return(obj)
}


