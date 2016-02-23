mmfit=function(g, x, start){
  g = ifelse(class(g)=="character", builtInDists(g), g)
  
  res = gmm(g=g, x=x, t0=start)
  coef = res$coefficients
  se = sqrt(diag(res$vcov))
  d <- ggplot(data.frame(x), aes(x=x)) + geom_histogram() 
  d <- d + geom_density(data=data.frame())
  
  obj = structure(list(coef = coef,
                       se = se,
                       d = d), class="mmfit")
  return(obj)
}


