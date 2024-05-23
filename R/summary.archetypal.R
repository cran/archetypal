summary.archetypal=function(object,...){
  A=object$A
  B=object$B
  BY=object$BY
  cat(paste0("Number of observations / data rows n = ",dim(A)[1]),"\n")
  cat(paste0("Dimension of data variables d = ",dim(BY)[2]),"\n")
  cat(paste0("Number of archetypes k = ",dim(BY)[1]),"\n")
  cat("\n") 
  cat("Archetypes:","\n")
  cat("\n") 
  print(data.frame(BY))
  basics=data.frame("SSE"=object$SSE, "VarianceExplained"=object$varexpl, "Convergence"= object$converges, "Iterations"=object$iterations, "ElapsedTime"=object$time)
  cat("\n") 
  cat("Run details:","\n")
  cat("\n") 
  print(basics)
  cat("\n") 
  cat("Call:\n")
  print(object$call)
  return(NULL)
}