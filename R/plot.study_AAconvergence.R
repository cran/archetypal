plot.study_AAconvergence=function(x, ...){
  #
  #
  # Check class
  #
  if(!inherits(x,"study_AAconvergence")){stop("Error, input must be an object of class 'study_AAconvergence'")} #RE SET!
  #
  #
  sse=x$SSE
  ssely=x$SSE_lowess
  sselx=1:length(sse)
  uiklowess=x$UIK_lowess
  daitken=x$aitken
  p_est=x$order_estimation
  c_est=x$rate_estimation
  dcp=x$significance_estimations
  PCS=x$used_on_convexhull
  chvertices=x$chvertices
  archsaligned=x$aligned_archetypes
  aa=x$solution_used
  ifinals=(uiklowess+1):length(sse)
  Blist=x$solution_used$run_results$Blist
  Bfinals=Blist[ifinals]
  names(Bfinals)=ifinals
  archslist=x$solution_used$run_results$archslist
  archsfinals=archslist[ifinals]
  names(archsfinals)=ifinals
  #
  #
  #################################
  # Use Convex Hull vertices or not
  #################################
  #
  ifelse(!is.null(chvertices),{ch=chvertices;hull=TRUE},{ch=NULL;hull=FALSE})
  #
  #
  #
  ### MAIN PLOT
  #
  #
  def.par <- par(no.readonly = TRUE)
  par(mar = c(2.5, 3.5, 1.5, 0.5))
  par(mgp = c(1.5, 0.5, 0))
  mai = c(1, 1, 0.1, 0.1)
  par(oma = c(0, 0, 0, 0))
  #
  layout(matrix(c(1,1,2,3,4,5), 3, 2, byrow = TRUE),respect = FALSE)
  #1
  #
  x1=1:length(sse)
  y1=sse
  # ! sse
  plot(x1,y1,type='b',xlab="iteration",ylab="", pch=19,cex=0.5, 
       main = "UIK: all SSE", cex.axis = 0.8, tcl = -0.4, las = 1)
  knee1=uik(x1,y1)
  abline(v=knee1)
  text(x=knee1+2,y=max(y1), knee1, font = 2)
  legend("topright",pch=c(19,NA),lty=c(2,1),col=c('black','black'),
         legend=c("SSE","UIK"),bty="n")
  grid()
  #2
  plot(x1,y1,type='b',xlab="iteration", ylab="", pch = 19, cex = 0.5,
       main = "UIK: lowess(SSE)", cex.axis = 0.8, tcl = -0.4, las = 1)
  # ! ssel
  lines(sselx,ssely,col='blue',lwd=2,lty=1)
  abline(v=uiklowess,col='blue')
  text(x=uiklowess+2,y=max(y1),uiklowess, font = 2)
  legend("topright",pch=c(19,NA,NA),lty=c(4,1,1),lwd=c(1,2,1),
         col=c('black','blue','blue'),legend=c("SSE","lowess(SSE)","UIK"),bty="n")
  grid()
  #3
  # ! ifinals
  x3=ifinals
  y3=sse[x3]
  plot(x3,y3,type='b',xlab="iteration", ylab="", pch=19,cex=0.5, 
       main="UIK: last SSE",cex.axis = 0.8, tcl = -0.4, las = 1)
  knee3=x3[ede(x3,y3,0)[1]]
  abline(v=knee3,col='black')
  text(x=knee3+2,y=max(y3),knee3, font = 2)
  legend("topright",pch=c(19,NA),lty=c(4,1),col=c('black','black'),
         legend=c("SSE","UIK"),bty="n")
  grid()
  #4
  if(hull){
    plot(daitken$i,daitken$pxi,type='b',pch=19,cex=0.5, xlab="iteration", ylab="",
         main="Aitken extrapolation SSE",cex.axis = 0.8, tcl = -0.4, las = 1)
    grid()
  }else{
    # extrapolation
    plot(daitken$i,daitken$pxi,type='b',pch=19,cex=0.5, xlab="iteration", ylab="",
         main="Aitken extrapolation SSE",cex.axis = 0.8, tcl = -0.4, las = 1)
    grid()
    # error
    plot(daitken$i,daitken$ei,type='b',pch=19,cex=0.5, xlab="iteration", ylab="",
         main="Aitken error SSE",cex.axis = 0.8,  tcl = -0.4, las = 1)
    grid()
  }
  #5
  if(hull){
    plot(ifinals,PCS,ylim=c(0,100),type='b',pch=19, xlab="iteration", ylab="",
         main="% of used rows on CH",cex.axis = 0.8,  tcl = -0.4, las = 1)
    grid()
    }
  #
  invisible(par(def.par))
  #
  #  
}