plot.kappa_tools=function(x, ...){  
  #
  # Check class
  #
  if(!inherits(x,"kappa_tools")){stop("Error, input must be an object of class 'kappa_tools'")} #RE SET!
  #
  # READ OUTPUT
  #
  dxy=x$ecdf
  xe=dxy$x
  ye=dxy$y
  puik_ecdf=x$UIK
  inflection_ecdf=x$INFLECTION
  rused=x$RowsUsed
  nrowsused=x$NumberRowsUsed
  # CORRESPONDING UIK KNOT
  ipuik=which(xe==puik_ecdf)
  # CORRESPONDING PERCENTAGE
  pcipuik=ye[ipuik]
  #
  # BEGIN PLOT
  #
  #
  def.par <- par(no.readonly = TRUE)
  par(mar = c(3.5, 4, 3.5, 0.5))
  par(mgp = c(1.5, 0.5, 0))
  mai = c(1, 1, 0.5, 0.1)
  par(oma = c(0.5, 0.5, 0.5, 0.5))
  #
  layout(matrix(c(1,1,2,2), 2, 2, byrow = TRUE),respect = FALSE)
  #
  # 1 PLOT ECDF OF SE
  #
  txpc=paste0("The ECDF of SE is getting flatten after the ",
              as.character(round(100*pcipuik,2))," % of it")
  plot(xe, ye, xaxs = "i", yaxs = "i", pch = 19, 
       xaxt="n", yaxt="n",
       cex = 0.5, xlab = "SE", ylab = "")
  xtiks=pretty(xe)
  xlabs=round(xtiks,3)
  axis(1,at=xtiks, labels = xlabs, las=2)
  puikinf=c(inflection_ecdf,puik_ecdf)
  axis(1,at=puikinf, labels = round(puikinf,2),las=2)
  axis(2,at=seq(0,1,by=0.1),las=2)
  abline(v=puik_ecdf,h=pcipuik, col="blue")
  abline(v=inflection_ecdf,col="red")
  mtext("Cumulative Probability", side = 2, line = 3)
  mtext(txpc,3,2)
  #
  # 2 PLOT NUMBER OF USED ROWS
  #
  txnrowsused=paste0("The totally used rows were ",nrowsused)
  plot(rused,type="h", lwd=15,col="lightgray", yaxt="n",
       xlab="Archetypal row",ylab="")     
  axis(2,at=round(pretty(1:max(rused))))
  mtext("Number of used rows", side = 2, line = 2)
  mtext(txnrowsused,3,2)
  #
  invisible(par(def.par))
  #
  #
}