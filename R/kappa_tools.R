kappa_tools <- function(x, ...) UseMethod("kappa_tools")
#
kappa_tools <- function(aa, df = NULL, numBins = 100, 
                        chvertices = NULL,
                        verbose = FALSE, ...){
  
  # requires "entropy" package
  #
  # df: the set of cases from which the archetypes were derived
  # aa: the output from "archetypal" for n_arch = one value
  # numBins: number of bins for discretization step in calculating entropy
  # 
  #
  if(!is.null(df)){
    Y <- df # the initial data frame of observations as matrix
  }else{
    Y <- aa$Y # the initial data frame of observations as matrix
  }
  rownames(Y)=1:dim(Y)[1]
  #
  if(is.null(numBins)){numBins=100} 
  #
  SSE=aa$SSE
  size=dim(Y)[1]
  A <- aa$A # the case weights
  n_arch=dim(A)[2]
  kk=n_arch
  B <- aa$B # # the other weights, sparse matrix
  BY <- aa$BY # the archetypes
  # kk <- dim(BY)[1]
  # main
  #
  #############################################################################
  # Compute: Estimations, Residuals, Squared Residuals (Squared Errors, SE)
  #############################################################################
  #
  Y_est <- A%*%BY # the estimation of Y ~ A B Y
  Y_resid=as.matrix(Y-Y_est) # the resiluals
  Y_resid2=rowSums(Y_resid^2) # the squared residuals
  ser=Y_resid2 # squared errors abbreviation
  #
  #
  #############################
  ####### FIND ECDF of SE
  #############################
  #
  #
  ce=ecdf(ser)
  xe=knots(ce)
  ye=ce(xe)
  check_curve(xe,ye)
  # UIK
  puik_ecdf=xe[ede(xe,ye,0)[2]]
  puik_ecdf
  # CORRESPONDING KNOT
  ipuikecdf=which(xe==puik_ecdf)
  # CORRESPONDING PERCENTAGE
  pcipuikecdf=ye[ipuikecdf]
  txipuikecdf=paste0("The ECDF of SE is getting flatten after the ",
                     as.character(round(100*pcipuikecdf,2))," % of it")
  if(verbose){cat(txipuikecdf,"\n")}
  #
  ########################
  # FIND INFLECTION POINT
  ########################
  #
  bb=ede(xe,ye,0)
  bb
  inflection_ecdf=bb[3]
  #
  #############################
  # Data Frame of ECDF for SE
  #############################
  #
  dxy=data.frame("x" = xe, "y" = ye)
  #
  ####################################################
  # CHECK APPROXIMATE CONVEXITY TYPE OF THE ECDF CURVE
  ####################################################
  #
  cxcv <- as.numeric(!as.logical(check_curve(xe,ye)$index))
  cxcv
  #
  #
  ###################################################################
  ### CHECK HOW MANY POINTS HAVE BEEN USED FOR EACH ARCHETYPAL ROW 
  ###################################################################
  #
  #
  if(!is.null(chvertices)){
    # GIVEN CONVEX HULL VERTICES
    cb=check_Bmatrix(aa$B, chvertices = chvertices,  verbose = F)
    use_chull = TRUE
  }else{
    # NOT GIVEN CONVEX HULL VERTICES
    cb=check_Bmatrix(aa$B,verbose = F)
    use_chull = FALSE
  }
  #
  #####################################
  # Count used rows for creating archs
  #####################################
  #
  rused=sapply(cb$used_weights,length)
  nrowsused=sum(rused)
  #
  txnrowsused=paste0("The totally used rows were ",nrowsused)
  if(verbose){cat(txnrowsused,"\n")}
  #
  #############################################################################
  # Find percentage of used rows that lie on Convex Hull, if CH is available
  #############################################################################
  #
  ifelse(use_chull,{cxh=100*cb$used_on_convexhull},{cxh=NA})
  #
  ################
  #### CALC BICs
  ################
  #
  # main
  fit2 <- sum(Y_resid2) # optimized
  var_resid <- var(Y_resid2)
  p <- size*(n_arch - 1) + length(B[B>0])  + 1 
  # free parameters, here A, B > 0 & +1 for the variance estimate
  #
  # now a BIC scoring formula 
  # 1. FIT
  # fit <- sum(Y_resid^2) #ORIGINAL
  # note: this will be similar but not the same SSE as for the input AA solution
  # it is the fit of the overall case solutions as above and consistent with the
  # definition of residuals used here
  #
  # calculate adjusted BIC
  # builds on Murari et al "Entropy" 2019 for residual distributions diverging 
  # from Gaussian IID. Favors solutions with more uniformly distributed residuals.
  # 
  if(numBins < 10){cat("WARNING: too few bins?\n")}
  disc_resid <- discretize(Y_resid2,numBins = numBins)
  res_ent <- entropy(disc_resid) # default is natural units 
  fit22 <- fit2/res_ent
  #
  # 2. PENALTY
  penalty <- 2 * var_resid * p * log(size/p) 
  # Abramovitch et al 2006 Annals of Statistics for models with many free parameters
  #
  BIC <- {fit2 + penalty}/size 
  # rescale a la Volker Roth presentation
  # allows comparison across different sample sizes
  adjBIC <- {fit22 + penalty}/size
  #
  out=list("ecdf"=dxy,"Convexity"=cxcv, "UIK"=puik_ecdf,"INFLECTION"=inflection_ecdf,
           "NumberRowsUsed"=nrowsused, "RowsUsed"=rused,
           "SSE"=SSE, "BIC"=BIC, "adjBIC" = adjBIC,
           "CXHE"=cxh) 
  #
  class(out)="kappa_tools"
  return(out)
}