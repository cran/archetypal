find_outmost_projected_convexhull_points=function(df, kappas, npr = 2, rseed = NULL, 
                                                  doparallel = FALSE, nworkers = NULL,
                                                  uniquerows= FALSE){
  # Dimensions of data frame
  n=dim(df)[1]
  d=dim(df)[2]
  # Flatten row names of data frame
  rownames(df)=1:n
  # Check the extreme case of dimension = 1 and change default projection 
  if(d==1){
    if(kappas==1){
      dfmean=mean(df[,1])
      rowsmaxall=which.min(abs(df[,1]-dfmean))
      rowsmaxkappas=rowsmaxall
      out=list("outmost"=rowsmaxkappas,"outmostall"=rowsmaxall,"outmostfrequency"=NA,
               "usedrandom"=NA,"chprojections"=NA,"projected"=NA)
      print(out)
    }else if(kappas==2){
      dmin=min(df[,1])
      dmax=max(df[,1])
      rowmins=which(df[,1]==dmin)
      rowmaxs=which(df[,1]==dmax)
      rowsmaxall=c(rowmins,rowmaxs)
      if(length(rowmins)>1){r1=sample(rowmins,1)}else{r1=rowmins}
      if(length(rowmaxs)>1){r2=sample(rowmaxs,1)}else{r2=rowmaxs}
      rowsmaxkappas=c(r1,r2)
      out=list("outmost"=rowsmaxkappas,"outmostall"=rowsmaxall,"outmostfrequency"=NA,
               "usedrandom"=NA,"chprojections"=NA,"projected"=NA)
    }else{
      dmin=min(df[,1])
      dmax=max(df[,1])
      rowmins=which(df[,1]==dmin)
      rowmaxs=which(df[,1]==dmax)
      rowsmaxall=c(rowmins,rowmaxs)
      rowsmaxkappas = if(length(rowmins)>1 & length(rowmaxs)>1){
        irand=sample.int(n,1)
        ifelse(irand%%2==0,{ichoose=TRUE},{ichoose=FALSE})
        if(ichoose){c(sample(rowmins,kappas-1),sample(rowmaxs,1))}else{c(sample(rowmins,1),sample(rowmaxs,kappas-1))}
      }else if(length(rowmins)<2){
        c(rowmins,sample(rowmaxs,kappas-1))
      }else{
        c(sample(rowmins,kappas-1),rowmaxs)
      }
      out=list("outmost"=rowsmaxkappas,"outmostall"=rowsmaxall,"outmostfrequency"=NA,
               "usedrandom"=NA,"chprojections"=NA,"projected"=NA)
    }
  }else{
    # Check given npr of dimension for the projected subspaces:
    if(npr>d){
      warning(paste0('Given dimension of Projected Space was npr = ',npr,
                     ' was greater than column dimension of data frame\n It was set to npr = ',dim(df)[2]))
      npr=d
    }
    if(npr<1){
      warning(paste0('Given dimension of Projected Space was npr = ',npr,' \n It was set to npr = 2'))
      npr=2
    }
    # Set computation mode parameters
    # Check logical processors and define "nworkers" if necessary
    if(doparallel){
      nwa=parallel::detectCores()
      if(is.null(nworkers)){
        ifelse(nwa>2,{nworkers=nwa-2},{nworkers=nwa})
      }else{
        if(nworkers>nwa){nworkers=nwa}
      }
    }
    # Find all possible monads-pairs-triads-... of variables-columns
    jcombs=combn(1:d,npr)
    # Separate between 1D and other dimensions
    if(npr!=1){
      # Work for 2D, 3D, ... projections:
      ###################################
      # Make list of convex hull for relevant facets -projections- 
      if(npr==d & d<=6){
        if(npr==2){
          # Use 'chull' for n=2 only
          ichall=chull(df)
          ichlist=list(ichall)
          names(ichlist)="1;2"
        }else{
          ch=as.list(convhulln(df,'Fx'))
          ichall=unique(do.call(c,ch))
          ichlist=list(ichall)
          names(ichlist)=paste0(1:npr,collapse = ";")
        }
      }else{
        # Function
        fconvhull=function(j, npr, jcombs, df){
          if(npr==2){
            # Use 'chull' for n=2 only
            out=chull(df[,jcombs[,j]])
          }else {
            chj=as.list(convhulln(df[,jcombs[,j]],'Fx'))
            out=unique(do.call(c,chj))
          }
          return(out)
        }
        environment(fconvhull) <- .GlobalEnv
        # Choose mode and proceed
        if(!doparallel){
          # In serial
          ichlist=lapply(1:dim(jcombs)[2], fconvhull, npr=npr, jcomb=jcombs, df=df)
        }else{
          #In parallel
          cl <- makeCluster(nworkers);registerDoParallel(cl)
          clusterExport(cl=cl,varlist='convhulln')
          ichlist=parallel::parLapply(cl=cl, 1:dim(jcombs)[2], fconvhull, npr=npr, jcomb=jcombs, df=df)
          stopCluster(cl)
          #
        }
      }
      # Name the list of convex hull rows for coordinate subspaces
      jcombsnames=apply(jcombs,2,paste0,collapse=";")
      names(ichlist)=jcombsnames
      # Store all those rows in a vector
      ichall=unlist(ichlist)
      # Find associated data points of initial data frame
      dfichall=data.frame(df[ichall,])
      # Store all available rows despite their multiplicity
      rowschall=unique(as.integer(rownames(dfichall)))
      nchall=length(rowschall)
      # Find unique rows that have been projected
      dfichallu=data.frame(df[rowschall,])
      # Find distances between all rows which are associated with projected convex hulls:
      if(!uniquerows){
        dm=as.matrix(dist(dfichall,method = "euclidean", diag = F, upper = F, p = 2))
        outmostrows=max.col(dm, ties.method="first")
        # Find maximum distance per row and store outermost rows:
        outmostrows=as.integer(rownames(dfichall[outmostrows,]))
      }else{
        dm=as.matrix(dist(dfichallu,method = "euclidean", diag = F, upper = F, p = 2))
        outmostrows=max.col(dm, ties.method="first")
        # Find maximum distance per row and store outermost rows:
        outmostrows=as.integer(rownames(dfichallu[outmostrows,]))
      }
    }else{
      # Work for the 1D projection case:
      ##################################
      ichlist=lapply(1:d,function(i,df){x=df[,i];as.vector(c(which.min(x),which.max(x)))},df)
      # Name the list of convex hullss for coordinate axes
      jcombsnames=apply(jcombs,2,paste0,collapse=";")
      names(ichlist)=jcombsnames
      # Store all of them in a vector
      ichall=unlist(ichlist)
      # Find associated data points of initial data frame
      dfichall=data.frame(df[ichall,])
      # Find unique rows and their length
      rowschall=unique(ichall)
      nchall=length(rowschall)
      # Find unique rows that have been projected
      dfichallu=data.frame(df[rowschall,])
      # Find distances between unique rows:
      dm=as.matrix(dist(dfichallu,method = "euclidean", diag = F, upper = F, p = 2))
      # Find maximum distance per row and store outermost rows:
      outmostrows=max.col(dm, ties.method="first")
      outmostrows=as.integer(rownames(dfichallu[outmostrows,]))
    }
    # Create frequency table and collect the kappas most frequent from the outermost rows:
    ######################################################################################
    di=as.data.frame(table(outmostrows),stringsAsFactors = F)
    di=di[order(-di$Freq),]
    di$outmostrows=as.integer(di$outmostrows)
    di$FreqPerCent=di$Freq/sum(di$Freq)
    di$CumFreqPerCent=cumsum(di$FreqPerCent)
    rownames(di)=1:dim(di)[1]
    rowsmaxall=di$outmostrows
    nrowsmaxall=length(rowsmaxall)
    # Check if rows are less than kappas
    ####################################
    usedrandom=0
    if(nrowsmaxall<kappas){
      if(nchall>kappas){
        # Use rseed if it is given
        if(!is.null(rseed)){set.seed(rseed)}
        rowsmaxall=c(rowsmaxall,sample(setdiff(rowschall,rowsmaxall),kappas-nrowsmaxall))
      }else{
        n1=kappas-nchall
        usedrandom=n1
        r1=c(rowsmaxall,setdiff(rowschall,rowsmaxall))
        # Use rseed if it is given
        if(!is.null(rseed)){set.seed(rseed)}
        rowsmaxall=c(r1, sample(setdiff(1:n,r1),n1))
      }
    }
    #Find the kappas outermost of all outermost
    rowsmaxkappas=as.integer(rowsmaxall[1:kappas])
    ##############################################
    # Set output
    out=list("outmost"=rowsmaxkappas,"outmostall"=rowsmaxall,"outmostfrequency"=di,
             "usedrandom"=usedrandom,"chprojections"=ichlist,"projected"=dfichallu)
  }
  # Return from all cases
  return(out)
}
