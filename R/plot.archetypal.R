plot.archetypal=function(x, ...){
  # Plot an an archetypal class object
  #
  # Check class
  #
  if(!inherits(x,"archetypal")){stop("Error, input must be an object of class 'archetypal'")} #RE SET!
  #
  ### EXTRACT ARCHETYPES FROM CLASS OBJECT
  archs=x$BY
  # Data frame
  da=as.data.frame(archs)
  # Find dimension of the relevant vector space
  nd=dim(da)[2]
  # Find number of archetypes
  kappas=dim(da)[1]
  # Store data/tables of variables for making fatser plot
  if(nd<=3){
    df=x$data_tables
  }else{
    dtlist=x$data_tables
  }
  #
  ##################
  ### DIMENSION = 1 
  ##################
  #
  if(nd==1){
    # Archetyps
    dp12=cbind(archs,rep(0,dim(archs)[1]))
    # Data
    x=as.numeric(as.matrix(df))
    dt=data.frame(table(x))
    dt$x=as.numeric(as.character(dt$x))
    dt=dt[order(dt$Freq,decreasing = T),]
    dt$rf=dt$Freq/sum(dt$Freq)
    xx0=dt$x
    yy0=rep(0,dim(dt)[1])
    xx1=xx0
    yy1=dt$rf
    # Plot
    parmarold=par("mar")
    par(mar=c(0,0,0,0))
    plot(xx1, yy1, xlim=c(min(xx0),max(xx0)),ylim=c(-max(yy1),max(yy1)), type="h",
         axes=F, xlab="",ylab="",yaxs="i", main = "", bty="n", col = "gray")
    segments(x0=xx0,y0=yy0,x1=xx1,y1=yy1, col = "gray")
    segments(x0=xx0,y0=yy0,x1=xx1,y1=-yy1, col = "gray")
    points(dp12, pch = 19, col="gray40", cex=1.5)
    xtiksrange=c(min(xx1),max(xx1))
    xtikspretty=pretty(xtiksrange)
    xtiksin=xtikspretty[xtikspretty<max(xtiksrange) & xtikspretty>min(xtiksrange)]
    xtiks=sort(c(xtiksrange,xtiksin))
    xlabs=round(xtiks,digits = 1)
    axis(1, pos=0, at = xtiks, labels = xlabs, las=2, font = 2)
    # Multiple labels
    xtiksa = dp12[,1]
    dlabsk=data.frame("lab"=NA,"k"=NA)
    xlabsa=c()
    for(k in 1:kappas){
      xlabreal=round(xtiksa[k],1)
      if(xlabreal%in%dlabsk$lab){
        ilab=which(dlabsk$lab==xlabreal)
        vlabs=paste0(paste0("A",c(ilab,k), collapse = " , ")  ," = ", xlabreal)
        xlabsa[k]=vlabs
        xlabsa[dlabsk[ilab,"k"]]=vlabs
        dlabsk[k,]=c(xlabreal,k)
      }else{
        xlabsa[k]=paste0("A",k," = ", xlabreal)
        dlabsk[k,]=c(xlabreal,k)
      }
    }
    text(x = xtiksa,  y = 0.05*max(yy1), xlabsa, srt = 90, adj = 0.0, font = 2)
    par("mar"=parmarold)
    #
  }
  #
  ##################
  ### DIMENSION = 2 
  ##################
  #
  if(nd==2){
    # k>=1
    ich=chull(archs)
    charchs=archs[ich,]
    # ADD DATA POINTS 
    plot(df,pch=19,cex=0.25,xlab="",ylab="")
    colarchs = "gray40"
    text(x=archs[,1], y=archs[,2],paste0("A",1:kappas), adj = 1, font = 2, cex = 1.5)
    # colpol=rgb(176/255,196/255,222/255,0.25)
    colpol=rgb(211/255,211/255,211/255,alpha = 0.35)
    if(kappas>1){polygon(charchs,col=colpol,lty=2)}
    points(archs, pch=19, cex=1.5, col = colarchs)
    #
  }
  #
  ##################
  ### DIMENSION = 3 
  ##################
  #
  if(nd==3){
    ich=chull(archs)
    charchs=archs[ich,]
    #
    parold=par()
    # ADD DATA POINTS 
    parmarold=par("mar")
    paromaold=par("oma")
    par(mar = c(0,0,0,0))
    par(oma = c(0,0,0,0))
    par(xpd=TRUE)
    colarchs = "gray40"
    scatter3D(df[,1],df[,2], df[,3], phi = 0, theta = -40, bty = "g",colvar = NULL,
              pch = 19, cex = 0.5, ticktype = "detailed",col=colarchs)
    pcg=0.75
    colpol = rgb(pcg,pcg,pcg,0.25)
    scatter3D(archs[,1],archs[,2],archs[,3], phi = 0, theta = -40, bty = "g",colvar = NULL,
              pch = 20, cex = 2., ticktype = "detailed",col=colarchs,add = T)
    text3D(x=archs[,1], y=archs[,2], z = archs[,3], paste0("A",1:kappas), adj = 1, font = 2, cex = 1.5, add = TRUE)
    # if(kappas>1){
      # lines3D(charchs[,1],charchs[,2],charchs[,3],add=TRUE,colvar = NULL,lty=2,col=colarchs)
      # rends=c(1,dim(charchs)[1])
      # lines3D(charchs[rends,1],charchs[rends,2],charchs[rends,3],add=TRUE,colvar = NULL,lty=2,col=colarchs)
      # polygon3D(charchs[,1], charchs[,2], charchs[,3], add=TRUE, colvar = NULL, col=colpol)
    # }
    par("mar"=parmarold)
    par("oma"=paromaold)
    #
  }
  #
  ##################
  ### DIMENSION >= 4 
  ##################
  #
  if(nd>=4){
    varnames=colnames(da)
    segs=5
    # RANGE OF INITIAL DATA FRAME
    dmin=min(sapply(dtlist, function(dt){min(dt[,1])}))
    dmax=max(sapply(dtlist, function(dt){max(dt[,1])}))
    rmax=dmax-dmin
    rmaxs=seq(0,rmax,length.out=segs+1)
    rmaxlabs=round(rmaxs+dmin,digits = 1)
    # ANGLES
    w=2*pi/nd
    ths=pi/2-(1:(nd-1)*w)
    thetas=c(pi/2,ths)
    # POINTS
    ptsx=c(0,rmax*cos(ths))
    ptsy=c(rmax,rmax*sin(ths))
    # NORMALS
    thnormals=atan2(ptsy,-ptsx)
    vsrt=180*thnormals/pi
    vsrt=90-vsrt
    vsrt2=sapply(vsrt, function(w){
      if(w==180){
        out=0
      }
      else if(abs(w)<=90){
        out=w
      }else if(w>180){
        # out=270-w
        out=-(180-w)
      }else if(w>90 & w<180){
        out=-(180-w)
      }else{
        out=w
      }
      return(out)
    })
    # Find proper layout for plotting archetypes with 3 plots per row
    if(kappas==1){
      nrows = 1
      c2 = c(1)
      pasp = 1
    }else if(kappas==2){
      nrows = 1
      c2 = c(1,2)
      pasp = 1
    }else if(kappas==3){
      nrows = 1
      c2 = c(1,2,3)
      pasp = 1
    }else{
      ncols = 3
      k3=kappas%%3
      k3
      if(k3==0){
        nrows = kappas/3
        c2 = 1:(nrows*ncols)
      }else if(k3==1){
        nrows = ceiling(kappas/3)
        c2 = c(1:((nrows-1)*ncols),kappas+1,(nrows-1)*ncols+1,kappas+2)
      }else{
        nrows = ceiling(kappas/3)
        c2 = c(1:((nrows-1)*ncols),(nrows-1)*ncols+1,kappas+1,(nrows-1)*ncols+2)
        c2
      }
      pasp = NA
    }
    laymat=matrix(c2,nrow = nrows, byrow = TRUE)
    ##################
    #### Main plot
    ##################
    # Convert archetypes to (0,rmax) for easy plot
    dh=da-dmin
    colpols=rgb(211/255,211/255,211/255,alpha = 0.35)
    # Keep "mar"
    parmarold=par("mar")
    # Make layout
    layout(laymat)
    # Plot archetypes
    for(i in 1:dim(da)[1]){
      # par(mar=rep(0,4))
      par(mar=c(0,0,0,0))
      # Plot perimeter
      plot(ptsx,ptsy,pch=19,axes=F,xlab="",ylab="",xlim=1.1*c(-rmax,rmax),ylim=1.2*c(-rmax,rmax), 
           cex = 0.5, col = "darkgray", asp = pasp)
      text(ptsx[1], 1.2*ptsy[1], paste0("A",i), font = 2, cex = 1.5)
      # Add polygons 
      ypols=sapply(rev(rmaxs), function(rmax,ths){
        ptsx=c(0,rmax*cos(ths))
        ptsy=c(rmax,rmax*sin(ths))
        polygon(ptsx,ptsy,lty=2,lwd=0.5, col = colpols, border = "darkgray")
      },rev(ths))
      # Add line segments for the web
      segments(x0=0,y0=0,x1=ptsx,y1=ptsy,lty=2,lwd=0.5, col = "darkgray")
      # Add variable names normally to the relevant axis
      ytexts=lapply(1:length(vsrt), function(i,varnames,vsrt,ptsx,ptsy){
        text(1.05*ptsx[i],1.05*ptsy[i],labels = varnames[i], srt = vsrt[i], font = 2, cex = 1.25)
      },varnames,vsrt2,ptsx,ptsy)
      # Add labels for the range 
      yrange=lapply(1:length(rmaxs), function(i,rmaxs,rmaxlabs){
        text(0,0.95*rmaxs[i],rmaxlabs[i], font = 2)
      },rmaxs,rmaxlabs)
      # Plot polygons for each archetype
      # thetas=c(pi/2,ths)
      rmaxi=dh[i,]
      ptsxi=unlist(rmaxi*cos(thetas))
      ptsyi=unlist(rmaxi*sin(thetas))
      dxyi=cbind(ptsxi,ptsyi)
      ichi=chull(dxyi)
      if(length(ichi)==nd){
        dxyi=dxyi[ichi,]
      }
      polygon(dxyi[,1],dxyi[,2],lty=1,lwd=2)
      points(ptsxi,ptsyi,col=1,type="b",pch=19,cex=1.5)
      lines(dxyi[c(1,dim(dxyi)[1]),1],dxyi[c(1,dim(dxyi)[1]),2],col=1)
      # Plot spikes for each variable - ray
      yspikes=lapply(1:nd, function(i,dtlist,dmin,thetas){
        dt=dtlist[[i]]
        xx=dt[,1]-dmin
        rmaxx=max(xx)
        # yy=dt$rf*max(c(1,rmaxx))*1.5
        if(rmaxx>1){cfact=0.5}else{cfact=1.5}
        yy=dt$rf*max(c(1,rmaxx))*cfact
        phi=thetas[i]
        RR=matrix(c(cos(phi),-sin(phi),sin(phi),cos(phi)),nrow = 2,ncol=2,byrow=T)
        xyr=t(RR%*%t(cbind(xx,yy)))
        xr=xyr[,1]
        yr=xyr[,2]
        xyrm=t(RR%*%t(cbind(xx,-yy)))
        xrm=xyrm[,1]
        yrm=xyrm[,2]
        points(xr,yr, pch=".",col="darkgray")
        points(xrm,yrm,pch=".",col="darkgray")
        segments(x0=xrm,y0=yrm,x1=xr,y1=yr,col="darkgray")
      },dtlist,dmin, thetas)
    }
    # Restore previously eixsting settings
    par("mar"=parmarold)
    par(mfrow=c(1,1))
    #    
  }
  # 
}