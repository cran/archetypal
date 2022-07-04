print.archetypal=function(x,...){
  A=x$A
  B=x$B
  BY=x$BY
  cat("Archetypes:","\n")
  cat("\n") 
  print(data.frame(BY))
  cat("\n") 
  nr=dim(A)[1]
  nc=dim(A)[2]
  nda=6
  ndb=6
  if(nr<=20){
    A0=A
    A0=data.frame(round(A,digits = nda))
    vlines=matrix("|",nrow=dim(A0)[1],ncol=1)
    rsums=round(rowSums(A0),digits=2)
    APF=cbind(A0,vlines,rsums)
    colnames(APF)=c(1:nc,"|","Sums")
    if(nr>10){nda=3}
    if(nc>1){
      B0=data.frame(round(B,digits = ndb))
      vlines=matrix("|",nrow=dim(B0)[1],ncol=1)
      rsums=c(round(rowSums(B),digits=2))
      BPF=cbind(B0,vlines,rsums)
      colnames(BPF)=c(1:nr,"|","Sums")
    }else{
      B0=data.frame(rbind(round(B[,1:5],digits = ndb)))
      B1=data.frame(rbind(round(B[,(nr-4):nr],digits = ndb)))
      vdots=data.frame(matrix("...",nrow=1,ncol=1))
      BP=cbind(B0,vdots,B1)
      colnames(BP)=c(1:5," ",(nr-4):nr)
      BPF=cbind(BP,"|",rowSums(B))
      colnames(BPF)=c(colnames(BP),"|","Sum")
    }
  }else {
    A0=data.frame(round(A[1:5,],digits = nda))
    colnames(A0)=1:nc
    A1=data.frame(round(A[(nr-4):nr,],digits = nda))
    colnames(A1)=1:nc
    hdots=data.frame(matrix(rep(".",3*nc),nrow = 3,ncol = nc))
    colnames(hdots)=1:nc
    AP=data.frame(rbind(A0,hdots,A1))
    rownames(AP)=c(1:5," ","  ","   ",(nr-4):nr)
    colnames(AP)=1:nc
    vlines=matrix("|",nrow=dim(AP)[1],ncol=1)
    rsums=c(round(rowSums(A1),digits=2),rep(".",3),round(rowSums(A1),digits = 2))
    APF=cbind(AP,vlines,rsums)
    colnames(APF)=c(1:nc,"|","Sums")
    if(nc>1){
      B0=data.frame(round(B[,1:5],digits = ndb))
      B1=data.frame(round(B[,(nr-4):nr],digits = ndb))
      vdots=data.frame(matrix(rep(".",3*nc),nrow=nc,ncol=3))
      BP=data.frame(cbind(B0,vdots,B1))
      colnames(BP)=c(1:5,rep(" ",3),(nr-4):nr)
      vlines=matrix("|",nrow=dim(BP)[1],ncol=1)
      rsums=c(round(rowSums(B),digits=2))
      BPF=cbind(BP,vlines,rsums)
      colnames(BPF)=c(colnames(BP),"|","Sums")
    }else{
      B0=data.frame(rbind(round(B[,1:5],digits = ndb)))
      B1=data.frame(rbind(round(B[,(nr-4):nr],digits = ndb)))
      vdots=data.frame(matrix("...",nrow=1,ncol=1))
      BP=cbind(B0,vdots,B1)
      colnames(BP)=c(1:5," ",(nr-4):nr)
      BPF=cbind(BP,"|",rowSums(B))
      colnames(BPF)=c(colnames(BP),"|","Sum")
    }
  }
  cat("A matrix:","\n")
  cat("\n") 
  print(APF)
  cat("\n")  
  cat("B matrix:","\n")
  cat("\n") 
  print(BPF)
  return(NULL)
}
