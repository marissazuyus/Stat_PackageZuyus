#'The function pareto() will take a vector of numbers (and also titles) and will
#'display a pareto chart of the data. This chart includes a barplot of individual
#'values represented in descending order, as well as the cumulative total
#'represented by the line. The data used to represent this function is from Assignment 1.
#'
#'@param x = vector of numbers
#'
#'@return nothing, displays pareto graph
#'
#'@examples
#'limb.counts = c(15, 8, 63, 20);
#'limb.labels = c("None", "Both", "Legs ONLY", "Wheels ONLY");
#'limb.freq.df = as.data.frame(matrix(data= limb.counts/sum(limb.counts), nrow=4, ncol=1), row.name
#'s = limb.labels);
#'limb.freq.df
#'limb.raw = rep(limb.labels, limb.counts);
#'pareto(limb.raw)
#'
#'@export
pareto<-function(x,mn="Pareto barplot",...){  # x is a vector
  x.tab=table(x)
  xx.tab=sort(x.tab, decreasing=TRUE,index.return=FALSE)
  cumsum(as.vector(xx.tab))->cs
  length(x.tab)->lenx
  bp<-barplot(xx.tab,ylim=c(0,max(cs)),las=2)
  lb<-seq(0,cs[lenx],l=11)
  axis(side=4,at=lb,labels=paste(seq(0,100,length=11),"%",sep=""),las=1,line=-1,col="Blue",col.axis="Red")
  for(i in 1:(lenx-1)){
    segments(bp[i],cs[i],bp[i+1],cs[i+1],col=i,lwd=2)
  }
  title(main=mn,...)
}
