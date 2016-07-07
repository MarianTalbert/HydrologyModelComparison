
 for(i in 1:12){
print(paste(month.abb[i]))
print(cbind(quantile(monthlyChk[[i]],na.rm=TRUE,probs=c(.05,.1,.25,.5,.75,.9,.95)),
      quantile(combineMapLst[[i]],na.rm=TRUE,probs=c(.05,.1,.25,.5,.75,.9,.95)),
      quantile(ImgLst[[1]][[i]],na.rm=TRUE,probs=c(.05,.1,.25,.5,.75,.9,.95))),digits=4)
}