identify.and.discard.internal.inconsistencies<-function(df){
  require(dplyr)
  #
  #find species that have more than one growth form in the database
  #
  how.many.levels<-summarize(group_by(df,sp),num.l=length(unique(support)))
  prob.sp<-how.many.levels$sp[how.many.levels$num.l>1]
  #
  #remove conflicted species from the "good" database"
  #
  good.sp<-subset(df,!df$sp%in%prob.sp)
  return(good.sp)
}


output.unique.species.dataset<-function(good.sp){
  require(dplyr)
  #
  #select the three key columns
  #
  dropped.source<-select(good.sp,sp,support)
  #
  #drop exact duplicates 
  #
  unique.spp <- dropped.source[!duplicated(dropped.source),]
  return(unique.spp)
}