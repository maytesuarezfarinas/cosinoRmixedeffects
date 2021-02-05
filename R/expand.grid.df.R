expand.grid.df<-function(...) Reduce(function(...) merge(..., by=NULL), list(...))
