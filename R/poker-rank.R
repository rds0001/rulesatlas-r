.ra_poker_compare <- function(a,b) {
  n<-max(length(a),length(b));a<-c(a,rep(-1,n-length(a)));b<-c(b,rep(-1,n-length(b)));different<-which(a!=b)
  if(!length(different)) 0L else if(a[different[1]]>b[different[1]]) 1L else -1L
}
.ra_poker_five <- function(cards) {
  cards<-unlist(cards);ranks<-sort(cards%%13+2,decreasing=TRUE);counts<-table(ranks);rs<-as.numeric(names(counts));ns<-as.numeric(counts);order<-order(-ns,-rs);rs<-rs[order];ns<-ns[order];flush<-length(unique(cards%/%13))==1
  straight<-if(identical(as.numeric(ranks),c(14,5,4,3,2))) 5 else if(length(counts)==5 && max(ranks)-min(ranks)==4) max(ranks) else 0
  if(straight>0 && flush) return(c(8,straight))
  if(ns[1]==4) return(c(7,rs[1],rs[2]))
  if(identical(ns,c(3,2))) return(c(6,rs[1],rs[2]))
  if(flush) return(c(5,ranks))
  if(straight>0) return(c(4,straight))
  if(ns[1]==3) return(c(3,rs[1],sort(rs[-1],decreasing=TRUE)))
  if(length(ns)>=2 && all(ns[1:2]==2)) return(c(2,sort(rs[1:2],decreasing=TRUE),rs[3]))
  if(ns[1]==2) return(c(1,rs[1],sort(rs[-1],decreasing=TRUE)))
  c(0,ranks)
}
.ra_pai_five <- function(cards) {
  cards<-unlist(cards)
  if(!52 %in% cards) return(.ra_poker_five(cards))
  rest<-cards[cards!=52]
  if(all(rest%%13==12)) return(c(9,14))
  best<-c(-1)
  for(c in setdiff(0:51,rest)) {
    result<-.ra_poker_five(c(rest,c))
    if((c%%13==12 || result[1] %in% c(4,5,8)) && .ra_poker_compare(result,best)>0) best<-result
  }
  best
}
.ra_pai_two <- function(cards) {
  cards<-unlist(cards);ranks<-sort(ifelse(cards==52,14,cards%%13+2),decreasing=TRUE)
  if(ranks[1]==ranks[2]) c(1,ranks[1]) else c(0,ranks)
}
