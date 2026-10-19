.ra_draw_poker_begin <- function(d) {d$street<-1L;d$phase<-"drawing";d$street_bets<-list(0,0);d$acted<-list(FALSE,FALSE);d$last_raise<-40L;d$turn<-1L-d$dealer;d}
.ra_draw_poker_runout <- function(d) {
  bets<-unlist(d$committed);p<-if(bets[1]>bets[2]) 1L else 2L;refund<-abs(bets[1]-bets[2]);d$committed[[p]]<-d$committed[[p]]-refund;d$street_bets[[p]]<-d$street_bets[[p]]-refund;d$stacks[[p]]<-d$stacks[[p]]+refund
  if(d$street==0) .ra_draw_poker_begin(d) else .ra_poker_settle(d,5)
}
.ra_draw_poker_progress <- function(d) {
  active<-which(unlist(d$stacks)>0)-1L
  if(!length(active)) return(.ra_draw_poker_runout(d))
  if(length(active)==1) {
    p<-active[1]
    if(d$street_bets[[p+1L]]>=d$street_bets[[2L-p]]) return(.ra_draw_poker_runout(d))
    d$turn<-p;return(d)
  }
  if(all(unlist(d$acted)) && d$street_bets[[1]]==d$street_bets[[2]]) return(if(d$street==0) .ra_draw_poker_begin(d) else .ra_poker_settle(d,5))
  d
}
.ra_draw_poker_validate <- function(d) {
  if(d$phase=="setup") {
    .ra_poker_validate(d,5,FALSE)
    if(length(d$discard) || any(unlist(d$draw_done))) .ra_fail("Setup cannot contain draws or discards",code="INVALID_STATE")
    return(invisible(NULL))
  }
  cards<-c(unlist(d$holes),unlist(d$deck),unlist(d$discard))
  if(length(cards)!=52 || !identical(as.integer(sort(cards)),0:51) || any(lengths(d$holes)!=5) || length(d$board) || length(d$burned) || !d$street %in% c(0,1)) .ra_fail("Invalid five-card draw inventory",code="INVALID_STATE")
  if(any(unlist(d$stacks)!=unlist(d$initial_stacks)-unlist(d$committed)+unlist(d$payouts)) || any(unlist(d$street_bets)>unlist(d$committed)) || any(unlist(d$committed)>unlist(d$initial_stacks))) .ra_fail("Stacks and contributions do not reconcile",code="INVALID_STATE")
  if(d$street==0 && (length(d$discard)>0 || any(unlist(d$draw_done)))) .ra_fail("Cards cannot be exchanged before the draw phase",code="INVALID_STATE")
  if(length(d$discard)>5*sum(unlist(d$draw_done))) .ra_fail("More discarded cards than completed draws permit",code="INVALID_STATE")
  if(d$phase=="drawing") {
    first<-1L-d$dealer;count<-sum(unlist(d$draw_done))
    if(d$street!=1 || count>=2 || d$turn!=if(count==0) first else 1L-first) .ra_fail("Invalid pending draw order or unsettled contributions",code="INVALID_STATE")
    if(d$draw_done[[d$turn+1L]] || d$committed[[1]]!=d$committed[[2]] || any(c(unlist(d$street_bets),unlist(d$payouts))!=0)) .ra_fail("Invalid pending draw order or unsettled contributions",code="INVALID_STATE")
  } else if(d$phase=="finished") {
    if(is.null(d$folded) && (d$street!=1 || !all(unlist(d$draw_done)) || d$committed[[1]]!=d$committed[[2]])) .ra_fail("Showdown requires completed draws and matched contributions",code="INVALID_STATE")
    winners<-.ra_poker_winners(d,5);pot<-sum(unlist(d$committed));expected<-vapply(0:1,function(p) if(p %in% winners) pot%/%length(winners) else 0,numeric(1))
    if(any(unlist(d$payouts)!=expected)) .ra_fail("Final pot distribution is inconsistent",code="INVALID_STATE")
  } else {
    t<-d$turn
    if((d$street==1 && !all(unlist(d$draw_done))) || !is.null(d$folded) || any(unlist(d$payouts)!=0) || d$stacks[[t+1L]]==0 || d$street_bets[[t+1L]]>d$street_bets[[2L-t]]) .ra_fail("Invalid pending betting decision",code="INVALID_STATE")
    if((all(unlist(d$acted)) && d$street_bets[[1]]==d$street_bets[[2]]) || (d$stacks[[2L-t]]==0 && d$street_bets[[t+1L]]>=d$street_bets[[2L-t]])) .ra_fail("Completed betting must advance",code="INVALID_STATE")
  }
}
.ra_draw_poker_apply <- function(state,action) {
  if(state$data$phase!="drawing") return(.ra_poker_apply(state,action,5,FALSE))
  d<-state$data;p<-action$params;t<-d$turn
  if(!is.null(action$actor) && action$actor!=state$players[[t+1L]]$id) .ra_fail("Wrong actor")
  if(action$type=="fold") {.ra_fields(p,structure(list(),names=character()),"/params");d$folded<-t;return(.ra_poker_settle(d,5))}
  if(action$type!="draw") .ra_fail("Select zero through five cards to exchange")
  .ra_fields(p,list(indices=list(type="array",minItems=0,maxItems=5,items=list(type="integer",minimum=0,maximum=4))),"/params")
  if(anyDuplicated(unlist(p$indices))) .ra_fail("Draw indices must be distinct")
  if(!is.null(action$actor) && action$actor!=state$players[[t+1L]]$id) .ra_fail("Wrong actor")
  for(i in sort(unlist(p$indices))) {d$discard<-c(d$discard,list(d$holes[[t+1L]][[i+1L]]));d$holes[[t+1L]][[i+1L]]<-d$deck[[1]];d$deck<-d$deck[-1]}
  d$draw_done[[t+1L]]<-TRUE;d$turn<-1L-t
  if(all(unlist(d$draw_done))) {d$phase<-"betting";d$turn<-1L-d$dealer;return(.ra_draw_poker_progress(d))}
  d
}
.ra_draw_poker_legal <- function(state) {
  if(state$data$phase!="drawing") return(.ra_poker_legal(state,5,FALSE))
  actions<-list(list(type="fold",params=structure(list(),names=character())),list(type="draw",params=list(indices=list())))
  for(n in 1:5) for(indices in utils::combn(0:4,n,simplify=FALSE)) actions[[length(actions)+1L]]<-list(type="draw",params=list(indices=as.list(indices)))
  actions
}
.ra_draw_poker_outcome <- function(state,pack) .ra_poker_outcome(state,pack,5,FALSE)
