.ra_31_score <- function(h) {
  h<-unlist(h);r<-h%%8
  if(length(unique(r))==1) return(61L)
  max(vapply(0:3,function(s) sum(ifelse(r[h%/%8==s]==7,11,pmin(10,7+r[h%/%8==s])))*2,numeric(1)))
}
.ra_31_finish <- function(d) {
  if(any(vapply(d$hands,.ra_31_score,numeric(1))==62)) {d$phase<-"finished";d$reason<-"thirty_one";return(d)}
  if(!is.null(d$knocker)) {
    d$remaining<-d$remaining-1L
    if(d$remaining==0) {d$phase<-"finished";d$reason<-"last_round";return(d)}
  }
  d$turn<-(d$turn+1L)%%3L
  if(d$passes==3) d$phase<-"refresh"
  d
}
.ra_31_validate <- function(d) {
  if(d$phase=="setup") {
    if(any(lengths(d$hands)) || length(d$table) || length(d$stock) || length(d$discard) || !is.null(d$knocker) || d$remaining!=0 || d$passes!=0 || !is.null(d$reason)) .ra_fail("Setup must be empty",code="INVALID_STATE")
    return(invisible(NULL))
  }
  cards<-c(unlist(d$hands),unlist(d$table),unlist(d$stock),unlist(d$discard))
  if(any(lengths(d$hands)!=3) || length(d$table)!=3 || length(cards)!=32 || !identical(as.integer(sort(cards)),0:31)) .ra_fail("Three cards per hand and table; every deck card must occur once",code="INVALID_STATE")
  if((is.null(d$knocker) && d$remaining!=0) || (!is.null(d$knocker) && d$remaining>2)) .ra_fail("Last-round counter requires a knocker",code="INVALID_STATE")
  high<-any(vapply(d$hands,.ra_31_score,numeric(1))==62)
  if(d$phase=="finished") {
    if(identical(d$reason,"thirty_one")) {if(!high) .ra_fail("Thirty-one ending requires a thirty-one hand",code="INVALID_STATE")}
    else if(identical(d$reason,"last_round")) {if(is.null(d$knocker) || d$remaining!=0 || high) .ra_fail("Last round must be completed",code="INVALID_STATE")}
    else .ra_fail("Terminal reason is required",code="INVALID_STATE")
  } else if(!is.null(d$reason) || high || (!is.null(d$knocker) && d$remaining==0)) .ra_fail("Completed hands must be terminal",code="INVALID_STATE")
  if(d$phase!="finished" && ((d$phase=="refresh")!=(d$passes==3))) .ra_fail("Three consecutive passes require a table refresh",code="INVALID_STATE")
  if(!is.null(d$knocker) && d$phase!="finished" && d$turn!=(d$knocker+3L-d$remaining)%%3L) .ra_fail("Turn must follow the knocker in the last round",code="INVALID_STATE")
}
.ra_31_apply <- function(state,action) {
  d<-state$data;p<-action$params;k<-action$type;t<-d$turn;cs<-list(type="integer",minimum=0,maximum=31);empty<-structure(list(),names=character())
  if(k=="deal") {
    .ra_fields(p,list(deck=list(type="array",minItems=32,maxItems=32,items=cs)),"/params")
    if(d$phase!="setup" || anyDuplicated(unlist(p$deck))) .ra_fail("Supply the full thirty-two-card deck once")
    d$hands<-lapply(1:3,function(i) p$deck[seq(i,9,3)]);d$table<-p$deck[10:12];d$stock<-p$deck[13:32];d$phase<-"play"
    if(any(vapply(d$hands,.ra_31_score,numeric(1))==62)) {d$phase<-"finished";d$reason<-"thirty_one"}
    return(d)
  }
  if(!is.null(action$actor) && action$actor!=state$players[[t+1L]]$id) .ra_fail("Wrong actor")
  if(k=="recycle") {
    .ra_fields(p,list(order=list(type="array",minItems=1,maxItems=31,items=cs)),"/params")
    if(d$phase!="refresh" || length(d$stock)>=3 || !length(d$discard) || !identical(sort(unlist(p$order)),sort(unlist(d$discard)))) .ra_fail("Recycle discarded former tables only when the refresh stock is short")
    d$stock<-c(d$stock,p$order);d$discard<-list();return(d)
  }
  if(k=="refresh") {
    .ra_fields(p,empty,"/params")
    if(d$phase!="refresh" || length(d$stock)<3) .ra_fail("Refresh after three passes, recycling first if needed")
    d$discard<-c(d$discard,d$table);d$table<-head(d$stock,3);d$stock<-tail(d$stock,length(d$stock)-3L);d$passes<-0L;d$phase<-"play";return(d)
  }
  if(d$phase!="play") .ra_fail("Deal or refresh before taking a turn")
  if(k=="knock") {
    .ra_fields(p,empty,"/params")
    if(!is.null(d$knocker)) .ra_fail("Only the first knock starts the last round")
    d$knocker<-t;d$remaining<-2L;d$turn<-(t+1L)%%3L;d$passes<-0L;return(d)
  }
  if(k=="pass") {.ra_fields(p,empty,"/params");d$passes<-d$passes+1L;return(.ra_31_finish(d))}
  if(k=="exchange_all") {.ra_fields(p,empty,"/params");old<-d$hands[[t+1L]];d$hands[[t+1L]]<-d$table;d$table<-old}
  else if(k=="exchange") {
    .ra_fields(p,list(hand=list(type="integer",minimum=0,maximum=2),table=list(type="integer",minimum=0,maximum=2)),"/params");i<-p$hand+1L;j<-p$table+1L;old<-d$hands[[t+1L]][[i]];d$hands[[t+1L]][[i]]<-d$table[[j]];d$table[[j]]<-old
  } else .ra_fail("Unknown action")
  d$passes<-0L;.ra_31_finish(d)
}
.ra_31_legal <- function(state) {
  d<-state$data;types<-list();actions<-list();cs<-list(type="integer",minimum=0,maximum=31);empty<-structure(list(),names=character())
  if(d$phase=="setup") types<-list(list(type="deal",params_schema=list(deck=list(type="array",minItems=32,maxItems=32,items=cs)),preconditions=list("Every card once, in deal/draw order."),external_observations=list("External deck order.")))
  else if(d$phase=="refresh") {
    if(length(d$stock)>=3) actions<-list(list(type="refresh",params=empty)) else types<-list(list(type="recycle",params_schema=list(order=list(type="array",minItems=1,maxItems=31,items=cs)),preconditions=list("Permutation of the discarded former tables; existing stock stays first."),external_observations=list("External reshuffle order.")))
  } else {
    for(i in 0:2) for(j in 0:2) actions<-c(actions,list(list(type="exchange",params=list(hand=i,table=j))))
    for(k in c("exchange_all","pass")) actions<-c(actions,list(list(type=k,params=empty)))
    if(is.null(d$knocker)) actions<-c(actions,list(list(type="knock",params=empty)))
  }
  list(mode="event_types",actions=actions,action_types=types,constraints=list(turn=d$turn,remaining=d$remaining),completeness=FALSE,reason="card_actions_and_external_deck_order")
}
.ra_31_outcome <- function(state,pack) {
  d<-state$data;done<-d$phase=="finished";scores<-if(d$phase!="setup") vapply(d$hands,.ra_31_score,numeric(1)) else numeric();wins<-if(done) which(scores==max(scores)) else integer()
  list(status=if(done) "terminal" else "ongoing",result_kind=if(done) if(length(wins)>1) "draw" else "win" else "none",winners=lapply(wins,function(p) state$players[[p]]$id),losers=if(done) lapply(setdiff(1:3,wins),function(p) state$players[[p]]$id) else list(),placements=if(done) lapply(1:3,function(p) list(player_id=state$players[[p]]$id,place=1L+sum(scores>scores[p]),score_half_points=scores[p])) else list(),reason=if(done) d$reason else "ongoing",evidence_required=list(),rule_id=if(done) paste0(pack$ruleset_id,"@",pack$ruleset_version) else NULL)
}
