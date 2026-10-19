.ra_stud_exposed <- function(d,p) d$holes[[p+1L]][3:min(6,length(d$holes[[p+1L]]))]
.ra_stud_first <- function(d) {
  if(d$street==0) {
    doors<-vapply(d$holes,function(h) h[[3]],numeric(1));return(order(doors%%13,doors%/%13)[1]-1L)
  }
  rank<-function(p) {
    cards<-unlist(.ra_stud_exposed(d,p));counts<-table(cards%%13+2);rs<-as.numeric(names(counts));ns<-as.numeric(counts);ix<-order(-ns,-rs);rs<-rs[ix];ns<-ns[ix]
    category<-if(ns[1]==4) 7 else if(ns[1]==3) 3 else if(length(ns)>1 && all(ns[1:2]==2)) 2 else if(ns[1]==2) 1 else 0;c(category,rs)
  }
  compare<-.ra_poker_compare(rank(0),rank(1));if(compare==0) 1L-d$dealer else if(compare>0) 0L else 1L
}
.ra_stud_pair_option <- function(d) d$street==1 && !d$big_fourth && any(vapply(0:1,function(p) length(unique(unlist(.ra_stud_exposed(d,p))%%13))==1,logical(1)))
.ra_stud_unit <- function(d) if(d$street>=2 || d$big_fourth) 40L else 20L
.ra_stud_next <- function(d) {
  d$burned<-c(d$burned,list(d$deck[[1]]));d$deck<-d$deck[-1]
  for(p in c(1L-d$dealer,d$dealer)) {d$holes[[p+1L]]<-c(d$holes[[p+1L]],list(d$deck[[1]]));d$deck<-d$deck[-1]}
  d$street<-d$street+1L;d$street_bets<-list(0,0);d$acted<-list(FALSE,FALSE);d$raise_count<-0L;d$turn<-.ra_stud_first(d);d
}
.ra_stud_runout <- function(d) {
  bets<-unlist(d$committed);p<-if(bets[1]>bets[2]) 1L else 2L;refund<-abs(bets[1]-bets[2]);d$committed[[p]]<-d$committed[[p]]-refund;d$street_bets[[p]]<-max(0,d$street_bets[[p]]-refund);d$stacks[[p]]<-d$stacks[[p]]+refund
  while(d$street<4) d<-.ra_stud_next(d)
  .ra_poker_settle(d,7)
}
.ra_stud_progress <- function(d) {
  active<-which(unlist(d$stacks)>0)-1L
  if(!length(active)) return(.ra_stud_runout(d))
  if(length(active)==1) {
    p<-active[1]
    if(d$street_bets[[p+1L]]>=d$street_bets[[2L-p]]) return(.ra_stud_runout(d))
    d$turn<-p;return(d)
  }
  if(all(unlist(d$acted)) && d$street_bets[[1]]==d$street_bets[[2]]) return(if(d$street==4) .ra_poker_settle(d,7) else .ra_stud_next(d))
  d
}
.ra_stud_raises <- function(d) {
  t<-d$turn;high<-max(unlist(d$street_bets));available<-d$street_bets[[t+1L]]+d$stacks[[t+1L]]
  if(d$raise_count>=4 || d$stacks[[2L-t]]==0 || available<=high) return(numeric())
  units<-c(.ra_stud_unit(d),if(.ra_stud_pair_option(d)) 40 else numeric())
  sort(unique(vapply(units,function(unit) min(available,if(d$street==0 && high<20) unit else high+unit),numeric(1))))
}
.ra_stud_validate <- function(d) {
  if(d$phase=="setup") {
    if(sum(lengths(d$holes))>0 || length(d$deck) || length(d$burned) || any(c(unlist(d$initial_stacks),unlist(d$stacks),unlist(d$committed),unlist(d$street_bets),unlist(d$payouts))!=0) || !is.null(d$folded)) .ra_fail("Setup must be empty",code="INVALID_STATE")
    return(invisible(NULL))
  }
  cards<-c(unlist(d$holes),unlist(d$deck),unlist(d$burned))
  if(length(cards)!=52 || !identical(as.integer(sort(cards)),0:51) || any(lengths(d$holes)!=3+d$street) || length(d$burned)!=d$street) .ra_fail("Card inventory does not match the stud street",code="INVALID_STATE")
  if(any(unlist(d$stacks)!=unlist(d$initial_stacks)-unlist(d$committed)+unlist(d$payouts)) || any(unlist(d$street_bets)>unlist(d$committed)) || any(unlist(d$committed)>unlist(d$initial_stacks))) .ra_fail("Stacks and contributions do not reconcile",code="INVALID_STATE")
  if(d$big_fourth && (d$street==0 || !any(vapply(d$holes,function(h) length(unique(unlist(h[3:4])%%13))==1,logical(1))))) .ra_fail("Large fourth-street bet requires an exposed pair",code="INVALID_STATE")
  if(d$phase=="bring_in") {
    if(d$street!=0 || any(c(unlist(d$street_bets),unlist(d$payouts))!=0) || d$turn!=.ra_stud_first(d) || any(unlist(d$acted)) || !is.null(d$folded) || min(unlist(d$stacks))==0) .ra_fail("Invalid bring-in decision",code="INVALID_STATE")
  } else if(d$phase=="finished") {
    if(is.null(d$folded) && (d$street!=4 || d$committed[[1]]!=d$committed[[2]])) .ra_fail("Showdown requires seven cards and matched contributions",code="INVALID_STATE")
    winners<-.ra_poker_winners(d,7);pot<-sum(unlist(d$committed));expected<-vapply(0:1,function(p) if(p %in% winners) pot%/%length(winners) else 0,numeric(1))
    if(any(unlist(d$payouts)!=expected)) .ra_fail("Final pot distribution is inconsistent",code="INVALID_STATE")
  } else {
    t<-d$turn
    if(!is.null(d$folded) || any(unlist(d$payouts)!=0) || d$stacks[[t+1L]]==0 || d$street_bets[[t+1L]]>d$street_bets[[2L-t]]) .ra_fail("Invalid pending betting decision",code="INVALID_STATE")
    if((all(unlist(d$acted)) && d$street_bets[[1]]==d$street_bets[[2]]) || (d$stacks[[2L-t]]==0 && d$street_bets[[t+1L]]>=d$street_bets[[2L-t]])) .ra_fail("Closed betting must advance",code="INVALID_STATE")
  }
}
.ra_stud_apply <- function(state,action) {
  d<-state$data;p<-action$params;kind<-action$type;t<-d$turn
  if(kind=="start") {
    .ra_fields(p,list(deck=list(type="array",minItems=52,maxItems=52,items=list(type="integer",minimum=0,maximum=51)),dealer=list(type="integer",minimum=0,maximum=1),stacks=list(type="array",minItems=2,maxItems=2,items=list(type="integer",minimum=1,maximum=1000000))),"/params")
    if(d$phase!="setup" || anyDuplicated(unlist(p$deck))) .ra_fail("Supply all cards and starting stacks once")
    d$dealer<-p$dealer;d$initial_stacks<-p$stacks;d$stacks<-p$stacks;d$deck<-p$deck
    for(i in 0:5) {q<-(1L-d$dealer+i)%%2;d$holes[[q+1L]]<-c(d$holes[[q+1L]],list(d$deck[[1]]));d$deck<-d$deck[-1]}
    for(q in 1:2) {amount<-min(5,d$stacks[[q]]);d$stacks[[q]]<-d$stacks[[q]]-amount;d$committed[[q]]<-amount}
    d$turn<-.ra_stud_first(d);d$phase<-"bring_in";return(if(min(unlist(d$stacks))==0) .ra_stud_progress(d) else d)
  }
  if(!is.null(action$actor) && action$actor!=state$players[[t+1L]]$id) .ra_fail("Wrong actor")
  if(d$phase=="bring_in") {
    if(kind!="bring_in") .ra_fail("The lowest door card must post the bring-in or complete")
    .ra_fields(p,list(complete=list(type="boolean")),"/params")
    if(p$complete && d$stacks[[t+1L]]<20) .ra_fail("Completing the bring-in requires twenty available chips")
    amount<-min(d$stacks[[t+1L]],if(p$complete) 20 else 10);d$stacks[[t+1L]]<-d$stacks[[t+1L]]-amount;d$committed[[t+1L]]<-d$committed[[t+1L]]+amount;d$street_bets[[t+1L]]<-amount;d$raise_count<-if(amount==20) 1L else 0L;d$acted[[t+1L]]<-TRUE;d$turn<-1L-t;d$phase<-"betting";return(.ra_stud_progress(d))
  }
  if(kind=="fold") {.ra_fields(p,structure(list(),names=character()),"/params");d$folded<-t;return(.ra_poker_settle(d,7))}
  if(kind %in% c("check","call")) {
    .ra_fields(p,structure(list(),names=character()),"/params");owed<-max(unlist(d$street_bets))-d$street_bets[[t+1L]]
    if((kind=="check")!=(owed==0)) .ra_fail("Check only with nothing owed; call only facing a bet")
    amount<-min(owed,d$stacks[[t+1L]]);d$stacks[[t+1L]]<-d$stacks[[t+1L]]-amount;d$committed[[t+1L]]<-d$committed[[t+1L]]+amount;d$street_bets[[t+1L]]<-d$street_bets[[t+1L]]+amount;d$acted[[t+1L]]<-TRUE
  } else if(kind=="raise") {
    .ra_fields(p,list(to=list(type="integer",minimum=1,maximum=1000000)),"/params");target<-p$to;high<-max(unlist(d$street_bets))
    if(!target %in% .ra_stud_raises(d)) .ra_fail("Raise must use the fixed betting unit, available all-in amount and four-bet cap")
    increment<-if(d$street==0 && high<20) target else target-high
    if(.ra_stud_pair_option(d) && increment==40) d$big_fourth<-TRUE
    if(increment>=.ra_stud_unit(d)) d$raise_count<-d$raise_count+1L
    amount<-target-d$street_bets[[t+1L]];d$stacks[[t+1L]]<-d$stacks[[t+1L]]-amount;d$committed[[t+1L]]<-d$committed[[t+1L]]+amount;d$street_bets[[t+1L]]<-target;d$acted<-list(FALSE,FALSE);d$acted[[t+1L]]<-TRUE
  } else .ra_fail("Unknown action")
  d$turn<-1L-t;.ra_stud_progress(d)
}
.ra_stud_legal <- function(state) {
  d<-state$data
  if(d$phase=="setup") return(list(mode="event_types",actions=list(),action_types=list(list(type="start",params_schema=list(deck=list(type="array",minItems=52,maxItems=52,items=list(type="integer",minimum=0,maximum=51)),dealer=list(type="integer",minimum=0,maximum=1),stacks=list(type="array",minItems=2,maxItems=2,items=list(type="integer",minimum=1,maximum=1000000))),preconditions=list("Complete deck and positive stacks; five-chip ante, ten-chip bring-in, twenty/forty limits."),external_observations=list("External deck order and table stacks."))),constraints=structure(list(),names=character()),completeness=FALSE,reason="external_deal"))
  if(d$phase=="bring_in") return(lapply(if(d$stacks[[d$turn+1L]]>=20) c(FALSE,TRUE) else FALSE,function(x) list(type="bring_in",params=list(complete=x))))
  owed<-max(unlist(d$street_bets))-d$street_bets[[d$turn+1L]]
  c(list(list(type="fold",params=structure(list(),names=character())),list(type=if(owed>0) "call" else "check",params=structure(list(),names=character()))),lapply(.ra_stud_raises(d),function(v) list(type="raise",params=list(to=v))))
}
.ra_stud_outcome <- function(state,pack) .ra_poker_outcome(state,pack,7,FALSE)
