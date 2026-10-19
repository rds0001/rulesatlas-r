.ra_poker_strength <- function(d,p,hole_count) {
  if(hole_count==5) return(.ra_poker_five(d$holes[[p+1L]]))
  best<-c(-1)
  if(hole_count %in% c(2,7)) {
    cards<-c(unlist(d$holes[[p+1L]]),if(hole_count==2) unlist(d$board) else NULL);groups<-utils::combn(seq_along(cards),5,simplify=FALSE)
    for(ix in groups) {rank<-.ra_poker_five(cards[ix]);if(.ra_poker_compare(rank,best)>0) best<-rank}
  } else {
    holes<-unlist(d$holes[[p+1L]]);board<-unlist(d$board)
    for(h in utils::combn(seq_along(holes),2,simplify=FALSE)) for(b in utils::combn(seq_along(board),3,simplify=FALSE)) {
      rank<-.ra_poker_five(c(holes[h],board[b]));if(.ra_poker_compare(rank,best)>0) best<-rank
    }
  }
  best
}
.ra_poker_winners <- function(d,hole_count) {
  if(!is.null(d$folded)) return(1L-d$folded)
  comparison<-.ra_poker_compare(.ra_poker_strength(d,0,hole_count),.ra_poker_strength(d,1,hole_count))
  if(comparison==0) 0:1 else if(comparison>0) 0L else 1L
}
.ra_poker_settle <- function(d,hole_count) {
  winners<-.ra_poker_winners(d,hole_count);pot<-sum(unlist(d$committed));d$payouts<-lapply(0:1,function(p) if(p %in% winners) pot%/%length(winners) else 0)
  for(p in 1:2) d$stacks[[p]]<-d$stacks[[p]]+d$payouts[[p]]
  d$phase<-"finished";d
}
.ra_poker_board_next <- function(d) {
  d$burned<-c(d$burned,list(d$deck[[1]]));d$deck<-d$deck[-1];n<-if(d$street==0) 3 else 1;d$board<-c(d$board,d$deck[seq_len(n)]);d$deck<-d$deck[-seq_len(n)];d$street<-d$street+1L;d
}
.ra_poker_runout <- function(d,hole_count) {
  bets<-unlist(d$committed);p<-if(bets[1]>bets[2]) 1L else 2L;refund<-abs(bets[1]-bets[2]);d$committed[[p]]<-d$committed[[p]]-refund;d$street_bets[[p]]<-d$street_bets[[p]]-refund;d$stacks[[p]]<-d$stacks[[p]]+refund
  while(d$street<3) {d<-.ra_poker_board_next(d);d$street_bets<-list(0,0)}
  .ra_poker_settle(d,hole_count)
}
.ra_poker_progress <- function(d,hole_count) {
  if(hole_count==5) return(.ra_draw_poker_progress(d))
  active<-which(unlist(d$stacks)>0)-1L
  if(!length(active)) return(.ra_poker_runout(d,hole_count))
  if(length(active)==1) {
    p<-active[1]
    if(d$street_bets[[p+1L]]>=d$street_bets[[2L-p]]) return(.ra_poker_runout(d,hole_count))
    d$turn<-p;return(d)
  }
  if(all(unlist(d$acted)) && d$street_bets[[1]]==d$street_bets[[2]]) {
    if(d$street==3) return(.ra_poker_settle(d,hole_count))
    d<-.ra_poker_board_next(d);d$street_bets<-list(0,0);d$acted<-list(FALSE,FALSE);d$last_raise<-40L;d$turn<-1L-d$dealer
  }
  d
}
.ra_poker_cap <- function(d,pot_limit) {
  t<-d$turn;owed<-max(unlist(d$street_bets))-d$street_bets[[t+1L]];maximum<-d$street_bets[[t+1L]]+d$stacks[[t+1L]]
  if(pot_limit) min(maximum,d$street_bets[[t+1L]]+sum(unlist(d$committed))+2*owed) else maximum
}
.ra_poker_validate <- function(d,hole_count,pot_limit) {
  if(d$phase=="setup") {
    if(sum(lengths(d$holes))>0 || length(d$deck) || length(d$board) || length(d$burned) || any(c(unlist(d$initial_stacks),unlist(d$stacks),unlist(d$committed),unlist(d$street_bets),unlist(d$payouts))!=0) || !is.null(d$folded)) .ra_fail("Setup must have no cards or committed chips",code="INVALID_STATE")
    return(invisible(NULL))
  }
  cards<-c(unlist(d$holes),unlist(d$deck),unlist(d$board),unlist(d$burned))
  if(length(cards)!=52 || !identical(as.integer(sort(cards)),0:51) || any(lengths(d$holes)!=hole_count) || length(d$board)!=c(0,3,4,5)[d$street+1L] || length(d$burned)!=d$street) .ra_fail("Card inventory does not match the betting street",code="INVALID_STATE")
  if(any(unlist(d$stacks)!=unlist(d$initial_stacks)-unlist(d$committed)+unlist(d$payouts)) || any(unlist(d$street_bets)>unlist(d$committed)) || any(unlist(d$committed)>unlist(d$initial_stacks))) .ra_fail("Stacks and contributions do not reconcile",code="INVALID_STATE")
  if(d$phase=="finished") {
    if(is.null(d$folded) && (d$street!=3 || d$committed[[1]]!=d$committed[[2]])) .ra_fail("Showdown needs five board cards and matched contributions",code="INVALID_STATE")
    winners<-.ra_poker_winners(d,hole_count);pot<-sum(unlist(d$committed));expected<-vapply(0:1,function(p) if(p %in% winners) pot%/%length(winners) else 0,numeric(1))
    if(any(unlist(d$payouts)!=expected)) .ra_fail("Final pot distribution is inconsistent",code="INVALID_STATE")
  } else {
    t<-d$turn
    if(!is.null(d$folded) || any(unlist(d$payouts)!=0) || d$stacks[[t+1L]]==0 || d$street_bets[[t+1L]]>d$street_bets[[2L-t]]) .ra_fail("Invalid pending betting decision",code="INVALID_STATE")
    if(all(unlist(d$acted)) && d$street_bets[[1]]==d$street_bets[[2]]) .ra_fail("Closed betting street must advance",code="INVALID_STATE")
    if(d$stacks[[2L-t]]==0 && d$street_bets[[t+1L]]>=d$street_bets[[2L-t]]) .ra_fail("Matched all-in must proceed to showdown",code="INVALID_STATE")
  }
}
.ra_poker_apply <- function(state,action,hole_count,pot_limit) {
  d<-state$data;p<-action$params;kind<-action$type;t<-d$turn
  if(kind=="start") {
    .ra_fields(p,list(deck=list(type="array",minItems=52,maxItems=52,items=list(type="integer",minimum=0,maximum=51)),dealer=list(type="integer",minimum=0,maximum=1),stacks=list(type="array",minItems=2,maxItems=2,items=list(type="integer",minimum=1,maximum=1000000))),"/params")
    if(d$phase!="setup" || anyDuplicated(unlist(p$deck))) .ra_fail("Supply all cards and starting stacks once")
    d$dealer<-p$dealer;d$initial_stacks<-p$stacks;d$stacks<-p$stacks;d$deck<-p$deck;d$phase<-"betting";d$turn<-d$dealer
    for(i in 0:(2*hole_count-1)) {q<-(1L-d$dealer+i)%%2;d$holes[[q+1L]]<-c(d$holes[[q+1L]],list(d$deck[[1]]));d$deck<-d$deck[-1]}
    for(q in 0:1) {amount<-min(d$stacks[[q+1L]],if(q==d$dealer) 20 else 40);d$stacks[[q+1L]]<-d$stacks[[q+1L]]-amount;d$committed[[q+1L]]<-amount;d$street_bets[[q+1L]]<-amount}
    return(.ra_poker_progress(d,hole_count))
  }
  if(!is.null(action$actor) && action$actor!=state$players[[t+1L]]$id) .ra_fail("Wrong actor")
  owed<-max(unlist(d$street_bets))-d$street_bets[[t+1L]]
  if(kind=="fold") {.ra_fields(p,structure(list(),names=character()),"/params");d$folded<-t;return(.ra_poker_settle(d,hole_count))}
  if(kind %in% c("check","call")) {
    .ra_fields(p,structure(list(),names=character()),"/params")
    if((kind=="check")!=(owed==0)) .ra_fail("Check only with nothing owed; call only when facing a bet")
    amount<-min(owed,d$stacks[[t+1L]]);d$stacks[[t+1L]]<-d$stacks[[t+1L]]-amount;d$committed[[t+1L]]<-d$committed[[t+1L]]+amount;d$street_bets[[t+1L]]<-d$street_bets[[t+1L]]+amount;d$acted[[t+1L]]<-TRUE
  } else if(kind=="raise") {
    .ra_fields(p,list(to=list(type="integer",minimum=1,maximum=1000000)),"/params");target<-p$to;high<-max(unlist(d$street_bets));increment<-target-high
    if(d$stacks[[2L-t]]==0 || target<=high || target>.ra_poker_cap(d,pot_limit)) .ra_fail("Raise exceeds available chips or the betting limit, or the opponent is all-in")
    if(increment<d$last_raise && target!=d$street_bets[[t+1L]]+d$stacks[[t+1L]]) .ra_fail("A smaller-than-minimum raise is permitted only as an all-in")
    amount<-target-d$street_bets[[t+1L]];d$stacks[[t+1L]]<-d$stacks[[t+1L]]-amount;d$committed[[t+1L]]<-d$committed[[t+1L]]+amount;d$street_bets[[t+1L]]<-target;d$acted<-list(FALSE,FALSE);d$acted[[t+1L]]<-TRUE
    if(increment>=d$last_raise) d$last_raise<-increment
  } else .ra_fail("Unknown action")
  d$turn<-1L-t;.ra_poker_progress(d,hole_count)
}
.ra_poker_legal <- function(state,hole_count,pot_limit) {
  d<-state$data
  if(d$phase=="setup") return(list(mode="event_types",actions=list(),action_types=list(list(type="start",params_schema=list(deck=list(type="array",minItems=52,maxItems=52,items=list(type="integer",minimum=0,maximum=51)),dealer=list(type="integer",minimum=0,maximum=1),stacks=list(type="array",minItems=2,maxItems=2,items=list(type="integer",minimum=1,maximum=1000000))),preconditions=list("Supply a complete deck and positive starting stacks; dealer posts small blind twenty, opponent big blind forty, capped by available chips."),external_observations=list("External deck order and starting stacks."))),constraints=structure(list(),names=character()),completeness=FALSE,reason="external_deal"))
  t<-d$turn;owed<-max(unlist(d$street_bets))-d$street_bets[[t+1L]];high<-max(unlist(d$street_bets));cap<-.ra_poker_cap(d,pot_limit);minimum<-high+d$last_raise;actions<-list(list(type="fold",params=structure(list(),names=character())),list(type=if(owed>0) "call" else "check",params=structure(list(),names=character())));types<-list()
  if(d$stacks[[2L-t]]>0 && cap>high && (cap>=minimum || cap==d$street_bets[[t+1L]]+d$stacks[[t+1L]])) types<-list(list(type="raise",params_schema=list(to=list(type="integer",minimum=high+1,maximum=cap)),preconditions=list("At least the full minimum raise, except an all-in may be smaller. The opponent must retain chips."),external_observations=list()))
  list(mode="event_types",actions=actions,action_types=types,constraints=list(to_call=min(owed,d$stacks[[t+1L]]),pot=sum(unlist(d$committed)),minimum_full_raise_to=minimum,maximum_raise_to=cap,limit=if(pot_limit) "pot_limit" else "no_limit"),completeness=FALSE,reason="bounded_bet_amounts")
}
.ra_poker_outcome <- function(state,pack,hole_count,pot_limit) {
  d<-state$data;done<-d$phase=="finished";winners<-if(done) .ra_poker_winners(d,hole_count) else integer()
  list(status=if(done) "terminal" else "ongoing",result_kind=if(done) if(length(winners)>1) "draw" else "win" else "none",winners=lapply(winners,function(p) state$players[[p+1L]]$id),losers=if(done) lapply(setdiff(0:1,winners),function(p) state$players[[p+1L]]$id) else list(),placements=if(done) lapply(0:1,function(p) list(player_id=state$players[[p+1L]]$id,place=if(p %in% winners) 1 else 2,stack=d$stacks[[p+1L]],net=d$stacks[[p+1L]]-d$initial_stacks[[p+1L]])) else list(),reason=if(done) if(!is.null(d$folded)) "opponent_folded" else "showdown" else "ongoing",evidence_required=list(),rule_id=if(done) paste0(pack$ruleset_id,"@",pack$ruleset_version) else NULL)
}
