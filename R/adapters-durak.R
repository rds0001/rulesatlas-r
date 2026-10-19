.ra_durak_rank <- function(c) c%%9
.ra_durak_suit <- function(c) c%/%9
.ra_durak_beats <- function(a,b,trump) (.ra_durak_suit(b)==.ra_durak_suit(a) && .ra_durak_rank(b)>.ra_durak_rank(a)) || (.ra_durak_suit(b)==trump && .ra_durak_suit(a)!=trump)
.ra_durak_ranks <- function(d) unique(.ra_durak_rank(unlist(lapply(d$table,function(pair) c(pair$attack,pair$defense)))))
.ra_durak_attackable <- function(d) {
  if(length(d$table)>=d$limit) return(list())
  Filter(function(c) !length(d$table) || .ra_durak_rank(c) %in% .ra_durak_ranks(d),d$hands[[d$attacker+1L]])
}
.ra_durak_finish <- function(d,pickup) {
  a<-d$attacker;b<-1L-a;cards<-as.list(unlist(lapply(d$table,function(pair) c(pair$attack,pair$defense))))
  if(pickup) d$hands[[b+1L]]<-c(d$hands[[b+1L]],cards) else d$discard<-c(d$discard,cards)
  d$table<-list()
  for(p in c(a,b)) while(length(d$hands[[p+1L]])<6 && length(d$stock)>0) {d$hands[[p+1L]]<-c(d$hands[[p+1L]],list(d$stock[[1]]));d$stock<-d$stock[-1]}
  if(!pickup) d$attacker<-b
  d$phase<-if(!length(d$stock) && any(lengths(d$hands)==0)) "finished" else "attack";d$limit<-min(6,length(d$hands[[2L-d$attacker]]));d
}
.ra_durak_validate <- function(d) {
  if(d$phase=="setup") {
    if(sum(lengths(d$hands))>0 || length(d$stock) || length(d$discard) || length(d$table) || !is.null(d$trump_card)) .ra_fail("Setup must be empty",code="INVALID_STATE")
    return(invisible(NULL))
  }
  cards<-c(unlist(d$hands),unlist(d$stock),unlist(d$discard),unlist(lapply(d$table,function(pair) c(pair$attack,pair$defense))))
  if(length(cards)!=36 || !identical(as.integer(sort(cards)),0:35) || is.null(d$trump_card) || length(d$discard)%%2!=0) .ra_fail("Invalid deck inventory or trump card",code="INVALID_STATE")
  if(length(d$stock)>0 && tail(unlist(d$stock),1)!=d$trump_card) .ra_fail("Face-up trump must remain at the bottom of the stock",code="INVALID_STATE")
  ended<-!length(d$stock) && !length(d$table) && any(lengths(d$hands)==0)
  if((d$phase=="finished")!=ended) .ra_fail("Game ends only after completing the bout and drawing available cards",code="INVALID_STATE")
  defender<-1L-d$attacker;covered<-sum(vapply(d$table,function(p) !is.null(p$defense),logical(1)))
  if(d$limit!=min(6,length(d$hands[[defender+1L]])+covered) || length(d$table)>d$limit) .ra_fail("Attack limit must match the defender\u2019s starting hand, capped at six",code="INVALID_STATE")
  seen<-numeric();uncovered<-FALSE
  for(pair in d$table) {
    a<-pair$attack;b<-pair$defense
    if(length(seen)>0 && !.ra_durak_rank(a) %in% seen) .ra_fail("Additional attack rank must already be on the table",code="INVALID_STATE")
    seen<-union(seen,.ra_durak_rank(a))
    if(is.null(b)) uncovered<-TRUE else {
      if(uncovered || !.ra_durak_beats(a,b,.ra_durak_suit(d$trump_card))) .ra_fail("Invalid cover or cover after pickup began",code="INVALID_STATE")
      seen<-union(seen,.ra_durak_rank(b))
    }
  }
  missing<-length(d$table)-covered
  if((d$phase=="attack" && missing>0) || (d$phase=="defend" && missing!=1) || (d$phase=="pickup" && missing<1)) .ra_fail("Bout phase and uncovered attacks mismatch",code="INVALID_STATE")
}
.ra_durak_apply <- function(state,action) {
  d<-state$data;p<-action$params;kind<-action$type;a<-d$attacker;b<-1L-a
  if(kind=="deal") {
    .ra_fields(p,list(deck=list(type="array",minItems=36,maxItems=36,items=list(type="integer",minimum=0,maximum=35)),fallback_first=list(type="integer",minimum=0,maximum=1)),"/params")
    if(d$phase!="setup" || anyDuplicated(unlist(p$deck))) .ra_fail("Supply the complete thirty-six-card deck once")
    d$hands<-list(p$deck[seq(1,12,by=2)],p$deck[seq(2,12,by=2)]);d$stock<-p$deck[13:36];d$trump_card<-tail(unlist(d$stock),1);trump<-.ra_durak_suit(d$trump_card);candidates<-list()
    for(q in 0:1) for(c in unlist(d$hands[[q+1L]])) if(.ra_durak_suit(c)==trump) candidates[[length(candidates)+1L]]<-c(.ra_durak_rank(c),q)
    d$attacker<-if(length(candidates)) candidates[[which.min(vapply(candidates,function(x) x[1],numeric(1)))]] [2] else p$fallback_first;d$limit<-6L;d$phase<-"attack";return(d)
  }
  actor<-if(d$phase=="defend") b else a
  if(!is.null(action$actor) && action$actor!=state$players[[actor+1L]]$id) .ra_fail("Wrong actor")
  if(kind=="attack") {
    .ra_fields(p,list(card=list(type="integer",minimum=0,maximum=35)),"/params");c<-p$card
    if(!d$phase %in% c("attack","pickup") || !c %in% unlist(.ra_durak_attackable(d))) .ra_fail("Attack must match a table rank, fit the limit and come from the attacker\u2019s hand")
    d$hands[[a+1L]]<-Filter(function(x) x!=c,d$hands[[a+1L]]);d$table[[length(d$table)+1L]]<-list(attack=c,defense=NULL)
    if(d$phase=="attack") d$phase<-"defend"
    return(d)
  }
  if(kind=="cover") {
    .ra_fields(p,list(card=list(type="integer",minimum=0,maximum=35)),"/params");c<-p$card
    if(d$phase!="defend" || !c %in% unlist(d$hands[[b+1L]]) || !.ra_durak_beats(d$table[[length(d$table)]]$attack,c,.ra_durak_suit(d$trump_card))) .ra_fail("Cover with a higher card of the same suit or a trump against a nontrump")
    d$hands[[b+1L]]<-Filter(function(x) x!=c,d$hands[[b+1L]]);d$table[[length(d$table)]]$defense<-c;d$phase<-"attack";return(d)
  }
  if(kind=="take") {
    .ra_fields(p,structure(list(),names=character()),"/params")
    if(d$phase!="defend") .ra_fail("Only the defender facing an uncovered attack may take")
    d$phase<-"pickup";return(d)
  }
  if(kind=="end_bout") {
    .ra_fields(p,structure(list(),names=character()),"/params")
    if(!d$phase %in% c("attack","pickup") || !length(d$table)) .ra_fail("Finish only a covered bout or a declared pickup")
    return(.ra_durak_finish(d,d$phase=="pickup"))
  }
  .ra_fail("Unknown action")
}
.ra_durak_legal <- function(state) {
  d<-state$data;a<-d$attacker;b<-1L-a
  if(d$phase=="setup") return(list(mode="event_types",actions=list(),action_types=list(list(type="deal",params_schema=list(deck=list(type="array",minItems=36,maxItems=36,items=list(type="integer",minimum=0,maximum=35)),fallback_first=list(type="integer",minimum=0,maximum=1)),preconditions=list("Complete deck in deal/draw order; its last card is the face-up trump. Lowest held trump starts, or fallback_first if neither hand has trump."),external_observations=list("External deck order and fallback starter."))),constraints=structure(list(),names=character()),completeness=FALSE,reason="external_deal"))
  if(d$phase=="defend") return(c(list(list(type="take",params=structure(list(),names=character()))),lapply(Filter(function(c) .ra_durak_beats(d$table[[length(d$table)]]$attack,c,.ra_durak_suit(d$trump_card)),d$hands[[b+1L]]),function(c) list(type="cover",params=list(card=c)))))
  actions<-lapply(.ra_durak_attackable(d),function(c) list(type="attack",params=list(card=c)))
  if(length(d$table)) actions<-c(actions,list(list(type="end_bout",params=structure(list(),names=character()))))
  actions
}
.ra_durak_outcome <- function(state,pack) {
  d<-state$data;done<-d$phase=="finished";wins<-if(done) which(lengths(d$hands)==0) else integer()
  list(status=if(done) "terminal" else "ongoing",result_kind=if(done) if(length(wins)==2) "draw" else "win" else "none",winners=lapply(wins,function(p) state$players[[p]]$id),losers=if(done) lapply(setdiff(1:2,wins),function(p) state$players[[p]]$id) else list(),placements=list(),reason=if(done) "stock_exhausted_and_bout_completed" else "ongoing",evidence_required=list(),rule_id=if(done) paste0(pack$ruleset_id,"@",pack$ruleset_version) else NULL)
}
