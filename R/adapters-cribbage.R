.ra_cr_card<-list(type="integer",minimum=0,maximum=51)
.ra_cr_rank<-function(c) c%%13L+1L
.ra_cr_value<-function(c) pmin(10L,.ra_cr_rank(c))
.ra_cr_show_score<-function(hand,starter,crib=FALSE) {
 hand<-unlist(hand);cards<-c(hand,starter);points<-0L
 for(n in 2:5) points<-points+2L*sum(utils::combn(cards,n,FUN=function(cs) sum(.ra_cr_value(cs))==15))
 points<-points+2L*sum(utils::combn(cards,2,FUN=function(cs) .ra_cr_rank(cs[1])==.ra_cr_rank(cs[2])))
 for(n in c(5,4,3)) {runs<-sum(utils::combn(cards,n,FUN=function(cs) {r<-.ra_cr_rank(cs);length(unique(r))==n && max(r)-min(r)==n-1L}));if(runs>0) {points<-points+n*runs;break}}
 same<-length(unique(hand%/%13L))==1
 if(same && (!crib || hand[1]%/%13L==starter%/%13L)) points<-points+4L+as.integer(hand[1]%/%13L==starter%/%13L)
 points+sum(.ra_cr_rank(hand)==11 & hand%/%13L==starter%/%13L)
}
.ra_cr_peg_score<-function(sequence) {
 s<-unlist(sequence);total<-sum(.ra_cr_value(s));points<-if(total %in% c(15,31)) 2L else 0L;n<-1L
 if(length(s)>1) for(c in rev(s[-length(s)])) {if(.ra_cr_rank(c)!=.ra_cr_rank(s[length(s)])) break;n<-n+1L}
 if(n>=2) points<-points+n*(n-1L)
 if(length(s)>=3) for(k in seq(length(s),3)) {r<-.ra_cr_rank(tail(s,k));if(length(unique(r))==k && max(r)-min(r)==k-1L) {points<-points+k;break}}
 points
}
.ra_cr_cards<-function(d,p) {h<-unlist(d$hands[[p+1L]]);h[sum(.ra_cr_value(unlist(d$sequence)))+.ra_cr_value(h)<=31]}
.ra_cr_award<-function(d,p,n) {d$scores[[p+1L]]<-d$scores[[p+1L]]+n;if(d$scores[[p+1L]]>=121) {d$phase<-"finished";d$winner<-p};d}
.ra_cr_reset<-function(d,last) {
 d$sequence<-list();d$go<-list(FALSE,FALSE);d["last"]<-list(NULL)
 if(!any(lengths(d$hands)>0)) {d$phase<-"show";d$show_index<-0L} else d$turn<-if(length(d$hands[[2L-last]])>0) 1L-last else last
 d
}
.ra_cr_close<-function(d) {
 last<-d$last
 if(!is.null(last)) {d<-.ra_cr_award(d,last,1L);if(d$phase=="finished") return(d)}
 .ra_cr_reset(d,if(is.null(last)) d$turn else last)
}
.ra_cr_validate<-function(d) {
 invalid<-function(msg) .ra_fail(msg,code="INVALID_STATE")
 if(d$phase=="deal") {
  if(any(lengths(d$hands)>0) || any(lengths(d$played)>0) || length(d$crib)>0 || length(d$deck)>0 || !is.null(d$starter) || length(d$sequence)>0 || any(unlist(d$discarded)) || any(unlist(d$go)) || !is.null(d$last) || !is.null(d$winner)) invalid("Awaiting deal must have no cards or pending pegging")
  if(max(unlist(d$scores))>=121) invalid("A winning score must finish the match")
  return(invisible(TRUE))
 }
 if(d$hand_number<1) invalid("A dealt hand requires a positive hand number")
 if(d$phase %in% c("discard","cut","peg") && d$show_index!=0) invalid("Counting index must be zero before the show")
 if(d$phase %in% c("discard","cut") && any(unlist(d$go))) invalid("Go declarations require pegging play")
 cards<-unlist(c(d$hands,d$played,list(d$crib,d$deck,d$starter)))
 if(!identical(sort(as.integer(cards)),0:51)) invalid("Each physical card must occur exactly once")
 if(d$phase=="discard") {
  if(!is.null(d$starter) || length(d$deck)!=40 || length(d$crib)!=2*sum(unlist(d$discarded)) || any(lengths(d$played)>0) || any(vapply(0:1,function(p) length(d$hands[[p+1L]])!=(if(d$discarded[[p+1L]]) 4 else 6),logical(1))) || all(unlist(d$discarded))) invalid("Discard phase requires six cards before and four after each pair is set aside")
 } else {
  if(!all(unlist(d$discarded)) || length(d$crib)!=4 || any(lengths(d$hands)+lengths(d$played)!=4)) invalid("Both retained hands contain four original cards and the crib four")
  if(d$phase=="cut") {if(!is.null(d$starter) || length(d$deck)!=40 || any(lengths(d$played)>0)) invalid("Cut follows both discards before any pegging")} else if(is.null(d$starter) || length(d$deck)!=39) invalid("Pegging and counting require a starter and remaining thirty-nine-card stock")
 }
 sequence<-unlist(d$sequence)
 if(anyDuplicated(sequence) || any(!(sequence %in% unlist(d$played))) || sum(.ra_cr_value(sequence))>31) invalid("Current pegging sequence must contain distinct played cards totaling at most thirty-one")
 if(length(sequence)>0) {if(is.null(d$last) || !(tail(sequence,1) %in% unlist(d$played[[d$last+1L]]))) invalid("Last-card owner must agree with the pegging sequence")} else if(!is.null(d$last)) invalid("Empty pegging sequence has no last-card owner")
 if(d$phase=="peg") {
  if(!length(d$hands[[d$turn+1L]]) || d$go[[d$turn+1L]] || sum(.ra_cr_value(sequence))==31) invalid("Pegging turn requires a player who still acts; thirty-one resets the count")
  for(p in 0:1) if(d$go[[p+1L]] && length(.ra_cr_cards(d,p))>0) invalid("A player may say go only when unable to play within thirty-one")
 }
 if(d$phase=="show" && (any(lengths(d$hands)>0) || length(sequence)>0 || any(unlist(d$go)))) invalid("Count hands only after all pegging cards have been played and reset")
 if(d$phase=="finished") {if(is.null(d$winner) || d$scores[[d$winner+1L]]<121 || d$scores[[2L-d$winner]]>=121) invalid("First player reaching one hundred twenty-one wins immediately")} else if(!is.null(d$winner) || max(unlist(d$scores))>=121) invalid("A winning score must finish the match")
 invisible(TRUE)
}
.ra_cr_apply<-function(state,action) {
 d<-state$data;p<-action$params;k<-action$type
 if(k=="deal") {
  .ra_fields(p,list(order=list(type="array",minItems=52,maxItems=52,items=.ra_cr_card)),"/params")
  if(d$phase!="deal" || anyDuplicated(unlist(p$order))) .ra_fail("Deal every physical card once at a hand boundary")
  fore<-1L-d$dealer;d$hands[[fore+1L]]<-p$order[seq(1,12,by=2)];d$hands[[d$dealer+1L]]<-p$order[seq(2,12,by=2)];d$deck<-p$order[13:52];d$phase<-"discard";d$hand_number<-d$hand_number+1L;return(d)
 }
 if(k=="discard_to_crib") {
  .ra_fields(p,list(player=list(type="integer",minimum=0,maximum=1),cards=list(type="array",minItems=2,maxItems=2,items=.ra_cr_card)),"/params");q<-p$player
  if(!is.null(action$actor) && action$actor!=state$players[[q+1L]]$id) .ra_fail("Wrong actor")
  if(d$phase!="discard" || d$discarded[[q+1L]] || anyDuplicated(unlist(p$cards)) || any(!(unlist(p$cards) %in% unlist(d$hands[[q+1L]])))) .ra_fail("Each player sets aside two distinct held cards once")
  d$hands[[q+1L]]<-as.list(setdiff(unlist(d$hands[[q+1L]]),unlist(p$cards)));d$crib<-c(d$crib,p$cards);d$discarded[[q+1L]]<-TRUE
  if(all(unlist(d$discarded))) d$phase<-"cut"
  return(d)
 }
 if(k=="cut") {
  .ra_fields(p,list(index=list(type="integer",minimum=0,maximum=39)),"/params")
  if(d$phase!="cut") .ra_fail("Cut after both players discard")
  d$starter<-d$deck[[p$index+1L]];d$deck<-d$deck[-(p$index+1L)];d$phase<-"peg";d$turn<-1L-d$dealer
  if(.ra_cr_rank(d$starter)==11) d<-.ra_cr_award(d,d$dealer,2L)
  return(d)
 }
 if(k %in% c("play","go")) {
  if(d$phase!="peg") .ra_fail("No pegging action is available")
  t<-d$turn
  if(!is.null(action$actor) && action$actor!=state$players[[t+1L]]$id) .ra_fail("Wrong actor")
  if(k=="go") {
   .ra_fields(p,list(),"/params");if(length(.ra_cr_cards(d,t))>0) .ra_fail("Say go only when no held card fits within thirty-one")
   d$go[[t+1L]]<-TRUE
   if(length(d$hands[[2L-t]])>0 && !d$go[[2L-t]]) {d$turn<-1L-t;return(d)}
   return(.ra_cr_close(d))
  }
  .ra_fields(p,list(card=.ra_cr_card),"/params");c<-p$card
  if(!(c %in% .ra_cr_cards(d,t))) .ra_fail("Play a held card without passing thirty-one")
  d$hands[[t+1L]]<-as.list(setdiff(unlist(d$hands[[t+1L]]),c));d$played[[t+1L]]<-c(d$played[[t+1L]],list(c));d$sequence<-c(d$sequence,list(c));d$last<-t;d<-.ra_cr_award(d,t,.ra_cr_peg_score(d$sequence))
  if(d$phase=="finished") return(d)
  if(sum(.ra_cr_value(unlist(d$sequence)))==31) return(.ra_cr_reset(d,t))
  if(!any(lengths(d$hands)>0)) return(.ra_cr_close(d))
  if(length(d$hands[[2L-t]])>0 && !d$go[[2L-t]]) d$turn<-1L-t else if(length(d$hands[[t+1L]])>0) d$turn<-t else return(.ra_cr_close(d))
  return(d)
 }
 if(k=="count_show") {
  .ra_fields(p,list(),"/params");if(d$phase!="show") .ra_fail("Complete pegging before counting the hands")
  index<-d$show_index;q<-if(index==0) 1L-d$dealer else d$dealer
  if(!is.null(action$actor) && action$actor!=state$players[[q+1L]]$id) .ra_fail("Wrong counting actor")
  d<-.ra_cr_award(d,q,.ra_cr_show_score(if(index==2) d$crib else d$played[[q+1L]],d$starter,index==2))
  if(d$phase=="finished") return(d)
  if(index<2) {d$show_index<-index+1L;return(d)}
  d$phase<-"deal";d$dealer<-1L-d$dealer;d$turn<-0L;d$hands<-list(list(),list());d$played<-list(list(),list());d$crib<-list();d$deck<-list();d["starter"]<-list(NULL);d$discarded<-list(FALSE,FALSE);d$sequence<-list();d$go<-list(FALSE,FALSE);d["last"]<-list(NULL);d$show_index<-0L;return(d)
 }
 .ra_fail("Unknown action")
}
.ra_cr_legal<-function(state) {
 d<-state$data;actions<-list();types<-list();empty<-structure(list(),names=character())
 event<-function(k,params,cond,obs) list(type=k,params_schema=params,preconditions=list(cond),external_observations=list(obs))
 if(d$phase=="deal") types<-list(event("deal",list(order=list(type="array",minItems=52,maxItems=52,items=.ra_cr_card)),"Deal six alternating cards starting with nondealer.","External complete deck order.")) else if(d$phase=="discard") {
  for(p in 0:1) if(!d$discarded[[p+1L]]) {cs<-utils::combn(sort(unlist(d$hands[[p+1L]])),2,simplify=FALSE);actions<-c(actions,lapply(cs,function(cards) list(type="discard_to_crib",params=list(player=p,cards=as.list(cards)))))}
 } else if(d$phase=="cut") types<-list(event("cut",list(index=list(type="integer",minimum=0,maximum=39)),"Both players have discarded.","External cut position selecting the starter.")) else if(d$phase=="peg") {
  cards<-.ra_cr_cards(d,d$turn);actions<-if(length(cards)>0) lapply(sort(cards),function(c) list(type="play",params=list(card=c))) else list(list(type="go",params=empty))
 } else if(d$phase=="show") actions<-list(list(type="count_show",params=empty))
 list(mode="event_types",actions=actions,action_types=types,constraints=list(turn=d$turn,phase=d$phase,pegging_total=sum(.ra_cr_value(unlist(d$sequence))),show_index=d$show_index),completeness=FALSE,reason="card_choices_with_external_deal_and_cut")
}
.ra_cr_outcome<-function(state,pack) {
 d<-state$data;done<-d$phase=="finished";w<-d$winner
 list(status=if(done) "terminal" else "ongoing",result_kind=if(done) "win" else "none",winners=if(done) list(state$players[[w+1L]]$id) else list(),losers=if(done) list(state$players[[2L-w]]$id) else list(),placements=if(done) lapply(0:1,function(p) list(player_id=state$players[[p+1L]]$id,place=if(p==w) 1L else 2L,score=d$scores[[p+1L]])) else list(),reason=if(done) "first_to_121" else "ongoing",evidence_required=list(),rule_id=if(done) paste0(pack$ruleset_id,"@",pack$ruleset_version) else NULL)
}
