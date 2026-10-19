.ra_be_card<-list(type="integer",minimum=0,maximum=31)
.ra_be_strength<-function(c,trump) (if(c %/% 8==trump) c(0,1,6,7,2,3,4,5) else 0:7)[c %% 8+1L]
.ra_be_winner<-function(trick,trump) {
 lead<-trick[[1]]$card %/% 8;groups<-vapply(trick,function(x) if(x$card %/% 8==trump) 2L else if(x$card %/% 8==lead) 1L else 0L,integer(1));strength<-vapply(trick,function(x) .ra_be_strength(x$card,trump),numeric(1));trick[[order(-groups,-strength)[1]]]$player
}
.ra_be_playable<-function(d) {
 hand<-d$hands[[d$turn+1L]];trick<-d$trick;trump<-d$trump;if(!length(trick)) return(hand)
 lead<-trick[[1]]$card %/% 8;follow<-Filter(function(c) c %/% 8==lead,hand);win<-.ra_be_winner(trick,trump);top<-Filter(function(x) x$player==win,trick)[[1]]$card;trumps<-Filter(function(c) c %/% 8==trump,hand)
 higher<-Filter(function(c) top %/% 8!=trump || .ra_be_strength(c,trump)>.ra_be_strength(top,trump),trumps)
 if(length(follow)) return(if(lead==trump && length(higher)) higher else follow)
 if(win %% 2==d$turn %% 2) return(hand)
 if(length(higher)) higher else if(length(trumps)) trumps else hand
}
.ra_be_validate<-function(d) {
 cards<-c(unlist(d$hands),unlist(d$stock),unlist(d$won),vapply(d$trick,function(x) x$card,numeric(1)),d$upcard)
 if(d$phase=="setup") {if(length(cards) || any(unlist(d$scores)!=0) || d$carry!=0) .ra_fail("Initial match must be empty",code="INVALID_STATE");return(invisible(TRUE))}
 if(!identical(as.integer(sort(cards)),0:31)) .ra_fail("Every card must occur exactly once",code="INVALID_STATE")
 bidding<-d$phase %in% c("bid_first","bid_second","redeal")
 if(bidding) {
  if(!is.null(d$trump) || !is.null(d$maker) || length(d$stock)!=11L || is.null(d$upcard) || any(lengths(d$hands)!=5L) || any(lengths(d$won)>0) || length(d$trick)>0 || d$turn!=(d$dealer+1L+d$passes) %% 4L) .ra_fail("Invalid trump-selection state",code="INVALID_STATE")
 } else {
  if(is.null(d$trump) || is.null(d$maker) || length(d$stock)>0 || !is.null(d$upcard)) .ra_fail("Playing contract is incomplete",code="INVALID_STATE")
  if(any(lengths(d$won) %% 4L!=0)) .ra_fail("Won piles must contain whole tricks",code="INVALID_STATE")
  completed<-sum(lengths(d$won)) %/% 4L;actors<-vapply(d$trick,function(x) x$player,numeric(1))
  if(length(actors)>3L || anyDuplicated(actors) || (length(actors)>1L && any(actors[-1]!=(head(actors,-1)+1L) %% 4L)) || (length(actors)>0 && d$turn!=(tail(actors,1)+1L) %% 4L)) .ra_fail("Partial trick order is invalid",code="INVALID_STATE")
  if(any(vapply(0:3,function(p) length(d$hands[[p+1L]])!=8L-completed-as.integer(p %in% actors),logical(1)))) .ra_fail("Hands do not match completed tricks",code="INVALID_STATE")
  if((d$phase %in% c("round_end","finished"))!=(completed==8L)) .ra_fail("Round phase and trick count disagree",code="INVALID_STATE")
 }
 if(is.null(d$belote_player)!=is.null(d$belote_card)) .ra_fail("Pending belote announcement is incomplete",code="INVALID_STATE")
 if(!is.null(d$belote_card)) {
  if(bidding || d$belote_card %/% 8!=d$trump || !d$belote_card %% 8 %in% c(4,5) || !d$belote_card %in% unlist(d$hands[[d$belote_player+1L]]) || !is.null(d$belote_team)) .ra_fail("Rebelote must remain in the announcing player hand",code="INVALID_STATE")
 }
 if((d$phase=="finished")!=(max(unlist(d$scores))>=1000 && d$scores[[1]]!=d$scores[[2]])) .ra_fail("Match threshold disagrees with phase",code="INVALID_STATE")
 invisible(TRUE)
}
.ra_be_score<-function(d) {
 team<-d$maker %% 2+1L
 points<-vapply(d$won,function(w) sum(vapply(w,function(c) (if(c %/% 8==d$trump) c(0,0,14,20,3,4,10,11) else c(0,0,0,2,3,4,10,11))[c %% 8+1L],numeric(1))),numeric(1))
 sweep<-any(lengths(d$won)==32L);points[d$turn %% 2+1L]<-points[d$turn %% 2+1L]+if(sweep) 100 else 10
 bonus<-vapply(0:1,function(i) if(!is.null(d$belote_team) && d$belote_team==i) 20 else 0,numeric(1))
 if(points[team]==points[3L-team]) {d$scores[[3L-team]]<-d$scores[[3L-team]]+points[3L-team];d$carry<-d$carry+points[team]} else {
  winning<-if(points[team]>points[3L-team]) team else 3L-team
  if(winning==team) {for(i in 1:2) d$scores[[i]]<-d$scores[[i]]+points[i]} else d$scores[[winning]]<-d$scores[[winning]]+sum(points)
  d$scores[[winning]]<-d$scores[[winning]]+d$carry;d$carry<-0L
 }
 for(i in 1:2) d$scores[[i]]<-d$scores[[i]]+bonus[i]
 d$phase<-if(max(unlist(d$scores))>=1000 && d$scores[[1]]!=d$scores[[2]]) "finished" else "round_end";d
}
.ra_be_apply<-function(state,action) {
 d<-state$data;p<-action$params;k<-action$type;t<-d$turn;phase<-d$phase
 if(k=="deal") {
  .ra_fields(p,list(deck=list(type="array",minItems=32,maxItems=32,items=.ra_be_card)),"/params")
  if(!phase %in% c("setup","round_end","redeal") || !identical(as.integer(sort(unlist(p$deck))),0:31)) .ra_fail("Supply a complete external deck between deals")
  if(phase!="setup") d$dealer<-(d$dealer+1L) %% 4L
  d$hands<-rep(list(list()),4);d$won<-rep(list(list()),2);d$stock<-p$deck[22:32];d$upcard<-p$deck[[21]];d["trump"]<-list(NULL);d["maker"]<-list(NULL);d$passes<-0L;d$trick<-list();d$turn<-(d$dealer+1L) %% 4L;d$phase<-"bid_first";d["belote_player"]<-list(NULL);d["belote_card"]<-list(NULL);d["belote_team"]<-list(NULL)
  for(i in 0:19) {j<-(d$dealer+1L+i) %% 4L+1L;d$hands[[j]]<-c(d$hands[[j]],p$deck[i+1L])}
  return(d)
 }
 if(!is.null(action$actor) && action$actor!=state$players[[t+1L]]$id) .ra_fail("Wrong actor")
 if(phase %in% c("bid_first","bid_second")) {
  if(k=="pass") {
   .ra_fields(p,list(),"/params");d$passes<-d$passes+1L;d$turn<-(t+1L) %% 4L
   if(d$passes==4L) {d$passes<-0L;d$phase<-if(phase=="bid_first") "bid_second" else "redeal"}
  } else if(k=="take") {
   .ra_fields(p,list(trump=list(type="integer",minimum=0,maximum=3)),"/params")
   if((p$trump==d$upcard %/% 8)!=(phase=="bid_first")) .ra_fail("First selection uses the upcard suit; second selection must use another suit")
   d$trump<-p$trump;d$maker<-t;d$hands[[t+1L]]<-c(d$hands[[t+1L]],list(d$upcard));d["upcard"]<-list(NULL)
   for(j in 0:3) {player<-(d$dealer+1L+j) %% 4L;n<-if(player==t) 2L else 3L;d$hands[[player+1L]]<-c(d$hands[[player+1L]],d$stock[seq_len(n)]);d$stock<-d$stock[-seq_len(n)]}
   d$turn<-(d$dealer+1L) %% 4L;d$phase<-"play"
  } else .ra_fail("Pass or take the trump contract")
  return(d)
 }
 if(phase!="play" || k!="play") .ra_fail("Play a legal card during a trick")
 .ra_fields(p,list(card=.ra_be_card,announce=list(type="boolean")),"/params");card<-p$card
 if(!card %in% unlist(.ra_be_playable(d))) .ra_fail("Follow suit, head trump, and trump opponents according to the selected obligations")
 pending<-!is.null(d$belote_player) && d$belote_player==t && d$belote_card==card
 if(p$announce) {
  if(pending) d$belote_team<-t %% 2 else {
   other<-if(card %% 8==4) card+1L else card-1L
   if(!is.null(d$belote_team) || card %/% 8!=d$trump || !card %% 8 %in% c(4,5) || !other %in% unlist(d$hands[[t+1L]])) .ra_fail("Belote requires both trump Queen and King when first announced")
   d$belote_player<-t;d$belote_card<-other
  }
 }
 if(pending) {d["belote_player"]<-list(NULL);d["belote_card"]<-list(NULL)}
 d$hands[[t+1L]]<-Filter(function(c) c!=card,d$hands[[t+1L]]);d$trick<-c(d$trick,list(list(player=t,card=card)));d$turn<-(t+1L) %% 4L
 if(length(d$trick)==4L) {
  w<-.ra_be_winner(d$trick,d$trump);d$won[[w %% 2+1L]]<-c(d$won[[w %% 2+1L]],lapply(d$trick,function(x) x$card));d$trick<-list();d$turn<-w
  if(sum(lengths(d$won))==32L) d<-.ra_be_score(d)
 }
 d
}
.ra_be_legal<-function(state) {
 d<-state$data;phase<-d$phase;empty<-setNames(list(),character())
 if(phase %in% c("setup","round_end","redeal")) return(list(mode="event_types",actions=list(),action_types=list(list(type="deal",params_schema=list(deck=list(type="array",minItems=32,maxItems=32,items=.ra_be_card)),preconditions=list("A full externally supplied deck permutation is required."),external_observations=list("Externally shuffled deck."))),constraints=empty,completeness=FALSE,reason="external_deal"))
 if(phase %in% c("bid_first","bid_second")) {
  suits<-if(phase=="bid_first") d$upcard %/% 8 else setdiff(0:3,d$upcard %/% 8)
  return(c(list(list(type="pass",params=empty)),lapply(suits,function(s) list(type="take",params=list(trump=s)))))
 }
 result<-list()
 for(card in .ra_be_playable(d)) {
  result<-c(result,list(list(type="play",params=list(card=card,announce=FALSE))));other<-if(card %% 8==4) card+1L else card-1L
  pending<-!is.null(d$belote_player) && d$belote_player==d$turn && d$belote_card==card
  if(pending || (card %/% 8==d$trump && card %% 8 %in% c(4,5) && other %in% unlist(d$hands[[d$turn+1L]]) && is.null(d$belote_team))) result<-c(result,list(list(type="play",params=list(card=card,announce=TRUE))))
 }
 result
}
.ra_be_outcome<-function(state,pack) {
 d<-state$data;done<-d$phase=="finished";team<-which.max(unlist(d$scores))-1L
 list(status=if(done) "terminal" else "ongoing",result_kind=if(done) "win" else "none",winners=if(done) lapply(Filter(function(i) i %% 2==team,0:3),function(i) state$players[[i+1L]]$id) else list(),losers=if(done) lapply(Filter(function(i) i %% 2!=team,0:3),function(i) state$players[[i+1L]]$id) else list(),placements=list(),reason=if(done) "team_reached_one_thousand" else "ongoing",evidence_required=list(),rule_id=if(done) paste0(pack$ruleset_id,"@",pack$ruleset_version) else NULL)
}
