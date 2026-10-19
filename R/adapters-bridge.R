.ra_br_card<-list(type="integer",minimum=0,maximum=51)
.ra_br_auction<-function(d) {
 high<-NULL;bidder<-NULL;double<-0L;passes<-0L;first<-list();done<-FALSE
 for(i in seq_along(d$auction)) {
  call<-d$auction[[i]];p<-(d$dealer+i-1L)%%4L
  if(done) .ra_fail("Auction continued after closing",code="INVALID_STATE")
  if(call==0) passes<-passes+1L else if(call==1) {
   if(is.null(high) || double!=0 || p%%2L==bidder%%2L) .ra_fail("Double requires an opposing undoubled contract",code="INVALID_STATE")
   double<-1L;passes<-0L
  } else if(call==2) {
   if(is.null(high) || double!=1 || p%%2L!=bidder%%2L) .ra_fail("Redouble requires own side's doubled contract",code="INVALID_STATE")
   double<-2L;passes<-0L
  } else {
   if(!is.null(high) && call<=high) .ra_fail("Bids must increase by level and denomination",code="INVALID_STATE")
   high<-call;bidder<-p;double<-0L;passes<-0L;key<-paste(p%%2L,(call-3L)%%5L,sep=":")
   if(is.null(first[[key]])) first[[key]]<-p
  }
  done<-passes>=(if(is.null(high)) 4 else 3)
 }
 contract<-if(is.null(high)) NULL else list(level=(high-3L)%/%5L+1L,strain=(high-3L)%%5L,declarer=first[[paste(bidder%%2L,(high-3L)%%5L,sep=":")]],double=double)
 list(done=done,contract=contract)
}
.ra_br_winner<-function(cards,leader,strain) {
 cards<-unlist(cards);led<-cards[1]%/%13L;trump<-if(strain<4) strain else -1L
 weights<-ifelse(cards%/%13L==trump,2L,ifelse(cards%/%13L==led,1L,0L))*13L+cards%%13L
 (leader+which.max(weights)-1L)%%4L
}
.ra_br_playable<-function(d) {
 hand<-unlist(d$hands[[d$turn+1L]])
 if(!length(d$trick)) return(hand)
 follow<-hand[hand%/%13L==d$trick[[1]]%/%13L]
 if(length(follow)) follow else hand
}
.ra_br_validate<-function(d) {
 invalid<-function(msg) .ra_fail(msg,code="INVALID_STATE")
 if(d$phase=="deal") {
  if(any(lengths(d$hands)>0) || any(lengths(d$won)>0) || length(d$trick)>0 || length(d$auction)>0 || !is.null(d$claim)) invalid("Undealt board must contain no cards or calls")
  return(invisible(TRUE))
 }
 if(!identical(sort(as.integer(unlist(c(d$hands,d$won,list(d$trick))))),0:51) || any(lengths(d$won)%%4L!=0)) invalid("Conserve all fifty-two cards and complete captured tricks")
 result<-.ra_br_auction(d);closed<-result$done;contract<-result$contract;completed<-sum(lengths(d$won)%/%4L)
 if(!closed) {
  if(d$phase!="auction" || completed!=0 || length(d$trick)>0 || any(lengths(d$hands)!=13) || d$turn!=(d$dealer+length(d$auction))%%4L || !is.null(d$claim)) invalid("Active auction must retain full hands and correct caller")
  return(invisible(TRUE))
 }
 if(is.null(contract)) {
  if(d$phase!="finished" || completed!=0 || length(d$trick)>0 || !is.null(d$claim) || any(lengths(d$hands)!=13)) invalid("Four opening passes finish an unplayed board")
  return(invisible(TRUE))
 }
 for(p in 0:3) {
  played<-(p-d$leader)%%4L<length(d$trick)
  if(length(d$hands[[p+1L]])!=13-completed-as.integer(played)) invalid("Hand sizes disagree with completed and current tricks")
 }
 if(d$turn!=(d$leader+length(d$trick))%%4L) invalid("Current trick and player turn disagree")
 if(completed==0 && d$leader!=(contract$declarer+1L)%%4L) invalid("Opening lead belongs to declarer's left-hand opponent")
 if(!is.null(d$claim)) {if(d$claim>13-completed || completed==13 || d$phase!="finished") invalid("Adjudicated claim must allocate remaining tricks and finish play")} else if(d$phase!=(if(completed==13) "finished" else "play")) invalid("Thirteen completed tricks finish the board")
 invisible(TRUE)
}
.ra_br_apply<-function(state,action) {
 d<-state$data;p<-action$params;k<-action$type
 if(k=="deal") {
  .ra_fields(p,list(order=list(type="array",minItems=52,maxItems=52,items=.ra_br_card),dealer=list(type="integer",minimum=0,maximum=3),vulnerable=list(type="array",minItems=2,maxItems=2,items=list(type="boolean"))),"/params")
  if(d$phase!="deal" || anyDuplicated(unlist(p$order))) .ra_fail("Supply a complete unique deck for an undealt board")
  d$dealer<-p$dealer;d$vulnerable<-p$vulnerable;d$turn<-p$dealer;d$hands<-lapply(1:4,function(i) p$order[seq(i,52,by=4)]);d$phase<-"auction";return(d)
 }
 if(k=="call") {
  .ra_fields(p,list(call=list(type="integer",minimum=0,maximum=37)),"/params")
  if(d$phase!="auction") .ra_fail("Calls are allowed only in an open auction")
  if(!is.null(action$actor) && action$actor!=state$players[[d$turn+1L]]$id) .ra_fail("Wrong actor")
  d$auction<-c(d$auction,list(p$call));result<-.ra_br_auction(d);d$turn<-(d$turn+1L)%%4L
  if(result$done) {if(is.null(result$contract)) d$phase<-"finished" else {d$phase<-"play";d$leader<-(result$contract$declarer+1L)%%4L;d$turn<-d$leader}}
  return(d)
 }
 if(k=="settle_claim") {
  .ra_fields(p,list(ns_remaining=list(type="integer",minimum=0,maximum=13)),"/params")
  if(d$phase!="play" || p$ns_remaining>13-sum(lengths(d$won)%/%4L)) .ra_fail("Allocate only the remaining tricks by agreement or official adjudication")
  d$claim<-p$ns_remaining;d$phase<-"finished";return(d)
 }
 if(k!="play") .ra_fail("Unknown action")
 .ra_fields(p,list(card=.ra_br_card),"/params")
 if(d$phase!="play") .ra_fail("Card play requires a completed non-passed-out auction")
 contract<-.ra_br_auction(d)$contract;controller<-if(d$turn==(contract$declarer+2L)%%4L) contract$declarer else d$turn
 if(!is.null(action$actor) && action$actor!=state$players[[controller+1L]]$id) .ra_fail("The declarer controls dummy; other players control their own hands")
 if(!(p$card %in% .ra_br_playable(d))) .ra_fail("Play a held card and follow the led suit when possible")
 d$hands[[d$turn+1L]]<-as.list(setdiff(unlist(d$hands[[d$turn+1L]]),p$card));d$trick<-c(d$trick,list(p$card))
 if(length(d$trick)==4) {
  w<-.ra_br_winner(d$trick,d$leader,contract$strain);d$won[[w+1L]]<-c(d$won[[w+1L]],d$trick);d$trick<-list();d$leader<-w;d$turn<-w
  if(!any(lengths(d$hands)>0)) d$phase<-"finished"
 } else d$turn<-(d$turn+1L)%%4L
 d
}
.ra_br_score<-function(c,tricks,vulnerable) {
 made<-tricks-(6L+c$level);double<-c$double;factor<-2^double;strain<-c$strain
 if(made<0) {
  down<- -made
  if(double==0) return(-down*(if(vulnerable) 100L else 50L))
  penalty<-if(vulnerable) 200L+300L*(down-1L) else 100L+200L*min(2,down-1L)+300L*max(0,down-3L)
  return(-penalty*(if(double==2) 2L else 1L))
 }
 base<-(if(strain<2) 20L*c$level else 30L*c$level+if(strain==4) 10L else 0L)*factor
 bonus<-if(base>=100) if(vulnerable) 500L else 300L else 50L
 if(c$level==6) bonus<-bonus+if(vulnerable) 750L else 500L
 if(c$level==7) bonus<-bonus+if(vulnerable) 1500L else 1000L
 insult<-if(double>0) 50L*(factor%/%2L) else 0L
 over<-made*(if(double>0) (if(vulnerable) 200L else 100L)*(if(double==2) 2L else 1L) else if(strain<2) 20L else 30L)
 base+bonus+insult+over
}
.ra_br_legal<-function(state) {
 d<-state$data;actions<-list();types<-list();constraints<-list(turn=d$turn)
 if(d$phase=="deal") types<-list(list(type="deal",params_schema=list(order=list(type="array",minItems=52,maxItems=52,items=.ra_br_card),dealer=list(type="integer",minimum=0,maximum=3),vulnerable=list(type="array",minItems=2,maxItems=2,items=list(type="boolean"))),preconditions=list("Every physical card exactly once."),external_observations=list("External card allocation, board dealer and NS/EW vulnerability."))) else if(d$phase=="auction") {
  c<-.ra_br_auction(d)$contract;bids<-unlist(d$auction);bids<-bids[bids>=3];high<-if(length(bids)) max(bids) else 2L
  calls<-c(0L,if(high<37) seq(high+1L,37L) else integer())
  if(!is.null(c)) {if(d$turn%%2L!=c$declarer%%2L && c$double==0) calls<-c(calls,1L);if(d$turn%%2L==c$declarer%%2L && c$double==1) calls<-c(calls,2L)}
  actions<-lapply(sort(calls),function(c) list(type="call",params=list(call=c)))
 } else {
  c<-.ra_br_auction(d)$contract;actions<-lapply(sort(.ra_br_playable(d)),function(x) list(type="play",params=list(card=x)));dummy<-(c$declarer+2L)%%4L
  constraints<-c(constraints,list(declarer=c$declarer,dummy=dummy,dummy_exposed=length(d$trick)>0 || any(lengths(d$won)>0),controller=if(d$turn==dummy) c$declarer else d$turn))
  types<-list(list(type="settle_claim",params_schema=list(ns_remaining=list(type="integer",minimum=0,maximum=13-sum(lengths(d$won)%/%4L))),preconditions=list("Agreement by the opponents or official adjudication fixes all remaining tricks."),external_observations=list("Agreed or adjudicated number of remaining tricks awarded to North-South.")))
 }
 list(mode="event_types",actions=actions,action_types=types,constraints=constraints,completeness=FALSE,reason="enumerated_calls_and_cards_with_external_deal_or_claim")
}
.ra_br_outcome<-function(state,pack) {
 d<-state$data;done<-d$phase=="finished";c<-if(d$phase!="deal") .ra_br_auction(d)$contract else NULL;score<-0L
 if(done && !is.null(c)) {ns<-sum(lengths(d$won)[c(1,3)]%/%4L)+(if(!is.null(d$claim)) d$claim else 0L);tricks<-if(c$declarer%%2L==0) ns else 13L-ns;score<-.ra_br_score(c,tricks,d$vulnerable[[c$declarer%%2L+1L]])*(if(c$declarer%%2L==0) 1L else -1L)}
 side<-if(score>0) 0L else 1L
 list(status=if(done) "terminal" else "ongoing",result_kind=if(done) if(score==0) "draw" else "win" else "none",winners=if(done && score!=0) lapply((0:3)[(0:3)%%2L==side],function(p) state$players[[p+1L]]$id) else list(),losers=if(done && score!=0) lapply((0:3)[(0:3)%%2L!=side],function(p) state$players[[p+1L]]$id) else list(),placements=if(done) list(list(side="NS",score=score),list(side="EW",score=-score)) else list(),reason=if(done) if(is.null(c)) "passed_out" else "contract_scored" else "ongoing",evidence_required=list(),rule_id=if(done) paste0(pack$ruleset_id,"@",pack$ruleset_version) else NULL)
}
