.ra_schafkopf_card<-list(type="integer",minimum=0,maximum=31)
.ra_schafkopf_index<-list(type="integer",minimum=0,maximum=3)
.ra_schafkopf_category<-function(c,trump) if(c%%8 %in% c(3,4) || c%/%8==trump) 4 else c%/%8
.ra_schafkopf_strength<-function(c) if(c%%8==4) 103-c%/%8 else if(c%%8==3) 93-c%/%8 else c%%8
.ra_schafkopf_winner<-function(trick,trump) {
 lead<-.ra_schafkopf_category(trick[[1]]$card,trump)
 keys<-vapply(trick,function(x) {cat<-.ra_schafkopf_category(x$card,trump);(if(cat==4) 2 else if(cat==lead) 1 else 0)*1000+.ra_schafkopf_strength(x$card)},numeric(1))
 trick[[which.max(keys)]]$player
}
.ra_schafkopf_playable<-function(d) {
 hand<-d$hands[[d$turn+1L]];if(length(d$trick)==0) return(hand)
 led<-.ra_schafkopf_category(d$trick[[1]]$card,d$trump);follow<-Filter(function(c) .ra_schafkopf_category(c,d$trump)==led,hand)
 if(length(follow)>0) follow else hand
}
.ra_schafkopf_points<-function(d) vapply(d$won,function(w) sum(c(0,0,0,2,3,4,10,11)[as.integer(unlist(w))%%8+1L]),numeric(1))
.ra_schafkopf_validate<-function(d) {
 bad<-function(m) .ra_fail(m,code="INVALID_STATE");cards<-c(unlist(d$hands),unlist(d$won),vapply(d$trick,function(x) x$card,numeric(1)))
 if(d$phase=="setup") {if(length(cards)>0 || !is.null(d$trump) || !is.null(d$declarer)) bad("Unstarted solo must contain no dealt cards or contract");return(invisible(TRUE))}
 if(!identical(as.integer(sort(cards)),0:31) || is.null(d$trump) || is.null(d$declarer)) bad("A complete unique deck and solo contract are required")
 if(any(lengths(d$won)%%4!=0)) bad("Won piles must contain complete tricks")
 completed<-sum(lengths(d$won))/4;actors<-vapply(d$trick,function(x) x$player,numeric(1))
 if(anyDuplicated(actors)>0 || (length(actors)>1 && any(actors[-1]!=(actors[-length(actors)]+1)%%4)) || (length(actors)>0 && d$turn!=(tail(actors,1)+1)%%4)) bad("Partial trick turn order is inconsistent")
 for(p in 0:3) if(length(d$hands[[p+1L]])!=8-completed-as.integer(p %in% actors)) bad("Hand sizes disagree with the trick count")
 if((d$phase=="finished")!=(completed==8)) bad("Resolve all eight tricks before the hand ends")
 invisible(TRUE)
}
.ra_schafkopf_apply<-function(state,action) {
 d<-state$data;p<-action$params;k<-action$type;t<-d$turn
 if(k=="deal") {
  .ra_fields(p,list(deck=list(type="array",minItems=32,maxItems=32,items=.ra_schafkopf_card),declarer=.ra_schafkopf_index,trump=.ra_schafkopf_index,leader=.ra_schafkopf_index),"/params")
  if(d$phase!="setup" || !identical(as.integer(sort(unlist(p$deck))),0:31)) .ra_fail("Deal one complete external deck with the agreed solo contract")
  d$phase<-"play";d$hands<-lapply(1:4,function(i) p$deck[seq(i,32,by=4)]);d$won<-rep(list(list()),4);d$trick<-list();d$turn<-p$leader;d$declarer<-p$declarer;d$trump<-p$trump;return(d)
 }
 if(k!="play" || d$phase!="play") .ra_fail("Play a card during an active solo hand")
 .ra_fields(p,list(card=.ra_schafkopf_card),"/params")
 if(!is.null(action$actor) && action$actor!=state$players[[t+1L]]$id) .ra_fail("Wrong actor")
 c<-p$card
 if(!(c %in% unlist(.ra_schafkopf_playable(d)))) .ra_fail("Follow the led suit or trump category when possible")
 d$hands[[t+1L]]<-Filter(function(x) x!=c,d$hands[[t+1L]]);d$trick[[length(d$trick)+1L]]<-list(player=t,card=c);d$turn<-(t+1L)%%4L
 if(length(d$trick)==4) {
  w<-.ra_schafkopf_winner(d$trick,d$trump);d$won[[w+1L]]<-c(d$won[[w+1L]],lapply(d$trick,function(x) x$card));d$trick<-list();d$turn<-w
  if(sum(lengths(d$hands))==0) d$phase<-"finished"
 };d
}
.ra_schafkopf_legal<-function(state) {
 d<-state$data
 if(d$phase=="setup") return(list(mode="event_types",actions=list(),action_types=list(list(type="deal",params_schema=list(deck=list(type="array",minItems=32,maxItems=32,items=.ra_schafkopf_card),declarer=.ra_schafkopf_index,trump=.ra_schafkopf_index,leader=.ra_schafkopf_index),preconditions=list("Supply each card once and the pre-agreed solo contract."),external_observations=list("External shuffle and agreed declarer, trump suit and opening player."))),constraints=setNames(list(),character()),completeness=FALSE,reason="external_solo_deal"))
 if(d$phase=="play") lapply(.ra_schafkopf_playable(d),function(c) list(type="play",params=list(card=c))) else list()
}
.ra_schafkopf_outcome<-function(state,pack) {
 d<-state$data;done<-d$phase=="finished";p<-.ra_schafkopf_points(d);decl<-d$declarer;win<-done && p[decl+1L]>=61;ws<-if(done) {if(win) decl else setdiff(0:3,decl)} else integer()
 list(status=if(done) "terminal" else "ongoing",result_kind=if(done) "win" else "none",winners=lapply(ws,function(i) state$players[[i+1L]]$id),losers=if(done) lapply(setdiff(0:3,ws),function(i) state$players[[i+1L]]$id) else list(),placements=list(),reason=if(done) {if(win) "solo_at_least_sixty_one" else "defenders_at_least_sixty"} else "ongoing",evidence_required=list(),rule_id=if(done) paste0(pack$ruleset_id,"@",pack$ruleset_version) else NULL)
}
