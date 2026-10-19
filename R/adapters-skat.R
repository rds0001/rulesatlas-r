.ra_skat_card<-list(type="integer",minimum=0,maximum=31)
.ra_skat_index<-list(type="integer",minimum=0,maximum=2)
.ra_skat_suit<-list(type="integer",minimum=0,maximum=3)
.ra_skat_category<-function(c,trump) if(c%%8==3 || c%/%8==trump) 4 else c%/%8
.ra_skat_strength<-function(c) if(c%%8==3) 103-c%/%8 else c%%8
.ra_skat_winner<-function(trick,trump) {
 lead<-.ra_skat_category(trick[[1]]$card,trump)
 keys<-vapply(trick,function(x) {cat<-.ra_skat_category(x$card,trump);(if(cat==4) 2 else if(cat==lead) 1 else 0)*1000+.ra_skat_strength(x$card)},numeric(1))
 trick[[which.max(keys)]]$player
}
.ra_skat_playable<-function(d) {
 hand<-d$hands[[d$turn+1L]];if(length(d$trick)==0) return(hand)
 led<-.ra_skat_category(d$trick[[1]]$card,d$trump);follow<-Filter(function(c) .ra_skat_category(c,d$trump)==led,hand)
 if(length(follow)>0) follow else hand
}
.ra_skat_points<-function(d) {
 values<-c(0,0,0,2,3,4,10,11);p<-vapply(d$won,function(w) sum(values[as.integer(unlist(w))%%8+1L]),numeric(1))
 if(!is.null(d$declarer)) p[d$declarer+1L]<-p[d$declarer+1L]+sum(values[as.integer(unlist(d$skat))%%8+1L]);p
}
.ra_skat_validate<-function(d) {
 bad<-function(m) .ra_fail(m,code="INVALID_STATE");cards<-c(unlist(d$hands),unlist(d$won),unlist(d$skat),vapply(d$trick,function(x) x$card,numeric(1)))
 if(d$phase=="setup") {if(length(cards)>0 || !is.null(d$trump) || !is.null(d$declarer)) bad("Unstarted hand must contain no dealt cards or contract");return(invisible(TRUE))}
 if(length(d$skat)!=2 || !identical(as.integer(sort(cards)),0:31) || is.null(d$trump) || is.null(d$declarer)) bad("A complete unique deck and suit hand contract are required")
 if(any(lengths(d$won)%%3!=0)) bad("Won piles must contain complete tricks")
 completed<-sum(lengths(d$won))/3;actors<-vapply(d$trick,function(x) x$player,numeric(1))
 if(anyDuplicated(actors)>0 || (length(actors)>1 && any(actors[-1]!=(actors[-length(actors)]+1)%%3)) || (length(actors)>0 && d$turn!=(tail(actors,1)+1)%%3)) bad("Partial trick turn order is inconsistent")
 for(p in 0:2) if(length(d$hands[[p+1L]])!=10-completed-as.integer(p %in% actors)) bad("Hand sizes disagree with the trick count")
 if((d$phase=="finished")!=(completed==10)) bad("Resolve all ten tricks before the hand ends")
 invisible(TRUE)
}
.ra_skat_apply<-function(state,action) {
 d<-state$data;p<-action$params;k<-action$type;t<-d$turn
 if(k=="deal") {
  .ra_fields(p,list(deck=list(type="array",minItems=32,maxItems=32,items=.ra_skat_card),declarer=.ra_skat_index,trump=.ra_skat_suit,leader=.ra_skat_index),"/params")
  if(d$phase!="setup" || !identical(as.integer(sort(unlist(p$deck))),0:31)) .ra_fail("Deal one complete external deck with the agreed suit hand contract")
  d$phase<-"play";d$hands<-lapply(1:3,function(i) p$deck[seq(i,30,by=3)]);d$skat<-p$deck[31:32];d$won<-rep(list(list()),3);d$trick<-list();d$turn<-p$leader;d$declarer<-p$declarer;d$trump<-p$trump;return(d)
 }
 if(k!="play" || d$phase!="play") .ra_fail("Play a card during an active Skat hand")
 .ra_fields(p,list(card=.ra_skat_card),"/params")
 if(!is.null(action$actor) && action$actor!=state$players[[t+1L]]$id) .ra_fail("Wrong actor")
 c<-p$card
 if(!(c %in% unlist(.ra_skat_playable(d)))) .ra_fail("Follow the led suit or trump category when possible")
 d$hands[[t+1L]]<-Filter(function(x) x!=c,d$hands[[t+1L]]);d$trick[[length(d$trick)+1L]]<-list(player=t,card=c);d$turn<-(t+1L)%%3L
 if(length(d$trick)==3) {
  w<-.ra_skat_winner(d$trick,d$trump);d$won[[w+1L]]<-c(d$won[[w+1L]],lapply(d$trick,function(x) x$card));d$trick<-list();d$turn<-w
  if(sum(lengths(d$hands))==0) d$phase<-"finished"
 };d
}
.ra_skat_legal<-function(state) {
 d<-state$data
 if(d$phase=="setup") return(list(mode="event_types",actions=list(),action_types=list(list(type="deal",params_schema=list(deck=list(type="array",minItems=32,maxItems=32,items=.ra_skat_card),declarer=.ra_skat_index,trump=.ra_skat_suit,leader=.ra_skat_index),preconditions=list("Supply each card once and the pre-agreed suit hand contract."),external_observations=list("External shuffle and agreed declarer, trump suit and opening player."))),constraints=setNames(list(),character()),completeness=FALSE,reason="external_skat_deal"))
 if(d$phase=="play") lapply(.ra_skat_playable(d),function(c) list(type="play",params=list(card=c))) else list()
}
.ra_skat_outcome<-function(state,pack) {
 d<-state$data;done<-d$phase=="finished";p<-.ra_skat_points(d);decl<-d$declarer;win<-done && p[decl+1L]>=61;ws<-if(done) {if(win) decl else setdiff(0:2,decl)} else integer()
 list(status=if(done) "terminal" else "ongoing",result_kind=if(done) "win" else "none",winners=lapply(ws,function(i) state$players[[i+1L]]$id),losers=if(done) lapply(setdiff(0:2,ws),function(i) state$players[[i+1L]]$id) else list(),placements=list(),reason=if(done) {if(win) "declarer_at_least_sixty_one" else "defenders_at_least_sixty"} else "ongoing",evidence_required=list(),rule_id=if(done) paste0(pack$ruleset_id,"@",pack$ruleset_version) else NULL)
}
