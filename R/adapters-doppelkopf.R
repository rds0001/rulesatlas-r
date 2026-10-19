.ra_doppelkopf_card<-list(type="integer",minimum=0,maximum=47)
.ra_doppelkopf_index<-list(type="integer",minimum=0,maximum=3)
.ra_doppelkopf_category<-function(c) {c<-c%%24;s<-c%/%6;r<-c%%6;if(r %in% c(1,2) || s==3 || c==16) 4 else s}
.ra_doppelkopf_strength<-function(c) {c<-c%%24;s<-c%/%6;r<-c%%6;if(c==16) 200 else if(r==2) 103-s else if(r==1) 93-s else r}
.ra_doppelkopf_winner<-function(trick) {
 led<-.ra_doppelkopf_category(trick[[1]]$card);keys<-vapply(trick,function(x) {cat<-.ra_doppelkopf_category(x$card);(if(cat==4) 2 else if(cat==led) 1 else 0)*1000+.ra_doppelkopf_strength(x$card)},numeric(1));trick[[which.max(keys)]]$player
}
.ra_doppelkopf_playable<-function(d) {h<-d$hands[[d$turn+1L]];if(length(d$trick)==0) return(h);led<-.ra_doppelkopf_category(d$trick[[1]]$card);f<-Filter(function(c) .ra_doppelkopf_category(c)==led,h);if(length(f)>0) f else h}
.ra_doppelkopf_validate<-function(d) {
 bad<-function(m) .ra_fail(m,code="INVALID_STATE");cards<-c(unlist(d$hands),unlist(d$won),vapply(d$trick,function(x) x$card,numeric(1)))
 if(d$phase=="setup") {if(length(cards)>0 || length(d$re_players)>0) bad("Unstarted hand must be empty");return(invisible(TRUE))}
 if(!identical(as.integer(sort(cards)),0:47)) bad("Each physical card must occur exactly once")
 if(anyDuplicated(unlist(d$re_players))>0 || !(length(d$re_players) %in% c(1,2))) bad("Re consists of the original clubs-queen holders")
 for(i in 1:4) if(any(unlist(d$hands[[i]]) %in% c(2,26)) && !((i-1L) %in% unlist(d$re_players))) bad("A clubs-queen holder must belong to Re")
 if(any(lengths(d$won)%%4!=0)) bad("Won piles contain complete tricks")
 completed<-sum(lengths(d$won))/4;actors<-vapply(d$trick,function(x) x$player,numeric(1))
 if(anyDuplicated(actors)>0 || (length(actors)>1 && any(actors[-1]!=(actors[-length(actors)]+1)%%4)) || (length(actors)>0 && d$turn!=(tail(actors,1)+1)%%4)) bad("Partial trick order is inconsistent")
 for(p in 0:3) if(length(d$hands[[p+1L]])!=12-completed-as.integer(p %in% actors)) bad("Hand sizes disagree with completed tricks")
 if((d$phase=="finished")!=(completed==12)) bad("All twelve tricks must resolve")
 invisible(TRUE)
}
.ra_doppelkopf_apply<-function(state,action) {
 d<-state$data;p<-action$params;k<-action$type;t<-d$turn
 if(k=="deal") {
  .ra_fields(p,list(deck=list(type="array",minItems=48,maxItems=48,items=.ra_doppelkopf_card),leader=.ra_doppelkopf_index),"/params")
  if(d$phase!="setup" || !identical(as.integer(sort(unlist(p$deck))),0:47)) .ra_fail("Supply one complete external deck before play")
  d$phase<-"play";d$hands<-lapply(1:4,function(i) p$deck[seq(i,48,by=4)]);d$won<-rep(list(list()),4);d$trick<-list();d$turn<-p$leader;d$re_players<-as.list(which(vapply(d$hands,function(h) any(unlist(h) %in% c(2,26)),logical(1)))-1L);return(d)
 }
 if(k!="play" || d$phase!="play") .ra_fail("Play a card during the hand")
 .ra_fields(p,list(card=.ra_doppelkopf_card),"/params")
 if(!is.null(action$actor) && action$actor!=state$players[[t+1L]]$id) .ra_fail("Wrong actor")
 c<-p$card;if(!(c %in% unlist(.ra_doppelkopf_playable(d)))) .ra_fail("Follow the led effective suit or trump when possible")
 d$hands[[t+1L]]<-Filter(function(x) x!=c,d$hands[[t+1L]]);d$trick[[length(d$trick)+1L]]<-list(player=t,card=c);d$turn<-(t+1L)%%4L
 if(length(d$trick)==4) {w<-.ra_doppelkopf_winner(d$trick);d$won[[w+1L]]<-c(d$won[[w+1L]],lapply(d$trick,function(x) x$card));d$trick<-list();d$turn<-w;if(sum(lengths(d$hands))==0) d$phase<-"finished"};d
}
.ra_doppelkopf_legal<-function(state) {
 d<-state$data
 if(d$phase=="setup") return(list(mode="event_types",actions=list(),action_types=list(list(type="deal",params_schema=list(deck=list(type="array",minItems=48,maxItems=48,items=.ra_doppelkopf_card),leader=.ra_doppelkopf_index),preconditions=list("Supply every physical card once."),external_observations=list("External shuffled deck and opening player."))),constraints=setNames(list(),character()),completeness=FALSE,reason="external_doppelkopf_deal"))
 if(d$phase=="play") lapply(.ra_doppelkopf_playable(d),function(c) list(type="play",params=list(card=c))) else list()
}
.ra_doppelkopf_outcome<-function(state,pack) {
 d<-state$data;done<-d$phase=="finished";points<-c(0,2,3,4,10,11);score<-sum(vapply(d$re_players,function(i) sum(points[as.integer(unlist(d$won[[i+1L]]))%%6+1L]),numeric(1)));re_wins<-score>=121;ws<-if(done) Filter(function(i) (i %in% unlist(d$re_players))==re_wins,0:3) else integer()
 list(status=if(done) "terminal" else "ongoing",result_kind=if(done) "win" else "none",winners=lapply(ws,function(i) state$players[[i+1L]]$id),losers=if(done) lapply(setdiff(0:3,ws),function(i) state$players[[i+1L]]$id) else list(),placements=list(),reason=if(done) {if(re_wins) "re_at_least_one_hundred_twenty_one" else "contra_at_least_one_hundred_twenty"} else "ongoing",evidence_required=list(),rule_id=if(done) paste0(pack$ruleset_id,"@",pack$ruleset_version) else NULL)
}
