.ra_tarock_category<-function(c) if(c>=32) 4L else c%/%8L
.ra_tarock_strength<-function(c) if(c>=32) c-32L else c%%8L
.ra_tarock_protected<-function(c) c %in% c(32L,52L,53L) || c<32 && c%%8L==7L
.ra_tarock_value<-function(c) if(.ra_tarock_protected(c)) 5L else if(c>=32 || c%%8L<4) 1L else c%%8L-2L
.ra_tarock_points<-function(cards) sum(vapply(cards,.ra_tarock_value,numeric(1)))-2L*(length(cards)%/%3L)
.ra_tarock_solo_points<-function(d) if(is.null(d$declarer)) 0L else .ra_tarock_points(c(d$won[[d$declarer+1L]],d$discard))
.ra_tarock_playable<-function(d) {
 h<-d$hands[[d$turn+1L]]
 if(length(d$trick)==0) return(h)
 led<-.ra_tarock_category(d$trick[[1]]$card);same<-Filter(function(c) .ra_tarock_category(c)==led,h)
 if(length(same)>0) return(same)
 trumps<-Filter(function(c) c>=32,h)
 if(length(trumps)>0) trumps else h
}
.ra_tarock_winner<-function(trick) {
 led<-.ra_tarock_category(trick[[1]]$card)
 weight<-vapply(trick,function(e) (if(e$card>=32) 200L else if(.ra_tarock_category(e$card)==led) 100L else 0L)+.ra_tarock_strength(e$card),numeric(1))
 trick[[which.max(weight)]]$player
}
.ra_tarock_discardable<-function(d,cards) {
 hand<-unlist(d$hands[[d$declarer+1L]]);cc<-unlist(cards)
 if(length(unique(cc))!=3 || any(!cc %in% hand) || any(vapply(cards,.ra_tarock_protected,logical(1)))) return(FALSE)
 plain<-Filter(function(c) c<32 && !.ra_tarock_protected(c),hand)
 sum(cc<32)==min(3,length(plain))
}
.ra_tarock_validate<-function(d) {
 bad<-function(m) .ra_fail(m,code="INVALID_STATE")
 cards<-c(unlist(d$hands),unlist(d$won),unlist(d$talon),unlist(d$discard),vapply(d$trick,function(r) r$card,numeric(1)))
 if(d$phase=="setup") {if(length(cards)>0 || !is.null(d$declarer)) bad("Setup must precede dealing and the agreed contract");return(invisible(NULL))}
 if(length(cards)!=54 || !identical(as.integer(sort(cards)),0:53) || is.null(d$declarer)) bad("The state requires one complete unique deck and a declarer")
 if(any(vapply(d$won,function(w) length(w)%%3L!=0,logical(1)))) bad("Won piles contain complete three-card tricks")
 if(any(vapply(d$discard,.ra_tarock_protected,logical(1)))) bad("Kings and the three highest-value trumps cannot be discarded")
 if(d$phase %in% c("choose","discard")) {
  if(any(lengths(d$won)>0) || length(d$trick)>0 || length(d$discard)>0 || d$turn!=d$declarer) bad("Talon selection and discarding precede all tricks")
  expected<-rep(16L,3)
  if(d$phase=="discard") expected[d$declarer+1L]<-19L
  if(any(lengths(d$hands)!=expected) || length(d$talon)!=(if(d$phase=="choose") 6L else 3L)) bad("Talon phase hand sizes are inconsistent")
  return(invisible(NULL))
 }
 if(length(d$discard)!=3 || length(d$talon)!=3) bad("Play requires three discarded and three unselected talon cards")
 completed<-sum(lengths(d$won))%/%3L;actors<-vapply(d$trick,function(r) r$player,numeric(1))
 if(anyDuplicated(actors)>0 || (length(actors)>1 && any(actors[-1]!=(head(actors,-1)+1L)%%3L)) || (length(actors)>0 && d$turn!=(tail(actors,1)+1L)%%3L)) bad("Trick order and next player disagree")
 if(any(vapply(0:2,function(i) length(d$hands[[i+1L]])!=16L-completed-as.integer(i %in% actors),logical(1)))) bad("Hand sizes disagree with completed and partial tricks")
 if((d$phase=="finished")!=(completed==16)) bad("The hand ends after sixteen complete tricks")
}
.ra_tarock_apply<-function(state,action) {
 d<-state$data;p<-action$params;k<-action$type;phase<-d$phase;card<-list(type="integer",minimum=0,maximum=53);player<-list(type="integer",minimum=0,maximum=2)
 if(k=="deal") {
  .ra_fields(p,list(deck=list(type="array",minItems=54,maxItems=54,items=card),declarer=player,leader=player),"/params")
  if(phase!="setup" || !identical(as.integer(sort(unlist(p$deck))),0:53)) .ra_fail("Deal one complete deck and an agreed contract before play")
  d$phase<-"choose";d$hands<-lapply(1:3,function(i) p$deck[seq(i,48L,by=3L)]);d$talon<-p$deck[49:54];d$declarer<-p$declarer;d$leader<-p$leader;d$turn<-p$declarer;return(d)
 }
 if(!is.null(action$actor) && action$actor!=state$players[[d$turn+1L]]$id) .ra_fail("Wrong actor")
 if(k=="choose_talon") {
  .ra_fields(p,list(half=list(type="integer",minimum=0,maximum=1)),"/params")
  if(phase!="choose") .ra_fail("Choose a talon half immediately after the deal")
  h<-p$half;d$hands[[d$declarer+1L]]<-c(d$hands[[d$declarer+1L]],d$talon[seq(3L*h+1L,3L*h+3L)]);d$talon<-d$talon[seq(3L*(1L-h)+1L,3L*(1L-h)+3L)];d$phase<-"discard"
 } else if(k=="discard") {
  .ra_fields(p,list(cards=list(type="array",minItems=3,maxItems=3,items=card)),"/params")
  if(phase!="discard" || !.ra_tarock_discardable(d,p$cards)) .ra_fail("Discard three owned unprotected cards, exhausting eligible plain cards before trumps")
  d$hands[[d$declarer+1L]]<-Filter(function(c) !c %in% unlist(p$cards),d$hands[[d$declarer+1L]])
  d$discard<-as.list(sort(unlist(p$cards)));d$phase<-"play";d$turn<-d$leader
 } else if(k=="play") {
  .ra_fields(p,list(card=card),"/params")
  if(phase!="play" || !p$card %in% unlist(.ra_tarock_playable(d))) .ra_fail("Follow suit when possible; otherwise play a trump if available")
  c<-p$card;t<-d$turn;d$hands[[t+1L]]<-Filter(function(x) x!=c,d$hands[[t+1L]]);d$trick[[length(d$trick)+1L]]<-list(player=t,card=c);d$turn<-(t+1L)%%3L
  if(length(d$trick)==3) {
   w<-.ra_tarock_winner(d$trick);d$won[[w+1L]]<-c(d$won[[w+1L]],lapply(d$trick,function(r) r$card));d$trick<-list();d$turn<-w
   if(all(lengths(d$hands)==0)) d$phase<-"finished"
  }
 } else .ra_fail("Unknown Tarock action")
 d
}
.ra_tarock_legal<-function(state) {
 d<-state$data;ph<-d$phase;card<-list(type="integer",minimum=0,maximum=53);player<-list(type="integer",minimum=0,maximum=2)
 if(ph %in% c("setup","discard")) {
  schema<-if(ph=="setup") list(deck=list(type="array",minItems=54,maxItems=54,items=card),declarer=player,leader=player) else list(cards=list(type="array",minItems=3,maxItems=3,items=card))
  return(list(mode="event_types",actions=list(),action_types=list(list(type=if(ph=="setup") "deal" else "discard",params_schema=schema,preconditions=list(if(ph=="setup") "External complete deck and agreed contract." else "Three owned cards; kings and trull protected; plain cards before trumps."),external_observations=if(ph=="setup") list("External shuffle and contract agreement.") else list())),constraints=list(phase=ph,declarer=d$declarer,exposed_discard=Filter(function(c) c>=32,d$discard)),completeness=FALSE,reason=if(ph=="setup") "external_deal" else "constrained_card_selection"))
 }
 if(ph=="choose") return(lapply(0:1,function(h) list(type="choose_talon",params=list(half=h))))
 if(ph=="play") return(lapply(.ra_tarock_playable(d),function(c) list(type="play",params=list(card=c))))
 list()
}
.ra_tarock_outcome<-function(state,pack) {
 d<-state$data;done<-d$phase=="finished";decl<-d$declarer;win<-done && .ra_tarock_solo_points(d)>=36
 winners<-if(done) {if(win) decl else setdiff(0:2,decl)} else integer()
 list(status=if(done) "terminal" else "ongoing",result_kind=if(done) "win" else "none",winners=lapply(winners,function(i) state$players[[i+1L]]$id),losers=if(done) lapply(setdiff(0:2,winners),function(i) state$players[[i+1L]]$id) else list(),placements=list(),reason=if(done) {if(win) "declarer_at_least_thirty_six" else "defenders_at_least_thirty_five"} else "ongoing",evidence_required=list(),rule_id=if(done) paste0(pack$ruleset_id,"@",pack$ruleset_version) else NULL)
}
