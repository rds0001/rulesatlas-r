.ra_ja_card<-list(type="integer",minimum=0,maximum=35)
.ra_ja_strength<-function(c,trump) (if(c %/% 9==trump) c(0,1,2,7,5,8,3,4,6) else c(0,1,2,3,7,4,5,6,8))[c %% 9+1L]
.ra_ja_winner<-function(trick,trump) {
 lead<-trick[[1]]$card %/% 9;groups<-vapply(trick,function(x) if(x$card %/% 9==trump) 2L else if(x$card %/% 9==lead) 1L else 0L,integer(1));strength<-vapply(trick,function(x) .ra_ja_strength(x$card,trump),numeric(1));trick[[order(-groups,-strength)[1]]]$player
}
.ra_ja_playable<-function(d) {
 hand<-d$hands[[d$turn+1L]];trick<-d$trick;trump<-d$trump;if(!length(trick)) return(hand)
 lead<-trick[[1]]$card %/% 9;trumps<-Filter(function(c) c %/% 9==trump,hand)
 if(lead==trump) {if(length(trumps)==1L && trumps[[1]]==trump*9L+5L) return(hand);return(if(length(trumps)) trumps else hand)}
 follow<-Filter(function(c) c %/% 9==lead,hand);played_trumps<-lapply(Filter(function(x) x$card %/% 9==trump,trick),function(x) x$card)
 if(length(trumps)==length(hand)) return(hand)
 allowed<-Filter(function(c) !length(played_trumps) || .ra_ja_strength(c,trump)>max(vapply(played_trumps,function(x) .ra_ja_strength(x,trump),numeric(1))),trumps)
 Filter(function(c) c %in% unlist(allowed) || (if(length(follow)) c %in% unlist(follow) else c %/% 9!=trump),hand)
}
.ra_ja_validate<-function(d) {
 cards<-c(unlist(d$hands),unlist(d$won),vapply(d$trick,function(x) x$card,numeric(1)))
 if(d$phase=="setup") {if(length(cards) || any(unlist(d$scores)!=0) || !is.null(d$trump)) .ra_fail("Initial Jass match must be empty",code="INVALID_STATE");return(invisible(TRUE))}
 if(!identical(as.integer(sort(cards)),0:35)) .ra_fail("Every Jass card must occur exactly once",code="INVALID_STATE")
 choosing<-d$phase %in% c("choose_trump","choose_partner")
 if(choosing) {
  if(!is.null(d$trump) || any(lengths(d$hands)!=9L) || any(lengths(d$won)>0) || length(d$trick)>0 || d$turn!=(d$dealer+(if(d$phase=="choose_trump") 1L else 3L)) %% 4L) .ra_fail("Invalid trump-choice state",code="INVALID_STATE")
 } else {
  if(is.null(d$trump) || any(lengths(d$won) %% 4L!=0)) .ra_fail("Contract or won tricks are invalid",code="INVALID_STATE")
  completed<-sum(lengths(d$won)) %/% 4L;actors<-vapply(d$trick,function(x) x$player,numeric(1))
  if(length(actors)>3L || anyDuplicated(actors) || (length(actors)>1L && any(actors[-1]!=(head(actors,-1)+1L) %% 4L)) || (length(actors)>0 && d$turn!=(tail(actors,1)+1L) %% 4L)) .ra_fail("Partial trick order is invalid",code="INVALID_STATE")
  if(any(vapply(0:3,function(p) length(d$hands[[p+1L]])!=9L-completed-as.integer(p %in% actors),logical(1)))) .ra_fail("Hands do not match completed tricks",code="INVALID_STATE")
  if((d$phase %in% c("round_end","finished"))!=(completed==9L)) .ra_fail("Round phase and trick count disagree",code="INVALID_STATE")
 }
 if(is.null(d$stoeck_player)!=is.null(d$stoeck_card)) .ra_fail("Pending Stoeck announcement is incomplete",code="INVALID_STATE")
 if(!is.null(d$stoeck_card) && (choosing || d$stoeck_card %/% 9!=d$trump || !d$stoeck_card %% 9 %in% c(6,7) || !d$stoeck_card %in% unlist(d$hands[[d$stoeck_player+1L]]) || !is.null(d$stoeck_team))) .ra_fail("Second Stoeck card must remain in the announcing hand",code="INVALID_STATE")
 if((d$phase=="finished")!=(max(unlist(d$scores))>=1000 && d$scores[[1]]!=d$scores[[2]])) .ra_fail("Match threshold disagrees with phase",code="INVALID_STATE")
 invisible(TRUE)
}
.ra_ja_apply<-function(state,action) {
 d<-state$data;p<-action$params;k<-action$type;t<-d$turn;phase<-d$phase
 if(k=="deal") {
  .ra_fields(p,list(deck=list(type="array",minItems=36,maxItems=36,items=.ra_ja_card)),"/params")
  if(!phase %in% c("setup","round_end") || !identical(as.integer(sort(unlist(p$deck))),0:35)) .ra_fail("Supply a complete external deck between deals")
  if(phase!="setup") d$dealer<-(d$dealer+1L) %% 4L
  d$hands<-rep(list(list()),4);d$won<-rep(list(list()),2);d["trump"]<-list(NULL);d$trick<-list();d$turn<-(d$dealer+1L) %% 4L;d$phase<-"choose_trump";d["stoeck_player"]<-list(NULL);d["stoeck_card"]<-list(NULL);d["stoeck_team"]<-list(NULL)
  for(i in 0:35) {j<-(d$dealer+1L+i) %% 4L+1L;d$hands[[j]]<-c(d$hands[[j]],p$deck[i+1L])}
  return(d)
 }
 if(!is.null(action$actor) && action$actor!=state$players[[t+1L]]$id) .ra_fail("Wrong actor")
 if(phase %in% c("choose_trump","choose_partner")) {
  if(k=="push") {
   .ra_fields(p,list(),"/params");if(phase!="choose_trump") .ra_fail("Partner cannot push the choice back")
   d$turn<-(t+2L) %% 4L;d$phase<-"choose_partner"
  } else if(k=="choose_trump") {
   .ra_fields(p,list(trump=list(type="integer",minimum=0,maximum=3)),"/params");d$trump<-p$trump;d$turn<-(d$dealer+1L) %% 4L;d$phase<-"play"
  } else .ra_fail("Choose trump or push the initial choice to the partner")
  return(d)
 }
 if(phase!="play" || k!="play") .ra_fail("Play a legal Jass card during a trick")
 .ra_fields(p,list(card=.ra_ja_card,announce=list(type="boolean")),"/params");card<-p$card
 if(!card %in% unlist(.ra_ja_playable(d))) .ra_fail("Follow suit or legally trump; undertrumping and the sole trump Jack exception are checked")
 pending<-!is.null(d$stoeck_player) && d$stoeck_player==t && d$stoeck_card==card
 if(p$announce) {
  if(pending) d$stoeck_team<-t %% 2 else {
   other<-if(card %% 9==6) card+1L else card-1L
   if(!is.null(d$stoeck_team) || card %/% 9!=d$trump || !card %% 9 %in% c(6,7) || !other %in% unlist(d$hands[[t+1L]])) .ra_fail("Stoeck requires the trump Queen and King held together")
   d$stoeck_player<-t;d$stoeck_card<-other
  }
 }
 if(pending) {d["stoeck_player"]<-list(NULL);d["stoeck_card"]<-list(NULL)}
 d$hands[[t+1L]]<-Filter(function(c) c!=card,d$hands[[t+1L]]);d$trick<-c(d$trick,list(list(player=t,card=card)));d$turn<-(t+1L) %% 4L
 if(length(d$trick)==4L) {
  w<-.ra_ja_winner(d$trick,d$trump);d$won[[w %% 2+1L]]<-c(d$won[[w %% 2+1L]],lapply(d$trick,function(x) x$card));d$trick<-list();d$turn<-w
  if(sum(lengths(d$won))==36L) {
   for(team in 0:1) {
    points<-sum(vapply(d$won[[team+1L]],function(c) (if(c %/% 9==d$trump) c(0,0,0,14,10,20,3,4,11) else c(0,0,0,0,10,2,3,4,11))[c %% 9+1L],numeric(1)))
    d$scores[[team+1L]]<-d$scores[[team+1L]]+points+(if(w %% 2==team) 5L else 0L)+(if(length(d$won[[team+1L]])==36L) 100L else 0L)+(if(!is.null(d$stoeck_team) && d$stoeck_team==team) 20L else 0L)
   }
   d$phase<-if(max(unlist(d$scores))>=1000 && d$scores[[1]]!=d$scores[[2]]) "finished" else "round_end"
  }
 }
 d
}
.ra_ja_legal<-function(state) {
 d<-state$data;phase<-d$phase;empty<-setNames(list(),character())
 if(phase %in% c("setup","round_end")) return(list(mode="event_types",actions=list(),action_types=list(list(type="deal",params_schema=list(deck=list(type="array",minItems=36,maxItems=36,items=.ra_ja_card)),preconditions=list("A full externally supplied deck permutation is required."),external_observations=list("Externally shuffled deck."))),constraints=empty,completeness=FALSE,reason="external_deal"))
 if(phase %in% c("choose_trump","choose_partner")) return(c(if(phase=="choose_trump") list(list(type="push",params=empty)) else list(),lapply(0:3,function(s) list(type="choose_trump",params=list(trump=s)))))
 result<-list()
 for(card in .ra_ja_playable(d)) {
  result<-c(result,list(list(type="play",params=list(card=card,announce=FALSE))));other<-if(card %% 9==6) card+1L else card-1L
  pending<-!is.null(d$stoeck_player) && d$stoeck_player==d$turn && d$stoeck_card==card
  if(pending || (card %/% 9==d$trump && card %% 9 %in% c(6,7) && other %in% unlist(d$hands[[d$turn+1L]]) && is.null(d$stoeck_team))) result<-c(result,list(list(type="play",params=list(card=card,announce=TRUE))))
 }
 result
}
.ra_ja_outcome<-function(state,pack) {
 d<-state$data;done<-d$phase=="finished";team<-which.max(unlist(d$scores))-1L
 list(status=if(done) "terminal" else "ongoing",result_kind=if(done) "win" else "none",winners=if(done) lapply(Filter(function(i) i %% 2==team,0:3),function(i) state$players[[i+1L]]$id) else list(),losers=if(done) lapply(Filter(function(i) i %% 2!=team,0:3),function(i) state$players[[i+1L]]$id) else list(),placements=list(),reason=if(done) "team_reached_one_thousand" else "ongoing",evidence_required=list(),rule_id=if(done) paste0(pack$ruleset_id,"@",pack$ruleset_version) else NULL)
}
