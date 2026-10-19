.ra_pelota_player<-list(type="integer",minimum=0,maximum=1)
.ra_pelota_target<-function(d) if(length(d$games)==2) 5L else 10L
.ra_pelota_gamewinner<-function(s) as.integer(s[[2]]>s[[1]])
.ra_pelota_wins<-function(d) vapply(0:1,function(t) sum(vapply(d$games,function(s) .ra_pelota_gamewinner(s)==t,logical(1))),numeric(1))
.ra_pelota_point<-function(d,t) {
 d$points[[t+1L]]<-d$points[[t+1L]]+1L;d$server<-t;d$long_fault<-FALSE;d$phase<-"ready"
 if(d$points[[t+1L]]==.ra_pelota_target(d)) {
  d$games<-c(d$games,list(d$points))
  if(.ra_pelota_wins(d)[t+1L]==2) {d$phase<-"finished";d$winner<-t;d$reason<-"two_games"} else d$phase<-"interval"
 }
 d
}
.ra_pelota_validate<-function(d) {
 bad<-function(m) .ra_fail(m,code="INVALID_STATE")
 for(j in seq_along(d$games)) {
  s<-d$games[[j]]
  if(max(unlist(s))!=(if(j==3) 5 else 10) || s[[1]]==s[[2]]) bad("Completed game must end at its exact target")
  if(any(vapply(0:1,function(t) sum(vapply(head(d$games,j-1L),function(x) .ra_pelota_gamewinner(x)==t,logical(1)))>=2,logical(1)))) bad("No game follows a match-winning second game")
 }
 if(d$phase=="choose_server") {
  if(!is.null(d$server) || !length(d$games) %in% c(0,2) || any(unlist(d$points)!=0) || any(unlist(d$timeouts)) || d$long_fault) bad("Toss belongs before the first or deciding game")
 } else if(d$phase %in% c("ready","serve","rally","interval") && (is.null(d$server) || is.null(d$first_server))) bad("Active play requires a chosen server")
 if(d$phase %in% c("ready","serve","rally") && max(unlist(d$points))>=.ra_pelota_target(d)) bad("Completed game must enter its interval or result")
 if(d$phase=="interval" && (length(d$games)==0 || !identical(d$points,tail(d$games,1)[[1L]]) || max(.ra_pelota_wins(d))>=2)) bad("Interval follows a completed nonfinal game")
 if(d$phase %in% c("choose_server","interval","rally","finished","abandoned") && d$long_fault) bad("Long-service fault is relevant only before a valid service")
 if(d$phase=="finished") {
  if(is.null(d$reason) || (d$reason!="no_contest" && is.null(d$winner)) || (d$reason=="no_contest" && !is.null(d$winner))) bad("Finished match needs a consistent result")
  if(d$reason=="two_games" && (.ra_pelota_wins(d)[d$winner+1L]!=2 || !identical(d$points,tail(d$games,1)[[1L]]))) bad("Normal victory requires two completed games")
 } else if(!is.null(d$winner) || !is.null(d$reason)) bad("Only terminal state can declare a result")
 if(!d$phase %in% c("finished","abandoned") && max(.ra_pelota_wins(d))>=2) bad("Two games end the match")
}
.ra_pelota_schemas<-function(d) {
 phase<-d$phase;s<-list();empty<-setNames(list(),character());player<-.ra_pelota_player
 if(phase=="finished") return(s)
 if(phase=="abandoned") return(list(official_result=list(winner=c(player,list(nullable=TRUE)))))
 if(phase=="choose_server") s$choose_server<-list(player=player)
 if(phase=="ready") s<-list(start_serve=list(player=player),timeout=list(player=player))
 if(phase=="serve") s$service_decision<-list(result=list(type="enum",values=list("good","long","fault","replay")))
 if(phase=="rally") s<-list(award_rally=list(winner=player),replay_rally=empty)
 if(phase=="interval") s$next_game<-empty
 if(phase %in% c("ready","serve","rally")) s$penalty_point<-list(player=player)
 c(s,list(concede=list(player=player),disqualify=list(player=player),abandon=empty))
}
.ra_pelota_apply<-function(state,action) {
 d<-state$data;k<-action$type;p<-action$params;s<-.ra_pelota_schemas(d)
 if(!k %in% names(s)) .ra_fail("Action is not permitted in this phase")
 .ra_fields(p,s[[k]],"/params")
 if(k=="choose_server") {
  d$server<-p$player;d$phase<-"ready"
  if(length(d$games)==0) d$first_server<-p$player
 } else if(k=="start_serve") {
  if(p$player!=d$server) .ra_fail("Only the entitled player may serve")
  d$phase<-"serve"
 } else if(k=="service_decision") {
  result<-p$result
  if(result=="good") {d$phase<-"rally";d$long_fault<-FALSE} else if(result=="fault" || (result=="long" && d$long_fault)) d<-.ra_pelota_point(d,1L-d$server) else {
   d$phase<-"ready"
   if(result=="long") d$long_fault<-TRUE
  }
 } else if(k=="award_rally") d<-.ra_pelota_point(d,p$winner) else if(k=="replay_rally") {d$phase<-"ready";d$long_fault<-FALSE} else if(k=="timeout") {
  if(d$timeouts[[p$player+1L]]) .ra_fail("One timeout per player per game is available")
  d$timeouts[[p$player+1L]]<-TRUE
 } else if(k=="next_game") {
  d$points<-list(0L,0L);d$timeouts<-list(FALSE,FALSE);d$long_fault<-FALSE;d$phase<-if(length(d$games)==2) "choose_server" else "ready";d["server"]<-list(if(length(d$games)==2) NULL else 1L-d$first_server)
 } else if(k=="penalty_point") d<-.ra_pelota_point(d,1L-p$player) else if(k %in% c("concede","disqualify")) {d$phase<-"finished";d$winner<-1L-p$player;d$reason<-if(k=="concede") "concession" else "disqualification";d$long_fault<-FALSE} else if(k=="abandon") {d$phase<-"abandoned";d$long_fault<-FALSE} else if(k=="official_result") {d$phase<-"finished";d["winner"]<-list(p$winner);d$reason<-if(is.null(p$winner)) "no_contest" else "official_result"}
 d
}
.ra_pelota_legal<-function(state) {
 d<-state$data;s<-.ra_pelota_schemas(d)
 list(mode="event_types",actions=list(),action_types=lapply(names(s),function(k) list(type=k,params_schema=s[[k]],preconditions=list("Game phase, service entitlement and remaining timeout must permit the event."),external_observations=list("Official service, rally, disciplinary and toss decisions."))),constraints=list(phase=d$phase,server=d$server,service_attempt=if(d$long_fault) 2L else 1L,target=.ra_pelota_target(d),timeouts_remaining=lapply(d$timeouts,function(x) if(x) 0L else 1L)),completeness=FALSE,reason="official_fronton_hand_pelota_events")
}
.ra_pelota_outcome<-function(state,pack) {
 d<-state$data;done<-d$phase=="finished";w<-d$winner
 list(status=if(done) "terminal" else if(d$phase=="abandoned") "awaiting_adjudication" else "ongoing",result_kind=if(done) {if(is.null(w)) "no_contest" else "win"} else "none",winners=if(!is.null(w)) list(state$players[[w+1L]]$id) else list(),losers=if(!is.null(w)) list(state$players[[2L-w]]$id) else list(),placements=list(),reason=if(done) d$reason else if(d$phase=="abandoned") "official_result_required" else "ongoing",evidence_required=if(d$phase=="abandoned") list("Official result decision.") else list(),rule_id=if(done) paste0(pack$ruleset_id,"@",pack$ruleset_version) else NULL)
}
