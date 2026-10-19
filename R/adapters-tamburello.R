.ra_tamburello_team<-list(type="integer",minimum=0,maximum=1)
.ra_tamburello_player<-list(type="integer",minimum=0,maximum=4)
.ra_tamburello_active<-function(d,t) as.list(which(unlist(d$players[[t+1L]])=="active")-1L)
.ra_tamburello_serving<-function(d) {
 if(is.null(d$game_first_server)) return(NULL)
 if(all(unlist(d$games)==12)) (d$game_first_server+sum(unlist(d$points)))%%2 else d$game_first_server
}
.ra_tamburello_point<-function(d,t) {
 d$points[[t+1L]]<-d$points[[t+1L]]+1L;d$service_ms<-0L;d$phase<-"ready"
 if(d$points[[t+1L]]==4) {
  d$games[[t+1L]]<-d$games[[t+1L]]+1L
  if(d$games[[t+1L]]==13) {d$phase<-"finished";d$winner<-t;d$reason<-"thirteen_games"} else d$phase<-"game_break"
 }
 d
}
.ra_tamburello_validate<-function(d) {
 bad<-function(m) .ra_fail(m,code="INVALID_STATE")
 for(t in 0:1) {
  if(length(.ra_tamburello_active(d,t))>3) bad("At most three players may be active")
  if(!is.null(d$servers[[t+1L]]) && d$players[[t+1L]][[d$servers[[t+1L]]+1L]]!="active") bad("Nominated server must remain active")
 }
 if(d$phase=="toss" && (!sum(unlist(d$games)) %in% c(0,24) || any(unlist(d$points)!=0) || !is.null(d$game_first_server))) bad("Service toss belongs before the first or deciding game")
 if(d$phase %in% c("ready","service","rally","game_break") && (is.null(d$game_first_server) || is.null(d$first_server))) bad("Active game requires its starting-service decision")
 if(d$phase %in% c("service","rally")) {
  t<-.ra_tamburello_serving(d)
  if(is.null(d$servers[[t+1L]]) || any(vapply(0:1,function(j) length(.ra_tamburello_active(d,j))<2,logical(1)))) bad("Authorized play requires two active players per team and a nominated server")
 }
 if((d$phase=="service")!=(d$service_ms>0)) bad("Service clock only runs after authorization and before the hit")
 if(d$phase=="game_break" && (max(unlist(d$points))!=4 || d$points[[1]]==d$points[[2]] || max(unlist(d$games))>=13)) bad("Game break follows a completed nonfinal game")
 if(d$phase %in% c("ready","service","rally") && max(unlist(d$points))>=4) bad("Four points end the game")
 if(d$phase=="finished") {
  if(is.null(d$winner) || is.null(d$reason)) bad("Finished match needs a winner")
  if(d$reason=="thirteen_games" && (d$games[[d$winner+1L]]!=13 || d$games[[2L-d$winner]]>=13 || d$points[[d$winner+1L]]!=4)) bad("Normal result requires thirteen games")
 } else if(!is.null(d$winner) || !is.null(d$reason) || max(unlist(d$games))>=13) bad("Only a finished match declares a result")
}
.ra_tamburello_schemas<-function(d) {
 phase<-d$phase;s<-list();empty<-setNames(list(),character());team<-.ra_tamburello_team;player<-.ra_tamburello_player
 if(phase=="finished") return(s)
 if(phase=="toss") s$choose_service<-list(team=team)
 if(phase=="game_break") s$next_game<-empty
 if(phase=="ready") s<-c(s,list(nominate_server=list(team=team,player=player),authorize_service=empty))
 if(phase=="service") s<-c(s,list(serve=list(player=player),service_fault=empty,elapse_service=list(milliseconds=list(type="integer",minimum=1,maximum=10000))))
 if(phase=="rally") s$award_rally<-list(winner=team)
 if(phase %in% c("service","rally")) s$replay_point<-empty
 if(phase %in% c("toss","ready","service","game_break")) s<-c(s,list(substitute=list(team=team,outgoing=player,incoming=player),enter=list(team=team,player=player),withdraw_player=list(team=team,player=player),exclude_player=list(team=team,player=player),timeout=list(team=team)))
 if(phase %in% c("ready","service","rally")) s$penalty_point<-list(team=team)
 c(s,list(concede=list(team=team),declare_incomplete=list(team=team)))
}
.ra_tamburello_apply<-function(state,action) {
 d<-state$data;k<-action$type;p<-action$params;s<-.ra_tamburello_schemas(d)
 if(!k %in% names(s)) .ra_fail("Action is not permitted in this phase")
 .ra_fields(p,s[[k]],"/params");t<-p$team
 if(k=="choose_service") {
  d$game_first_server<-t;d$phase<-"ready"
  if(sum(unlist(d$games))==0) d$first_server<-t
 } else if(k=="next_game") {
  deciding<-all(unlist(d$games)==12);d$points<-list(0L,0L);d$servers<-list(NULL,NULL);d$service_ms<-0L;d$phase<-if(deciding) "toss" else "ready";d["game_first_server"]<-list(if(deciding) NULL else (d$first_server+sum(unlist(d$games)))%%2)
 } else if(k=="nominate_server") {
  if(t!=.ra_tamburello_serving(d) || d$players[[t+1L]][[p$player+1L]]!="active" || !is.null(d$servers[[t+1L]])) .ra_fail("Nominate an active server once for the entitled team in this game")
  d$servers[t+1L]<-list(p$player)
 } else if(k=="authorize_service") {
  if(is.null(d$servers[[.ra_tamburello_serving(d)+1L]]) || any(vapply(0:1,function(j) length(.ra_tamburello_active(d,j))<2,logical(1)))) .ra_fail("Both teams need two active players and the serving team needs its server")
  d$phase<-"service";d$service_ms<-10000L
 } else if(k=="serve") {
  if(p$player!=d$servers[[.ra_tamburello_serving(d)+1L]]) .ra_fail("Only the nominated game server may serve")
  d$phase<-"rally";d$service_ms<-0L
 } else if(k=="service_fault") d<-.ra_tamburello_point(d,1L-.ra_tamburello_serving(d)) else if(k=="elapse_service") {
  if(p$milliseconds>d$service_ms) .ra_fail("Service clock cannot pass its deadline")
  d$service_ms<-d$service_ms-p$milliseconds
  if(d$service_ms==0) d<-.ra_tamburello_point(d,1L-.ra_tamburello_serving(d))
 } else if(k=="award_rally") d<-.ra_tamburello_point(d,p$winner) else if(k=="replay_point") {d$phase<-"ready";d$service_ms<-0L} else if(k %in% c("substitute","enter")) {
  if(k=="substitute") {
   old<-p$outgoing
   if(d$players[[t+1L]][[old+1L]]!="active" || (!is.null(d$servers[[t+1L]]) && d$servers[[t+1L]]==old && d$phase!="game_break")) .ra_fail("The game server may not be ordinarily replaced before the game ends")
   d$players[[t+1L]][[old+1L]]<-"bench"
   if(!is.null(d$servers[[t+1L]]) && d$servers[[t+1L]]==old) d$servers[t+1L]<-list(NULL)
  }
  new<-if(k=="substitute") p$incoming else p$player
  if(d$players[[t+1L]][[new+1L]]!="bench" || length(.ra_tamburello_active(d,t))>=3 || (k=="substitute" && new==old)) .ra_fail("Replacement requires an eligible reserve and an available place")
  d$players[[t+1L]][[new+1L]]<-"active"
 } else if(k %in% c("withdraw_player","exclude_player")) {
  i<-p$player
  if(d$players[[t+1L]][[i+1L]] %in% c("withdrawn","dq")) .ra_fail("Player is already unavailable")
  d$players[[t+1L]][[i+1L]]<-if(k=="withdraw_player") "withdrawn" else "dq"
  if(!is.null(d$servers[[t+1L]]) && d$servers[[t+1L]]==i) d$servers[t+1L]<-list(NULL)
  if(d$phase=="service" && (is.null(d$servers[[.ra_tamburello_serving(d)+1L]]) || length(.ra_tamburello_active(d,t))<2)) {d$phase<-"ready";d$service_ms<-0L}
 } else if(k=="timeout") {
  if(d$timeouts[[t+1L]]>=2) .ra_fail("Two team timeouts are available for the match")
  d$timeouts[[t+1L]]<-d$timeouts[[t+1L]]+1L
  if(d$phase=="service") {d$phase<-"ready";d$service_ms<-0L}
 } else if(k=="penalty_point") d<-.ra_tamburello_point(d,1L-t) else if(k %in% c("concede","declare_incomplete")) {d$phase<-"finished";d$winner<-1L-t;d$reason<-if(k=="concede") "concession" else "incomplete_team";d$service_ms<-0L}
 d
}
.ra_tamburello_legal<-function(state) {
 d<-state$data;s<-.ra_tamburello_schemas(d);t<-.ra_tamburello_serving(d)
 list(mode="event_types",actions=list(),action_types=lapply(names(s),function(k) list(type=k,params_schema=s[[k]],preconditions=list("Game, service entitlement, roster and interruption rights must permit the event."),external_observations=list("Official rally, fault, exclusion and stoppage decisions."))),constraints=list(phase=d$phase,serving_team=t,server=if(is.null(t)) NULL else d$servers[[t+1L]],point_labels=list("0","15","30","45","game"),service_ms=d$service_ms,change_ends=d$phase=="game_break" && sum(unlist(d$games))%%3==0),completeness=FALSE,reason="official_indoor_tamburello_events")
}
.ra_tamburello_outcome<-function(state,pack) {
 d<-state$data;done<-d$phase=="finished";w<-d$winner;league<-NULL
 if(done && d$reason=="thirteen_games") {league<-c(0L,0L);league[w+1L]<-if(d$games[[2L-w]]<=10) 3L else 2L;league[2L-w]<-if(d$games[[2L-w]]<=10) 0L else 1L}
 list(status=if(done) "terminal" else "ongoing",result_kind=if(done) "win" else "none",winners=if(done) list(state$players[[w+1L]]$id) else list(),losers=if(done) list(state$players[[2L-w]]$id) else list(),placements=if(done) lapply(0:1,function(t) list(player_id=state$players[[t+1L]]$id,place=if(t==w) 1L else 2L,games=d$games[[t+1L]],championship_points=if(is.null(league)) NULL else league[t+1L])) else list(),reason=if(done) d$reason else "ongoing",evidence_required=list(),rule_id=if(done) paste0(pack$ruleset_id,"@",pack$ruleset_version) else NULL)
}
