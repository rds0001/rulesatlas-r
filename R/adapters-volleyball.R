.ra_vb_team <- list(type="integer",minimum=0,maximum=1)
.ra_vb_setwinner <- function(s,index) {s<-unlist(s);if(max(s)>=(if(index==4) 15 else 25) && abs(s[1]-s[2])>=2) if(s[1]>s[2]) 0L else 1L else NULL}
.ra_vb_wins <- function(d) vapply(0:1,function(p) sum(vapply(seq_along(d$sets),function(i) identical(.ra_vb_setwinner(d$sets[[i]],i-1L),p),logical(1))),numeric(1))
.ra_vb_ready <- function(d) {if(all(unlist(d$configured)) && !is.null(d$serving_team)) d$phase<-"ready";d}
.ra_vb_newset <- function(d) {
 d$phase<-"setup";d$points<-list(0L,0L);d$configured<-list(FALSE,FALSE);d$lineups<-list(as.list(0:5),as.list(0:5));d$timeouts<-list(0L,0L);d$expelled<-as.list(rep(FALSE,12));d["set_first_server"]<-list(if(length(d$sets)==4) NULL else 1L-d$set_first_server);d["serving_team"]<-list(d$set_first_server);d
}
.ra_vb_finishset <- function(d,w) {
 d$sets<-c(d$sets,list(d$points))
 if(.ra_vb_wins(d)[w+1L]==3) {d$phase<-"finished";d$winner<-w;d$reason<-"three_sets";return(d)}
 .ra_vb_newset(d)
}
.ra_vb_point <- function(d,w) {
 if(d$serving_team!=w) {line<-d$lineups[[w+1L]];d$lineups[[w+1L]]<-c(line[-1],line[1]);d$serving_team<-w}
 d$points[[w+1L]]<-d$points[[w+1L]]+1L;d$phase<-"ready";winner<-.ra_vb_setwinner(d$points,length(d$sets));if(!is.null(winner)) return(.ra_vb_finishset(d,winner));d
}
.ra_vb_incomplete <- function(d,team,permanent) {
 w<-1L-team
 repeat {
  target<-if(length(d$sets)==4) 15 else 25;d$points[[w+1L]]<-max(target,d$points[[team+1L]]+2);d$sets<-c(d$sets,list(d$points))
  if(.ra_vb_wins(d)[w+1L]==3) {d$phase<-"finished";d$winner<-w;d$reason<-"incomplete_team";return(d)}
  d<-.ra_vb_newset(d);if(!permanent) return(d)
 }
}
.ra_vb_validate <- function(d) {
 bad<-function(m) .ra_fail(m,code="INVALID_STATE")
 if(any(vapply(d$lineups,function(line) !identical(sort(as.integer(unlist(line))),0:5),logical(1)))) bad("Each six-player lineup is a permutation of the team roster")
 if(any(vapply(seq_along(d$sets),function(i) is.null(.ra_vb_setwinner(d$sets[[i]],i-1L)),logical(1)))) bad("Every completed set needs its target and a two-point margin")
 counts<-.ra_vb_wins(d)
 for(i in seq_along(d$sets)) for(p in 0:1) if(sum(vapply(seq_len(i-1L),function(j) identical(.ra_vb_setwinner(d$sets[[j]],j-1L),p),logical(1)))>=3) bad("No additional set may follow a match-winning third set")
 if(any(counts>3)) bad("No additional set may follow a match-winning third set")
 if(d$phase=="finished") {
  if(is.null(d$winner) || counts[d$winner+1L]!=3 || is.null(d$reason) || !identical(unlist(d$points),unlist(tail(d$sets,1)[[1]]))) bad("Terminal result requires three won sets and the final score")
 } else {
  if(max(counts)>=3 || !is.null(d$winner) || !is.null(d$reason) || any(unlist(d$sanctions)==3) || any(unlist(d$expelled))) bad("A six-player team cannot continue a set with an unavailable member")
  if(!is.null(.ra_vb_setwinner(d$points,length(d$sets)))) bad("A completed set must transition immediately")
 }
 if(d$phase %in% c("ready","rally") && (!all(unlist(d$configured)) || is.null(d$serving_team))) bad("A rally requires both lineups and a serving team")
 if(d$phase=="setup" && (any(unlist(d$points)!=0) || any(unlist(d$timeouts)!=0) || (all(unlist(d$configured)) && !is.null(d$serving_team)))) bad("Set setup precedes scoring and completes once both lineups and service are known")
 if(!is.null(d$serving_team) && is.null(d$set_first_server)) bad("Serving team requires a set starting-server decision")
 if(any(unlist(d$expelled) & unlist(d$sanctions)<2)) bad("Set expulsion requires a corresponding misconduct sanction")
 invisible(TRUE)
}
.ra_vb_apply <- function(state,action) {
 d<-state$data;p<-action$params;k<-action$type;team<-.ra_vb_team
 if(k=="set_lineup") {
  .ra_fields(p,list(team=team,players=list(type="array",minItems=6,maxItems=6,items=list(type="integer",minimum=0,maximum=5))),"/params");t<-p$team
  if(d$phase!="setup" || d$configured[[t+1L]] || !identical(sort(as.integer(unlist(p$players))),0:5)) .ra_fail("Submit each team lineup once before the set")
  d$lineups[[t+1L]]<-p$players;d$configured[[t+1L]]<-TRUE;return(.ra_vb_ready(d))
 }
 if(k=="choose_server") {
  .ra_fields(p,list(team=team),"/params");if(d$phase!="setup" || !is.null(d$set_first_server)) .ra_fail("Choose service from the external toss only before the first or fifth set")
  d$set_first_server<-p$team;d$serving_team<-p$team;return(.ra_vb_ready(d))
 }
 if(k=="start_rally") {
  .ra_fields(p,list(server=list(type="integer",minimum=0,maximum=11)),"/params")
  if(d$phase!="ready" || p$server!=6*d$serving_team+d$lineups[[d$serving_team+1L]][[1]]) .ra_fail("Only the correct player in service position one may start the rally")
  d$phase<-"rally";return(d)
 }
 if(k=="award_rally") {.ra_fields(p,list(winner=team),"/params");if(d$phase!="rally") .ra_fail("An adjudicated rally result requires a started rally");return(.ra_vb_point(d,p$winner))}
 if(k=="replay_rally") {.ra_fields(p,list(),"/params");if(d$phase!="rally") .ra_fail("Only an unresolved rally may be replayed");d$phase<-"ready";return(d)}
 if(k=="timeout") {
  .ra_fields(p,list(team=team),"/params");t<-p$team;if(d$phase!="ready" || d$timeouts[[t+1L]]>=2) .ra_fail("Each team may take two timeouts per set between rallies")
  d$timeouts[[t+1L]]<-d$timeouts[[t+1L]]+1L;return(d)
 }
 if(k %in% c("delay","improper_request","conduct_warning","misconduct")) {
  if(!(d$phase %in% c("ready","rally"))) .ra_fail("Resolve official sanctions in an active configured set")
  if(k=="misconduct") {
   .ra_fields(p,list(player=list(type="integer",minimum=0,maximum=11),kind=list(type="string",enum=as.list(c("rude","offensive","aggression")))),"/params");who<-p$player;t<-who%/%6
   level<-max(d$sanctions[[who+1L]]+1L,c(rude=1L,offensive=2L,aggression=3L)[p$kind]);d$sanctions[[who+1L]]<-min(3L,level)
   if(level>=2) {d$expelled[[who+1L]]<-TRUE;return(.ra_vb_incomplete(d,t,level>=3))}
   return(.ra_vb_point(d,1L-t))
  }
  .ra_fields(p,list(team=team),"/params");t<-p$team
  if(k=="conduct_warning") {if(d$conduct_warned[[t+1L]]) .ra_fail("The team has already received its formal conduct warning");d$conduct_warned[[t+1L]]<-TRUE;return(d)}
  if(k=="improper_request" && !d$improper_requested[[t+1L]]) {d$improper_requested[[t+1L]]<-TRUE;return(d)}
  d$delays[[t+1L]]<-d$delays[[t+1L]]+1L;if(d$delays[[t+1L]]>=2) return(.ra_vb_point(d,1L-t));return(d)
 }
 if(k=="concede") {.ra_fields(p,list(team=team),"/params");if(!(d$phase %in% c("ready","rally"))) .ra_fail("Concede after the set has been configured");return(.ra_vb_incomplete(d,p$team,TRUE))}
 .ra_fail("Unknown action")
}
.ra_vb_legal <- function(state) {
 d<-state$data;schemas<-list();team<-.ra_vb_team
 if(d$phase=="setup") {
  if(!all(unlist(d$configured))) schemas$set_lineup<-list(team=team,players=list(type="array",minItems=6,maxItems=6,items=list(type="integer",minimum=0,maximum=5)))
  if(is.null(d$set_first_server)) schemas$choose_server<-list(team=team)
 } else {
  if(d$phase=="ready") {schemas$start_rally<-list(server=list(type="integer",minimum=0,maximum=11));schemas$timeout<-list(team=team)} else if(d$phase=="rally") {schemas$award_rally<-list(winner=team);schemas$replay_rally<-setNames(list(),character())}
  for(k in c("delay","improper_request","conduct_warning","concede")) schemas[[k]]<-list(team=team)
  schemas$misconduct<-list(player=list(type="integer",minimum=0,maximum=11),kind=list(type="string",enum=as.list(c("rude","offensive","aggression"))))
 }
 list(mode="event_types",actions=list(),action_types=lapply(names(schemas),function(k) list(type=k,params_schema=schemas[[k]],preconditions=list("Set phase, lineup, service order and remaining allowances must permit the event."),external_observations=list("Official rally, toss, timeout or sanction decision."))),constraints=list(set_number=if(d$phase!="finished") length(d$sets)+1L else length(d$sets),target=if(length(d$sets)==4) 15L else 25L,serving_team=d$serving_team,server=if(is.null(d$serving_team)) NULL else 6*d$serving_team+d$lineups[[d$serving_team+1L]][[1]],timeouts_remaining=as.list(2-unlist(d$timeouts)),change_ends=length(d$sets)==4 && max(unlist(d$points))>=8),completeness=FALSE,reason="official_rallies_and_fixed_six_player_rosters")
}
.ra_vb_outcome <- function(state,pack) {
 d<-state$data;done<-d$phase=="finished";w<-d$winner;counts<-.ra_vb_wins(d)
 list(status=if(done) "terminal" else "ongoing",result_kind=if(done) "win" else "none",winners=if(done) list(state$players[[w+1L]]$id) else list(),losers=if(done) list(state$players[[2L-w]]$id) else list(),placements=if(done) lapply(0:1,function(p) list(player_id=state$players[[p+1L]]$id,place=if(p==w) 1L else 2L,sets=counts[p+1L])) else list(),reason=if(done) d$reason else "ongoing",evidence_required=list(),rule_id=if(done) paste0(pack$ruleset_id,"@",pack$ruleset_version) else NULL)
}
