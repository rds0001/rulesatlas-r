.ra_basketball_schemas<-function() {
 i<-function(a,b) list(type="integer",minimum=a,maximum=b);team<-i(0,1);slot<-i(0,11);bool<-list(type="boolean");empty<-setNames(list(),character())
 s<-list(set_lineup=list(team=team,roster_size=i(5,12),active=list(type="array",minItems=5,maxItems=5,items=slot)),start_match=list(first_control=team),pause=empty,resume=empty,elapse=list(milliseconds=i(1,600000)),field_goal=list(team=team,player=slot,value=list(type="integer",enum=list(2L,3L)),at_buzzer=bool),foul=list(team=team,player=slot,kind=list(type="string",enum=as.list(c("personal","technical","unsporting","disqualifying"))),shooter=slot,shooting_points=list(type="integer",enum=list(0L,2L,3L)),basket_made=bool,team_control=bool,at_buzzer=bool),bench_technical=list(team=team,shooter=slot),free_throw=list(made=bool,violation=list(type="string",enum=as.list(c("none","shooter","offense","defense","both")))),replace_shooter=list(penalty_id=i(1,1000000),player=slot),cancel_equal_penalties=list(first_id=i(1,1000000),second_id=i(1,1000000)),substitute=list(team=team,out=slot),enter_player=list(team=team,player=slot),timeout=list(team=team),end_timeout=empty,held_ball=empty,resolve_arrow_throwin=list(result=list(type="string",enum=as.list(c("completed","thrower_violation","defender_violation")))),end_period=empty,start_next_period=empty,forfeit=list(loser=team),no_contest=empty);s$substitute[["in"]]<-slot;s
}
.ra_basketball_active<-function(t) as.list(which(unlist(t$status)=="active")-1L)
.ra_basketball_timeouts<-function(d,i) {
 t<-d$teams[[i+1L]]
 if(d$period<=2) return(2L-t$timeouts_first)
 if(d$period>=5) return(1L-t$timeouts_overtime)
 if(d$period==4 && d$remaining_ms<=120000) min(3L-t$timeouts_second,2L-t$timeouts_late_fourth) else 3L-t$timeouts_second
}
.ra_basketball_finish<-function(d,w,reason) {d$phase<-"finished";d["winner"]<-list(w);d$reason<-reason;d$running<-FALSE;d$free_throws<-list();d["timeout_team"]<-list(NULL);d$pending_arrow<-FALSE;d$buzzer_pending<-FALSE;d}
.ra_basketball_default<-function(d) {
 for(i in 0:1) if(length(.ra_basketball_active(d$teams[[i+1L]]))<2) {w<-1L-i;if(d$scores[[w+1L]]<=d$scores[[i+1L]]) d$scores<-lapply(0:1,function(j) if(j==w) 2L else 0L);return(.ra_basketball_finish(d,w,"default"))};d
}
.ra_basketball_queue<-function(d,team,shooter,count,kind,restart) {
 if(count>0) {r<-list(id=d$next_penalty_id,team=team,shooter=shooter,total=count,remaining=count,kind=kind,restart=restart);d$free_throws<-if(kind %in% c("technical","bench")) c(list(r),d$free_throws) else c(d$free_throws,list(r));d$next_penalty_id<-d$next_penalty_id+1L};d
}
.ra_basketball_player<-function(d,team,player,playing=FALSE) {t<-d$teams[[team+1L]];if(player>=length(t$status) || t$status[[player+1L]]=="excluded" || playing && t$status[[player+1L]]!="active") .ra_fail("Player is not eligible for this event")}
.ra_basketball_live<-function(d) {if(d$phase!="period" || !is.null(d$timeout_team)) .ra_fail("Event requires an open playing period outside a timeout")}
.ra_basketball_buzzer<-function(d,flag) {if(d$remaining_ms==0) {if(!flag || !d$buzzer_pending) .ra_fail("An expired clock permits only the unresolved official buzzer play");d$buzzer_pending<-FALSE} else if(flag) .ra_fail("Buzzer ruling requires time expiry");d}
.ra_basketball_validate<-function(d) {
 bad<-function(m) .ra_fail(m,code="INVALID_STATE")
 if(d$remaining_ms>if(d$period<=4) 600000 else 300000) bad("Clock exceeds the current period duration")
 for(t in d$teams) {
  n<-length(t$status)
  if(any(vapply(c("fouls","technical","unsporting","disqualified"),function(k) length(t[[k]])!=n,logical(1)))) bad("Player eligibility and foul vectors must have matching lengths")
  if(length(.ra_basketball_active(t))>5 || !(d$phase %in% c("pre_game","finished")) && length(.ra_basketball_active(t))<2) bad("Active player count conflicts with match eligibility")
  for(i in seq_len(n)) {
   dq<-t$technical[[i]]+t$unsporting[[i]]>=2
   if(t$technical[[i]]+t$unsporting[[i]]>t$fouls[[i]] || dq && !t$disqualified[[i]]) bad("Disciplinary foul totals require consistent player disqualification")
   if((t$status[[i]]=="excluded")!=(t$fouls[[i]]>=5 || t$disqualified[[i]])) bad("Excluded status must follow five fouls or disqualification")
  }
  if(t$timeouts_late_fourth>t$timeouts_second) bad("Late-fourth timeouts are part of the second-half allowance")
 }
 if(d$phase=="pre_game" && (d$period!=1 || d$remaining_ms!=600000 || any(unlist(d$scores)!=0) || any(vapply(d$teams,function(t) any(unlist(t$fouls)>0) || t$team_fouls>0,logical(1))))) bad("Unstarted match must have no score or foul activity")
 if(d$running && (d$phase!="period" || d$remaining_ms==0 || length(d$free_throws)>0 || !is.null(d$timeout_team) || d$pending_arrow)) bad("Running clock conflicts with a stoppage or time expiry")
 if(d$phase %in% c("pre_game","interval","finished") && (d$running || length(d$free_throws)>0 || !is.null(d$timeout_team) || d$pending_arrow || d$buzzer_pending)) bad("Dead interval cannot retain an unresolved playing sequence")
 if(d$phase=="interval" && d$remaining_ms!=0) bad("The previous period must expire before its interval")
 if(d$buzzer_pending && (d$phase!="period" || d$remaining_ms!=0)) bad("Buzzer review exists only at period expiry")
 ids<-vapply(d$free_throws,function(r) r$id,numeric(1))
 if(anyDuplicated(ids) || any(ids>=d$next_penalty_id)) bad("Free-throw penalty identifiers must be distinct and already issued")
 for(r in d$free_throws) if(r$remaining>r$total || r$shooter>=length(d$teams[[r$team+1L]]$status)) bad("Free-throw queue has an invalid attempt count or shooter")
 if(d$phase=="finished") {
  if(is.null(d$reason) || (d$reason=="no_contest")!=is.null(d$winner)) bad("Finished match requires a consistent result")
  if(d$reason!="no_contest" && d$scores[[d$winner+1L]]<=d$scores[[2L-d$winner]]) bad("The winner must lead the final score")
  if(d$reason=="points" && (d$period<4 || d$remaining_ms!=0)) bad("Ordinary victory requires completed regulation or overtime")
 } else {if(!is.null(d$winner) || !is.null(d$reason)) bad("An open match cannot retain a winner");if(d$phase=="interval" && d$period>=4 && d$scores[[1]]!=d$scores[[2]]) bad("Overtime requires tied scores")};invisible(TRUE)
}
.ra_basketball_apply<-function(state,action) {
 d<-state$data;k<-action$type;p<-action$params;s<-.ra_basketball_schemas()
 if(!(k %in% names(s))) .ra_fail("Unknown basketball event")
 .ra_fields(p,s[[k]],"/params")
 if(d$phase=="finished") .ra_fail("Match is finished")
 if(k=="set_lineup") {
  if(d$phase!="pre_game") .ra_fail("Rosters are fixed after the match starts")
  n<-p$roster_size;a<-unlist(p$active)
  if(length(unique(a))!=5 || max(a)>=n) .ra_fail("A starting lineup requires five distinct roster players")
  ti<-p$team+1L;d$teams[[ti]]$status<-lapply(0:(n-1L),function(i) if(i %in% a) "active" else "bench");for(v in c("fouls","technical","unsporting")) d$teams[[ti]][[v]]<-as.list(rep(0L,n));d$teams[[ti]]$disqualified<-as.list(rep(FALSE,n))
 } else if(k=="start_match") {
  if(d$phase!="pre_game" || any(vapply(d$teams,function(t) length(.ra_basketball_active(t))!=5,logical(1)))) .ra_fail("Start with five eligible players per team")
  d$phase<-"period";d$running<-TRUE;d$arrow<-1L-p$first_control
 } else if(k=="pause") {.ra_basketball_live(d);if(!d$running) .ra_fail("Clock is already stopped");d$running<-FALSE
 } else if(k=="resume") {.ra_basketball_live(d);if(d$running || d$remaining_ms==0 || length(d$free_throws)>0 || d$pending_arrow) .ra_fail("Resolve the stoppage before starting the game clock");d$running<-TRUE
 } else if(k=="elapse") {
  .ra_basketball_live(d);if(!d$running || p$milliseconds>d$remaining_ms) .ra_fail("Advance only running time within the period")
  d$remaining_ms<-d$remaining_ms-p$milliseconds;if(d$remaining_ms==0) {d$running<-FALSE;d$buzzer_pending<-TRUE}
 } else if(k=="field_goal") {
  .ra_basketball_live(d);if(length(d$free_throws)>0 || d$pending_arrow) .ra_fail("Resolve the pending penalty or throw-in first")
  .ra_basketball_player(d,p$team,p$player,TRUE);d<-.ra_basketball_buzzer(d,p$at_buzzer);d$scores[[p$team+1L]]<-d$scores[[p$team+1L]]+p$value
  if(d$period>=4 && d$remaining_ms<=120000) d$running<-FALSE
 } else if(k=="foul") {
  .ra_basketball_live(d);team<-p$team;i<-p$player+1L;kind<-p$kind;value<-p$shooting_points;made<-p$basket_made;other<-1L-team
  .ra_basketball_player(d,team,p$player,kind=="personal");.ra_basketball_player(d,other,p$shooter,TRUE)
  if(made && value==0 || p$team_control && (kind!="personal" || value!=0 || made) || kind=="technical" && (value!=0 || made || p$team_control)) .ra_fail("Foul context and shooting outcome are inconsistent")
  if(d$remaining_ms==0 && length(d$free_throws)>0) {if(p$at_buzzer) .ra_fail("The buzzer play is already resolved")} else d<-.ra_basketball_buzzer(d,p$at_buzzer)
  t<-d$teams[[team+1L]];t$fouls[[i]]<-t$fouls[[i]]+1L;t$team_fouls<-t$team_fouls+1L
  if(kind %in% c("technical","unsporting")) t[[kind]][[i]]<-t[[kind]][[i]]+1L
  if(kind=="disqualifying" || t$technical[[i]]+t$unsporting[[i]]>=2) t$disqualified[[i]]<-TRUE
  if(t$fouls[[i]]>=5 || t$disqualified[[i]]) t$status[[i]]<-"excluded"
  if(made) d$scores[[other+1L]]<-d$scores[[other+1L]]+value
  count<-if(kind=="technical" || made) 1L else if(value>0) value else if(kind %in% c("unsporting","disqualifying") || !p$team_control && t$team_fouls>=5) 2L else 0L
  d$teams[[team+1L]]<-t;d<-.ra_basketball_queue(d,other,p$shooter,count,kind,if(kind %in% c("unsporting","disqualifying")) "retained" else if(kind=="technical") "status_quo" else "normal")
  if(p$team_control && d$pending_arrow && team==d$arrow) {d$arrow<-1L-d$arrow;d$pending_arrow<-FALSE}
  d$running<-FALSE;d<-.ra_basketball_default(d)
 } else if(k=="bench_technical") {.ra_basketball_live(d);.ra_basketball_player(d,1L-p$team,p$shooter,TRUE);d<-.ra_basketball_queue(d,1L-p$team,p$shooter,1L,"bench","status_quo");d$running<-FALSE
 } else if(k=="free_throw") {
  .ra_basketball_live(d);if(length(d$free_throws)==0) .ra_fail("No free throw is pending")
  r<-d$free_throws[[1]];.ra_basketball_player(d,r$team,r$shooter,TRUE);violation<-p$violation;made<-p$made && violation!="shooter"
  if(made) d$scores[[r$team+1L]]<-d$scores[[r$team+1L]]+1L
  if(!made && violation=="defense") return(d)
  r$remaining<-r$remaining-1L
  if(r$remaining==0) {d$free_throws<-d$free_throws[-1L];if(length(d$free_throws)==0 && !made && violation=="both" && r$restart=="normal" && d$remaining_ms>0) d$pending_arrow<-TRUE} else d$free_throws[[1]]<-r
 } else if(k=="replace_shooter") {
  .ra_basketball_live(d);idx<-which(vapply(d$free_throws,function(r) r$id==p$penalty_id,logical(1)))
  if(length(idx)==0) .ra_fail("Only an unavailable pending shooter needs replacement")
  r<-d$free_throws[[idx]];if(r$shooter %in% unlist(.ra_basketball_active(d$teams[[r$team+1L]]))) .ra_fail("Only an unavailable pending shooter needs replacement")
  .ra_basketball_player(d,r$team,p$player,TRUE);d$free_throws[[idx]]$shooter<-p$player
 } else if(k=="cancel_equal_penalties") {
  .ra_basketball_live(d);aa<-which(vapply(d$free_throws,function(r) r$id==p$first_id,logical(1)));bb<-which(vapply(d$free_throws,function(r) r$id==p$second_id,logical(1)))
  if(length(aa)==0 || length(bb)==0) .ra_fail("Only unadministered equal opposite penalties may cancel")
  a<-d$free_throws[[aa]];b<-d$free_throws[[bb]]
  if(a$team==b$team || a$remaining!=a$total || b$remaining!=b$total || a$total!=b$total || a$restart!=b$restart) .ra_fail("Only unadministered equal opposite penalties may cancel")
  d$free_throws<-d$free_throws[-c(aa,bb)]
 } else if(k %in% c("substitute","enter_player")) {
  if(!(d$phase %in% c("pre_game","interval"))) .ra_basketball_live(d)
  if(d$running) .ra_fail("Player changes require a stopped-clock opportunity")
  ti<-p$team+1L;t<-d$teams[[ti]];inc<-if(k=="substitute") p[["in"]] else p$player;.ra_basketball_player(d,p$team,inc)
  if(t$status[[inc+1L]]!="bench") .ra_fail("Only an eligible bench player may enter")
  if(k=="substitute") {
   if(length(d$free_throws)>0 && d$free_throws[[1]]$remaining<d$free_throws[[1]]$total) .ra_fail("Ordinary substitution cannot interrupt a free-throw series")
   out<-p$out;if(!(out %in% unlist(.ra_basketball_active(t))) || any(vapply(d$free_throws,function(r) r$team==p$team && r$shooter==out,logical(1)))) .ra_fail("Outgoing player must be active and cannot be a pending free-throw shooter")
   t$status[[out+1L]]<-"bench"
  } else if(length(.ra_basketball_active(t))>=5) .ra_fail("No active-player vacancy is available")
  t$status[[inc+1L]]<-"active";d$teams[[ti]]<-t
 } else if(k=="timeout") {
  .ra_basketball_live(d)
  if(d$running || .ra_basketball_timeouts(d,p$team)<=0 || length(d$free_throws)>0 && d$free_throws[[1]]$remaining<d$free_throws[[1]]$total) .ra_fail("No timeout is available at this stoppage")
  ti<-p$team+1L;t<-d$teams[[ti]]
  if(d$period<=2) t$timeouts_first<-t$timeouts_first+1L else if(d$period>=5) t$timeouts_overtime<-t$timeouts_overtime+1L else {t$timeouts_second<-t$timeouts_second+1L;if(d$period==4 && d$remaining_ms<=120000) t$timeouts_late_fourth<-t$timeouts_late_fourth+1L}
  d$teams[[ti]]<-t;d$timeout_team<-p$team
 } else if(k=="end_timeout") {if(is.null(d$timeout_team)) .ra_fail("No timeout is active");d["timeout_team"]<-list(NULL)
 } else if(k=="held_ball") {.ra_basketball_live(d);if(d$running || length(d$free_throws)>0 || d$pending_arrow || d$remaining_ms==0) .ra_fail("Alternating possession requires an available stopped-clock restart");d$pending_arrow<-TRUE
 } else if(k=="resolve_arrow_throwin") {.ra_basketball_live(d);if(!d$pending_arrow || length(d$free_throws)>0) .ra_fail("No unblocked alternating-possession throw-in is pending");if(p$result!="defender_violation") {d$arrow<-1L-d$arrow;d$pending_arrow<-FALSE}
 } else if(k=="end_period") {
  .ra_basketball_live(d);if(d$remaining_ms!=0 || length(d$free_throws)>0 || d$pending_arrow) .ra_fail("Time and all pending penalties must resolve before the period ends")
  d$buzzer_pending<-FALSE
  if(d$period>=4 && d$scores[[1]]!=d$scores[[2]]) d<-.ra_basketball_finish(d,if(d$scores[[1]]>d$scores[[2]]) 0L else 1L,"points") else d$phase<-"interval"
 } else if(k=="start_next_period") {
  if(d$phase!="interval") .ra_fail("No period interval is active")
  d$period<-d$period+1L;d$remaining_ms<-if(d$period<=4) 600000L else 300000L;d$phase<-"period";d$pending_arrow<-TRUE
  for(i in 1:2) if(d$period<=4) d$teams[[i]]$team_fouls<-0L else d$teams[[i]]$timeouts_overtime<-0L
 } else if(k=="forfeit") {w<-1L-p$loser;d$scores<-lapply(0:1,function(i) if(i==w) 20L else 0L);d<-.ra_basketball_finish(d,w,"forfeit")
 } else if(k=="no_contest") d<-.ra_basketball_finish(d,NULL,"no_contest");d
}
.ra_basketball_legal<-function(state) {
 d<-state$data;phase<-d$phase;names<-character()
 if(phase=="pre_game") names<-c("set_lineup","substitute","start_match") else if(phase=="interval") names<-c("start_next_period","substitute","enter_player") else if(phase=="period") {
  if(!is.null(d$timeout_team)) names<-"end_timeout" else {
   names<-c("foul","bench_technical",if(d$running) c("pause","elapse") else c("substitute","enter_player","timeout"))
   if(length(d$free_throws)>0) names<-c(names,"free_throw","replace_shooter","cancel_equal_penalties") else if(d$pending_arrow) names<-c(names,"resolve_arrow_throwin") else if(d$remaining_ms>0) names<-c(names,"field_goal",if(!d$running) c("resume","held_ball")) else names<-c(names,"end_period",if(d$buzzer_pending) "field_goal")
  }
 }
 if(phase!="finished") names<-c(names,"forfeit","no_contest")
 s<-.ra_basketball_schemas()
 list(mode="event_types",actions=list(),action_types=lapply(names,function(k) list(type=k,params_schema=s[[k]],preconditions=list("Period, stopped-clock opportunity, player eligibility and pending penalties must permit the event."),external_observations=list("Official valid baskets, foul context, completed buzzer play and restart rulings."))),constraints=list(phase=phase,period=d$period,remaining_ms=d$remaining_ms,running=d$running,arrow=d$arrow,active=lapply(d$teams,.ra_basketball_active),timeouts_remaining=lapply(0:1,function(i) .ra_basketball_timeouts(d,i)),next_free_throw=if(length(d$free_throws)>0) d$free_throws[[1]] else NULL),completeness=FALSE,reason="official_basketball_events")
}
.ra_basketball_outcome<-function(state,pack) {
 d<-state$data;done<-d$phase=="finished";w<-d$winner
 list(status=if(done) "terminal" else "ongoing",result_kind=if(done) {if(is.null(w)) "no_contest" else if(d$reason %in% c("forfeit","default")) "forfeit" else "win"} else "none",winners=if(done && !is.null(w)) list(state$players[[w+1L]]$id) else list(),losers=if(done && !is.null(w)) list(state$players[[2L-w]]$id) else list(),placements=list(),reason=if(done) d$reason else "ongoing",evidence_required=list(),rule_id=if(done) paste0(pack$ruleset_id,"@",pack$ruleset_version) else NULL)
}
