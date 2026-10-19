.ra_netball_schemas<-function() {
 i<-function(a,b) list(type="integer",minimum=a,maximum=b);team<-i(0,1);slot<-i(0,11);bool<-list(type="boolean");empty<-setNames(list(),character());line<-list(type="array",minItems=7,maxItems=7,items=c(slot,list(nullable=TRUE)))
 list(set_roster=list(team=team,size=i(7,12),lineup=line),start_match=list(first_centre=team),centre_pass=list(team=team,player=slot),goal=list(team=team,player=slot),elapse=list(milliseconds=i(1,900000)),pause=empty,resume=empty,change_lineup=list(team=team,lineup=line),warn=list(team=team,player=slot),suspend=list(team=team,player=slot),dismiss=list(team=team,player=slot),award_expiry_shot=list(team=team,player=slot),expiry_shot=list(scored=bool),end_quarter=empty,start_next_quarter=empty,concede=list(team=team),no_contest=empty)
}
.ra_netball_finish<-function(d,w,reason) {d$phase<-"finished";d["winner"]<-list(w);d$reason<-reason;d$running<-FALSE;d["pending_shot"]<-list(NULL);d$expiry_open<-FALSE;d}
.ra_netball_available<-function(t,i) !t$dismissed[[i+1L]] && t$suspension_ms[[i+1L]]==0
.ra_netball_playing<-function(t) Filter(function(i) !is.null(i) && .ra_netball_available(t,i),t$lineup)
.ra_netball_period<-function(d) {if(d$phase!="quarter") .ra_fail("Event requires an open quarter")}
.ra_netball_scorer<-function(d,team,i) {t<-d$teams[[team+1L]];if(!(i %in% unlist(t$lineup[1:2])) || !.ra_netball_available(t,i)) .ra_fail("Only an eligible goal shooter or goal attack may score")}
.ra_netball_validate<-function(d) {
 bad<-function(m) .ra_fail(m,code="INVALID_STATE")
 for(t in d$teams) {
  n<-length(t$warned);ids<-unlist(t$lineup)
  if(length(t$suspension_ms)!=n || length(t$dismissed)!=n || anyDuplicated(ids) || any(ids>=n)) bad("Roster vectors and distinct position assignments must agree")
  if(any(vapply(seq_len(n),function(i) t$dismissed[[i]] && t$suspension_ms[[i]]>0,logical(1)))) bad("Dismissal supersedes a temporary suspension")
  if(d$phase!="finished" && length(.ra_netball_playing(t))<5) bad("At least five eligible players are required to continue")
 }
 if(d$phase=="pre_game" && (d$quarter!=1 || d$remaining_ms!=900000 || any(unlist(d$scores)!=0) || any(vapply(d$teams,function(t) any(unlist(t$warned)) || any(unlist(t$dismissed)) || any(unlist(t$suspension_ms)>0),logical(1))))) bad("New match cannot contain playing or disciplinary history")
 if(d$running && (d$phase!="quarter" || d$remaining_ms==0 || !is.null(d$pending_shot))) bad("Running clock requires an open quarter with time remaining")
 if(d$phase %in% c("pre_game","interval","finished") && (d$running || !is.null(d$pending_shot) || d$expiry_open)) bad("Inactive phase cannot retain an unresolved expiry play")
 if(d$phase=="interval" && (d$remaining_ms>0 || d$quarter>=4)) bad("Quarter interval requires a completed non-final quarter")
 if(d$expiry_open && (d$phase!="quarter" || d$remaining_ms>0)) bad("Expiry decision requires a quarter at zero time")
 if(!is.null(d$pending_shot)) {r<-d$pending_shot;if(d$phase!="quarter" || d$remaining_ms>0 || d$expiry_open || !(r$player %in% unlist(d$teams[[r$team+1L]]$lineup[1:2]))) bad("Pending penalty shot must follow the expiry ruling and a shooting position")}
 if(d$phase=="finished") {
  if(is.null(d$reason) || (d$reason %in% c("draw","no_contest"))!=is.null(d$winner)) bad("Finished match needs a consistent result")
  if(d$reason=="points" && (d$quarter!=4 || d$remaining_ms>0 || d$scores[[d$winner+1L]]<=d$scores[[2L-d$winner]])) bad("Points win requires completed regulation and a lead")
  if(d$reason=="draw" && (d$quarter!=4 || d$remaining_ms>0 || d$scores[[1]]!=d$scores[[2]])) bad("Draw requires tied completed regulation")
 } else if(!is.null(d$winner) || !is.null(d$reason)) bad("Open match has no final result");invisible(TRUE)
}
.ra_netball_apply<-function(state,action) {
 d<-state$data;k<-action$type;p<-action$params;s<-.ra_netball_schemas()
 if(!(k %in% names(s))) .ra_fail("Unknown netball event")
 .ra_fields(p,s[[k]],"/params")
 if(d$phase=="finished") .ra_fail("Match is finished")
 if(k=="set_roster") {
  if(d$phase!="pre_game") .ra_fail("Set rosters before starting")
  n<-p$size;ids<-unlist(p$lineup)
  if(length(ids)<5 || anyDuplicated(ids) || any(ids>=n)) .ra_fail("Lineup requires five to seven distinct eligible roster players")
  d$teams[[p$team+1L]]<-list(lineup=p$lineup,warned=as.list(rep(FALSE,n)),suspension_ms=as.list(rep(0L,n)),dismissed=as.list(rep(FALSE,n)))
 } else if(k=="start_match") {
  if(d$phase!="pre_game" || any(vapply(d$teams,function(t) length(.ra_netball_playing(t))!=7,logical(1)))) .ra_fail("This format starts with all seven positions occupied")
  d$phase<-"quarter";d$centre_team<-p$first_centre;d$centre_due<-TRUE;d$running<-TRUE
 } else if(k=="centre_pass") {
  .ra_netball_period(d);t<-d$teams[[p$team+1L]]
  if(d$remaining_ms==0 || !d$centre_due || p$team!=d$centre_team || is.null(t$lineup[[4]]) || t$lineup[[4]]!=p$player || !.ra_netball_available(t,p$player)) .ra_fail("The next centre pass belongs to the eligible centre of the due team")
  d$centre_due<-FALSE;d$centre_team<-1L-d$centre_team
 } else if(k=="goal") {
  .ra_netball_period(d);if(d$centre_due || d$remaining_ms==0) .ra_fail("A goal requires a completed centre pass and time remaining")
  .ra_netball_scorer(d,p$team,p$player);d$scores[[p$team+1L]]<-d$scores[[p$team+1L]]+1L;d$centre_due<-TRUE
 } else if(k=="elapse") {
  .ra_netball_period(d);if(!d$running || p$milliseconds>d$remaining_ms) .ra_fail("Advance only running time within the quarter")
  dt<-p$milliseconds;d$remaining_ms<-d$remaining_ms-dt
  for(i in 1:2) d$teams[[i]]$suspension_ms<-lapply(d$teams[[i]]$suspension_ms,function(x) max(0L,x-dt))
  if(d$remaining_ms==0) {d$running<-FALSE;d$expiry_open<-TRUE}
 } else if(k=="pause") {.ra_netball_period(d);if(!d$running) .ra_fail("Clock is already stopped");d$running<-FALSE
 } else if(k=="resume") {.ra_netball_period(d);if(d$running || d$remaining_ms==0 || !is.null(d$pending_shot)) .ra_fail("Clock cannot resume at this stoppage");d$running<-TRUE
 } else if(k=="change_lineup") {
  if(!(d$phase %in% c("pre_game","interval")) && (d$phase!="quarter" || d$running || d$remaining_ms==0)) .ra_fail("Lineup changes require an official substitution stoppage or interval")
  ti<-p$team+1L;t<-d$teams[[ti]];new<-p$lineup;blocked<-unlist(Filter(function(i) !is.null(i) && !.ra_netball_available(t,i),t$lineup));eligible<-unlist(Filter(function(i) !is.null(i) && !(i %in% blocked),new));ids<-unlist(new)
  if(!all(blocked %in% ids) || length(eligible)<5 || anyDuplicated(ids) || any(vapply(as.list(eligible),function(i) i>=length(t$warned) || !.ra_netball_available(t,i),logical(1)))) .ra_fail("Substitution cannot fill a sanctioned position or introduce an ineligible player")
  d$teams[[ti]]$lineup<-new
 } else if(k %in% c("warn","suspend","dismiss")) {
  .ra_netball_period(d);ti<-p$team+1L;t<-d$teams[[ti]];i<-p$player+1L
  if(i>length(t$warned) || t$dismissed[[i]]) .ra_fail("Discipline requires a roster player who has not been dismissed")
  if(k=="warn") {if(t$warned[[i]]) .ra_fail("An existing formal warning cannot be repeated");t$warned[[i]]<-TRUE} else if(k=="suspend") {if(!(p$player %in% unlist(.ra_netball_playing(t)))) .ra_fail("Suspension requires an active eligible player");t$suspension_ms[[i]]<-120000L} else {t$dismissed[[i]]<-TRUE;t$suspension_ms[[i]]<-0L}
  d$teams[[ti]]<-t;d$running<-FALSE;if(length(.ra_netball_playing(t))<5) d<-.ra_netball_finish(d,1L-p$team,"default")
 } else if(k=="award_expiry_shot") {
  .ra_netball_period(d);if(!d$expiry_open || !is.null(d$pending_shot)) .ra_fail("Only one unresolved expiry ruling is available")
  .ra_netball_scorer(d,p$team,p$player);d$pending_shot<-list(team=p$team,player=p$player);d$expiry_open<-FALSE
 } else if(k=="expiry_shot") {
  .ra_netball_period(d);r<-d$pending_shot;if(is.null(r)) .ra_fail("No expiry penalty shot is pending")
  .ra_netball_scorer(d,r$team,r$player);if(p$scored) d$scores[[r$team+1L]]<-d$scores[[r$team+1L]]+1L;d["pending_shot"]<-list(NULL)
 } else if(k=="end_quarter") {
  .ra_netball_period(d);if(d$remaining_ms>0 || !is.null(d$pending_shot)) .ra_fail("Resolve time and any awarded penalty shot first")
  d$expiry_open<-FALSE
  if(d$quarter==4) {tied<-d$scores[[1]]==d$scores[[2]];d<-.ra_netball_finish(d,if(tied) NULL else if(d$scores[[1]]>d$scores[[2]]) 0L else 1L,if(tied) "draw" else "points")} else d$phase<-"interval"
 } else if(k=="start_next_quarter") {
  if(d$phase!="interval") .ra_fail("No quarter interval is active")
  d$phase<-"quarter";d$quarter<-d$quarter+1L;d$remaining_ms<-900000L;d$running<-TRUE;d$centre_due<-TRUE
 } else if(k=="concede") d<-.ra_netball_finish(d,1L-p$team,"concession") else if(k=="no_contest") d<-.ra_netball_finish(d,NULL,"no_contest");d
}
.ra_netball_legal<-function(state) {
 d<-state$data;phase<-d$phase;names<-character()
 if(phase=="pre_game") names<-c("set_roster","change_lineup","start_match") else if(phase=="interval") names<-c("change_lineup","start_next_quarter") else if(phase=="quarter") {
  names<-c("warn","suspend","dismiss")
  if(d$remaining_ms>0) names<-c(names,if(d$centre_due) "centre_pass" else "goal",if(d$running) c("pause","elapse") else c("resume","change_lineup")) else if(!is.null(d$pending_shot)) names<-c(names,"expiry_shot") else names<-c(names,"end_quarter",if(d$expiry_open) "award_expiry_shot")
 }
 if(phase!="finished") names<-c(names,"concede","no_contest")
 s<-.ra_netball_schemas()
 list(mode="event_types",actions=list(),action_types=lapply(names,function(k) list(type=k,params_schema=s[[k]],preconditions=list("Quarter, centre-pass order, position eligibility and sanction time must permit the event."),external_observations=list("Official goals, disciplinary rulings, substitution opportunities and expiry penalty shots."))),constraints=list(phase=phase,quarter=d$quarter,remaining_ms=d$remaining_ms,centre_team=d$centre_team,centre_due=d$centre_due,active=lapply(d$teams,.ra_netball_playing),shooters=lapply(d$teams,function(t) Filter(function(i) !is.null(i) && .ra_netball_available(t,i),t$lineup[1:2])),pending_shot=d$pending_shot),completeness=FALSE,reason="official_netball_events")
}
.ra_netball_outcome<-function(state,pack) {
 d<-state$data;done<-d$phase=="finished";w<-d$winner
 list(status=if(done) "terminal" else "ongoing",result_kind=if(done) {if(d$reason=="draw") "draw" else if(is.null(w)) "no_contest" else "win"} else "none",winners=if(done && !is.null(w)) list(state$players[[w+1L]]$id) else list(),losers=if(done && !is.null(w)) list(state$players[[2L-w]]$id) else list(),placements=list(),reason=if(done) d$reason else "ongoing",evidence_required=list(),rule_id=if(done) paste0(pack$ruleset_id,"@",pack$ruleset_version) else NULL)
}
