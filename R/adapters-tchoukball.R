.ra_tchoukball_i<-function(a,b) list(type="integer",minimum=a,maximum=b)
.ra_tchoukball_active<-function(d,t) which(unlist(d$status[[t+1L]])=="active")-1L
.ra_tchoukball_finish<-function(d,w,reason) {d$phase<-"finished";d["winner"]<-list(w);d$reason<-reason;d$clock_running<-FALSE;d}
.ra_tchoukball_forfeit<-function(d,t) {d$scores<-lapply(0:1,function(j) if(j!=t) 30L else 0L);.ra_tchoukball_finish(d,1L-t,"forfeit")}
.ra_tchoukball_reset<-function(d,phase,t) {d$phase<-phase;d$next_team<-t;d["last_frame"]<-list(NULL);d$frame_count<-0L;d}
.ra_tchoukball_point<-function(d,t) {d$scores[[t+1L]]<-d$scores[[t+1L]]+1L;d<-.ra_tchoukball_reset(d,"restart",1L-t);d$entry_limit<-list(7L,7L);d}
.ra_tchoukball_endperiod<-function(d) {
 d$clock_running<-FALSE
 if(d$period>=3 && (d$scores[[1]]!=d$scores[[2]] || d$allow_draw)) d<-.ra_tchoukball_finish(d,if(d$scores[[1]]==d$scores[[2]]) NULL else as.integer(d$scores[[2]]>d$scores[[1]]),if(d$period==3) "regulation" else "extra_time") else d$phase<-"interval"
 d
}
.ra_tchoukball_validate<-function(d) {
 bad<-function(m) .ra_fail(m,code="INVALID_STATE")
 for(t in 0:1) {
  if(length(.ra_tchoukball_active(d,t))>d$entry_limit[[t+1L]]) bad("Active lineup exceeds current replacement entitlement")
  if(!d$phase %in% c("pre_game","finished","abandoned") && length(.ra_tchoukball_active(d,t))<5) bad("Fewer than five active players requires forfeiture")
  for(i in 1:12) if(d$yellow[[t+1L]][[i]]==2 && d$status[[t+1L]][[i]]!="excluded") bad("Two yellow cards expel the player")
 }
 if(is.null(d$last_frame)!=(d$frame_count==0)) bad("Frame identity and consecutive shot count must agree")
 if(d$phase %in% c("restart","breakdown","pre_game") && d$frame_count!=0) bad("Restarts and breakdowns reset the frame count")
 if(d$phase=="pre_game" && (any(unlist(d$scores)!=0) || d$period!=1 || d$clock_running || !is.null(d$first_team))) bad("Pre-game state precedes score, clock and toss")
 if(!d$phase %in% c("pre_game","finished","abandoned") && is.null(d$first_team)) bad("Play requires initial restart choice")
 if(d$period>3 && d$remaining_ms>300000) bad("Extra periods last five minutes")
 if(d$phase=="interval" && (d$remaining_ms!=0 || d$clock_running)) bad("Interval follows an expired period")
 if(d$phase %in% c("restart","play","breakdown") && d$remaining_ms==0) bad("Period expiry immediately stops play")
 if(d$clock_running && !d$phase %in% c("restart","play","breakdown")) bad("Clock runs only during a period")
 if(d$phase=="finished") {
  if(is.null(d$reason)) bad("Finished match requires a result")
  if(d$reason %in% c("regulation","extra_time")) {
   expected<-if(d$scores[[1]]==d$scores[[2]]) NULL else as.integer(d$scores[[2]]>d$scores[[1]])
   if(d$period<3 || d$remaining_ms!=0 || !identical(expected,d$winner) || (is.null(expected) && !d$allow_draw)) bad("Timed result must agree with score and match format")
  }
  if(d$reason=="forfeit" && (is.null(d$winner) || d$scores[[d$winner+1L]]!=30 || d$scores[[2L-d$winner]]!=0)) bad("Forfeit is recorded as thirty to nil")
 } else if(!is.null(d$winner) || !is.null(d$reason)) bad("Only finished matches have a result")
}
.ra_tchoukball_schemas<-function(d) {
 ph<-d$phase;s<-list();i<-.ra_tchoukball_i;t<-i(0,1);p<-i(0,11);b<-list(type="boolean");empty<-setNames(list(),character())
 if(ph=="finished") return(s)
 if(ph=="abandoned") return(list(official_result=list(winner=c(t,list(nullable=TRUE)))))
 if(ph=="pre_game") {s$configure<-list(allow_draw=b);s$start_game<-list(first_team=t)}
 if(ph=="interval") s$next_period<-empty
 if(ph %in% c("pre_game","interval","restart")) {s$substitute<-list(team=t,out=p,"in"=p);s$enter<-list(team=t,player=p)}
 if(ph %in% c("restart","play","breakdown")) {
  s$withdraw<-list(team=t,player=p);s$sanction<-list(team=t,player=p,kind=list(type="enum",values=list("oral","oral_breakdown","yellow","red")));s$official_breakdown<-list(team=t)
  if(d$clock_running) {
   s$elapse<-list(milliseconds=i(1,900000));s$stop_clock<-empty
   if(ph=="restart") {s$restart<-list(team=t,player=p);s$restart_fault<-empty}
   if(ph=="breakdown") s$take_breakdown<-list(team=t,player=p,correct=b)
   if(ph=="play") s$shot<-list(team=t,player=p,frame=t,passes=i(0,3),median_crossed=b,result=list(type="enum",values=list("scored","conceded","caught","faulty_rebound")))
  } else s$resume_clock<-empty
 }
 c(s,list(concede=list(team=t),abandon=empty))
}
.ra_tchoukball_apply<-function(state,action) {
 d<-state$data;k<-action$type;p<-action$params;s<-.ra_tchoukball_schemas(d);t<-p$team
 if(!k %in% names(s)) .ra_fail("Action is not permitted in this phase")
 .ra_fields(p,s[[k]],"/params")
 if(k=="configure") d$allow_draw<-p$allow_draw else if(k=="start_game") {
  if(any(vapply(0:1,function(t) length(.ra_tchoukball_active(d,t))<5,logical(1)))) .ra_fail("At least five players per team are required")
  d$first_team<-p$first_team;d$period_starter<-p$first_team;d$clock_running<-TRUE;d<-.ra_tchoukball_reset(d,"restart",p$first_team)
 } else if(k=="next_period") {
  d$period<-d$period+1L
  starter<-if(d$period==2) 1L-d$first_team else if(d$period==3) {if(d$scores[[1]]==d$scores[[2]]) d$first_team else as.integer(d$scores[[2]]<d$scores[[1]])} else 1L-d$period_starter
  d$period_starter<-starter;d$remaining_ms<-if(d$period<=3) 900000L else 300000L;d$clock_running<-TRUE;d$entry_limit<-list(7L,7L);d<-.ra_tchoukball_reset(d,"restart",starter)
 } else if(k=="substitute") {
  if(!p$out %in% .ra_tchoukball_active(d,t) || d$status[[t+1L]][[p[["in"]]+1L]]!="bench") .ra_fail("Substitution requires an active player and eligible bench player")
  d$status[[t+1L]][[p$out+1L]]<-"bench";d$status[[t+1L]][[p[["in"]]+1L]]<-"active"
 } else if(k=="enter") {
  if(d$status[[t+1L]][[p$player+1L]]!="bench" || length(.ra_tchoukball_active(d,t))>=d$entry_limit[[t+1L]]) .ra_fail("No eligible replacement place is available")
  d$status[[t+1L]][[p$player+1L]]<-"active"
 } else if(k=="withdraw") {
  if(!p$player %in% .ra_tchoukball_active(d,t)) .ra_fail("Only an active player may leave play")
  d$status[[t+1L]][[p$player+1L]]<-"unavailable";d$entry_limit[[t+1L]]<-length(.ra_tchoukball_active(d,t))
  if(length(.ra_tchoukball_active(d,t))<5) d<-.ra_tchoukball_forfeit(d,t)
 } else if(k=="sanction") {
  idx<-p$player+1L;kind<-p$kind
  if(d$status[[t+1L]][[idx]]=="excluded") .ra_fail("An expelled player cannot receive another match sanction")
  if(kind=="yellow") d$yellow[[t+1L]][[idx]]<-d$yellow[[t+1L]][[idx]]+1L
  if(kind=="red" || d$yellow[[t+1L]][[idx]]==2) {
   was_active<-p$player %in% .ra_tchoukball_active(d,t);d$status[[t+1L]][[idx]]<-"excluded"
   if(was_active) d$entry_limit[[t+1L]]<-length(.ra_tchoukball_active(d,t))
  }
  if(kind!="oral") d<-.ra_tchoukball_reset(d,"breakdown",1L-t)
  if(length(.ra_tchoukball_active(d,t))<5) d<-.ra_tchoukball_forfeit(d,t)
 } else if(k=="official_breakdown") d<-.ra_tchoukball_reset(d,"breakdown",t) else if(k=="restart_fault") d<-.ra_tchoukball_reset(d,"restart",1L-d$next_team) else if(k %in% c("restart","take_breakdown")) {
  if(t!=d$next_team || !p$player %in% .ra_tchoukball_active(d,t)) .ra_fail("Restart requires an active player from the entitled team")
  if(k=="restart" || p$correct) {d$phase<-"play";d$attack_origin<-if(k=="restart") "restart" else "breakdown"}
 } else if(k=="shot") {
  if(t!=d$next_team || !p$player %in% .ra_tchoukball_active(d,t)) .ra_fail("Shot requires an active player from the entitled team")
  if(d$attack_origin=="restart" && !p$median_crossed) .ra_fail("First shot after restart requires crossing the median line")
  if(d$attack_origin=="breakdown" && p$passes==0) .ra_fail("A breakdown requires at least one pass before shooting")
  if(!is.null(d$last_frame) && d$last_frame==p$frame && d$frame_count==3) .ra_fail("A fourth consecutive shot at the same frame is prohibited")
  d$frame_count<-if(!is.null(d$last_frame) && d$last_frame==p$frame) d$frame_count+1L else 1L;d$last_frame<-p$frame
  if(p$result=="scored") d<-.ra_tchoukball_point(d,t) else if(p$result=="conceded") d<-.ra_tchoukball_point(d,1L-t) else if(p$result=="faulty_rebound") d<-.ra_tchoukball_reset(d,"breakdown",1L-t) else {d$next_team<-1L-t;d$attack_origin<-"catch"}
 } else if(k=="elapse") {
  if(p$milliseconds>d$remaining_ms) .ra_fail("Elapsed time exceeds the current period")
  d$remaining_ms<-d$remaining_ms-p$milliseconds
  if(d$remaining_ms==0) d<-.ra_tchoukball_endperiod(d)
 } else if(k=="stop_clock") d$clock_running<-FALSE else if(k=="resume_clock") d$clock_running<-TRUE else if(k=="concede") d<-.ra_tchoukball_forfeit(d,t) else if(k=="abandon") {d$phase<-"abandoned";d$clock_running<-FALSE} else if(k=="official_result") d<-.ra_tchoukball_finish(d,p$winner,if(is.null(p$winner)) "no_contest" else "official_result")
 d
}
.ra_tchoukball_legal<-function(state) {
 d<-state$data;s<-.ra_tchoukball_schemas(d)
 list(mode="event_types",actions=list(),action_types=lapply(names(s),function(k) list(type=k,params_schema=s[[k]],preconditions=list("Period, restart entitlement, active lineup and attack restrictions must permit the event."),external_observations=list("Official shot outcome, pass count, median-line crossing and sanctions."))),constraints=list(phase=d$phase,period=d$period,remaining_ms=d$remaining_ms,clock_running=d$clock_running,next_team=d$next_team,active=lapply(0:1,function(t) as.list(.ra_tchoukball_active(d,t))),entry_limit=d$entry_limit,last_frame=d$last_frame,frame_count=d$frame_count,minimum_passes=if(d$phase=="play" && d$attack_origin=="breakdown") 1L else 0L,median_required=d$phase=="play" && d$attack_origin=="restart"),completeness=FALSE,reason="official_tchoukball_events")
}
.ra_tchoukball_outcome<-function(state,pack) {
 d<-state$data;done<-d$phase=="finished";w<-d$winner
 list(status=if(done) "terminal" else if(d$phase=="abandoned") "awaiting_adjudication" else "ongoing",result_kind=if(done) {if(!is.null(w)) "win" else if(d$reason=="no_contest") "no_contest" else "draw"} else "none",winners=if(!is.null(w)) list(state$players[[w+1L]]$id) else list(),losers=if(!is.null(w)) list(state$players[[2L-w]]$id) else list(),placements=list(),reason=if(done) d$reason else if(d$phase=="abandoned") "official_result_required" else "ongoing",evidence_required=if(d$phase=="abandoned") list("Official result decision.") else list(),rule_id=if(done) paste0(pack$ruleset_id,"@",pack$ruleset_version) else NULL)
}
