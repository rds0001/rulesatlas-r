.ra_sambo_i<-function(a,b) list(type="integer",minimum=a,maximum=b)
.ra_sambo_summary<-function(d) {
 tech<-vapply(0:1,function(t) sum(vapply(d$techniques,function(x) if(x$player==t) x$points else 0,numeric(1))),numeric(1))
 penalty<-c(0,0);warnings<-c(0,0);prohibited<-c(FALSE,FALSE);last<-NULL
 for(e in d$warnings) {
  for(t in unlist(e$players)) {i<-t+1L;warnings[i]<-warnings[i]+1L;penalty[2L-t]<-penalty[2L-t]+as.integer(warnings[i]>1 || e$kind=="prohibited");if(e$kind=="prohibited") prohibited[i]<-TRUE}
  if(length(e$players)==1) last<-e$players[[1]]
 }
 list(tech=tech,scores=tech+penalty,warnings=warnings,prohibited=prohibited,last=last)
}
.ra_sambo_decision<-function(d) {
 s<-.ra_sambo_summary(d)
 counts<-function(value) vapply(0:1,function(t) sum(vapply(d$techniques,function(x) x$player==t && x$points==value,logical(1))),numeric(1))
 for(v in list(s$scores,s$tech,counts(4),counts(2))) if(v[1]!=v[2]) return(as.integer(v[2]>v[1]))
 if(length(d$techniques)>0) return(tail(d$techniques,1)[[1]]$player)
 if(s$warnings[1]!=s$warnings[2]) return(as.integer(s$warnings[2]<s$warnings[1]))
 if(is.null(s$last)) NULL else 1L-s$last
}
.ra_sambo_finish<-function(d,w,reason) {d$phase<-"finished";d["winner"]<-list(w);d$reason<-reason;d}
.ra_sambo_superiority<-function(d) {s<-.ra_sambo_summary(d)$scores;if(abs(s[1]-s[2])>=8) d<-.ra_sambo_finish(d,as.integer(s[2]>s[1]),"superiority");d}
.ra_sambo_validate<-function(d) {
 bad<-function(m) .ra_fail(m,code="INVALID_STATE")
 for(t in 0:1) {
  holds<-vapply(Filter(function(x) x$player==t && x$kind=="hold_down",d$techniques),function(x) x$points,numeric(1))
  if(sum(holds)>4 || length(holds)>2 || any(!holds %in% c(2,4))) bad("Scored hold-downs cannot exceed four points per athlete")
 }
 s<-.ra_sambo_summary(d)
 if(max(s$warnings)>3) bad("Fourth warning must be a terminal withdrawal, not a scored warning")
 for(t in 0:1) if(sum(vapply(d$warnings,function(e) t %in% unlist(e$players) && e$kind=="prohibited",logical(1)))>1) bad("Repeated prohibited technique requires withdrawal")
 for(e in d$warnings) if(anyDuplicated(unlist(e$players))) bad("Simultaneous warnings require distinct athletes")
 if(d$phase=="ready" && (d$remaining_ms!=300000 || length(d$techniques)>0 || length(d$warnings)>0 || any(unlist(d$assistance_ms)!=0))) bad("Ready bout has no prior activity")
 if(d$phase %in% c("fighting","paused") && d$remaining_ms==0) bad("Expired bout requires score-sheet review")
 if(d$phase=="review" && d$remaining_ms!=0) bad("Review follows the final gong")
 if(d$phase=="finished") {
  if(is.null(d$reason) || is.null(d$winner)!=(d$reason=="double_defeat")) bad("Finished bout requires a consistent result")
  if(d$reason=="points" && (d$remaining_ms!=0 || !identical(.ra_sambo_decision(d),d$winner))) bad("Final decision must follow the score-sheet tie-break")
  if(d$reason=="superiority" && (is.null(d$winner) || s$scores[d$winner+1L]-s$scores[2L-d$winner]<8)) bad("Superiority requires an eight-point lead")
 } else if(!is.null(d$winner) || !is.null(d$reason) || abs(s$scores[1]-s$scores[2])>=8) bad("An ongoing bout cannot retain a decided result")
}
.ra_sambo_schemas<-function(d) {
 ph<-d$phase;s<-list();i<-.ra_sambo_i;p<-i(0,1);empty<-setNames(list(),character())
 if(ph=="finished") return(s)
 if(ph=="ready") s$start<-empty
 if(ph=="fighting") {s$pause<-empty;s$elapse<-list(milliseconds=i(1,300000))}
 if(ph=="paused") {s$resume<-empty;s$assistance<-list(player=p,milliseconds=i(1,120001))}
 if(ph %in% c("fighting","paused","review")) {
  s$throw<-list(player=p,points=list(type="enum",values=list(1L,2L,4L)))
  s$hold_down<-list(player=p,duration_ms=i(0,20000))
  s$warning<-list(players=list(type="array",minItems=1,maxItems=2,items=p),kind=list(type="enum",values=list("ordinary","prohibited")))
  s$total_victory<-list(player=p,method=list(type="enum",values=list("perfect_throw","submission")))
 }
 if(ph=="review") s$confirm_result<-empty
 c(s,list(withdraw=list(player=p),disqualify=list(player=p),double_defeat=empty))
}
.ra_sambo_apply<-function(state,action) {
 d<-state$data;k<-action$type;p<-action$params;s<-.ra_sambo_schemas(d)
 if(!k %in% names(s)) .ra_fail("Action is not permitted in this phase")
 .ra_fields(p,s[[k]],"/params")
 if(k=="start") d$phase<-"fighting" else if(k=="pause") d$phase<-"paused" else if(k=="resume") d$phase<-"fighting" else if(k=="elapse") {
  if(p$milliseconds>d$remaining_ms) .ra_fail("Elapsed time exceeds the bout clock")
  d$remaining_ms<-d$remaining_ms-p$milliseconds
  if(d$remaining_ms==0) d$phase<-"review"
 } else if(k=="assistance") {
  t<-p$player;d$assistance_ms[[t+1L]]<-d$assistance_ms[[t+1L]]+p$milliseconds
  if(d$assistance_ms[[t+1L]]>120000) d<-.ra_sambo_finish(d,1L-t,"assistance_limit")
 } else if(k=="throw") {
  d$techniques<-c(d$techniques,list(list(player=p$player,kind="throw",points=p$points)));d<-.ra_sambo_superiority(d)
 } else if(k=="hold_down") {
  t<-p$player;duration<-p$duration_ms
  if(duration>300000-d$remaining_ms) .ra_fail("Recorded hold duration cannot exceed elapsed bout time")
  holds<-Filter(function(x) x$player==t && x$kind=="hold_down",d$techniques)
  if(sum(vapply(holds,function(x) x$points,numeric(1)))>=4) .ra_fail("The athlete has exhausted scored hold-down entitlement")
  value<-if(duration==20000) 4L else if(duration>=10000) 2L else 0L
  if(value>0) {
   if(value==4) d$techniques<-Filter(function(x) !(x$player==t && x$kind=="hold_down"),d$techniques)
   d$techniques<-c(d$techniques,list(list(player=t,kind="hold_down",points=value)));d<-.ra_sambo_superiority(d)
  }
 } else if(k=="warning") {
  players<-unlist(p$players);kind<-p$kind;info<-.ra_sambo_summary(d)
  if(anyDuplicated(players)) .ra_fail("Warning recipients must be distinct")
  excluded<-players[vapply(players,function(t) info$warnings[t+1L]==3 || (kind=="prohibited" && info$prohibited[t+1L]),logical(1))]
  if(length(excluded)>0) d<-.ra_sambo_finish(d,if(length(excluded)==2) NULL else 1L-excluded[1],if(length(excluded)==2) "double_defeat" else "warnings") else {
   d$warnings<-c(d$warnings,list(list(players=p$players,kind=kind)));d<-.ra_sambo_superiority(d)
  }
 } else if(k=="total_victory") d<-.ra_sambo_finish(d,p$player,p$method) else if(k=="confirm_result") {
  w<-.ra_sambo_decision(d)
  if(is.null(w)) .ra_fail("Official activity warning is required to resolve an unscored indistinguishable bout")
  d<-.ra_sambo_finish(d,w,"points")
 } else if(k %in% c("withdraw","disqualify")) d<-.ra_sambo_finish(d,1L-p$player,if(k=="withdraw") "withdrawal" else "disqualification") else if(k=="double_defeat") d<-.ra_sambo_finish(d,NULL,"double_defeat")
 d
}
.ra_sambo_legal<-function(state) {
 d<-state$data;s<-.ra_sambo_schemas(d);info<-.ra_sambo_summary(d)
 list(mode="event_types",actions=list(),action_types=lapply(names(s),function(k) list(type=k,params_schema=s[[k]],preconditions=list("Bout phase, score-sheet limits and official adjudication must permit the event."),external_observations=list("Official evaluated techniques, completed hold durations and warning decisions; late entries concern actions before the gong."))),constraints=list(phase=d$phase,remaining_ms=d$remaining_ms,scores=as.list(info$scores),technical_points=as.list(info$tech),warnings=as.list(info$warnings),hold_points_remaining=lapply(0:1,function(t) 4L-sum(vapply(d$techniques,function(x) if(x$player==t && x$kind=="hold_down") x$points else 0,numeric(1)))),activity_decision_required=d$remaining_ms<=150000 && is.null(.ra_sambo_decision(d))),completeness=FALSE,reason="official_sport_sambo_score_sheet")
}
.ra_sambo_outcome<-function(state,pack) {
 d<-state$data;done<-d$phase=="finished";w<-d$winner
 list(status=if(done) "terminal" else if(d$phase=="review") "awaiting_adjudication" else "ongoing",result_kind=if(done) {if(d$reason=="double_defeat") "disqualification" else "win"} else "none",winners=if(!is.null(w)) list(state$players[[w+1L]]$id) else list(),losers=if(done) lapply(if(is.null(w)) 0:1 else 1L-w,function(t) state$players[[t+1L]]$id) else list(),placements=list(),reason=if(done) d$reason else if(d$phase=="review") "score_sheet_review_required" else "ongoing",evidence_required=if(d$phase=="review") list("Official final score-sheet and any unresolved activity warning.") else list(),rule_id=if(done) paste0(pack$ruleset_id,"@",pack$ruleset_version) else NULL)
}
