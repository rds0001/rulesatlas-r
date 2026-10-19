.ra_sj_result<-list(type="object",properties=list(status=list(type="enum",values=list("finished","eliminated","dns","dq")),completed=list(type="integer",minimum=0,maximum=12),refusals=list(type="integer",minimum=0,maximum=2),obstacle_faults=list(type="integer",minimum=0,maximum=52),elapsed_ms=list(type="integer",minimum=0,maximum=320001),correction_ms=list(type="integer",minimum=0,maximum=6000)),required=list("status","completed","refusals","obstacle_faults","elapsed_ms","correction_ms"))
.ra_sj_current<-function(d) {ix<-which(vapply(d$riders,function(r) r$status %in% c("pending","active"),logical(1)));if(length(ix)) ix[1]-1L else NULL}
.ra_sj_settle<-function(d) {d$phase<-if(!is.null(.ra_sj_current(d))) "ready" else "review";d$bell_remaining_ms<-45000L;d$start_crossed<-FALSE;d}
.ra_sj_effective<-function(r) r$elapsed_ms+r$correction_ms
.ra_sj_expire<-function(d,i) {if(.ra_sj_effective(d$riders[[i]])>160000L) {d$riders[[i]]$status<-"eliminated";d<-.ra_sj_settle(d)};d}
.ra_sj_faults<-function(r) r$obstacle_faults+max(0,(.ra_sj_effective(r)-80000+999) %/% 1000)
.ra_sj_validate<-function(d) {
 active<-which(vapply(d$riders,function(r) r$status=="active",logical(1)))
 if((d$phase %in% c("bell","course","rebuild"))!=(length(active)==1L)) .ra_fail("Course phase and active rider disagree",code="INVALID_STATE")
 if(length(active) && active[1]-1L!=.ra_sj_current(d)) .ra_fail("Active rider is out of start order",code="INVALID_STATE")
 if(length(active)>1L) .ra_fail("Only one rider may be on course",code="INVALID_STATE")
 if(d$phase=="ready" && is.null(.ra_sj_current(d))) .ra_fail("Completed round must enter review",code="INVALID_STATE")
 if(d$phase %in% c("review","finished") && !is.null(.ra_sj_current(d))) .ra_fail("Resolve every entrant first",code="INVALID_STATE")
 if(d$phase=="bell" && (d$start_crossed || d$bell_remaining_ms==0L)) .ra_fail("Bell countdown must precede start",code="INVALID_STATE")
 if(d$phase=="rebuild" && !d$start_crossed) .ra_fail("Obstacle rebuilding requires a started course",code="INVALID_STATE")
 for(r in d$riders) {
  if(r$obstacle_faults %% 4!=0 || r$obstacle_faults<4*min(1,r$refusals) || !r$correction_ms %in% c(0,6000)) .ra_fail("Obstacle faults or time correction are inconsistent",code="INVALID_STATE")
  if(r$correction_ms>0 && r$refusals==0) .ra_fail("Time correction requires a refusal",code="INVALID_STATE")
  if(r$refusals==2L && !r$status %in% c("eliminated","dq")) .ra_fail("Second disobedience eliminates the entrant",code="INVALID_STATE")
  if(r$status=="finished" && (r$completed!=12L || .ra_sj_effective(r)==0 || .ra_sj_effective(r)>160000)) .ra_fail("Finish requires all fences within the time limit",code="INVALID_STATE")
  if(r$status %in% c("pending","dns") && (r$completed>0 || r$elapsed_ms>0 || r$obstacle_faults>0 || r$refusals>0 || r$correction_ms>0)) .ra_fail("Course history exists for an unstarted rider",code="INVALID_STATE")
 }
 invisible(TRUE)
}
.ra_sj_apply<-function(state,action) {
 d<-state$data;p<-action$params;k<-action$type;phase<-d$phase;i0<-.ra_sj_current(d);i<-if(is.null(i0)) NULL else i0+1L
 if(k %in% c("ring_bell","cross_start","resume_after_rebuild","finish","withdraw","confirm_result")) {
  .ra_fields(p,list(),"/params")
  if(k=="ring_bell") {if(phase!="ready") .ra_fail("No entrant can be called now");d$riders[[i]]$status<-"active";d$phase<-"bell"} else if(k=="cross_start") {
   if(!phase %in% c("bell","course") || d$start_crossed) .ra_fail("Start line cannot be crossed now")
   d$start_crossed<-TRUE;d$phase<-"course"
  } else if(k=="resume_after_rebuild") {if(phase!="rebuild") .ra_fail("No obstacle rebuild is pending");d$phase<-"course"} else if(k=="finish") {
   if(phase!="course" || !d$start_crossed || d$riders[[i]]$completed!=12L || .ra_sj_effective(d$riders[[i]])==0) .ra_fail("Finish requires a started and completed timed course")
   d$riders[[i]]$status<-"finished";d<-.ra_sj_settle(d)
  } else if(k=="withdraw") {
   if(!phase %in% c("ready","bell","course","rebuild")) .ra_fail("No active entrant can withdraw")
   d$riders[[i]]$status<-if(phase=="ready") "dns" else "eliminated";d<-.ra_sj_settle(d)
  } else {if(phase!="review") .ra_fail("Review every entrant before confirmation");d$phase<-"finished"}
 } else if(k=="amend_result") {
  .ra_fields(p,list(participant=list(type="integer",minimum=0,maximum=7),result=.ra_sj_result),"/params")
  if(phase!="review") .ra_fail("Result amendments require official review")
  d$riders[[p$participant+1L]]<-p$result
 } else if(k=="elapse") {
  .ra_fields(p,list(milliseconds=list(type="integer",minimum=1,maximum=160001)),"/params");ms<-p$milliseconds
  if(phase=="bell") {
   if(ms>d$bell_remaining_ms) .ra_fail("Advance to the countdown boundary before recording course time")
   d$bell_remaining_ms<-d$bell_remaining_ms-ms;if(d$bell_remaining_ms==0L) d$phase<-"course"
  } else if(phase=="course") {d$riders[[i]]$elapsed_ms<-d$riders[[i]]$elapsed_ms+ms;d<-.ra_sj_expire(d,i)} else .ra_fail("Course timing is not running")
 } else if(k %in% c("jump_fence","refusal")) {
  schema<-list(fence=list(type="integer",minimum=0,maximum=11));schema[[if(k=="jump_fence") "knocked" else "destroyed"]]<-list(type="boolean");.ra_fields(p,schema,"/params")
  if(phase!="course" || !d$start_crossed || p$fence!=d$riders[[i]]$completed) .ra_fail("The next prescribed fence must be attempted after crossing the start")
  if(k=="jump_fence") {d$riders[[i]]$completed<-d$riders[[i]]$completed+1L;d$riders[[i]]$obstacle_faults<-d$riders[[i]]$obstacle_faults+4L*as.integer(p$knocked)} else {
   d$riders[[i]]$refusals<-d$riders[[i]]$refusals+1L
   if(d$riders[[i]]$refusals==2L) {d$riders[[i]]$status<-"eliminated";d<-.ra_sj_settle(d)} else {
    d$riders[[i]]$obstacle_faults<-d$riders[[i]]$obstacle_faults+4L
    if(p$destroyed) {d$riders[[i]]$correction_ms<-d$riders[[i]]$correction_ms+6000L;d<-.ra_sj_expire(d,i);if(d$riders[[i]]$status=="active") d$phase<-"rebuild"}
   }
  }
 } else if(k %in% c("eliminate","disqualify")) {
  schema<-list(reason=list(type="enum",values=list("wrong_course","fall","unauthorized_assistance","equipment","other_official")));if(k=="disqualify") schema$participant<-list(type="integer",minimum=0,maximum=7)
  .ra_fields(p,schema,"/params")
  if(k=="eliminate") {
   if(!phase %in% c("bell","course","rebuild")) .ra_fail("No active entrant can be eliminated")
   d$riders[[i]]$status<-"eliminated";d<-.ra_sj_settle(d)
  } else {
   j<-p$participant+1L;if(d$riders[[j]]$status=="dq") .ra_fail("Entrant is already disqualified")
   d$riders[[j]]$status<-"dq";if(!is.null(i) && j==i) d<-.ra_sj_settle(d)
  }
 } else .ra_fail("Unknown show-jumping action")
 d
}
.ra_sj_legal<-function(state) {
 d<-state$data;phase<-d$phase;specs<-list();i<-.ra_sj_current(d);empty<-setNames(list(),character())
 if(phase=="ready") specs$ring_bell<-empty
 if(phase %in% c("bell","course")) {specs$elapse<-list(milliseconds=list(type="integer",minimum=1,maximum=160001));if(!d$start_crossed) specs$cross_start<-empty}
 if(phase=="course" && d$start_crossed) {
  if(d$riders[[i+1L]]$completed<12L) {specs$jump_fence<-list(fence=list(type="integer",minimum=0,maximum=11),knocked=list(type="boolean"));specs$refusal<-list(fence=list(type="integer",minimum=0,maximum=11),destroyed=list(type="boolean"))} else specs$finish<-empty
 }
 if(phase=="rebuild") specs$resume_after_rebuild<-empty
 if(phase %in% c("ready","bell","course","rebuild")) specs$withdraw<-empty
 reason<-list(type="enum",values=list("wrong_course","fall","unauthorized_assistance","equipment","other_official"))
 if(phase %in% c("bell","course","rebuild")) specs$eliminate<-list(reason=reason)
 if(phase=="review") {specs$confirm_result<-empty;specs$amend_result<-list(participant=list(type="integer",minimum=0,maximum=7),result=.ra_sj_result)}
 if(phase!="finished") specs$disqualify<-list(participant=list(type="integer",minimum=0,maximum=7),reason=reason)
 list(mode="event_types",actions=list(),action_types=lapply(names(specs),function(k) list(type=k,params_schema=specs[[k]],preconditions=list("Rider phase, start line, next fence and time limits must permit the event."),external_observations=list("Official fence judgment, valid start/finish crossing, running elapsed time, obstacle readiness or exclusion."))),constraints=list(phase=phase,current_participant=i,bell_remaining_ms=d$bell_remaining_ms,allowed_time_ms=80000L,time_limit_ms=160000L),completeness=FALSE,reason="official_show_jumping_events")
}
.ra_sj_outcome<-function(state,pack) {
 d<-state$data;done<-d$phase=="finished";eligible<-if(done) which(vapply(d$riders,function(r) r$status=="finished",logical(1))) else integer();faults<-vapply(d$riders[eligible],.ra_sj_faults,numeric(1));times<-vapply(d$riders[eligible],.ra_sj_effective,numeric(1));o<-order(faults,times,eligible);eligible<-eligible[o];faults<-faults[o];times<-times[o]
 ws<-if(length(eligible)) eligible[faults==faults[1] & times==times[1]] else integer();winners<-lapply(ws,function(i) state$players[[i]]$id)
 list(status=if(done) "terminal" else if(d$phase=="review") "awaiting_adjudication" else "ongoing",result_kind=if(done) if(length(eligible)) "ranking" else "no_contest" else "none",winners=winners,losers=if(length(winners)) lapply(setdiff(1:8,ws),function(i) state$players[[i]]$id) else list(),placements=lapply(seq_along(eligible),function(j) list(player_id=state$players[[eligible[j]]]$id,place=1L+sum(faults<faults[j] | (faults==faults[j] & times<times[j])),faults=faults[j],time_ms=times[j])),reason=if(done) "round_confirmed" else if(d$phase=="review") "result_review" else "ongoing",evidence_required=if(d$phase=="review") list("Official round confirmation.") else list(),rule_id=if(done) paste0(pack$ruleset_id,"@",pack$ruleset_version) else NULL)
}
