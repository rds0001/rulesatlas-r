.ra_ag_course<-list("jump","jump","tunnel","jump","dogwalk","jump","weave","tunnel","jump","aframe","jump","jump","tunnel","seesaw","jump","tunnel","jump","jump","tunnel","jump")
.ra_ag_props<-list(status=list(type="enum",values=list("finished","eliminated","dns","dq")),completed=list(type="integer",minimum=0,maximum=20),refusals=list(type="integer",minimum=0,maximum=3),faults=list(type="integer",minimum=0,maximum=310),elapsed_ms=list(type="integer",minimum=0,maximum=240010))
.ra_ag_result<-list(type="object",properties=.ra_ag_props,required=as.list(names(.ra_ag_props)))
.ra_ag_current<-function(d) {ix<-which(vapply(d$dogs,function(r) r$status %in% c("pending","active"),logical(1)));if(length(ix)) ix[1]-1L else NULL}
.ra_ag_settle<-function(d) {d$phase<-if(!is.null(.ra_ag_current(d))) "ready" else "review";d}
.ra_ag_score<-function(r) r$faults*100+max(0,(r$elapsed_ms-60000) %/% 10)
.ra_ag_validate<-function(d) {
 active<-which(vapply(d$dogs,function(r) r$status=="active",logical(1)))
 if((d$phase=="course")!=(length(active)==1L) || length(active)>1L) .ra_fail("Course phase requires exactly one active dog",code="INVALID_STATE")
 if(length(active) && active[1]-1L!=.ra_ag_current(d)) .ra_fail("Active dog is out of start order",code="INVALID_STATE")
 if(d$phase=="ready" && is.null(.ra_ag_current(d))) .ra_fail("Completed event must enter review",code="INVALID_STATE")
 if(d$phase %in% c("review","finished") && !is.null(.ra_ag_current(d))) .ra_fail("Resolve all entrants first",code="INVALID_STATE")
 for(r in d$dogs) {
  if(r$faults %% 5!=0 || r$faults<5*min(2,r$refusals) || r$elapsed_ms %% 10!=0) .ra_fail("Fault units or timing precision are invalid",code="INVALID_STATE")
  if(r$refusals==3L && !r$status %in% c("eliminated","dq")) .ra_fail("Third refusal eliminates the entrant",code="INVALID_STATE")
  if(r$status=="finished" && (r$completed!=20L || r$elapsed_ms==0 || r$elapsed_ms>120000)) .ra_fail("Finish requires every obstacle and a valid course time",code="INVALID_STATE")
  if(r$status=="active" && r$elapsed_ms>120000) .ra_fail("Maximum course time has been exceeded",code="INVALID_STATE")
  if(r$status %in% c("pending","dns") && (r$completed>0 || r$refusals>0 || r$faults>0 || r$elapsed_ms>0)) .ra_fail("Unstarted entrant has course history",code="INVALID_STATE")
 }
 invisible(TRUE)
}
.ra_ag_apply<-function(state,action) {
 d<-state$data;p<-action$params;k<-action$type;phase<-d$phase;i0<-.ra_ag_current(d);i<-if(is.null(i0)) NULL else i0+1L;index<-list(type="integer",minimum=0,maximum=7)
 if(k %in% c("start","finish","withdraw","confirm_result")) {
  .ra_fields(p,list(),"/params")
  if(k=="start") {if(phase!="ready") .ra_fail("Next entrant cannot start now");d$dogs[[i]]$status<-"active";d$phase<-"course"} else if(k=="finish") {
   if(phase!="course" || d$dogs[[i]]$completed!=20L || d$dogs[[i]]$elapsed_ms==0) .ra_fail("Finish requires every obstacle and recorded running time")
   d$dogs[[i]]$status<-"finished";d<-.ra_ag_settle(d)
  } else if(k=="withdraw") {
   if(!phase %in% c("ready","course")) .ra_fail("No entrant can withdraw now")
   d$dogs[[i]]$status<-if(phase=="ready") "dns" else "eliminated";d<-.ra_ag_settle(d)
  } else {if(phase!="review") .ra_fail("Resolve and review every entrant first");d$phase<-"finished"}
 } else if(k=="elapse") {
  .ra_fields(p,list(milliseconds=list(type="integer",minimum=10,maximum=120010)),"/params")
  if(phase!="course" || p$milliseconds %% 10!=0) .ra_fail("Running course time must use hundredth-second increments")
  d$dogs[[i]]$elapsed_ms<-d$dogs[[i]]$elapsed_ms+p$milliseconds
  if(d$dogs[[i]]$elapsed_ms>120000) {d$dogs[[i]]$status<-"eliminated";d<-.ra_ag_settle(d)}
 } else if(k %in% c("complete_obstacle","refusal")) {
  schema<-list(obstacle=list(type="integer",minimum=0,maximum=19));if(k=="complete_obstacle") schema$execution_faults<-list(type="integer",minimum=0,maximum=3)
  .ra_fields(p,schema,"/params")
  if(phase!="course" || p$obstacle!=d$dogs[[i]]$completed) .ra_fail("Attempt the next prescribed obstacle in course order")
  if(k=="refusal") {
   d$dogs[[i]]$refusals<-d$dogs[[i]]$refusals+1L
   if(d$dogs[[i]]$refusals==3L) {d$dogs[[i]]$status<-"eliminated";d<-.ra_ag_settle(d)} else d$dogs[[i]]$faults<-d$dogs[[i]]$faults+5L
  } else {
   limit<-switch(.ra_ag_course[[p$obstacle+1L]],tunnel=0L,jump=1L,weave=1L,3L)
   if(p$execution_faults>limit) .ra_fail("Execution-fault count is not supported for this obstacle type")
   d$dogs[[i]]$faults<-d$dogs[[i]]$faults+5L*p$execution_faults;d$dogs[[i]]$completed<-d$dogs[[i]]$completed+1L
  }
 } else if(k=="amend_result") {
  .ra_fields(p,list(participant=index,result=.ra_ag_result),"/params");if(phase!="review") .ra_fail("Official result review is required");d$dogs[[p$participant+1L]]<-p$result
 } else if(k %in% c("eliminate","disqualify")) {
  schema<-list(reason=list(type="enum",values=list("wrong_course","unsafe_behavior","handler_assistance","equipment","other_official")));if(k=="disqualify") schema$participant<-index
  .ra_fields(p,schema,"/params")
  if(k=="eliminate") {if(phase!="course") .ra_fail("No active course can be eliminated");d$dogs[[i]]$status<-"eliminated";d<-.ra_ag_settle(d)} else {
   j<-p$participant+1L;if(d$dogs[[j]]$status=="dq") .ra_fail("Entrant is already disqualified")
   d$dogs[[j]]$status<-"dq";if(!is.null(i) && j==i) d<-.ra_ag_settle(d)
  }
 } else .ra_fail("Unknown agility action")
 d
}
.ra_ag_legal<-function(state) {
 d<-state$data;phase<-d$phase;i<-.ra_ag_current(d);specs<-list();empty<-setNames(list(),character());index<-list(type="integer",minimum=0,maximum=7)
 if(phase=="ready") specs$start<-empty
 if(phase %in% c("ready","course")) specs$withdraw<-empty
 if(phase=="course") {
  specs$elapse<-list(milliseconds=list(type="integer",minimum=10,maximum=120010))
  if(d$dogs[[i+1L]]$completed<20L) {specs$complete_obstacle<-list(obstacle=list(type="integer",minimum=0,maximum=19),execution_faults=list(type="integer",minimum=0,maximum=3));specs$refusal<-list(obstacle=list(type="integer",minimum=0,maximum=19))} else specs$finish<-empty
 }
 reason<-list(type="enum",values=list("wrong_course","unsafe_behavior","handler_assistance","equipment","other_official"))
 if(phase=="course") specs$eliminate<-list(reason=reason)
 if(phase!="finished") specs$disqualify<-list(participant=index,reason=reason)
 if(phase=="review") {specs$amend_result<-list(participant=index,result=.ra_ag_result);specs$confirm_result<-empty}
 list(mode="event_types",actions=list(),action_types=lapply(names(specs),function(k) list(type=k,params_schema=specs[[k]],preconditions=list("Current entrant, obstacle order, fault kind and timing must permit the event."),external_observations=list("Official obstacle completion, refusal, execution fault, elapsed running time or exclusion."))),constraints=list(phase=phase,current_participant=i,course=.ra_ag_course,standard_time_ms=60000L,maximum_time_ms=120000L),completeness=FALSE,reason="official_agility_events")
}
.ra_ag_outcome<-function(state,pack) {
 d<-state$data;done<-d$phase=="finished";eligible<-if(done) which(vapply(d$dogs,function(r) r$status=="finished",logical(1))) else integer();keys<-lapply(d$dogs[eligible],function(r) c(.ra_ag_score(r),r$faults,r$elapsed_ms))
 places<-vapply(keys,function(k) 1L+sum(vapply(keys,function(other) .ra_ps_compare(other,k)<0,logical(1))),integer(1));o<-order(places,eligible);eligible<-eligible[o];keys<-keys[o];places<-places[o];winners<-lapply(eligible[places==1L],function(i) state$players[[i]]$id)
 list(status=if(done) "terminal" else if(d$phase=="review") "awaiting_adjudication" else "ongoing",result_kind=if(done) if(length(eligible)) "ranking" else "no_contest" else "none",winners=winners,losers=if(length(winners)) lapply(Filter(function(i) !state$players[[i]]$id %in% unlist(winners),1:8),function(i) state$players[[i]]$id) else list(),placements=lapply(seq_along(eligible),function(j) list(player_id=state$players[[eligible[j]]]$id,place=places[j],fault_hundredths=keys[[j]][1],course_faults=keys[[j]][2],time_ms=keys[[j]][3])),reason=if(done) "course_confirmed" else if(d$phase=="review") "result_review" else "ongoing",evidence_required=if(d$phase=="review") list("Official course confirmation.") else list(),rule_id=if(done) paste0(pack$ruleset_id,"@",pack$ruleset_version) else NULL)
}
