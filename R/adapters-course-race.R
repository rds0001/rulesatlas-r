.ra_course_internal<-function(state) {for(i in seq_along(state$data$runners)) state$data$runners[[i]]$warnings<-0L;state}
.ra_course_validate<-function(d,config) {
 .ra_tr_validate(.ra_course_internal(list(data=d))$data)
 for(r in d$runners) {
  if(!r$started && (r$checkpoints_completed>0 || !is.null(r$time_us) || r$penalty_us>0 || r$status %in% c("finished","dnf","mp","ot"))) .ra_fail("An unstarted entrant cannot have course observations",code="INVALID_STATE")
  if(r$started && r$status %in% c("ready","dns")) .ra_fail("A started entrant cannot be ready or DNS",code="INVALID_STATE")
  for(k in c("time_us","penalty_us")) if(!is.null(r[[k]]) && r[[k]]%%config$quantum_us!=0) .ra_fail("Classification time precision mismatch",code="INVALID_STATE")
 };invisible(TRUE)
}
.ra_course_apply<-function(state,action,config) {
 d<-state$data;p<-action$params;k<-action$type;found<-Filter(function(a) a$type==k,.ra_course_legal(state,config)$action_types)
 if(length(found)==0) .ra_fail("Event is unavailable in this phase")
 .ra_fields(p,found[[1]]$params_schema,"/params")
 if(k=="start_participant") {
  i<-p$participant+1L;r<-d$runners[[i]]
  if(d$phase!="racing" || r$status!="running" || r$started) .ra_fail("Start an eligible entrant once after opening the event")
  d$runners[[i]]$started<-TRUE;return(d)
 }
 if(k %in% c("visit_control","record_finish","assess_penalty","correct_penalty") && !d$runners[[p$participant+1L]]$started) .ra_fail("This observation requires an individual start")
 if(k %in% c("record_finish","assess_penalty","correct_penalty","amend_result")) for(key in c("time_us","penalty_us")) if(!is.null(p[[key]]) && p[[key]]%%config$quantum_us!=0) .ra_fail("Classification time precision mismatch")
 a<-action;if(k=="amend_result") a$params$warnings<-0L
 result<-.ra_tr_apply(.ra_course_internal(state),a)
 for(i in seq_along(result$runners)) result$runners[[i]]$warnings<-NULL
 if(k=="withdraw" && !d$runners[[p$participant+1L]]$started) result$runners[[p$participant+1L]]$status<-"dns"
 result
}
.ra_course_legal<-function(state,config) {
 result<-.ra_tr_legal(.ra_course_internal(state));d<-state$data
 for(i in seq_along(result$action_types)) if(result$action_types[[i]]$type=="amend_result") result$action_types[[i]]$params_schema$warnings<-NULL
 if(d$phase=="racing") result$action_types[[length(result$action_types)+1L]]<-list(type="start_participant",params_schema=list(participant=list(type="integer",minimum=0,maximum=7)),preconditions=list("The entrant is eligible and has not started."),external_observations=list("Official individual start."))
 result$constraints$time_quantum_us<-config$quantum_us;result$constraints$started<-lapply(d$runners,function(r) r$started);result
}
.ra_course_outcome<-function(state,pack,config) .ra_tr_outcome(.ra_course_internal(state),pack)
