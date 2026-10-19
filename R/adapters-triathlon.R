.ra_triathlon_route<-function() as.list(c("swim_exit","transition_one_exit","bike_exit","transition_two_exit","run_finish"))
.ra_triathlon_config<-function() list(quantum_us=1000L)
.ra_triathlon_limits<-function() {t<-list(type="integer",minimum=1,maximum=1000000000000,nullable=TRUE);list(time_limit_us=t,cutoffs_us=list(type="array",minItems=5,maxItems=5,items=t))}
.ra_triathlon_validate<-function(d) {
 .ra_course_validate(d,.ra_triathlon_config())
 if(!identical(unlist(d$route),unlist(.ra_triathlon_route()))) .ra_fail("Triathlon requires swimming, first transition, cycling, second transition and running in order",code="INVALID_STATE")
 invisible(TRUE)
}
.ra_triathlon_apply<-function(state,action) {
 if(action$type=="configure_limits") {
  .ra_fields(action$params,.ra_triathlon_limits(),"/params");d<-state$data
  if(d$phase!="ready" || d$route_locked) .ra_fail("Set course limits before opening the event")
  d["time_limit_us"]<-list(action$params$time_limit_us);d$cutoffs_us<-action$params$cutoffs_us;return(d)
 }
 if(action$type=="configure_route") .ra_fail("The discipline sequence is fixed for this triathlon")
 .ra_course_apply(state,action,.ra_triathlon_config())
}
.ra_triathlon_legal<-function(state) {
 result<-.ra_course_legal(state,.ra_triathlon_config());d<-state$data
 result$action_types<-Filter(function(a) a$type!="configure_route",result$action_types)
 if(d$phase=="ready" && !d$route_locked) result$action_types[[length(result$action_types)+1L]]<-list(type="configure_limits",params_schema=.ra_triathlon_limits(),preconditions=list("Course limits are set before the event opens."),external_observations=list("Declared elapsed cutoffs and overall time limit."))
 segments<-c("swimming","transition_one","cycling","transition_two","running","awaiting_finish")
 result$constraints$segments<-lapply(d$runners,function(r) if(r$status=="running" && r$started) segments[[r$checkpoints_completed+1L]] else if(r$status!="running") r$status else "awaiting_start")
 result
}
.ra_triathlon_outcome<-function(state,pack) .ra_course_outcome(state,pack,.ra_triathlon_config())
