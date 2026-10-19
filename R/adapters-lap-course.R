.ra_lap_course_route<-function(d) {if(d$lap_count==0) return(list());unlist(lapply(seq_len(d$lap_count),function(lap) lapply(d$control_names,function(gate) paste0("lap_",lap,"/",gate))),recursive=FALSE)}
.ra_lap_course_schema<-function(config) list(controls=list(type="array",minItems=1,maxItems=12,items=list(type="string",minLength=1,maxLength=40)),laps=list(type="integer",minimum=1,maximum=config$maximum_laps),time_limit_us=list(type="integer",minimum=1,maximum=1000000000000,nullable=TRUE))
.ra_lap_course_validate<-function(d,config) {
 .ra_course_validate(d,config)
 if(d$lap_count>config$maximum_laps || anyDuplicated(unlist(d$control_names)) || any(vapply(d$control_names,function(x) grepl("/",x,fixed=TRUE),logical(1)))) .ra_fail("Invalid lap-course configuration",code="INVALID_STATE")
 if((d$lap_count>0)!=(length(d$control_names)>0) || !identical(unlist(d$route),unlist(.ra_lap_course_route(d)))) .ra_fail("Every configured lap requires the same ordered controls",code="INVALID_STATE")
 if(any(vapply(d$cutoffs_us,function(x) !is.null(x),logical(1)))) .ra_fail("This time-trial format uses an overall cutoff only",code="INVALID_STATE")
 invisible(TRUE)
}
.ra_lap_course_apply<-function(state,action,config) {
 k<-action$type;p<-action$params
 if(k=="configure_track") {
  .ra_fields(p,.ra_lap_course_schema(config),"/params");d<-state$data
  if(d$phase!="ready" || d$route_locked) .ra_fail("Configure the complete track before opening the event")
  d$control_names<-p$controls;d$lap_count<-p$laps;d$route<-.ra_lap_course_route(d);d$cutoffs_us<-lapply(d$route,function(x) NULL);d["time_limit_us"]<-list(p$time_limit_us);return(d)
 }
 if(k=="configure_route") .ra_fail("Configure the lap track through configure_track")
 .ra_course_apply(state,action,config)
}
.ra_lap_course_legal<-function(state,config) {
 result<-.ra_course_legal(state,config);d<-state$data;result$action_types<-Filter(function(a) a$type!="configure_route",result$action_types)
 if(d$phase=="ready" && !d$route_locked) result$action_types[[length(result$action_types)+1L]]<-list(type="configure_track",params_schema=.ra_lap_course_schema(config),preconditions=list("The event has not opened and the course is not locked."),external_observations=list("Complete ordered control list, lap count and optional overall elapsed limit."))
 n<-length(d$control_names);result$constraints$lap_count<-d$lap_count;result$constraints$next_control<-lapply(d$runners,function(r) if(r$status=="running" && r$started && r$checkpoints_completed<length(d$route)) d$route[[r$checkpoints_completed+1L]] else NULL);result$constraints$completed_laps<-lapply(d$runners,function(r) if(n>0) r$checkpoints_completed%/%n else 0L);result
}
.ra_lap_course_outcome<-function(state,pack,config) .ra_course_outcome(state,pack,config)
