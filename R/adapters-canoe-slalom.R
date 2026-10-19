.ra_sl_config <- list(recall=FALSE,conduct_warnings=FALSE,time_quantum_us=10000,false_start_disqualifies=FALSE,controls=as.list(paste0("gate_",1:20)))
.ra_sl_penalty <- function(g) if(!g$passed_correctly) 5e7 else if(g$touched) 2e6 else 0
.ra_sl_validate <- function(d) {
 .ra_race_validate(d,config=.ra_sl_config)
 if(length(d$upstream)>0 && (length(d$upstream)!=20 || sum(unlist(d$upstream))!=6)) .ra_fail("The declared twenty-gate course requires exactly six upstream gates",code="INVALID_STATE")
 if((d$phase=="racing" || any(vapply(d$runners,function(r) length(r$gates)>0 || r$status=="finished",logical(1)))) && !length(d$upstream)) .ra_fail("Configure the course before starting the final",code="INVALID_STATE")
 for(r in d$runners) if(length(r$gates)!=r$checkpoints_completed) .ra_fail("Gate judgments must match the completed checkpoint prefix",code="INVALID_STATE")
 invisible(TRUE)
}
.ra_sl_apply <- function(state,action) {
 d<-state$data;p<-action$params;k<-action$type
 if(k=="configure_course") {
  .ra_fields(p,list(upstream=list(type="array",minItems=20,maxItems=20,items=list(type="boolean"))),"/params")
  if(d$phase!="ready" || sum(unlist(p$upstream))!=6) .ra_fail("Configure exactly six upstream gates before starting")
  d$upstream<-p$upstream;return(d)
 }
 if(k=="start" && !length(d$upstream)) .ra_fail("Configure the course before starting")
 if(k=="visit_control") .ra_fail("Record a gate judgment instead of an unclassified checkpoint visit")
 if(k %in% c("record_gate","correct_gate")) {
  schema<-list(participant=list(type="integer",minimum=0,maximum=7),gate=list(type="integer",minimum=0,maximum=19),passed_correctly=list(type="boolean"),touched=list(type="boolean"))
  if(k=="record_gate") schema$time_us<-list(type="integer",minimum=1,maximum=1e12,nullable=FALSE)
  .ra_fields(p,schema,"/params");r<-d$runners[[p$participant+1L]];g<-p$gate;verdict<-p[c("passed_correctly","touched")]
  if(k=="record_gate") {
   if(d$phase!="racing" || r$status!="running" || g!=length(r$gates)) .ra_fail("Judge each gate exactly once in course order for an active paddler")
   d<-.ra_race_apply(state,list(type="visit_control",params=list(participant=p$participant,control=paste0("gate_",g+1L),time_us=p$time_us)),config=.ra_sl_config);d$runners[[p$participant+1L]]$gates<-c(d$runners[[p$participant+1L]]$gates,list(verdict))
  } else {
   if(d$phase!="review" || g>=length(r$gates)) .ra_fail("Correct an existing gate judgment only during official review")
   d$runners[[p$participant+1L]]$gates[[g+1L]]<-verdict
  }
  return(d)
 }
 .ra_race_apply(state,action,config=.ra_sl_config)
}
.ra_sl_legal <- function(state) {
 d<-state$data;result<-.ra_race_legal(state,config=.ra_sl_config)
 result$action_types<-Filter(function(a) a$type!="visit_control" && !(a$type=="start" && !length(d$upstream)),result$action_types);specs<-list()
 if(d$phase=="ready") specs$configure_course<-list(upstream=list(type="array",minItems=20,maxItems=20,items=list(type="boolean")))
 schema<-list(participant=list(type="integer",minimum=0,maximum=7),gate=list(type="integer",minimum=0,maximum=19),passed_correctly=list(type="boolean"),touched=list(type="boolean"))
 if(d$phase=="racing") specs$record_gate<-c(schema,list(time_us=list(type="integer",minimum=1,maximum=1e12,nullable=FALSE)))
 if(d$phase=="review") specs$correct_gate<-schema
 for(k in names(specs)) result$action_types<-c(result$action_types,list(list(type=k,params_schema=specs[[k]],preconditions=list("Course configuration, gate order and review phase must permit the decision."),external_observations=list("Official gate passage in the prescribed direction and pole contact; multiple contacts still give only two seconds."))))
 result$constraints$upstream<-d$upstream;result
}
.ra_sl_outcome <- function(state,pack) {
 adjusted<-state
 for(i in seq_along(adjusted$data$runners)) if(adjusted$data$runners[[i]]$status=="finished") adjusted$data$runners[[i]]$time_us<-adjusted$data$runners[[i]]$time_us+sum(vapply(adjusted$data$runners[[i]]$gates,.ra_sl_penalty,numeric(1)))
 result<-.ra_race_outcome(adjusted,pack,config=.ra_sl_config)
 for(j in seq_along(result$placements)) {id<-result$placements[[j]]$player_id;i<-which(vapply(state$players,function(p) p$id==id,logical(1)));r<-state$data$runners[[i]];result$placements[[j]]$elapsed_us<-r$time_us;result$placements[[j]]$penalty_us<-sum(vapply(r$gates,.ra_sl_penalty,numeric(1)))}
 result
}
