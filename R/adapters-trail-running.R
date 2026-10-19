.ra_tr_config <- list(recall=FALSE,conduct_warnings=FALSE,time_quantum_us=1,route_configurable=TRUE,false_start_disqualifies=FALSE)
.ra_tr_validate <- function(d) {
 .ra_race_validate(d,config=.ra_tr_config)
 if(length(d$cutoffs_us)!=length(d$route)) .ra_fail("Each checkpoint requires an explicit cutoff or null",code="INVALID_STATE")
 for(r in d$runners) {
  if(r$status %in% c("ready","dns") && r$penalty_us>0) .ra_fail("Time penalties require a started runner",code="INVALID_STATE")
  n<-r$checkpoints_completed
  if(n>0 && !is.null(d$cutoffs_us[[n]]) && r$last_checkpoint_time>d$cutoffs_us[[n]] && r$status %in% c("running","finished")) .ra_fail("A missed checkpoint cutoff excludes a runner from classification",code="INVALID_STATE")
 }
 invisible(TRUE)
}
.ra_tr_apply <- function(state,action) {
 p<-action$params;k<-action$type;d<-state$data;tm<-list(type="integer",minimum=1,maximum=1e12,nullable=TRUE);ix<-list(type="integer",minimum=0,maximum=7)
 if(k=="configure_route") {
  .ra_fields(p,list(controls=list(type="array",items=list(type="string",minLength=1,maxLength=64),minItems=1,maxItems=128),time_limit_us=tm,cutoffs_us=list(type="array",minItems=1,maxItems=128,items=tm)),"/params")
  if(length(p$cutoffs_us)!=length(p$controls)) .ra_fail("Supply one nullable cutoff per checkpoint")
  a<-action;a$params<-p[c("controls","time_limit_us")];d<-.ra_race_apply(state,a,config=.ra_tr_config);d$cutoffs_us<-p$cutoffs_us;return(d)
 }
 if(k=="assess_penalty") {
  .ra_fields(p,list(participant=ix,time_us=list(type="integer",minimum=1,maximum=1e12)),"/params");r<-d$runners[[p$participant+1L]]
  if(!(d$phase %in% c("racing","review")) || !(r$status %in% c("running","finished"))) .ra_fail("Assess a time penalty only against an active or provisionally finished runner")
  d$runners[[p$participant+1L]]$penalty_us<-r$penalty_us+p$time_us;return(d)
 }
 if(k=="correct_penalty") {
  .ra_fields(p,list(participant=ix,penalty_us=list(type="integer",minimum=0,maximum=1e12)),"/params")
  if(d$phase!="review" || d$runners[[p$participant+1L]]$status %in% c("dns","ready")) .ra_fail("Penalty correction requires official result review and a started runner")
  d$runners[[p$participant+1L]]$penalty_us<-p$penalty_us;return(d)
 }
 d<-.ra_race_apply(state,action,config=.ra_tr_config)
 if(k=="visit_control") {
  r<-d$runners[[p$participant+1L]];limit<-d$cutoffs_us[[r$checkpoints_completed]]
  if(!is.null(limit) && p$time_us>limit) {d$runners[[p$participant+1L]]$status<-"ot";if(!any(vapply(d$runners,function(r) r$status %in% c("ready","running"),logical(1)))) d$phase<-"review"}
 }
 d
}
.ra_tr_legal <- function(state) {
 result<-.ra_race_legal(state,config=.ra_tr_config);d<-state$data;tm<-list(type="integer",minimum=1,maximum=1e12,nullable=TRUE);ix<-list(type="integer",minimum=0,maximum=7)
 for(i in seq_along(result$action_types)) if(result$action_types[[i]]$type=="configure_route") result$action_types[[i]]$params_schema$cutoffs_us<-list(type="array",minItems=1,maxItems=128,items=tm)
 schemas<-list()
 if(d$phase %in% c("racing","review")) schemas$assess_penalty<-list(participant=ix,time_us=list(type="integer",minimum=1,maximum=1e12))
 if(d$phase=="review") schemas$correct_penalty<-list(participant=ix,penalty_us=list(type="integer",minimum=0,maximum=1e12))
 for(k in names(schemas)) result$action_types<-c(result$action_types,list(list(type=k,params_schema=schemas[[k]],preconditions=list("Started runner and result phase must permit the penalty decision."),external_observations=list("Official event-specific time penalty or review correction."))))
 result$constraints$cutoffs_us<-d$cutoffs_us;result
}
.ra_tr_outcome <- function(state,pack) {
 adjusted<-state
 for(i in seq_along(adjusted$data$runners)) if(adjusted$data$runners[[i]]$status=="finished") adjusted$data$runners[[i]]$time_us<-adjusted$data$runners[[i]]$time_us+adjusted$data$runners[[i]]$penalty_us
 result<-.ra_race_outcome(adjusted,pack,config=.ra_tr_config)
 for(j in seq_along(result$placements)) {id<-result$placements[[j]]$player_id;i<-which(vapply(state$players,function(p) p$id==id,logical(1)));r<-state$data$runners[[i]];result$placements[[j]]$elapsed_us<-r$time_us;result$placements[[j]]$penalty_us<-r$penalty_us}
 result
}
