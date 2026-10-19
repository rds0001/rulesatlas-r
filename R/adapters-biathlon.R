.ra_biathlon_validate<-function(d) {
 .ra_cross_country_validate(d)
 if(!identical(unlist(d$route),c("prone_1","standing_1","prone_2","standing_2")) || length(d$cutoffs_us)!=4 || any(vapply(d$cutoffs_us,function(x) !is.null(x),logical(1))) || !is.null(d$time_limit_us)) .ra_fail("The individual event uses four fixed ordered shooting controls",code="INVALID_STATE")
 for(r in d$runners) {
  if(r$warnings!=0) .ra_fail("This format records official penalties or exclusions, not reprimand escalation",code="INVALID_STATE")
  n<-r$checkpoints_completed
  for(j in seq_len(4)) if((length(r$shooting[[j]])>0 && j>n) || (j<n && length(r$shooting[[j]])!=5)) .ra_fail("Shooting bouts must follow the ordered range visits",code="INVALID_STATE")
  if(r$status=="finished" && any(lengths(r$shooting)!=5)) .ra_fail("Finish requires twenty resolved shots",code="INVALID_STATE")
 };invisible(TRUE)
}
.ra_biathlon_apply<-function(state,action) {
 k<-action$type;p<-action$params;d<-state$data;ix<-list(type="integer",minimum=0,maximum=7)
 if(k %in% c("configure_route","written_reprimand","correct_reprimands")) .ra_fail("This event has a fixed range sequence and official penalty decisions")
 if(k %in% c("record_shot","correct_shot")) {
  schema<-list(participant=ix,hit=list(type="boolean"))
  if(k=="correct_shot") {schema$bout<-list(type="integer",minimum=0,maximum=3);schema$shot<-list(type="integer",minimum=0,maximum=4)}
  .ra_fields(p,schema,"/params");i<-p$participant+1L;r<-d$runners[[i]]
  if(k=="record_shot") {
   n<-r$checkpoints_completed
   if(d$phase!="racing" || r$status!="running" || !r$started || n==0 || length(r$shooting[[n]])==5) .ra_fail("Record a shot only during the current incomplete shooting bout")
   r$shooting[[n]]<-c(r$shooting[[n]],list(p$hit))
  } else {
   if(d$phase!="review" || r$status!="finished" || p$shot>=length(r$shooting[[p$bout+1L]])) .ra_fail("Correct a recorded shot for a classified finisher during review")
   r$shooting[[p$bout+1L]][[p$shot+1L]]<-p$hit
  };d$runners[[i]]<-r;return(d)
 }
 if(k %in% c("visit_control","record_finish")) {
  found<-Filter(function(a) a$type==k,.ra_cross_country_legal(state)$action_types)
  if(length(found)==0) .ra_fail("The course event is unavailable now")
  .ra_fields(p,found[[1]]$params_schema,"/params");r<-d$runners[[p$participant+1L]];n<-r$checkpoints_completed
  if(n>0 && length(r$shooting[[n]])!=5) .ra_fail("Resolve all five targets before leaving the shooting bout")
  if(k=="record_finish" && any(lengths(r$shooting)!=5)) .ra_fail("Complete all four shooting bouts before finishing")
 }
 .ra_cross_country_apply(state,action)
}
.ra_biathlon_legal<-function(state) {
 result<-.ra_cross_country_legal(state);d<-state$data;ix<-list(type="integer",minimum=0,maximum=7)
 result$action_types<-Filter(function(a) !(a$type %in% c("configure_route","written_reprimand","correct_reprimands")),result$action_types);specs<-list()
 if(d$phase=="racing") specs$record_shot<-list(participant=ix,hit=list(type="boolean"))
 if(d$phase=="review") specs$correct_shot<-list(participant=ix,hit=list(type="boolean"),bout=list(type="integer",minimum=0,maximum=3),shot=list(type="integer",minimum=0,maximum=4))
 for(k in names(specs)) result$action_types[[length(result$action_types)+1L]]<-list(type=k,params_schema=specs[[k]],preconditions=list("Current shooting bout, shot count and result phase must permit the event."),external_observations=list("Official hit or missed-target ruling."))
 result$constraints$shooting_order<-list("prone","standing","prone","standing");result$constraints$miss_penalty_us<-60000000;result$constraints$shots_recorded<-lapply(d$runners,function(r) as.list(lengths(r$shooting)));result
}
.ra_biathlon_outcome<-function(state,pack) {
 adjusted<-state
 for(i in seq_along(adjusted$data$runners)) {r<-adjusted$data$runners[[i]];adjusted$data$runners[[i]]$penalty_us<-r$penalty_us+60000000*sum(!as.logical(unlist(r$shooting)))}
 result<-.ra_cross_country_outcome(adjusted,pack)
 for(j in seq_along(result$placements)) {id<-result$placements[[j]]$player_id;i<-which(vapply(state$players,function(p) p$id==id,logical(1)));r<-state$data$runners[[i]];result$placements[[j]]$missed_targets<-sum(!as.logical(unlist(r$shooting)));result$placements[[j]]$other_penalty_us<-r$penalty_us}
 result
}
