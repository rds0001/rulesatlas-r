.ra_nordic_offsets<-function(j) {
 scores<-lapply(j$entrants,function(r) if(r$status=="scored") .ra_ski_jump_components(r$marks) else NULL);best<-max(c(0,unlist(scores)))
 lapply(scores,function(s) if(is.null(s)) NULL else (best-s)*400000)
}
.ra_nordic_start_ski<-function(j) {
 gaps<-.ra_nordic_offsets(j)
 list(phase=if(any(vapply(gaps,function(g) !is.null(g),logical(1)))) "ready" else "review",runners=lapply(gaps,function(g) list(status=if(is.null(g)) "dns" else "ready",time_us=NULL,warnings=0L,checkpoints_completed=0L,last_checkpoint_time=NULL,penalty_us=0,started=FALSE)),route=list(),route_locked=FALSE,time_limit_us=NULL,cutoffs_us=list())
}
.ra_nordic_validate<-function(d) {
 .ra_ski_jump_validate(d$jump)
 if(d$stage=="jumping") {
  if(d$jump$phase=="finished" || !is.null(d$ski)) .ra_fail("Ski race starts only after the jump result is confirmed",code="INVALID_STATE")
 } else {
  if(d$jump$phase!="finished" || is.null(d$ski)) .ra_fail("Confirmed jumping result is required",code="INVALID_STATE")
  .ra_cross_country_validate(d$ski);gaps<-.ra_nordic_offsets(d$jump)
  for(i in seq_len(8)) {r<-d$ski$runners[[i]]
   if(is.null(gaps[[i]]) && (r$status!="dns" || r$started)) .ra_fail("Only classified jumpers may enter the ski race",code="INVALID_STATE")
   if(r$warnings!=0) .ra_fail("Use official penalties or exclusion decisions",code="INVALID_STATE")
  }
  if((d$stage=="finished")!=(d$ski$phase=="finished")) .ra_fail("Combined phase and ski confirmation disagree",code="INVALID_STATE")
 };invisible(TRUE)
}
.ra_nordic_apply<-function(state,action) {
 d<-state$data;k<-action$type;inner<-state;a<-action
 if(d$stage=="jumping") {
  if(!startsWith(k,"jump_")) .ra_fail("Complete jumping before the ski race")
  inner$data<-d$jump;a$type<-substring(k,6);d$jump<-.ra_ski_jump_apply(inner,a)
  if(d$jump$phase=="finished") {d$stage<-"skiing";d$ski<-.ra_nordic_start_ski(d$jump)}
 } else {
  if(!startsWith(k,"ski_") || substring(k,5) %in% c("written_reprimand","correct_reprimands")) .ra_fail("Use an available ski race event")
  p<-action$params;i<-p$participant
  if(!is.null(i) && is.numeric(i) && length(i)==1 && i==floor(i) && i>=0 && i<8 && is.null(.ra_nordic_offsets(d$jump)[[i+1L]])) .ra_fail("This entrant has no classified jump")
  inner$data<-d$ski;a$type<-substring(k,5);d$ski<-.ra_cross_country_apply(inner,a)
  if(d$ski$phase=="finished") d$stage<-"finished"
 };d
}
.ra_nordic_legal<-function(state) {
 d<-state$data;inner<-state;is_jump<-d$stage=="jumping";inner$data<-if(is_jump) d$jump else d$ski
 result<-if(is_jump) .ra_ski_jump_legal(inner) else .ra_cross_country_legal(inner)
 result$action_types<-Filter(function(a) is_jump || !(a$type %in% c("written_reprimand","correct_reprimands")),result$action_types)
 for(i in seq_along(result$action_types)) result$action_types[[i]]$type<-paste0(if(is_jump) "jump_" else "ski_",result$action_types[[i]]$type)
 result$constraints$stage<-d$stage;result$constraints$start_deficits_us<-if(is_jump) list() else .ra_nordic_offsets(d$jump);result
}
.ra_nordic_outcome<-function(state,pack) {
 d<-state$data;inner<-state
 if(d$stage=="jumping") {inner$data<-d$jump;return(.ra_ski_jump_outcome(inner,pack))}
 inner$data<-d$ski;gaps<-.ra_nordic_offsets(d$jump)
 for(i in seq_len(8)) if(!is.null(gaps[[i]])) inner$data$runners[[i]]$penalty_us<-inner$data$runners[[i]]$penalty_us+gaps[[i]]
 result<-.ra_cross_country_outcome(inner,pack)
 for(j in seq_along(result$placements)) {id<-result$placements[[j]]$player_id;i<-which(vapply(state$players,function(p) p$id==id,logical(1)));result$placements[[j]]$start_deficit_us<-gaps[[i]];result$placements[[j]]$penalty_us<-d$ski$runners[[i]]$penalty_us}
 result
}
