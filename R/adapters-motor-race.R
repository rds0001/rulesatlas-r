.ra_motor_classified<-function(d) {
 if(is.null(d$end_mode)) return(integer())
 lead<-max(c(0L,vapply(Filter(function(r) !(r$status %in% c("dns","dq")),d$runners),function(r) length(r$laps),integer(1))))
 if(lead==0) return(integer())
 minimum<-max(1L,floor(lead*75/100))
 Filter(function(i) {r<-d$runners[[i+1L]];!(r$status %in% c("ready","running","dns","dq")) && length(r$laps)>=minimum && (d$end_mode=="early" || r$status=="finished" && r$laps[[length(r$laps)]]<=d$checkered_ms+300000L)},0:7)
}
.ra_motor_validate<-function(d,kind) {
 projected<-d
 if(kind=="motocross") {
  if(is.null(d$target_laps)) projected$target_laps<-200L else if(d$target_laps<3) .ra_fail("Timed motocross needs a triggering leader lap followed by two laps",code="INVALID_STATE")
  if(d$phase=="ready" && !is.null(d$target_laps)) .ra_fail("Timed race distance is established only when the leading racer crosses after time",code="INVALID_STATE")
  if(identical(d$end_mode,"normal") && is.null(d$target_laps)) .ra_fail("Normal timed finish requires the two-lap target",code="INVALID_STATE")
 }
 .ra_circuit_validate(projected,"kart")
}
.ra_motor_apply<-function(state,action,kind) {
 if(kind=="motocross" && action$type=="configure") {
  .ra_fields(action$params,list(duration_ms=list(type="integer",minimum=300000,maximum=3600000)),"/params")
  if(state$data$phase!="ready") .ra_fail("Race duration is fixed after the start")
  d<-state$data;d$duration_ms<-action$params$duration_ms;return(d)
 }
 projected<-state
 if(kind=="motocross" && action$type=="cross_line") {
  .ra_fields(action$params,.ra_circuit_schemas()$cross_line,"/params");d<-projected$data;p<-action$params;n<-length(d$runners[[p$player+1L]]$laps)+1L
  if(is.null(d$target_laps) && p$elapsed_ms>=d$duration_ms && n>max(c(0L,vapply(Filter(function(r) r$status=="running",d$runners[-(p$player+1L)]),function(r) length(r$laps),integer(1))))) {
   if(n+2L>200) .ra_fail("Timed sprint exceeds the declared two-hundred-lap bound")
   projected$data$target_laps<-n+2L
  }
 }
 unlocked<-kind=="motocross" && is.null(projected$data$target_laps)
 if(unlocked) projected$data$target_laps<-201L
 result<-.ra_circuit_apply(projected,action,kind)
 if(unlocked) result["target_laps"]<-list(NULL)
 result
}
.ra_motor_legal<-function(state,kind) {
 result<-.ra_circuit_legal(state,kind)
 if(kind=="motocross") {
  for(i in seq_along(result$action_types)) if(result$action_types[[i]]$type=="configure") result$action_types[[i]]$params_schema<-list(duration_ms=list(type="integer",minimum=300000,maximum=3600000))
  result$constraints$duration_ms<-state$data$duration_ms;result$constraints$additional_laps<-2L
 }
 result$constraints$classification_percent<-75L;result$constraints$provisionally_classified<-as.list(.ra_motor_classified(state$data));result$constraints$finish_window_ms<-300000L;result$constraints$normal_finish_required<-TRUE;result
}
.ra_motor_outcome<-function(state,pack,kind) {
 d<-state$data;done<-d$phase=="finished";ids<-.ra_motor_classified(d);keys<-lapply(ids,function(i) .ra_circuit_key(d$runners[[i+1L]]));ids<-ids[order(vapply(keys,function(k) k[[1]],numeric(1)),vapply(keys,function(k) k[[2]],numeric(1)),ids)];best<-if(length(ids)>0) .ra_circuit_key(d$runners[[ids[[1]]+1L]]) else NULL;w<-Filter(function(i) all(.ra_circuit_key(d$runners[[i+1L]])==best),ids)
 list(status=if(done) "terminal" else if(d$phase=="review") "awaiting_adjudication" else "ongoing",result_kind=if(done) {if(length(ids)>0) "ranking" else "no_contest"} else "none",winners=if(done) lapply(w,function(i) state$players[[i+1L]]$id) else list(),losers=if(done) lapply(setdiff(ids,w),function(i) state$players[[i+1L]]$id) else list(),placements=if(done) lapply(ids,function(i) list(player_id=state$players[[i+1L]]$id,place=1L+sum(vapply(ids,function(j) .ra_circuit_less(.ra_circuit_key(d$runners[[j+1L]]),.ra_circuit_key(d$runners[[i+1L]])),logical(1))),laps=length(d$runners[[i+1L]]$laps),adjusted_ms=.ra_circuit_key(d$runners[[i+1L]])[[2]])) else list(),reason=if(done) "classification_confirmed" else "ongoing",evidence_required=list(),rule_id=if(done) paste0(pack$ruleset_id,"@",pack$ruleset_version) else NULL)
}
