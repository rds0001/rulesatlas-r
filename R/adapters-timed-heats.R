.ra_th_eligible<-function(d) Filter(function(i) all(vapply(seq_len(d$heat),function(j) d$results[[j]][[i+1L]]$status=="finished",logical(1))),0:7)
.ra_th_next<-function(d) {for(i in d$order) if(d$results[[d$heat+1L]][[i+1L]]$status=="pending") return(i);NULL}
.ra_th_validate<-function(d,config) {
 h<-d$heat;phase<-d$phase;eligible<-.ra_th_eligible(d)
 for(j in seq_along(d$results)) for(i in 1:8) {
  r<-d$results[[j]][[i]]
  if((r$status=="finished")!=(!is.null(r$time_us))) .ra_fail("Only completed runs carry elapsed times",code="INVALID_STATE")
  if(!is.null(r$time_us) && r$time_us %% config$quantum_us!=0) .ra_fail("Elapsed time precision mismatch",code="INVALID_STATE")
  if(j>h+1L && r$status!="not_eligible") .ra_fail("Future heat already has a result",code="INVALID_STATE")
  if(j<h+1L && r$status=="pending") .ra_fail("Prior heat is unresolved",code="INVALID_STATE")
  if(j<h+1L && (r$status=="not_eligible")!=any(vapply(seq_len(j-1L),function(k) d$results[[k]][[i]]$status!="finished",logical(1)))) .ra_fail("Prior heat eligibility mismatch",code="INVALID_STATE")
  if(j<=h+1L && j>1L && any(vapply(seq_len(j-1L),function(k) d$results[[k]][[i]]$status!="finished",logical(1))) && r$status!="not_eligible") .ra_fail("Failed earlier runs prevent continuation",code="INVALID_STATE")
 }
 if(phase=="configure") {
  if(length(d$order) || any(vapply(d$results[[h+1L]],function(r) r$status!="not_eligible",logical(1)))) .ra_fail("Heat must await its official start order",code="INVALID_STATE")
 } else {
  if(!identical(as.integer(sort(unlist(d$order))),as.integer(eligible))) .ra_fail("Start order must include every eligible entrant once",code="INVALID_STATE")
  for(i in 0:7) if((!i %in% eligible)!=(d$results[[h+1L]][[i+1L]]$status=="not_eligible")) .ra_fail("Current heat eligibility mismatch",code="INVALID_STATE")
  if((phase %in% c("run","active"))!=(!is.null(.ra_th_next(d)))) .ra_fail("Heat phase and unresolved starts disagree",code="INVALID_STATE")
 }
 if(phase=="finished" && h+1L<config$heats && any(vapply(d$results[[h+1L]],function(r) r$status=="finished",logical(1)))) .ra_fail("Remaining heats must be completed",code="INVALID_STATE")
 invisible(TRUE)
}
.ra_th_apply<-function(state,action,config) {
 d<-state$data;k<-action$type;p<-action$params;phase<-d$phase;h<-d$heat+1L
 index<-list(type="integer",minimum=0,maximum=7);time<-list(type="integer",minimum=1,maximum=1000000000000,nullable=TRUE);status<-list(type="enum",values=list("finished","dns","dnf","dq"))
 if(k=="set_start_order") {
  .ra_fields(p,list(order=list(type="array",minItems=0,maxItems=8,items=index)),"/params")
  if(phase!="configure" || !identical(as.integer(sort(unlist(p$order))),as.integer(.ra_th_eligible(d)))) .ra_fail("Supply one official start for each eligible entrant")
  d$order<-p$order;for(i in d$order) d$results[[h]][[i+1L]]$status<-"pending"
  d$phase<-if(length(d$order)) "run" else "review"
 } else if(k=="start_next") {
  .ra_fields(p,list(),"/params");if(phase!="run") .ra_fail("No next entrant can start now");d$phase<-"active"
 } else if(k=="void_start") {
  .ra_fields(p,list(),"/params");if(phase!="active") .ra_fail("Only an active run can be officially voided for a restart");d$phase<-"run"
 } else if(k %in% c("record_result","amend_result")) {
  schema<-list(status=status,time_us=time);if(k=="amend_result") schema$participant<-index
  .ra_fields(p,schema,"/params")
  if(k=="amend_result") {
   if(phase!="review" || !p$participant %in% unlist(d$order)) .ra_fail("Amend only an eligible current-heat result during review")
   i<-p$participant
  } else {
   if(!phase %in% c("run","active") || (phase=="run" && !p$status %in% c("dns","dq"))) .ra_fail("A completed run requires a valid start; only DNS or exclusion can be recorded before starting")
   if(phase=="active" && p$status=="dns") .ra_fail("A started entrant cannot be DNS")
   i<-.ra_th_next(d)
  }
  if((p$status=="finished")!=(!is.null(p$time_us))) .ra_fail("Only a finished run has an elapsed time")
  if(!is.null(p$time_us) && p$time_us %% config$quantum_us!=0) .ra_fail("Elapsed time precision mismatch")
  d$results[[h]][[i+1L]]<-list(status=p$status,time_us=p$time_us)
  if(k=="record_result") d$phase<-if(!is.null(.ra_th_next(d))) "run" else "review"
 } else if(k=="confirm_heat") {
  .ra_fields(p,list(),"/params");if(phase!="review") .ra_fail("Resolve every start and review the heat first")
  if(h==config$heats || !any(vapply(d$results[[h]],function(r) r$status=="finished",logical(1)))) d$phase<-"finished" else {d$heat<-h;d$order<-list();d$phase<-"configure"}
 } else .ra_fail("Unknown timed-heat action")
 d
}
.ra_th_legal<-function(state,config) {
 d<-state$data;phase<-d$phase;specs<-list();index<-list(type="integer",minimum=0,maximum=7);time<-list(type="integer",minimum=1,maximum=1000000000000,nullable=TRUE);status<-list(type="enum",values=list("finished","dns","dnf","dq"));empty<-setNames(list(),character())
 if(phase=="configure") specs$set_start_order<-list(order=list(type="array",minItems=0,maxItems=8,items=index))
 if(phase=="run") specs$start_next<-empty
 if(phase %in% c("run","active")) specs$record_result<-list(status=status,time_us=time)
 if(phase=="active") specs$void_start<-empty
 if(phase=="review") {specs$amend_result<-list(participant=index,status=status,time_us=time);specs$confirm_heat<-empty}
 list(mode="event_types",actions=list(),action_types=lapply(names(specs),function(k) list(type=k,params_schema=specs[[k]],preconditions=list("Heat phase, next entrant, result status and timing precision must match."),external_observations=list("Official start order, valid completion, elapsed time, exclusion or authorized restart."))),constraints=list(phase=phase,heat=d$heat,next_participant=.ra_th_next(d),quantum_us=config$quantum_us),completeness=FALSE,reason="official_timed_heat_events")
}
.ra_th_outcome<-function(state,pack,config) {
 d<-state$data;done<-d$phase=="finished";eligible<-if(done) Filter(function(i) all(vapply(d$results,function(row) row[[i]]$status=="finished",logical(1))),1:8) else integer()
 totals<-vapply(eligible,function(i) sum(vapply(d$results,function(row) row[[i]]$time_us,numeric(1))),numeric(1));o<-order(totals,eligible);eligible<-eligible[o];totals<-totals[o]
 ws<-if(length(eligible)) eligible[totals==min(totals)] else integer();winners<-lapply(ws,function(i) state$players[[i]]$id)
 list(status=if(done) "terminal" else if(d$phase=="review") "awaiting_adjudication" else "ongoing",result_kind=if(done) if(length(eligible)) "ranking" else "no_contest" else "none",winners=winners,losers=if(length(winners)) lapply(setdiff(1:8,ws),function(i) state$players[[i]]$id) else list(),placements=lapply(seq_along(eligible),function(j) list(player_id=state$players[[eligible[j]]]$id,place=1L+sum(totals<totals[j]),total_time_us=totals[j])),reason=if(done) "aggregate_confirmed" else if(d$phase=="review") "heat_review" else "ongoing",evidence_required=if(d$phase=="review") list("Official heat confirmation.") else list(),rule_id=if(done) paste0(pack$ruleset_id,"@",pack$ruleset_version) else NULL)
}
