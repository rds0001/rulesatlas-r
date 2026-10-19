.ra_or_validate<-function(d,config) {
 .ra_race_validate(d,config=config)
 for(r in d$runners) if((r$status=="finished")!=(!is.null(r$official_order))) .ra_fail("Only finishers have an official finish-order value",code="INVALID_STATE")
 invisible(TRUE)
}
.ra_or_apply<-function(state,action,config) {
 p<-action$params;k<-action$type;d<-state$data;index<-list(type="integer",minimum=0,maximum=length(d$runners)-1L)
 if(k=="recall_start") {
  .ra_fields(p,list(),"/params")
  if(d$phase!="racing" || any(vapply(d$runners,function(r) r$status=="finished",logical(1)))) .ra_fail("An officially invalid start can be recalled only before any finisher")
  for(i in seq_along(d$runners)) {if(d$runners[[i]]$status=="running") d$runners[[i]]$status<-"ready" else if(d$runners[[i]]$status=="dnf") d$runners[[i]]$status<-"dns"}
  d$phase<-"ready";return(.ra_race_settle(d))
 }
 if(k %in% c("record_finish","amend_result")) {
  time<-.ra_race_time;time$nullable<-k=="amend_result"
  schema<-list(participant=index,time_us=time,official_order=list(type="integer",minimum=1,maximum=length(d$runners),nullable=k=="amend_result"))
  if(k=="amend_result") {schema$status<-list(type="enum",values=as.list(.ra_race_statuses(config)));schema$warnings<-list(type="integer",minimum=0,maximum=2)}
  .ra_fields(p,schema,"/params")
  if(k=="amend_result" && (p$status=="finished")!=(!is.null(p$official_order))) .ra_fail("Only a finished amended result has an official finish-order value")
  a<-action;a$params<-p[setdiff(names(p),"official_order")];d<-.ra_race_apply(state,a,config=config);d$runners[[p$participant+1L]]["official_order"]<-list(p$official_order);return(d)
 }
 d<-.ra_race_apply(state,action,config=config)
 for(i in seq_along(d$runners)) if(d$runners[[i]]$status!="finished") d$runners[[i]]["official_order"]<-list(NULL)
 d
}
.ra_or_legal<-function(state,config) {
 result<-.ra_race_legal(state,config=config);d<-state$data
 for(i in seq_along(result$action_types)) if(result$action_types[[i]]$type %in% c("record_finish","amend_result")) result$action_types[[i]]$params_schema$official_order<-list(type="integer",minimum=1,maximum=length(d$runners),nullable=result$action_types[[i]]$type=="amend_result")
 if(d$phase=="racing" && !any(vapply(d$runners,function(r) r$status=="finished",logical(1)))) result$action_types<-c(result$action_types,list(list(type="recall_start",params_schema=setNames(list(),character()),preconditions=list("No contestant has already finished."),external_observations=list("Official decision that the collective start is invalid and must be repeated."))))
 result$constraints$ranking_basis<-"official_order";result
}
.ra_or_outcome<-function(state,pack,config) {
 d<-state$data;done<-d$phase=="finished";finishers<-if(done) which(vapply(d$runners,function(r) r$status=="finished",logical(1))) else integer()
 if(length(finishers)) finishers<-finishers[order(vapply(d$runners[finishers],function(r) r$official_order,numeric(1)),finishers)]
 orders<-vapply(d$runners[finishers],function(r) r$official_order,numeric(1));ws<-if(length(finishers)) finishers[orders==min(orders)] else integer();winners<-lapply(ws,function(i) state$players[[i]]$id)
 placements<-lapply(finishers,function(i) list(player_id=state$players[[i]]$id,place=1L+sum(orders<d$runners[[i]]$official_order),time_us=d$runners[[i]]$time_us,official_order=d$runners[[i]]$official_order))
 list(status=if(done) "terminal" else if(d$phase=="review") "awaiting_adjudication" else "ongoing",result_kind=if(done) if(length(finishers)) "ranking" else "no_contest" else "none",winners=winners,losers=if(length(winners)) lapply(setdiff(seq_along(state$players),ws),function(i) state$players[[i]]$id) else list(),placements=placements,reason=if(done) "results_confirmed" else if(d$phase=="review") "result_review" else "ongoing",evidence_required=if(d$phase=="review") list("Official result confirmation.") else list(),rule_id=if(done) paste0(pack$ruleset_id,"@",pack$ruleset_version) else NULL)
}
