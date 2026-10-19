.ra_gy_index<-list(type="integer",minimum=0,maximum=7)
.ra_gy_schema<-function(config) {
 arr<-function(n,m) list(type="array",minItems=n,maxItems=n,items=list(type="integer",minimum=0,maximum=m))
 list(difficulty=arr(config$difficulty_panels,20000),execution=arr(config$judges,10000),artistry=arr(config$artistry_judges,10000),penalty=list(type="integer",minimum=0,maximum=100000))
}
.ra_gy_current<-function(d) {for(i in d$order) if(d$entrants[[i+1L]]$status %in% c("pending","active","awaiting_marks")) return(i);NULL}
.ra_gy_settle<-function(d) {d$phase<-if(is.null(.ra_gy_current(d))) "review" else "ready";d}
.ra_gy_validate<-function(d,config) {
 bad<-function(m) .ra_fail(m,code="INVALID_STATE")
 if(d$phase=="setup") {if(length(d$order)>0 || any(vapply(d$entrants,function(r) r$status!="pending" || !is.null(r$marks),logical(1)))) bad("Unstarted final must be empty");return(invisible(TRUE))}
 if(!identical(as.integer(sort(unlist(d$order))),0:7)) bad("Start order must be a permutation of entrants")
 for(r in d$entrants) {
  if((r$status=="scored" && is.null(r$marks)) || (!r$status %in% c("scored","dq") && !is.null(r$marks))) bad("Marks and entrant status disagree")
  if(!is.null(r$marks)) .ra_fields(r$marks,.ra_gy_schema(config),"/marks")
 }
 i<-.ra_gy_current(d);expected<-if(is.null(i)) "review" else switch(d$entrants[[i+1L]]$status,pending="ready",active="routine",awaiting_marks="judging")
 if((d$phase!="finished" && d$phase!=expected) || (d$phase=="finished" && !is.null(i))) bad("Phase does not match the next routine")
 active<-which(vapply(d$entrants,function(r) r$status %in% c("active","awaiting_marks"),logical(1)))
 if(length(active)>1) bad("Only one routine may be active or awaiting marks")
 if(!is.null(i) && length(active)>0 && any(active!=i+1L)) bad("Routine violates the start order")
}
.ra_gy_available<-function(d,config) {
 phase<-d$phase;s<-list();empty<-setNames(list(),character())
 if(phase=="setup") return(list(set_start_order=list(order=list(type="array",minItems=8,maxItems=8,items=.ra_gy_index))))
 if(phase=="ready") s<-list(start_routine=list(participant=.ra_gy_index),withdraw=list(participant=.ra_gy_index))
 if(phase=="routine") s<-list(complete_routine=empty,authorized_restart=empty,abandon_routine=empty)
 if(phase=="judging") s$record_marks<-.ra_gy_schema(config)
 if(phase=="review") s<-list(correct_marks=c(list(participant=.ra_gy_index),.ra_gy_schema(config)),confirm_result=empty)
 if(phase!="finished") s$disqualify<-list(participant=.ra_gy_index)
 s
}
.ra_gy_apply<-function(state,action,config) {
 d<-state$data;k<-action$type;p<-action$params;schemas<-.ra_gy_available(d,config)
 if(!k %in% names(schemas)) .ra_fail("Action is not permitted in this phase")
 .ra_fields(p,schemas[[k]],"/params");i<-.ra_gy_current(d)
 if(k=="set_start_order") {
  if(!identical(as.integer(sort(unlist(p$order))),0:7)) .ra_fail("Each entrant must appear once")
  d$order<-p$order;d$phase<-"ready"
 } else if(k=="start_routine") {
  if(p$participant!=i) .ra_fail("Start the next scheduled entrant")
  d$entrants[[i+1L]]$status<-"active";d$phase<-"routine"
 } else if(k=="complete_routine") {d$entrants[[i+1L]]$status<-"awaiting_marks";d$phase<-"judging"} else if(k=="authorized_restart") {d$entrants[[i+1L]]$status<-"pending";d$phase<-"ready"} else if(k=="abandon_routine") {d$entrants[[i+1L]]$status<-"dnf";d<-.ra_gy_settle(d)} else if(k %in% c("record_marks","correct_marks")) {
  j<-if(k=="record_marks") i else p$participant;r<-d$entrants[[j+1L]]
  if(k=="correct_marks" && (!r$status %in% c("scored","dq") || is.null(r$marks))) .ra_fail("Correct an existing judged routine only")
  d$entrants[[j+1L]]$marks<-p[names(.ra_gy_schema(config))]
  if(k=="record_marks") {d$entrants[[j+1L]]$status<-"scored";d<-.ra_gy_settle(d)}
 } else if(k %in% c("withdraw","disqualify")) {
  j<-p$participant;r<-d$entrants[[j+1L]]
  if(r$status=="dq" || (k=="withdraw" && r$status!="pending")) .ra_fail("Entrant cannot receive this status now")
  d$entrants[[j+1L]]$status<-if(k=="withdraw") "dns" else "dq"
  if((!is.null(i) && i==j) || d$phase %in% c("ready","review")) d<-.ra_gy_settle(d)
 } else if(k=="confirm_result") d$phase<-"finished"
 d
}
.ra_gy_components<-function(m,config) {
 n<-config$judges-2L;e<-10000*n-sum(sort(unlist(m$execution))[2:(n+1L)]);a<-0
 if(config$artistry_judges>0) a<-10000*n-sum(sort(unlist(m$artistry))[2:(n+1L)])
 difficulty<-sum(unlist(m$difficulty))*n;total<-max(0,difficulty+e*(if(is.null(config$execution_weight)) 1 else config$execution_weight)+a-m$penalty*n)
 if(config$artistry_judges>0) c(total,e,a,difficulty) else c(total,e,difficulty)
}
.ra_gy_legal<-function(state,config) {
 d<-state$data;s<-.ra_gy_available(d,config)
 list(mode="event_types",actions=list(),action_types=lapply(names(s),function(k) list(type=k,params_schema=s[[k]],preconditions=list("Start order, routine completion and review phase must permit the event."),external_observations=list("Official difficulty components, individual execution/artistry deductions and neutral deductions."))),constraints=list(phase=d$phase,current_participant=.ra_gy_current(d),score_unit_denominator=(config$judges-2L)*1000),completeness=FALSE,reason="official_apparatus_judgments")
}
.ra_gy_outcome<-function(state,pack,config) {
 d<-state$data;done<-d$phase=="finished";ranked<-list()
 if(done) for(i in 1:8) if(d$entrants[[i]]$status=="scored") ranked[[length(ranked)+1L]]<-list(key=.ra_gy_components(d$entrants[[i]]$marks,config),i=i)
 if(length(ranked)>0) {mat<-do.call(rbind,lapply(ranked,function(r) c(-r$key,r$i)));ranked<-ranked[do.call(order,as.data.frame(mat))]}
 winners<-lapply(Filter(function(r) all(r$key==ranked[[1L]]$key),ranked),function(r) state$players[[r$i]]$id)
 placements<-lapply(ranked,function(r) list(player_id=state$players[[r$i]]$id,place=1L+sum(vapply(ranked,function(other) .ra_ps_compare(other$key,r$key)>0,logical(1))),score_units=r$key[1],score_unit_denominator=(config$judges-2L)*1000))
 list(status=if(done) "terminal" else if(d$phase=="review") "awaiting_adjudication" else "ongoing",result_kind=if(done) {if(length(ranked)>0) "ranking" else "no_contest"} else "none",winners=winners,losers=if(length(winners)>0) lapply(Filter(function(p) !p$id %in% unlist(winners),state$players),function(p) p$id) else list(),placements=placements,reason=if(done) "scores_confirmed" else if(d$phase=="review") "result_review" else "ongoing",evidence_required=if(d$phase=="review") list("Official result confirmation.") else list(),rule_id=if(done) paste0(pack$ruleset_id,"@",pack$ruleset_version) else NULL)
}
