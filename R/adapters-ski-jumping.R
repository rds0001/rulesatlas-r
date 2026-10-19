.ra_ski_jump_index<-list(type="integer",minimum=0,maximum=7)
.ra_ski_jump_schema<-function(config=NULL) list(distance_half_m=list(type="integer",minimum=0,maximum=260),style_half_points=list(type="array",minItems=5,maxItems=5,items=list(type="integer",minimum=0,maximum=40)),penalty_tenths=list(type="integer",minimum=0,maximum=3000))
.ra_ski_jump_current<-function(d) {for(i in d$order) if(d$entrants[[i+1L]]$status %in% c("pending","active","awaiting_marks")) return(i);NULL}
.ra_ski_jump_settle<-function(d) {d$phase<-if(is.null(.ra_ski_jump_current(d))) "review" else "ready";d}
.ra_ski_jump_validate<-function(d,config=NULL) {
 bad<-function(m) .ra_fail(m,code="INVALID_STATE")
 if(d$phase=="setup") {if(length(d$order)>0 || any(vapply(d$entrants,function(r) r$status!="pending" || !is.null(r$marks),logical(1)))) bad("Unstarted final must be empty");return(invisible(TRUE))}
 if(!identical(as.integer(sort(unlist(d$order))),0:7)) bad("Start order must be a permutation of entrants")
 for(r in d$entrants) {
  if((r$status=="scored" && is.null(r$marks)) || (!r$status %in% c("scored","dq") && !is.null(r$marks))) bad("Marks and entrant status disagree")
  if(!is.null(r$marks)) .ra_fields(r$marks,.ra_ski_jump_schema(config),"/marks")
 }
 i<-.ra_ski_jump_current(d);expected<-if(is.null(i)) "review" else switch(d$entrants[[i+1L]]$status,pending="ready",active="jump",awaiting_marks="judging")
 if((d$phase!="finished" && d$phase!=expected) || (d$phase=="finished" && !is.null(i))) bad("Phase does not match the next jump")
 active<-which(vapply(d$entrants,function(r) r$status %in% c("active","awaiting_marks"),logical(1)))
 if(length(active)>1) bad("Only one jump may be active or awaiting marks")
 if(!is.null(i) && length(active)>0 && any(active!=i+1L)) bad("Jump violates the start order")
}
.ra_ski_jump_available<-function(d,config=NULL) {
 phase<-d$phase;s<-list();empty<-setNames(list(),character())
 if(phase=="setup") return(list(set_start_order=list(order=list(type="array",minItems=8,maxItems=8,items=.ra_ski_jump_index))))
 if(phase=="ready") s<-list(start_jump=list(participant=.ra_ski_jump_index),withdraw=list(participant=.ra_ski_jump_index))
 if(phase=="jump") s<-list(complete_jump=empty,authorized_restart=empty,abandon_jump=empty)
 if(phase=="judging") s$record_marks<-.ra_ski_jump_schema(config)
 if(phase=="review") s<-list(correct_marks=c(list(participant=.ra_ski_jump_index),.ra_ski_jump_schema(config)),confirm_result=empty)
 if(phase!="finished") s$disqualify<-list(participant=.ra_ski_jump_index)
 s
}
.ra_ski_jump_apply<-function(state,action,config=NULL) {
 d<-state$data;k<-action$type;p<-action$params;schemas<-.ra_ski_jump_available(d,config)
 if(!k %in% names(schemas)) .ra_fail("Action is not permitted in this phase")
 .ra_fields(p,schemas[[k]],"/params");i<-.ra_ski_jump_current(d)
 if(k=="set_start_order") {
  if(!identical(as.integer(sort(unlist(p$order))),0:7)) .ra_fail("Each entrant must appear once")
  d$order<-p$order;d$phase<-"ready"
 } else if(k=="start_jump") {
  if(p$participant!=i) .ra_fail("Start the next scheduled entrant")
  d$entrants[[i+1L]]$status<-"active";d$phase<-"jump"
 } else if(k=="complete_jump") {d$entrants[[i+1L]]$status<-"awaiting_marks";d$phase<-"judging"} else if(k=="authorized_restart") {d$entrants[[i+1L]]$status<-"pending";d$phase<-"ready"} else if(k=="abandon_jump") {d$entrants[[i+1L]]$status<-"dnf";d<-.ra_ski_jump_settle(d)} else if(k %in% c("record_marks","correct_marks")) {
  j<-if(k=="record_marks") i else p$participant;r<-d$entrants[[j+1L]]
  if(k=="correct_marks" && (!r$status %in% c("scored","dq") || is.null(r$marks))) .ra_fail("Correct an existing judged jump only")
  d$entrants[[j+1L]]$marks<-p[names(.ra_ski_jump_schema(config))]
  if(k=="record_marks") {d$entrants[[j+1L]]$status<-"scored";d<-.ra_ski_jump_settle(d)}
 } else if(k %in% c("withdraw","disqualify")) {
  j<-p$participant;r<-d$entrants[[j+1L]]
  if(r$status=="dq" || (k=="withdraw" && r$status!="pending")) .ra_fail("Entrant cannot receive this status now")
  d$entrants[[j+1L]]$status<-if(k=="withdraw") "dns" else "dq"
  if((!is.null(i) && i==j) || d$phase %in% c("ready","review")) d<-.ra_ski_jump_settle(d)
 } else if(k=="confirm_result") d$phase<-"finished"
 d
}
.ra_ski_jump_components<-function(m,config=NULL) max(0,600+10*(m$distance_half_m-180)+5*sum(sort(unlist(m$style_half_points))[2:4])-m$penalty_tenths)
.ra_ski_jump_legal<-function(state,config=NULL) {
 d<-state$data;s<-.ra_ski_jump_available(d,config)
 list(mode="event_types",actions=list(),action_types=lapply(names(s),function(k) list(type=k,params_schema=s[[k]],preconditions=list("Start order, jump completion and review phase must permit the event."),external_observations=list("Official half-metre distance, five half-point style marks and neutral deductions."))),constraints=list(phase=d$phase,current_participant=.ra_ski_jump_current(d),score_unit_denominator=10),completeness=FALSE,reason="official_ski_jump_judgments")
}
.ra_ski_jump_outcome<-function(state,pack,config=NULL) {
 d<-state$data;done<-d$phase=="finished";ranked<-list()
 if(done) for(i in 1:8) if(d$entrants[[i]]$status=="scored") ranked[[length(ranked)+1L]]<-list(key=.ra_ski_jump_components(d$entrants[[i]]$marks,config),i=i)
 if(length(ranked)>0) {mat<-do.call(rbind,lapply(ranked,function(r) c(-r$key,r$i)));ranked<-ranked[do.call(order,as.data.frame(mat))]}
 winners<-lapply(Filter(function(r) all(r$key==ranked[[1L]]$key),ranked),function(r) state$players[[r$i]]$id)
 placements<-lapply(ranked,function(r) list(player_id=state$players[[r$i]]$id,place=1L+sum(vapply(ranked,function(other) .ra_ps_compare(other$key,r$key)>0,logical(1))),score_units=r$key[1],score_unit_denominator=10))
 list(status=if(done) "terminal" else if(d$phase=="review") "awaiting_adjudication" else "ongoing",result_kind=if(done) {if(length(ranked)>0) "ranking" else "no_contest"} else "none",winners=winners,losers=if(length(winners)>0) lapply(Filter(function(p) !p$id %in% unlist(winners),state$players),function(p) p$id) else list(),placements=placements,reason=if(done) "scores_confirmed" else if(d$phase=="review") "result_review" else "ongoing",evidence_required=if(d$phase=="review") list("Official result confirmation.") else list(),rule_id=if(done) paste0(pack$ruleset_id,"@",pack$ruleset_version) else NULL)
}
