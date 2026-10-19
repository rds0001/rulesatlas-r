.ra_figure_skating_index<-list(type="integer",minimum=0,maximum=7)
.ra_figure_skating_slots<-c("axel","solo_jump","combination","flying_spin","single_position_spin","combination_spin","step_sequence")
.ra_figure_skating_schema<-function(config=NULL) {
 integer<-function(a,b) list(type="integer",minimum=a,maximum=b);array<-function(x,n) list(type="array",minItems=n,maxItems=n,items=x)
 props<-list(slot=list(type="enum",values=as.list(.ra_figure_skating_slots)),base_units=integer(0,10000),bonus=list(type="boolean"),goe_units=array(integer(-5000,5000),7))
 list(elements=list(type="array",minItems=0,maxItems=7,items=list(type="object",properties=props,required=as.list(names(props)))),components=array(array(integer(0,40),7),5),deductions_units=integer(0,10000))
}
.ra_figure_skating_current<-function(d) {for(i in d$order) if(d$entrants[[i+1L]]$status %in% c("pending","active","awaiting_marks")) return(i);NULL}
.ra_figure_skating_settle<-function(d) {d$phase<-if(is.null(.ra_figure_skating_current(d))) "review" else "ready";d}
.ra_figure_skating_validate<-function(d,config=NULL) {
 bad<-function(m) .ra_fail(m,code="INVALID_STATE")
 if(d$phase=="setup") {if(length(d$order)>0 || any(vapply(d$entrants,function(r) r$status!="pending" || !is.null(r$marks),logical(1)))) bad("Unstarted final must be empty");return(invisible(TRUE))}
 if(!identical(as.integer(sort(unlist(d$order))),0:7)) bad("Start order must be a permutation of entrants")
 for(r in d$entrants) {
  if((r$status=="scored" && is.null(r$marks)) || (!r$status %in% c("scored","dq") && !is.null(r$marks))) bad("Marks and entrant status disagree")
  if(!is.null(r$marks)) {
   .ra_fields(r$marks,.ra_figure_skating_schema(),"/marks");m<-r$marks;es<-m$elements;slots<-vapply(es,function(e) e$slot,character(1))
   if(anyDuplicated(slots)) bad("Each short-program element slot may be credited once")
   jumps<-which(slots %in% .ra_figure_skating_slots[1:3])
   for(j in seq_along(es)) {
    e<-es[[j]]
    if(min(unlist(e$goe_units)) < -e$base_units || (e$base_units==0 && any(unlist(e$goe_units)!=0))) bad("Invalidated elements have zero GOE; no individual negative GOE may exceed base value")
    if(e$bonus && (length(jumps)==0 || j!=tail(jumps,1))) bad("Only the final jump element may receive the approved second-half bonus")
   }
  }
 }
 i<-.ra_figure_skating_current(d);expected<-if(is.null(i)) "review" else switch(d$entrants[[i+1L]]$status,pending="ready",active="routine",awaiting_marks="judging")
 if((d$phase!="finished" && d$phase!=expected) || (d$phase=="finished" && !is.null(i))) bad("Phase does not match the next routine")
 active<-which(vapply(d$entrants,function(r) r$status %in% c("active","awaiting_marks"),logical(1)))
 if(length(active)>1) bad("Only one routine may be active or awaiting marks")
 if(!is.null(i) && length(active)>0 && any(active!=i+1L)) bad("Jump violates the start order")
}
.ra_figure_skating_available<-function(d,config=NULL) {
 phase<-d$phase;s<-list();empty<-setNames(list(),character())
 if(phase=="setup") return(list(set_start_order=list(order=list(type="array",minItems=8,maxItems=8,items=.ra_figure_skating_index))))
 if(phase=="ready") s<-list(start_routine=list(participant=.ra_figure_skating_index),withdraw=list(participant=.ra_figure_skating_index))
 if(phase=="routine") s<-list(complete_routine=empty,authorized_restart=empty,abandon_routine=empty)
 if(phase=="judging") s$record_marks<-.ra_figure_skating_schema(config)
 if(phase=="review") s<-list(correct_marks=c(list(participant=.ra_figure_skating_index),.ra_figure_skating_schema(config)),confirm_result=empty)
 if(phase!="finished") s$disqualify<-list(participant=.ra_figure_skating_index)
 s
}
.ra_figure_skating_apply<-function(state,action,config=NULL) {
 d<-state$data;k<-action$type;p<-action$params;schemas<-.ra_figure_skating_available(d,config)
 if(!k %in% names(schemas)) .ra_fail("Action is not permitted in this phase")
 .ra_fields(p,schemas[[k]],"/params");i<-.ra_figure_skating_current(d)
 if(k=="set_start_order") {
  if(!identical(as.integer(sort(unlist(p$order))),0:7)) .ra_fail("Each entrant must appear once")
  d$order<-p$order;d$phase<-"ready"
 } else if(k=="start_routine") {
  if(p$participant!=i) .ra_fail("Start the next scheduled entrant")
  d$entrants[[i+1L]]$status<-"active";d$phase<-"routine"
 } else if(k=="complete_routine") {d$entrants[[i+1L]]$status<-"awaiting_marks";d$phase<-"judging"} else if(k=="authorized_restart") {d$entrants[[i+1L]]$status<-"pending";d$phase<-"ready"} else if(k=="abandon_routine") {d$entrants[[i+1L]]$status<-"dnf";d<-.ra_figure_skating_settle(d)} else if(k %in% c("record_marks","correct_marks")) {
  j<-if(k=="record_marks") i else p$participant;r<-d$entrants[[j+1L]]
  if(k=="correct_marks" && (!r$status %in% c("scored","dq") || is.null(r$marks))) .ra_fail("Correct an existing judged routine only")
  d$entrants[[j+1L]]$marks<-p[names(.ra_figure_skating_schema(config))]
  if(k=="record_marks") {d$entrants[[j+1L]]$status<-"scored";d<-.ra_figure_skating_settle(d)}
 } else if(k %in% c("withdraw","disqualify")) {
  j<-p$participant;r<-d$entrants[[j+1L]]
  if(r$status=="dq" || (k=="withdraw" && r$status!="pending")) .ra_fail("Entrant cannot receive this status now")
  d$entrants[[j+1L]]$status<-if(k=="withdraw") "dns" else "dq"
  if((!is.null(i) && i==j) || d$phase %in% c("ready","review")) d<-.ra_figure_skating_settle(d)
 } else if(k=="confirm_result") d$phase<-"finished"
 d
}
.ra_figure_skating_components<-function(m,config=NULL) {
 tes<-sum(vapply(m$elements,function(e) (if(e$bonus) floor((e$base_units*11+5)/10) else e$base_units)+floor((sum(sort(unlist(e$goe_units))[2:6])+2)/5),numeric(1)))
 pcs<-sum(vapply(m$components,function(row) sum(sort(unlist(row))[2:6])*5,numeric(1)))
 c(tes+pcs-m$deductions_units,tes)
}
.ra_figure_skating_legal<-function(state,config=NULL) {
 d<-state$data;s<-.ra_figure_skating_available(d,config)
 list(mode="event_types",actions=list(),action_types=lapply(names(s),function(k) list(type=k,params_schema=s[[k]],preconditions=list("Start order, routine completion and review phase must permit the event."),external_observations=list("Official technical-panel base values, converted GOE values, bonus eligibility, component marks and deductions."))),constraints=list(phase=d$phase,current_participant=.ra_figure_skating_current(d),score_unit_denominator=100,component_order=list("skating_skills","transitions","performance","composition","interpretation")),completeness=FALSE,reason="official_short_program_sheet")
}
.ra_figure_skating_outcome<-function(state,pack,config=NULL) {
 d<-state$data;done<-d$phase=="finished";ranked<-list()
 if(done) for(i in 1:8) if(d$entrants[[i]]$status=="scored") ranked[[length(ranked)+1L]]<-list(key=.ra_figure_skating_components(d$entrants[[i]]$marks,config),i=i)
 if(length(ranked)>0) {mat<-do.call(rbind,lapply(ranked,function(r) c(-r$key,r$i)));ranked<-ranked[do.call(order,as.data.frame(mat))]}
 winners<-lapply(Filter(function(r) all(r$key==ranked[[1L]]$key),ranked),function(r) state$players[[r$i]]$id)
 placements<-lapply(ranked,function(r) list(player_id=state$players[[r$i]]$id,place=1L+sum(vapply(ranked,function(other) .ra_ps_compare(other$key,r$key)>0,logical(1))),score_units=r$key[1],score_unit_denominator=100))
 list(status=if(done) "terminal" else if(d$phase=="review") "awaiting_adjudication" else "ongoing",result_kind=if(done) {if(length(ranked)>0) "ranking" else "no_contest"} else "none",winners=winners,losers=if(length(winners)>0) lapply(Filter(function(p) !p$id %in% unlist(winners),state$players),function(p) p$id) else list(),placements=placements,reason=if(done) "scores_confirmed" else if(d$phase=="review") "result_review" else "ongoing",evidence_required=if(d$phase=="review") list("Official result confirmation.") else list(),rule_id=if(done) paste0(pack$ruleset_id,"@",pack$ruleset_version) else NULL)
}
