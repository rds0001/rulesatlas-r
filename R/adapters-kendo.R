.ra_ke_i<-function(a,b) list(type="integer",minimum=a,maximum=b)
.ra_ke_a<-function(x) list(type="array",minItems=2,maxItems=2,items=x)
.ra_ke_record<-list(points=.ra_ke_a(.ra_ke_i(0,2)),hansoku=.ra_ke_a(.ra_ke_i(0,1)),remaining_ms=.ra_ke_i(0,300000))
.ra_ke_empty<-setNames(list(),character())
.ra_ke_schemas<-list(start=.ra_ke_empty,resume=.ra_ke_empty,pause=.ra_ke_empty,elapse=list(milliseconds=.ra_ke_i(1,300000)),ippon=list(player=.ra_ke_i(0,1),target=list(type="string",enum=list("men","kote","do","tsuki"))),hansoku=list(offenders=.ra_ke_a(list(type="boolean"))),correct_record=.ra_ke_record,begin_encho=.ra_ke_empty,hantei=list(votes=list(type="array",minItems=3,maxItems=3,items=.ra_ke_i(0,1))),forfeit=list(player=.ra_ke_i(0,1)),disqualify=list(player=.ra_ke_i(0,1)),both_disqualified=.ra_ke_empty,confirm=.ra_ke_empty)
.ra_ke_settle<-function(d) {
 d["winner"]<-list(NULL);d["reason"]<-list(NULL);a<-d$points[[1]];b<-d$points[[2]]
 if(max(a,b)==2 || ((d$encho || d$remaining_ms==0) && a!=b)) {d$winner<-if(a>b) 0L else 1L;d$reason<-"points";d$phase<-"review"} else if(d$remaining_ms==0) d$phase<-if(d$encho) "decision" else "interval" else d$phase<-"paused"
 d
}
.ra_ke_validate<-function(d) {
 bad<-function(x) .ra_fail(x,code="INVALID_STATE");terminal<-d$phase %in% c("review","finished");pts<-unlist(d$points)
 if(terminal!=!is.null(d$reason)) bad("Result phase mismatch")
 if(d$remaining_ms>if(d$encho) 180000 else 300000) bad("Clock exceeds period")
 if(d$encho && d$phase=="ready") bad("Overtime cannot be unstarted")
 if(d$phase=="ready" && (any(pts!=0) || any(unlist(d$hansoku)!=0) || d$remaining_ms!=300000)) bad("Invalid unstarted bout")
 if(d$phase %in% c("interval","decision") && (d$remaining_ms!=0 || pts[1]!=pts[2] || (d$phase=="decision")!=d$encho)) bad("Invalid period boundary")
 if(!terminal) {
  if(!is.null(d$winner) || max(pts)>=2) bad("Unresolved winning score")
  if(d$phase %in% c("active","paused") && (d$remaining_ms==0 || (d$encho && pts[1]!=pts[2]))) bad("Invalid live period")
 } else if(d$reason=="both_disqualified") {if(!is.null(d$winner) || any(pts!=0)) bad("Invalid double disqualification")} else {
  w<-d$winner;if(is.null(w) || pts[w+1]<=pts[2-w]) bad("Winner must lead")
  if(d$reason=="points" && !(max(pts)==2 || d$encho || d$remaining_ms==0)) bad("Premature points victory")
  if(d$reason=="disqualify" && (pts[w+1]!=2 || pts[2-w]!=0)) bad("Invalid disqualification score")
  if(d$reason=="hantei" && (!d$encho || d$remaining_ms!=0)) bad("Premature hantei")
 }
}
.ra_ke_available<-function(d) {
 names<-switch(d$phase,ready=c("start"),active=c("pause","elapse","ippon","hansoku"),paused=c("resume","hansoku","correct_record"),interval=c("begin_encho","correct_record"),decision=c("hantei","correct_record"),review=c("confirm",if(d$reason=="points") "correct_record" else character()),character())
 if(!d$phase %in% c("review","finished")) names<-c(names,"forfeit","disqualify","both_disqualified")
 names
}
.ra_ke_apply<-function(state,action) {
 d<-state$data;k<-action$type;p<-action$params
 if(!k %in% .ra_ke_available(d)) .ra_fail("Action is not permitted in this phase")
 .ra_fields(p,.ra_ke_schemas[[k]],"/params")
 if(k %in% c("start","resume")) d$phase<-"active" else if(k=="pause") d$phase<-"paused" else if(k=="elapse") {
  if(p$milliseconds>d$remaining_ms) .ra_fail("Elapsed time crosses a period boundary")
  d$remaining_ms<-d$remaining_ms-p$milliseconds;if(d$remaining_ms==0) d<-.ra_ke_settle(d)
 } else if(k=="ippon") {i<-p$player+1L;d$points[[i]]<-d$points[[i]]+1L;d<-.ra_ke_settle(d)} else if(k=="hansoku") {
  o<-unlist(p$offenders);if(!any(o)) .ra_fail("At least one offender is required")
  if(!(all(o) && all(unlist(d$points)==1) && all(unlist(d$hansoku)==1))) for(i in 1:2) if(o[i]) {d$hansoku[[i]]<-d$hansoku[[i]]+1L;if(d$hansoku[[i]]==2) {d$hansoku[[i]]<-0L;d$points[[3L-i]]<-d$points[[3L-i]]+1L}}
  d<-.ra_ke_settle(d)
 } else if(k=="correct_record") {for(n in names(p)) d[[n]]<-p[[n]];d<-.ra_ke_settle(d);.ra_ke_validate(d)} else if(k=="begin_encho") {d$encho<-TRUE;d$remaining_ms<-180000L;d$phase<-"paused"} else if(k=="hantei") {
  w<-if(sum(unlist(p$votes))>=2) 1L else 0L;d$points[[w+1L]]<-d$points[[w+1L]]+1L;d$winner<-w;d$reason<-k;d$phase<-"review"
 } else if(k %in% c("forfeit","disqualify")) {
  w<-1L-p$player;d$points[[w+1L]]<-if(d$encho && k=="forfeit") d$points[[w+1L]]+1L else 2L
  if(k=="disqualify") d$points[[2L-w]]<-0L
  d$winner<-w;d$reason<-k;d$phase<-"review"
 } else if(k=="both_disqualified") {d$points<-list(0L,0L);d["winner"]<-list(NULL);d$reason<-k;d$phase<-"review"} else if(k=="confirm") d$phase<-"finished"
 d
}
.ra_ke_legal<-function(state) {
 d<-state$data
 list(mode="event_types",actions=list(),action_types=lapply(.ra_ke_available(d),function(k) list(type=k,params_schema=.ra_ke_schemas[[k]],preconditions=list("Official decisions only; clock updates may not cross a period boundary."),external_observations=list("Final referee decisions and measured active time."))),constraints=list(phase=d$phase,remaining_ms=d$remaining_ms,encho=d$encho),completeness=FALSE,reason="externally_adjudicated_bout")
}
.ra_ke_outcome<-function(state,pack) {
 d<-state$data;t<-d$phase=="finished";w<-d$winner;reason<-if(t) d$reason else if(d$phase=="review") "awaiting_confirmation" else "ongoing"
 list(status=if(t) "terminal" else "ongoing",result_kind=if(t) {if(is.null(w)) "no_contest" else if(reason=="disqualify") "disqualification" else if(reason=="forfeit") "forfeit" else "win"} else "none",winners=if(t && !is.null(w)) list(state$players[[w+1L]]$id) else list(),losers=if(t) {if(is.null(w)) lapply(state$players,function(p) p$id) else list(state$players[[2L-w]]$id)} else list(),placements=list(),reason=reason,evidence_required=list(),rule_id=if(t) paste0(pack$ruleset_id,"@",pack$ruleset_version) else NULL)
}
