.ra_di_index<-list(type="integer",minimum=0,maximum=7)
.ra_di_marks<-list(type="array",minItems=7,maxItems=7,items=list(type="integer",minimum=0,maximum=20))
.ra_di_dive<-list(type="object",properties=list(number=list(type="integer",minimum=100,maximum=5999),position=list(type="enum",values=list("A","B","C","D")),group=list(type="integer",minimum=1,maximum=5),difficulty_tenths=list(type="integer",minimum=10,maximum=60)),required=list("number","position","group","difficulty_tenths"))
.ra_di_result<-list(type="object",properties=list(marks=c(.ra_di_marks,list(nullable=TRUE)),failed=list(type="boolean"),balks=list(type="integer",minimum=0,maximum=2)),required=list("marks","failed","balks"))
.ra_di_current<-function(d) {ix<-which(vapply(d$entrants,function(r) r$status=="eligible" && length(r$results)<6L,logical(1)));if(!length(ix)) return(NULL);ix[order(vapply(d$entrants[ix],function(r) length(r$results),integer(1)),ix)][1]-1L}
.ra_di_settle<-function(d) {d$phase<-if(is.null(.ra_di_current(d))) "review" else "ready";d$balks<-0L;d}
.ra_di_score<-function(result,difficulty) {if(result$failed) return(0);marks<-sort(pmax(0,unlist(result$marks)-4L*result$balks));sum(marks[3:5])*difficulty}
.ra_di_validate<-function(d) {
 if(d$phase=="setup") {if(length(d$catalog)>0 || any(lengths(d$programs)>0) || any(vapply(d$entrants,function(r) length(r$results)>0,logical(1))) || d$balks>0) .ra_fail("Initial diving contest must be empty",code="INVALID_STATE");return(invisible(TRUE))}
 if(!length(d$catalog) || anyDuplicated(vapply(d$catalog,function(x) paste(x$number,x$position,sep=":"),character(1))) || any(vapply(d$catalog,function(x) as.integer(substr(as.character(x$number),1,1))!=x$group,logical(1)))) .ra_fail("Dive catalog must use unique numbered positions and consistent groups",code="INVALID_STATE")
 for(i in 1:8) {
  program<-unlist(d$programs[[i]])
  if(length(program)!=6L || any(program>=length(d$catalog))) .ra_fail("Every entrant must declare six catalog dives",code="INVALID_STATE")
  dives<-d$catalog[program+1L]
  if(length(unique(vapply(dives,function(x) x$number,numeric(1))))!=6L || !identical(as.integer(sort(unique(vapply(dives,function(x) x$group,numeric(1))))),1:5)) .ra_fail("Program must cover all five groups without repeating a dive number",code="INVALID_STATE")
  for(result in d$entrants[[i]]$results) if(result$failed!=is.null(result$marks) || (!result$failed && result$balks==2L)) .ra_fail("Failed dives have no marks; a second balk must fail",code="INVALID_STATE")
  if(d$entrants[[i]]$status=="dns" && length(d$entrants[[i]]$results)>0) .ra_fail("DNS entrant already has competition dives",code="INVALID_STATE")
  if(d$entrants[[i]]$status=="dnf" && !length(d$entrants[[i]]$results)) .ra_fail("DNF requires a previously attempted dive",code="INVALID_STATE")
 }
 if(d$phase!="dive" && d$balks>0) .ra_fail("Balk counter exists outside an active dive",code="INVALID_STATE")
 if((d$phase %in% c("ready","dive"))!=(!is.null(.ra_di_current(d)))) .ra_fail("Contest phase and remaining dives disagree",code="INVALID_STATE")
 invisible(TRUE)
}
.ra_di_apply<-function(state,action) {
 d<-state$data;p<-action$params;k<-action$type;phase<-d$phase;i<-.ra_di_current(d)
 if(k=="declare_programs") {
  .ra_fields(p,list(catalog=list(type="array",minItems=6,maxItems=100,items=.ra_di_dive),programs=list(type="array",minItems=8,maxItems=8,items=list(type="array",minItems=6,maxItems=6,items=list(type="integer",minimum=0,maximum=99)))),"/params")
  if(phase!="setup") .ra_fail("Programs are locked when the contest starts")
  d$catalog<-p$catalog;d$programs<-p$programs;d<-.ra_di_settle(d)
 } else if(k=="start_dive") {
  .ra_fields(p,list(participant=.ra_di_index),"/params")
  if(phase!="ready" || p$participant!=i) .ra_fail("The next scheduled diver must start")
  d$phase<-"dive"
 } else if(k %in% c("balk","fail_dive","judge_dive")) {
  .ra_fields(p,if(k=="judge_dive") list(marks=.ra_di_marks) else list(),"/params")
  if(phase!="dive") .ra_fail("No announced dive is active")
  if(k=="balk" && d$balks==0L) d$balks<-1L else {
   result<-list(marks=if(k=="judge_dive") p$marks else NULL,failed=k!="judge_dive",balks=if(k=="balk") 2L else d$balks)
   d$entrants[[i+1L]]$results<-c(d$entrants[[i+1L]]$results,list(result));d<-.ra_di_settle(d)
  }
 } else if(k %in% c("withdraw","disqualify")) {
  .ra_fields(p,list(participant=.ra_di_index),"/params");j<-p$participant+1L;r<-d$entrants[[j]]
  if(r$status!="eligible") .ra_fail("Entrant is already excluded from classification")
  if(k=="withdraw") {
   if(!phase %in% c("setup","ready") || length(r$results)==6L) .ra_fail("Withdraw between scheduled dives before completing the program")
   d$entrants[[j]]$status<-if(length(r$results)) "dnf" else "dns"
  } else d$entrants[[j]]$status<-"dq"
  if(phase!="setup" && (phase!="dive" || (!is.null(i) && i==j-1L))) d<-.ra_di_settle(d)
 } else if(k=="correct_dive") {
  .ra_fields(p,list(participant=.ra_di_index,dive=list(type="integer",minimum=0,maximum=5),result=.ra_di_result),"/params");j<-p$participant+1L
  if(phase!="review" || p$dive>=length(d$entrants[[j]]$results)) .ra_fail("Correct an existing dive during result review")
  d$entrants[[j]]$results[[p$dive+1L]]<-p$result
 } else if(k=="confirm_result") {
  .ra_fields(p,list(),"/params");if(phase!="review") .ra_fail("Resolve every scheduled dive first");d$phase<-"finished"
 } else .ra_fail("Unknown diving action")
 d
}
.ra_di_legal<-function(state) {
 d<-state$data;phase<-d$phase;specs<-list();i<-.ra_di_current(d);empty<-setNames(list(),character())
 if(phase=="setup") specs$declare_programs<-list(catalog=list(type="array",minItems=6,maxItems=100,items=.ra_di_dive),programs=list(type="array",minItems=8,maxItems=8,items=list(type="array",minItems=6,maxItems=6,items=list(type="integer",minimum=0,maximum=99))))
 if(phase=="ready") specs$start_dive<-list(participant=.ra_di_index)
 if(phase=="dive") {specs$balk<-empty;specs$fail_dive<-empty;specs$judge_dive<-list(marks=.ra_di_marks)}
 if(phase %in% c("setup","ready")) specs$withdraw<-list(participant=.ra_di_index)
 if(phase!="finished") specs$disqualify<-list(participant=.ra_di_index)
 if(phase=="review") {specs$correct_dive<-list(participant=.ra_di_index,dive=list(type="integer",minimum=0,maximum=5),result=.ra_di_result);specs$confirm_result<-empty}
 list(mode="event_types",actions=list(),action_types=lapply(names(specs),function(k) list(type=k,params_schema=specs[[k]],preconditions=list("Locked program, round order, balk limit and result-review phase must permit the event."),external_observations=list("Official dive metadata and difficulty, seven final judge marks, balk or failed-dive decision."))),constraints=list(phase=phase,current_participant=i,next_dive=if(is.null(i)) NULL else length(d$entrants[[i+1L]]$results),score_unit_denominator=20L),completeness=FALSE,reason="official_diving_judgments")
}
.ra_di_outcome<-function(state,pack) {
 d<-state$data;done<-d$phase=="finished";eligible<-if(done) which(vapply(d$entrants,function(r) r$status=="eligible",logical(1))) else integer()
 scores<-vapply(eligible,function(i) sum(vapply(seq_along(d$entrants[[i]]$results),function(j) .ra_di_score(d$entrants[[i]]$results[[j]],d$catalog[[d$programs[[i]][[j]]+1L]]$difficulty_tenths),numeric(1))),numeric(1));o<-order(-scores,eligible);eligible<-eligible[o];scores<-scores[o]
 ws<-if(length(eligible)) eligible[scores==max(scores)] else integer();winners<-lapply(ws,function(i) state$players[[i]]$id)
 list(status=if(done) "terminal" else if(d$phase=="review") "awaiting_adjudication" else "ongoing",result_kind=if(done) if(length(eligible)) "ranking" else "no_contest" else "none",winners=winners,losers=if(length(winners)) lapply(setdiff(1:8,ws),function(i) state$players[[i]]$id) else list(),placements=lapply(seq_along(eligible),function(j) list(player_id=state$players[[eligible[j]]]$id,place=1L+sum(scores>scores[j]),score_units=scores[j],score_unit_denominator=20L)),reason=if(done) "dives_confirmed" else if(d$phase=="review") "result_review" else "ongoing",evidence_required=if(d$phase=="review") list("Official dive result confirmation.") else list(),rule_id=if(done) paste0(pack$ruleset_id,"@",pack$ruleset_version) else NULL)
}
