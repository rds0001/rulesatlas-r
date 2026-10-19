.ra_bodybuilding_rankings<-list(type="array",minItems=5,maxItems=5,items=list(type="array",minItems=8,maxItems=8,items=list(type="integer",minimum=1,maximum=8,nullable=TRUE)))
.ra_bodybuilding_schemas<-function() {e<-setNames(list(),character());ix<-list(type="integer",minimum=0,maximum=7);list(start_comparison=e,complete_comparison=e,submit_rankings=list(rankings=.ra_bodybuilding_rankings),correct_rankings=list(rankings=.ra_bodybuilding_rankings),withdraw=list(participant=ix),disqualify=list(participant=ix),confirm_result=e)}
.ra_bodybuilding_validate<-function(d) {
 bad<-function(m) .ra_fail(m,code="INVALID_STATE")
 if((d$phase %in% c("review","finished"))!=(!is.null(d$rankings))) bad("Complete judge ballots are required for result review")
 if(d$phase=="ready" && "dnf" %in% unlist(d$status)) bad("A pre-comparison withdrawal is DNS")
 if(!is.null(d$rankings)) {
  eligible<-which(unlist(d$status)=="entered")
  for(row in d$rankings) {
   if(any(vapply(1:8,function(i) (!is.null(row[[i]]))!=(i %in% eligible),logical(1))) || !identical(as.integer(sort(unlist(row[eligible]))),seq_along(eligible))) bad("Every judge must rank each eligible entrant once without ties")
  }
 };invisible(TRUE)
}
.ra_bodybuilding_exclude<-function(d,i,status) {
 d$status[[i]]<-status
 if(!is.null(d$rankings)) for(k in seq_along(d$rankings)) {
  row<-d$rankings[[k]];old<-row[[i]];row[i]<-list(NULL)
  if(!is.null(old)) for(j in seq_along(row)) if(!is.null(row[[j]]) && row[[j]]>old) row[[j]]<-row[[j]]-1L
  d$rankings[[k]]<-row
 };d
}
.ra_bodybuilding_apply<-function(state,action) {
 d<-state$data;p<-action$params;k<-action$type;phase<-d$phase;s<-.ra_bodybuilding_schemas()
 if(!(k %in% names(s))) .ra_fail("Unknown comparison-final event")
 .ra_fields(p,s[[k]],"/params")
 if(k=="start_comparison") {if(phase!="ready") .ra_fail("Comparison has already started");d$phase<-"comparing"} else if(k=="complete_comparison") {if(phase!="comparing") .ra_fail("No comparison is active");d$phase<-"judging"} else if(k %in% c("submit_rankings","correct_rankings")) {
  if(phase!=(if(k=="submit_rankings") "judging" else "review")) .ra_fail("Judge ballots require the proper scoring or review phase")
  d$rankings<-p$rankings;d$phase<-"review"
 } else if(k=="withdraw") {
  i<-p$participant+1L
  if(!(phase %in% c("ready","comparing")) || d$status[[i]]!="entered") .ra_fail("Withdrawal requires an unresolved entrant")
  d<-.ra_bodybuilding_exclude(d,i,if(phase=="ready") "dns" else "dnf")
 } else if(k=="disqualify") {
  i<-p$participant+1L;if(d$status[[i]]=="dq") .ra_fail("Entrant is already disqualified");d<-.ra_bodybuilding_exclude(d,i,"dq")
 } else if(k=="confirm_result") {if(phase!="review") .ra_fail("Complete and review all judge ballots first");d$phase<-"finished"};d
}
.ra_bodybuilding_total<-function(d,i) sum(sort(vapply(d$rankings,function(row) row[[i]],numeric(1)))[2:4])
.ra_bodybuilding_legal<-function(state) {
 d<-state$data;names<-switch(d$phase,ready=c("start_comparison","withdraw"),comparing=c("complete_comparison","withdraw"),judging="submit_rankings",review=c("correct_rankings","confirm_result"),character())
 if(d$phase!="finished") names<-c(names,"disqualify")
 s<-.ra_bodybuilding_schemas()
 list(mode="event_types",actions=list(),action_types=lapply(names,function(k) list(type=k,params_schema=s[[k]],preconditions=list("Comparison completion, eligibility and complete judge permutations must permit the event."),external_observations=list("Five final official placement ballots or an eligibility ruling."))),constraints=list(phase=d$phase,eligible=as.list(which(unlist(d$status)=="entered")-1L),trim_each_end=1L),completeness=FALSE,reason="official_comparison_judgments")
}
.ra_bodybuilding_outcome<-function(state,pack) {
 d<-state$data;done<-d$phase=="finished";eligible<-if(done) which(unlist(d$status)=="entered") else integer();totals<-vapply(eligible,function(i) .ra_bodybuilding_total(d,i),numeric(1));o<-order(totals,eligible);eligible<-eligible[o];totals<-totals[o]
 ws<-if(length(eligible)>0) eligible[totals==min(totals)] else integer();winners<-lapply(ws,function(i) state$players[[i]]$id)
 list(status=if(done) "terminal" else if(d$phase=="review") "awaiting_adjudication" else "ongoing",result_kind=if(done) {if(length(eligible)>0) "ranking" else "no_contest"} else "none",winners=winners,losers=if(length(winners)>0) lapply(setdiff(1:8,ws),function(i) state$players[[i]]$id) else list(),placements=lapply(seq_along(eligible),function(j) list(player_id=state$players[[eligible[j]]]$id,place=1L+sum(totals<totals[j]),judge_place_sum=totals[j])),reason=if(done) "comparison_final_confirmed" else "ongoing",evidence_required=list(),rule_id=if(done) paste0(pack$ruleset_id,"@",pack$ruleset_version) else NULL)
}
