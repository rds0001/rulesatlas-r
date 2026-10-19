.ra_dr_index<-list(type="integer",minimum=0,maximum=7)
.ra_dr_marks<-list(type="array",minItems=5,maxItems=5,items=list(type="integer",minimum=0,maximum=20))
.ra_dr_test<-list(type="array",minItems=1,maxItems=40,items=list(type="object",properties=list(name=list(type="string",minLength=1,maxLength=80),coefficient=list(type="integer",minimum=1,maximum=2),collective=list(type="boolean")),required=list("name","coefficient","collective")))
.ra_dr_current<-function(d) {ix<-which(vapply(d$entrants,function(r) r$status=="eligible" && length(r$marks)<length(d$test),logical(1)));if(length(ix)) ix[1]-1L else NULL}
.ra_dr_settle<-function(d) {d$phase<-if(is.null(.ra_dr_current(d))) "review" else "ready";d}
.ra_dr_totals<-function(d,r) {
 weighted<-0;collective<-0
 for(j in seq_along(r$marks)) {value<-sum(unlist(r$marks[[j]]))*d$test[[j]]$coefficient;weighted<-weighted+value;if(d$test[[j]]$collective) collective<-collective+value}
 denominator<-5L*20L*sum(vapply(d$test,function(e) e$coefficient,numeric(1)));penalty<-r$penalty_basis_points+200L*as.integer(r$errors>=1L)
 c(10000*weighted-penalty*denominator,collective,denominator)
}
.ra_dr_validate<-function(d) {
 if(d$phase=="setup") {if(length(d$test)>0 || any(vapply(d$entrants,function(r) length(r$marks)>0 || r$errors>0 || r$penalty_basis_points>0,logical(1)))) .ra_fail("Initial test must have no scored movements",code="INVALID_STATE");return(invisible(TRUE))}
 if(!length(d$test)) .ra_fail("A prescribed test is required",code="INVALID_STATE")
 for(r in d$entrants) {
  if(length(r$marks)>length(d$test)) .ra_fail("Too many scored test elements",code="INVALID_STATE")
  if(r$errors==2L && !r$status %in% c("eliminated","dq")) .ra_fail("Second course error eliminates the entrant",code="INVALID_STATE")
  if(r$status=="dns" && (length(r$marks)>0 || r$errors>0 || r$penalty_basis_points>0)) .ra_fail("DNS entrant already has test history",code="INVALID_STATE")
  if(r$status=="eligible" && r$errors>0 && !length(r$marks) && d$phase!="test") .ra_fail("Course-error history requires an active or completed test",code="INVALID_STATE")
 }
 if((d$phase %in% c("ready","test"))!=(!is.null(.ra_dr_current(d)))) .ra_fail("Phase and incomplete test sheets disagree",code="INVALID_STATE")
 invisible(TRUE)
}
.ra_dr_apply<-function(state,action) {
 d<-state$data;p<-action$params;k<-action$type;phase<-d$phase;i<-.ra_dr_current(d)
 if(k=="configure_test") {
  .ra_fields(p,list(test=.ra_dr_test),"/params");if(phase!="setup") .ra_fail("The prescribed test is locked after configuration")
  d$test<-p$test;d<-.ra_dr_settle(d)
 } else if(k=="start_test") {
  .ra_fields(p,list(participant=.ra_dr_index),"/params");if(phase!="ready" || p$participant!=i) .ra_fail("Start the next eligible entrant in the declared order");d$phase<-"test"
 } else if(k=="score_movement") {
  .ra_fields(p,list(movement=list(type="integer",minimum=0,maximum=39),marks=.ra_dr_marks),"/params")
  if(phase!="test" || p$movement!=length(d$entrants[[i+1L]]$marks)) .ra_fail("Enter all five marks for the next test element")
  d$entrants[[i+1L]]$marks<-c(d$entrants[[i+1L]]$marks,list(p$marks));if(length(d$entrants[[i+1L]]$marks)==length(d$test)) d<-.ra_dr_settle(d)
 } else if(k=="course_error") {
  .ra_fields(p,list(),"/params");if(phase!="test") .ra_fail("Course error requires an active test")
  d$entrants[[i+1L]]$errors<-d$entrants[[i+1L]]$errors+1L
  if(d$entrants[[i+1L]]$errors==2L) {d$entrants[[i+1L]]$status<-"eliminated";d<-.ra_dr_settle(d)}
 } else if(k %in% c("assess_penalty","correct_penalty")) {
  .ra_fields(p,list(participant=.ra_dr_index,basis_points=list(type="integer",minimum=if(k=="assess_penalty") 1 else 0,maximum=10000)),"/params");j<-p$participant+1L;r<-d$entrants[[j]]
  if(r$status!="eligible" || (k=="correct_penalty" && phase!="review") || (k=="assess_penalty" && !(phase=="review" || (phase=="test" && p$participant==i)))) .ra_fail("Penalty assessment requires the active test or official result review")
  value<-if(k=="assess_penalty") r$penalty_basis_points+p$basis_points else p$basis_points
  if(value>10000) .ra_fail("Technical penalty exceeds the supported bound")
  d$entrants[[j]]$penalty_basis_points<-value
 } else if(k=="correct_errors") {
  .ra_fields(p,list(participant=.ra_dr_index,errors=list(type="integer",minimum=0,maximum=1)),"/params")
  if(phase!="review" || d$entrants[[p$participant+1L]]$status!="eligible") .ra_fail("Correct a classified entrant course-error count during review")
  d$entrants[[p$participant+1L]]$errors<-p$errors
 } else if(k=="correct_mark") {
  .ra_fields(p,list(participant=.ra_dr_index,movement=list(type="integer",minimum=0,maximum=39),marks=.ra_dr_marks),"/params");j<-p$participant+1L
  if(phase!="review" || p$movement>=length(d$entrants[[j]]$marks)) .ra_fail("Correct an existing mark row during result review")
  d$entrants[[j]]$marks[[p$movement+1L]]<-p$marks
 } else if(k %in% c("withdraw","eliminate","disqualify")) {
  .ra_fields(p,list(participant=.ra_dr_index),"/params");j<-p$participant+1L;r<-d$entrants[[j]]
  if(r$status!="eligible") .ra_fail("Entrant is already excluded")
  if(k=="withdraw") {
   if(!phase %in% c("setup","ready") || length(r$marks)>0) .ra_fail("Withdraw as DNS before starting the test")
   d$entrants[[j]]$status<-"dns"
  } else if(k=="eliminate") {
   if(phase!="test" || p$participant!=i) .ra_fail("Only the active test can be officially eliminated")
   d$entrants[[j]]$status<-"eliminated"
  } else d$entrants[[j]]$status<-"dq"
  if(phase!="setup" && (phase!="test" || (!is.null(i) && i==j-1L))) d<-.ra_dr_settle(d)
 } else if(k=="confirm_result") {
  .ra_fields(p,list(),"/params");if(phase!="review") .ra_fail("Resolve every entrant before confirmation");d$phase<-"finished"
 } else .ra_fail("Unknown dressage action")
 d
}
.ra_dr_legal<-function(state) {
 d<-state$data;phase<-d$phase;i<-.ra_dr_current(d);specs<-list();empty<-setNames(list(),character())
 if(phase=="setup") specs$configure_test<-list(test=.ra_dr_test)
 if(phase=="ready") specs$start_test<-list(participant=.ra_dr_index)
 if(phase=="test") {specs$score_movement<-list(movement=list(type="integer",minimum=0,maximum=39),marks=.ra_dr_marks);specs$course_error<-empty;specs$eliminate<-list(participant=.ra_dr_index)}
 if(phase %in% c("test","review")) specs$assess_penalty<-list(participant=.ra_dr_index,basis_points=list(type="integer",minimum=1,maximum=10000))
 if(phase %in% c("setup","ready")) specs$withdraw<-list(participant=.ra_dr_index)
 if(phase!="finished") specs$disqualify<-list(participant=.ra_dr_index)
 if(phase=="review") {specs$correct_errors<-list(participant=.ra_dr_index,errors=list(type="integer",minimum=0,maximum=1));specs$correct_mark<-list(participant=.ra_dr_index,movement=list(type="integer",minimum=0,maximum=39),marks=.ra_dr_marks);specs$correct_penalty<-list(participant=.ra_dr_index,basis_points=list(type="integer",minimum=0,maximum=10000));specs$confirm_result<-empty}
 list(mode="event_types",actions=list(),action_types=lapply(names(specs),function(k) list(type=k,params_schema=specs[[k]],preconditions=list("Prescribed test order, judge count, course-error limit and review phase must permit the decision."),external_observations=list("Official test coefficients, five judge marks, technical deduction or elimination."))),constraints=list(phase=phase,current_participant=i,next_movement=if(is.null(i)) NULL else length(d$entrants[[i+1L]]$marks)),completeness=FALSE,reason="official_dressage_judgments")
}
.ra_dr_outcome<-function(state,pack) {
 d<-state$data;done<-d$phase=="finished";eligible<-if(done) which(vapply(d$entrants,function(r) r$status=="eligible",logical(1))) else integer();values<-lapply(d$entrants[eligible],function(r) .ra_dr_totals(d,r))
 places<-vapply(values,function(k) 1L+sum(vapply(values,function(other) .ra_ps_compare(other[1:2],k[1:2])>0,logical(1))),integer(1));o<-order(places,eligible);eligible<-eligible[o];values<-values[o];places<-places[o];winners<-lapply(eligible[places==1L],function(i) state$players[[i]]$id)
 list(status=if(done) "terminal" else if(d$phase=="review") "awaiting_adjudication" else "ongoing",result_kind=if(done) if(length(eligible)) "ranking" else "no_contest" else "none",winners=winners,losers=if(length(winners)) lapply(Filter(function(i) !state$players[[i]]$id %in% unlist(winners),1:8),function(i) state$players[[i]]$id) else list(),placements=lapply(seq_along(eligible),function(j) list(player_id=state$players[[eligible[j]]]$id,place=places[j],basis_points_numerator=values[[j]][1],denominator=values[[j]][3],collective_half_points=values[[j]][2])),reason=if(done) "test_confirmed" else if(d$phase=="review") "result_review" else "ongoing",evidence_required=if(d$phase=="review") list("Official test confirmation.") else list(),rule_id=if(done) paste0(pack$ruleset_id,"@",pack$ruleset_version) else NULL)
}
