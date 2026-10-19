.ra_sailing_i<-function(a,b,nullable=FALSE) {x<-list(type="integer",minimum=a,maximum=b);if(nullable) x$nullable<-TRUE;x}
.ra_sailing_entry<-list(type="object",properties=list(status=list(type="enum",values=as.list(c("finished","dns","dnf","ret","ocs","ufd","bfd","dsq","dnc","dne","redress"))),order=.ra_sailing_i(1,8,TRUE),redress_numerator=.ra_sailing_i(1,500,TRUE),redress_denominator=.ra_sailing_i(1,5,TRUE)),required=as.list(c("status","order","redress_numerator","redress_denominator")))
.ra_sailing_result<-list(type="array",minItems=8,maxItems=8,items=.ra_sailing_entry)
.ra_sailing_schemas<-list(start_race=setNames(list(),character()),abandon_race=setNames(list(),character()),submit_result=list(results=.ra_sailing_result),amend_result=list(results=.ra_sailing_result),confirm_race=setNames(list(),character()))
.ra_sailing_valid_result<-function(row) {
 for(r in row) {
  if((r$status=="finished" && is.null(r$order)) || (!(r$status %in% c("finished","redress")) && !is.null(r$order))) return(FALSE)
  red<-r$status=="redress"
  if(red!=(!is.null(r$redress_numerator) && !is.null(r$redress_denominator))) return(FALSE)
  if(!red && (!is.null(r$redress_numerator) || !is.null(r$redress_denominator))) return(FALSE)
  if(red && r$redress_numerator<r$redress_denominator) return(FALSE)
 }
 TRUE
}
.ra_sailing_points<-function(row) lapply(row,function(r) {
 if(r$status=="finished") {
  before<-sum(vapply(row,function(x) x$status %in% c("finished","redress") && !is.null(x$order) && x$order<r$order,logical(1)));tied<-sum(vapply(row,function(x) x$status %in% c("finished","redress") && !is.null(x$order) && x$order==r$order,logical(1)))
  (2*before+tied+1)*30
 } else if(r$status=="redress") r$redress_numerator*(60/r$redress_denominator) else 540
})
.ra_sailing_key<-function(d,i) {
 scores<-vapply(d$races,function(row) .ra_sailing_points(row)[[i]],numeric(1));eligible<-which(vapply(d$races,function(row) row[[i]]$status!="dne",logical(1)))
 discard<-if(length(eligible)) eligible[which.max(scores[eligible])] else NULL
 kept<-if(is.null(discard)) scores else scores[-discard]
 list(key=c(sum(kept),sort(kept),1000000,rev(scores)),discard=if(is.null(discard)) NULL else discard-1L,scores=scores)
}
.ra_sailing_validate<-function(d) {
 if(any(!vapply(d$races,.ra_sailing_valid_result,logical(1))) || (!is.null(d$pending) && !.ra_sailing_valid_result(d$pending))) .ra_fail("Official result fields are inconsistent",code="INVALID_STATE")
 if((d$phase=="review")!=!is.null(d$pending)) .ra_fail("Pending race result requires review",code="INVALID_STATE")
 if((d$phase=="finished")!=(length(d$races)==5)) .ra_fail("The series ends after five confirmed races",code="INVALID_STATE")
}
.ra_sailing_available<-function(d) switch(d$phase,ready="start_race",racing=c("abandon_race","submit_result"),review=c("abandon_race","amend_result","confirm_race"),character())
.ra_sailing_apply<-function(state,action) {
 d<-state$data;k<-action$type;p<-action$params
 if(!k %in% .ra_sailing_available(d)) .ra_fail("Action is not permitted in this race phase")
 .ra_fields(p,.ra_sailing_schemas[[k]],"/params")
 if(k=="start_race") d$phase<-"racing" else if(k=="abandon_race") {d$phase<-"ready";d["pending"]<-list(NULL)} else if(k %in% c("submit_result","amend_result")) {
  if(!.ra_sailing_valid_result(p$results)) .ra_fail("Finishers require order; redress may retain an occupied finish place and requires an explicit rational score")
  d$pending<-p$results;d$phase<-"review"
 } else if(k=="confirm_race") {d$races[[length(d$races)+1L]]<-d$pending;d["pending"]<-list(NULL);d$phase<-if(length(d$races)==5) "finished" else "ready"}
 d
}
.ra_sailing_legal<-function(state) {
 d<-state$data
 list(mode="event_types",actions=list(),action_types=lapply(.ra_sailing_available(d),function(k) list(type=k,params_schema=.ra_sailing_schemas[[k]],preconditions=list("A race must start, receive an official result and pass review before counting."),external_observations=list("Official finish order, exclusions, abandonment and redress decisions."))),constraints=list(phase=d$phase,confirmed_races=length(d$races),score_unit_denominator=60,provisional_points_units=if(!is.null(d$pending)) .ra_sailing_points(d$pending) else NULL),completeness=FALSE,reason="official_low_point_series")
}
.ra_sailing_outcome<-function(state,pack) {
 d<-state$data;done<-d$phase=="finished";ranked<-list()
 if(done) {
  ranked<-lapply(1:8,function(i) c(list(i=i),.ra_sailing_key(d,i)))
  for(j in 2:8) {a<-j;while(a>1 && .ra_sailing_compare(ranked[[a]]$key,ranked[[a-1L]]$key)<0) {tmp<-ranked[[a]];ranked[[a]]<-ranked[[a-1L]];ranked[[a-1L]]<-tmp;a<-a-1L}}
 }
 winners<-lapply(Filter(function(r) .ra_sailing_compare(r$key,ranked[[1L]]$key)==0,ranked),function(r) state$players[[r$i]]$id)
 list(status=if(done) "terminal" else if(d$phase=="review") "awaiting_adjudication" else "ongoing",result_kind=if(done) "ranking" else "none",winners=winners,losers=if(done) lapply(Filter(function(p) !p$id %in% unlist(winners),state$players),function(p) p$id) else list(),placements=lapply(ranked,function(r) list(player_id=state$players[[r$i]]$id,place=1L+sum(vapply(ranked,function(other) .ra_sailing_compare(other$key,r$key)<0,logical(1))),points_units=r$key[1L],score_unit_denominator=60,discarded_race=r$discard)),reason=if(done) "series_confirmed" else if(d$phase=="review") "race_review" else "ongoing",evidence_required=if(d$phase=="review") list("Official race confirmation.") else list(),rule_id=if(done) paste0(pack$ruleset_id,"@",pack$ruleset_version) else NULL)
}

.ra_sailing_compare<-function(a,b) {n<-min(length(a),length(b));ix<-which(a[seq_len(n)]!=b[seq_len(n)]);if(length(ix)) sign(a[ix[1]]-b[ix[1]]) else sign(length(a)-length(b))}
