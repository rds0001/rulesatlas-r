.ra_dancesport_couple<-list(type="integer",minimum=0,maximum=5)
.ra_dancesport_eligible<-function(d) as.list(which(unlist(d$status)=="eligible")-1L)
.ra_dancesport_settle<-function(d) {d$phase<-if(length(.ra_dancesport_eligible(d))==0 || all(vapply(d$ballots,function(b) !is.null(b),logical(1)))) "review" else "judging";d}
.ra_dancesport_validate<-function(d) {
 bad<-function(m) .ra_fail(m,code="INVALID_STATE");ids<-as.integer(unlist(.ra_dancesport_eligible(d)))
 for(ballot in d$ballots) if(!is.null(ballot) && !identical(as.integer(sort(unlist(ballot))),ids)) bad("Each judge must rank every eligible couple exactly once")
 if(d$phase %in% c("setup","dancing") && any(vapply(d$ballots,function(b) !is.null(b),logical(1)))) bad("Judging follows the completed dance")
 complete<-length(ids)==0 || all(vapply(d$ballots,function(b) !is.null(b),logical(1)))
 if((d$phase=="judging" && complete) || (d$phase %in% c("review","finished") && !complete)) bad("Judging phase and available ballots disagree")
 if(d$phase=="dancing" && length(ids)==0) bad("A dance needs an eligible couple")
}
.ra_dancesport_schemas<-function(d) {
 phase<-d$phase;s<-list();empty<-setNames(list(),character());cp<-.ra_dancesport_couple;ballot<-list(judge=list(type="integer",minimum=0,maximum=4),order=list(type="array",minItems=1,maxItems=6,items=cp))
 if(phase=="setup") s<-list(start_dance=empty,withdraw=list(couple=cp))
 if(phase=="dancing") s<-list(complete_dance=empty,authorized_restart=empty,withdraw=list(couple=cp))
 if(phase=="judging") s$record_ballot<-ballot
 if(phase=="review") {s$confirm_result<-empty;if(length(.ra_dancesport_eligible(d))>0) s$correct_ballot<-ballot}
 if(phase!="finished") s$disqualify<-list(couple=cp)
 s
}
.ra_dancesport_apply<-function(state,action) {
 d<-state$data;k<-action$type;p<-action$params;s<-.ra_dancesport_schemas(d)
 if(!k %in% names(s)) .ra_fail("Action is not permitted in this phase")
 .ra_fields(p,s[[k]],"/params")
 if(k=="start_dance") d$phase<-if(length(.ra_dancesport_eligible(d))>0) "dancing" else "review" else if(k=="complete_dance") d<-.ra_dancesport_settle(d) else if(k=="authorized_restart") d$phase<-"setup" else if(k %in% c("withdraw","disqualify")) {
  i<-p$couple
  if(d$status[[i+1L]]!="eligible") .ra_fail("Only an eligible couple can receive this status")
  d$status[[i+1L]]<-if(k=="disqualify") "dq" else if(d$phase=="setup") "dns" else "dnf"
  if(d$phase %in% c("judging","review")) {d$ballots<-rep(list(NULL),5);d<-.ra_dancesport_settle(d)} else if(d$phase=="dancing" && length(.ra_dancesport_eligible(d))==0) d$phase<-"review"
 } else if(k %in% c("record_ballot","correct_ballot")) {
  j<-p$judge
  if(k=="record_ballot" && !is.null(d$ballots[[j+1L]])) .ra_fail("Judge already submitted a ballot")
  if(!identical(as.integer(sort(unlist(p$order))),as.integer(unlist(.ra_dancesport_eligible(d))))) .ra_fail("Rank each eligible couple once, without ties or omissions")
  d$ballots[j+1L]<-list(p$order);d<-.ra_dancesport_settle(d)
 } else if(k=="confirm_result") d$phase<-"finished"
 d
}
.ra_dancesport_ranking<-function(d) {
 ids<-unlist(.ra_dancesport_eligible(d));result<-list()
 if(length(ids)==0) return(result)
 ranks<-lapply(0:5,function(i) vapply(d$ballots,function(b) match(i,unlist(b)),integer(1)));remaining<-ids;column<-1L;place<-1L
 while(length(remaining)>0) {
  group<-remaining[vapply(remaining,function(i) sum(ranks[[i+1L]]<=column)>=3,logical(1))]
  if(length(group)>0) {
   key<-function(i) unlist(lapply(seq.int(column,length(ids)),function(c) c(-sum(ranks[[i+1L]]<=c),sum(ranks[[i+1L]][ranks[[i+1L]]<=c]))))
   mat<-do.call(rbind,lapply(group,function(i) c(key(i),i)));group<-group[do.call(order,as.data.frame(mat))]
   while(length(group)>0) {
    tied<-group[vapply(group,function(i) identical(key(i),key(group[[1L]])),logical(1))];mean_twice<-2L*place+length(tied)-1L
    for(i in tied) result[[length(result)+1L]]<-list(couple=i,place=place,placement_numerator=mean_twice,placement_denominator=2L)
    remaining<-remaining[!remaining %in% tied];group<-group[!group %in% tied];place<-place+length(tied)
   }
  }
  column<-column+1L
 }
 result
}
.ra_dancesport_legal<-function(state) {
 d<-state$data;s<-.ra_dancesport_schemas(d)
 list(mode="event_types",actions=list(),action_types=lapply(names(s),function(k) list(type=k,params_schema=s[[k]],preconditions=list("Dance completion, eligibility and ballot status must permit the event."),external_observations=list("Official strict judge rankings and participation decisions."))),constraints=list(phase=d$phase,eligible_couples=.ra_dancesport_eligible(d),judges_remaining=as.list(which(vapply(d$ballots,is.null,logical(1)))-1L),majority=3L),completeness=FALSE,reason="official_dance_rankings")
}
.ra_dancesport_outcome<-function(state,pack) {
 d<-state$data;done<-d$phase=="finished";rows<-if(done) .ra_dancesport_ranking(d) else list();w<-lapply(Filter(function(r) r$place==1,rows),function(r) state$players[[r$couple+1L]]$id)
 list(status=if(done) "terminal" else if(d$phase=="review") "awaiting_adjudication" else "ongoing",result_kind=if(done) {if(length(rows)>0) "ranking" else "no_contest"} else "none",winners=w,losers=if(length(w)>0) lapply(Filter(function(p) !p$id %in% unlist(w),state$players),function(p) p$id) else list(),placements=lapply(rows,function(r) c(list(player_id=state$players[[r$couple+1L]]$id),r[names(r)!="couple"])),reason=if(done) "confirmed_skating_result" else if(d$phase=="review") "result_review" else "ongoing",evidence_required=if(d$phase=="review") list("Official result confirmation.") else list(),rule_id=if(done) paste0(pack$ruleset_id,"@",pack$ruleset_version) else NULL)
}
