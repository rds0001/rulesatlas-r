.ra_surfing_surfer<-list(type="integer",minimum=0,maximum=3)
.ra_surfing_wave<-list(type="integer",minimum=0,maximum=39)
.ra_surfing_marks<-list(type="array",minItems=5,maxItems=5,items=list(type="integer",minimum=0,maximum=1000))
.ra_surfing_pending<-function(d,i) length(d$waves[[i+1L]])>0 && is.null(tail(d$waves[[i+1L]],1)[[1L]])
.ra_surfing_dq<-function(d,i) d$status[[i+1L]]=="dq" || sum(vapply(d$interferences[[i+1L]],function(p) p$kind=="priority",logical(1)))==2
.ra_surfing_can<-function(d,i) d$status[[i+1L]]=="eligible" && length(d$interferences[[i+1L]])<2 && length(d$waves[[i+1L]])<d$wave_limit+d$extra_waves[[i+1L]]
.ra_surfing_settle<-function(d) {
 if(d$remaining_ms==0 && d$phase!="finished") d$phase<-if(any(vapply(0:3,function(i) .ra_surfing_pending(d,i),logical(1)))) "closing" else "review"
 d$priority<-Filter(function(i) .ra_surfing_can(d,i) && !.ra_surfing_pending(d,i),d$priority);d
}
.ra_surfing_values<-function(d,i,all_judges=FALSE) {
 zero<-unlist(lapply(d$interferences[[i+1L]],function(p) p$wave));scores<-numeric()
 for(j in seq_along(d$waves[[i+1L]])) {
  m<-d$waves[[i+1L]][[j]]
  if(!is.null(m)) scores<-c(scores,if((j-1L) %in% zero) 0 else if(all_judges) floor((sum(unlist(m))+2)/5) else floor((sum(sort(unlist(m))[2:4])+1)/3))
 }
 scores<-c(sort(scores,decreasing=TRUE),rep(0,40))[1:40];ps<-d$interferences[[i+1L]]
 if(length(ps)==1) scores[2]<-if(ps[[1]]$kind=="non_priority") floor((scores[2]+1)/2) else 0 else if(length(ps)==2) {
  scores[1]<-floor((scores[1]+1)/2);scores[2]<-if(all(vapply(ps,function(p) p$kind=="non_priority",logical(1)))) floor((scores[2]+1)/2) else 0
 }
 scores
}
.ra_surfing_key<-function(d,i) {
 result<-numeric()
 for(all_judges in c(FALSE,TRUE)) {
  s<-.ra_surfing_values(d,i,all_judges);result<-c(result,sum(s[1:2]),s[1],vapply(3:40,function(n) sum(s[seq_len(n)]),numeric(1)))
 }
 result
}
.ra_surfing_validate<-function(d) {
 bad<-function(m) .ra_fail(m,code="INVALID_STATE")
 for(i in 0:3) {
  ws<-d$waves[[i+1L]]
  if(length(ws)>d$wave_limit+d$extra_waves[[i+1L]] || (length(ws)>1 && any(vapply(head(ws,-1),is.null,logical(1))))) bad("Wave allowance or unfinished-wave order is invalid")
  if(.ra_surfing_pending(d,i) && d$status[[i+1L]] %in% c("dns","withdrawn")) bad("Resolve a started wave before withdrawal")
  if(d$status[[i+1L]]=="dns" && length(ws)>0) bad("A non-starter cannot have rides")
  for(p in d$interferences[[i+1L]]) if(!is.null(p$wave) && p$wave>=length(ws)) bad("Interference must refer to an existing ride or a paddling incident")
 }
 if(anyDuplicated(unlist(d$priority)) || any(vapply(d$priority,function(i) !.ra_surfing_can(d,i) || .ra_surfing_pending(d,i),logical(1)))) bad("Priority order contains an ineligible or duplicate surfer")
 if(d$phase=="setup" && (any(lengths(d$waves)>0) || any(lengths(d$interferences)>0) || any(unlist(d$extra_waves)!=0) || length(d$priority)>0 || d$remaining_ms!=d$duration_ms)) bad("Setup precedes rides, penalties and elapsed heat time")
 if(d$remaining_ms>d$duration_ms) bad("Remaining heat time exceeds configured duration")
 if(d$phase %in% c("live","paused") && d$remaining_ms==0) bad("Expired heat must enter closing or review")
 if(d$phase %in% c("closing","review","finished") && d$remaining_ms!=0) bad("Closed heat cannot retain time")
 pp<-any(vapply(0:3,function(i) .ra_surfing_pending(d,i),logical(1)))
 if((d$phase=="closing" && !pp) || (d$phase %in% c("review","finished") && pp)) bad("Pending ride marks and heat phase disagree")
}
.ra_surfing_schemas<-function(d) {
 phase<-d$phase;s<-list();empty<-setNames(list(),character());surfer<-.ra_surfing_surfer;wave<-.ra_surfing_wave;marks<-.ra_surfing_marks;kind<-list(type="enum",values=list("non_priority","priority"))
 if(phase=="finished") return(s)
 if(phase=="setup") return(list(configure=list(duration_ms=list(type="integer",minimum=600000,maximum=2400000),wave_limit=list(type="integer",minimum=1,maximum=20)),start_heat=empty,withdraw=list(surfer=surfer),disqualify=list(surfer=surfer)))
 if(phase=="live") s<-list(start_wave=list(surfer=surfer),elapse=list(milliseconds=list(type="integer",minimum=1,maximum=2400000)),pause_heat=empty)
 if(phase=="paused") s$resume_heat<-empty
 if(phase %in% c("live","paused")) s<-c(s,list(set_priority=list(order=list(type="array",minItems=0,maxItems=4,items=surfer)),grant_extra_wave=list(surfer=surfer),end_heat=empty,withdraw=list(surfer=surfer)))
 if(phase %in% c("live","paused","closing")) s$complete_wave<-list(surfer=surfer,marks=marks)
 if(phase=="review") s<-c(s,list(correct_marks=list(surfer=surfer,wave=wave,marks=marks),confirm_result=empty,correct_interference=list(surfer=surfer,index=list(type="integer",minimum=0,maximum=1),kind=c(kind,list(nullable=TRUE)))))
 c(s,list(interference=list(surfer=surfer,kind=kind,wave=c(wave,list(nullable=TRUE))),disqualify=list(surfer=surfer)))
}
.ra_surfing_apply<-function(state,action) {
 d<-state$data;k<-action$type;p<-action$params;s<-.ra_surfing_schemas(d)
 if(!k %in% names(s)) .ra_fail("Action is not permitted in this phase")
 .ra_fields(p,s[[k]],"/params");i<-p$surfer
 if(k=="configure") {d$duration_ms<-p$duration_ms;d$remaining_ms<-p$duration_ms;d$wave_limit<-p$wave_limit} else if(k=="start_heat") d$phase<-"live" else if(k=="start_wave") {
  if(!.ra_surfing_can(d,i) || .ra_surfing_pending(d,i)) .ra_fail("Surfer is unavailable, already riding or has used the wave allowance")
  d$waves[[i+1L]][length(d$waves[[i+1L]])+1L]<-list(NULL);d$priority<-Filter(function(j) j!=i,d$priority)
 } else if(k=="complete_wave") {
  if(!.ra_surfing_pending(d,i)) .ra_fail("A wave must begin before its marks are entered")
  d$waves[[i+1L]][length(d$waves[[i+1L]])]<-list(p$marks)
 } else if(k=="correct_marks") {
  if(p$wave>=length(d$waves[[i+1L]])) .ra_fail("Correct an existing ride only")
  d$waves[[i+1L]][p$wave+1L]<-list(p$marks)
 } else if(k=="elapse") {
  if(p$milliseconds>d$remaining_ms) .ra_fail("Heat clock cannot pass zero")
  d$remaining_ms<-d$remaining_ms-p$milliseconds
 } else if(k=="pause_heat") d$phase<-"paused" else if(k=="resume_heat") d$phase<-"live" else if(k=="set_priority") {
  if(anyDuplicated(unlist(p$order)) || any(vapply(p$order,function(j) !.ra_surfing_can(d,j) || .ra_surfing_pending(d,j),logical(1)))) .ra_fail("Priority must list distinct available surfers")
  d$priority<-p$order
 } else if(k=="grant_extra_wave") {
  if(d$status[[i+1L]]!="eligible" || length(d$interferences[[i+1L]])==2 || d$extra_waves[[i+1L]]>=20) .ra_fail("An additional official wave requires an eligible surfer")
  d$extra_waves[[i+1L]]<-d$extra_waves[[i+1L]]+1L
 } else if(k=="end_heat") d$remaining_ms<-0L else if(k %in% c("withdraw","disqualify")) {
  if(d$status[[i+1L]] %in% c("dns","dq") || (k=="withdraw" && d$status[[i+1L]]!="eligible")) .ra_fail("Surfer cannot receive this status now")
  if(k=="withdraw" && .ra_surfing_pending(d,i)) .ra_fail("Resolve the started ride before withdrawal")
  d$status[[i+1L]]<-if(k=="disqualify") "dq" else if(length(d$waves[[i+1L]])>0) "withdrawn" else "dns"
 } else if(k=="interference") {
  if(d$status[[i+1L]] %in% c("dns","dq") || length(d$interferences[[i+1L]])==2) .ra_fail("A surfer can receive at most two interference calls")
  if(!is.null(p$wave) && p$wave>=length(d$waves[[i+1L]])) .ra_fail("The interference ride must already exist")
  d$interferences[[i+1L]][[length(d$interferences[[i+1L]])+1L]]<-list(kind=p$kind,wave=p$wave);d$priority<-Filter(function(j) j!=i,d$priority)
 } else if(k=="correct_interference") {
  if(p$index>=length(d$interferences[[i+1L]])) .ra_fail("Correct an existing interference call only")
  if(is.null(p$kind)) d$interferences[[i+1L]]<-d$interferences[[i+1L]][-(p$index+1L)] else d$interferences[[i+1L]][[p$index+1L]]$kind<-p$kind
 } else if(k=="confirm_result") d$phase<-"finished"
 .ra_surfing_settle(d)
}
.ra_surfing_legal<-function(state) {
 d<-state$data;s<-.ra_surfing_schemas(d)
 list(mode="event_types",actions=list(),action_types=lapply(names(s),function(k) list(type=k,params_schema=s[[k]],preconditions=list("Clock, wave allowance, eligibility and judging phase must permit the event."),external_observations=list("Official ride starts, five judge marks, priority order and interference calls."))),constraints=list(phase=d$phase,remaining_ms=d$remaining_ms,priority=d$priority,wave_allowance_remaining=lapply(0:3,function(i) d$wave_limit+d$extra_waves[[i+1L]]-length(d$waves[[i+1L]])),score_unit_denominator=100),completeness=FALSE,reason="official_surfing_heat_events")
}
.ra_surfing_outcome<-function(state,pack) {
 d<-state$data;done<-d$phase=="finished";ranked<-list()
 if(done) for(i in 0:3) if(d$status[[i+1L]]!="dns" && !.ra_surfing_dq(d,i)) ranked[[length(ranked)+1L]]<-list(key=.ra_surfing_key(d,i),i=i)
 if(length(ranked)>0) {mat<-do.call(rbind,lapply(ranked,function(r) c(-r$key,r$i)));ranked<-ranked[do.call(order,as.data.frame(mat))]}
 winners<-lapply(Filter(function(r) all(r$key==ranked[[1L]]$key),ranked),function(r) state$players[[r$i+1L]]$id)
 placements<-lapply(ranked,function(r) list(player_id=state$players[[r$i+1L]]$id,place=1L+sum(vapply(ranked,function(other) .ra_ps_compare(other$key,r$key)>0,logical(1))),score_units=r$key[1],score_unit_denominator=100))
 list(status=if(done) "terminal" else if(d$phase=="review") "awaiting_adjudication" else "ongoing",result_kind=if(done) {if(length(ranked)>0) "ranking" else "no_contest"} else "none",winners=winners,losers=if(length(winners)>0) lapply(Filter(function(p) !p$id %in% unlist(winners),state$players),function(p) p$id) else list(),placements=placements,reason=if(done) "confirmed_heat" else if(d$phase=="review") "result_review" else "ongoing",evidence_required=if(d$phase=="review") list("Official result confirmation.") else list(),rule_id=if(done) paste0(pack$ruleset_id,"@",pack$ruleset_version) else NULL)
}
