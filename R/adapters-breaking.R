.ra_break_i<-function(a,b) list(type="integer",minimum=a,maximum=b)
.ra_break_votes<-list(type="array",minItems=9,maxItems=9,items=.ra_break_i(0,1))
.ra_break_schemas<-list(start_round=list(first=.ra_break_i(0,1)),start_performance=list(player=.ra_break_i(0,1)),complete_performance=setNames(list(),character()),record_votes=list(votes=.ra_break_votes),correct_vote=list(judge=.ra_break_i(0,8),player=.ra_break_i(0,1)),confirm_round=setNames(list(),character()),forfeit=list(player=.ra_break_i(0,1)),disqualify=list(player=.ra_break_i(0,1)))
.ra_break_winner<-function(v) if(sum(unlist(v))>=5) 1L else 0L
.ra_break_counts<-function(d) lapply(0:1,function(i) sum(vapply(d$rounds,function(v) .ra_break_winner(v)==i,logical(1))))
.ra_break_validate<-function(d) {
 bad<-function(m) .ra_fail(m,code="INVALID_STATE");wins<-unlist(.ra_break_counts(d));phase<-d$phase
 if(max(wins)>2) bad("No round may follow a second round win")
 if(length(d$rounds)>1) for(i in 1:(length(d$rounds)-1L)) if(max(vapply(0:1,function(j) sum(vapply(d$rounds[seq_len(i)],function(v) .ra_break_winner(v)==j,logical(1))),integer(1)))>=2) bad("No round may follow a second round win")
 if(phase=="setup" && (!is.null(d$first) || length(d$completed)>0 || !is.null(d$votes))) bad("New round must await its first performer")
 if(!phase %in% c("setup","finished")) {
  if(is.null(d$first) || !identical(as.integer(unlist(d$completed)),as.integer(c(d$first,1L-d$first)[seq_len(length(d$completed))]))) bad("Performance order is inconsistent")
  if((phase %in% c("ready","performing") && length(d$completed)>=2) || (phase %in% c("judging","round_review") && length(d$completed)!=2)) bad("Judging requires both completed performances")
  if((phase=="round_review")!=!is.null(d$votes)) bad("Votes require round review")
 }
 if(phase=="finished") {
  if(is.null(d$winner) || is.null(d$reason)) bad("Finished battle needs its result")
  if(d$reason=="two_rounds" && (wins[d$winner+1L]!=2 || !is.null(d$first) || length(d$completed)>0 || !is.null(d$votes))) bad("Match victory requires two confirmed round wins")
 } else if(max(wins)>=2 || !is.null(d$winner) || !is.null(d$reason)) bad("Winning battle must be terminal")
}
.ra_break_available<-function(d) c(switch(d$phase,setup="start_round",ready="start_performance",performing="complete_performance",judging="record_votes",round_review=c("correct_vote","confirm_round"),character()),if(d$phase!="finished") c("forfeit","disqualify") else character())
.ra_break_apply<-function(state,action) {
 d<-state$data;k<-action$type;p<-action$params
 if(!k %in% .ra_break_available(d)) .ra_fail("Action is not permitted in this battle phase")
 .ra_fields(p,.ra_break_schemas[[k]],"/params")
 if(k=="start_round") {d$first<-p$first;d$phase<-"ready"} else if(k=="start_performance") {
  expected<-if(!length(d$completed)) d$first else 1L-d$first
  if(p$player!=expected) .ra_fail("Only the scheduled breaker may perform")
  d$phase<-"performing"
 } else if(k=="complete_performance") {
  d$completed[[length(d$completed)+1L]]<-if(!length(d$completed)) d$first else 1L-d$first;d$phase<-if(length(d$completed)==2) "judging" else "ready"
 } else if(k=="record_votes") {d$votes<-p$votes;d$phase<-"round_review"} else if(k=="correct_vote") d$votes[[p$judge+1L]]<-p$player else if(k=="confirm_round") {
  d$rounds[[length(d$rounds)+1L]]<-d$votes;d["first"]<-list(NULL);d$completed<-list();d["votes"]<-list(NULL);d$phase<-"setup"
  w<-.ra_break_winner(d$rounds[[length(d$rounds)]])
  if(.ra_break_counts(d)[[w+1L]]==2) {d$phase<-"finished";d$winner<-w;d$reason<-"two_rounds"}
 } else if(k %in% c("forfeit","disqualify")) {d$phase<-"finished";d$winner<-1L-p$player;d$reason<-k}
 d
}
.ra_break_legal<-function(state) {
 d<-state$data;next_player<-if(is.null(d$first) || length(d$completed)==2) NULL else if(!length(d$completed)) d$first else 1L-d$first
 list(mode="event_types",actions=list(),action_types=lapply(.ra_break_available(d),function(k) list(type=k,params_schema=.ra_break_schemas[[k]],preconditions=list("Performance order, both completed performances and full jury review must permit the event."),external_observations=list("Official first-performer selection, performance completion and nine final comparative votes."))),constraints=list(phase=d$phase,round_number=if(d$phase!="finished") length(d$rounds)+1L else length(d$rounds),next_performer=next_player,round_wins=.ra_break_counts(d),provisional_round_winner=if(!is.null(d$votes)) .ra_break_winner(d$votes) else NULL),completeness=FALSE,reason="official_comparative_breaking_judgments")
}
.ra_break_outcome<-function(state,pack) {
 d<-state$data;t<-d$phase=="finished";w<-d$winner
 list(status=if(t) "terminal" else if(d$phase=="round_review") "awaiting_adjudication" else "ongoing",result_kind=if(t) {if(d$reason=="forfeit") "forfeit" else if(d$reason=="disqualify") "disqualification" else "win"} else "none",winners=if(t) list(state$players[[w+1L]]$id) else list(),losers=if(t) list(state$players[[2L-w]]$id) else list(),placements=list(),reason=if(t) d$reason else if(d$phase=="round_review") "round_review" else "ongoing",evidence_required=if(d$phase=="round_review") list("Official round confirmation.") else list(),rule_id=if(t) paste0(pack$ruleset_id,"@",pack$ruleset_version) else NULL)
}
