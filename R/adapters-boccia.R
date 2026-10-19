.ra_boccia_i<-function(a,b,nullable=FALSE) {s<-list(type="integer",minimum=a,maximum=b);if(nullable) s$nullable<-TRUE;s}
.ra_boccia_team<-.ra_boccia_i(0,1)
.ra_boccia_bool<-list(type="boolean")
.ra_boccia_ball<-list(status=list(type="enum",values=list("unplayed","live","dead")),rank=.ra_boccia_i(1,12,TRUE),retracted=.ra_boccia_bool)
.ra_boccia_board<-list(type="array",minItems=12,maxItems=12,items=list(type="object",properties=.ra_boccia_ball,required=as.list(names(.ra_boccia_ball))))
.ra_boccia_clocks<-list(type="array",minItems=2,maxItems=2,items=.ra_boccia_i(0,210000))
.ra_boccia_schemas<-function() {
 empty<-setNames(list(),character());tm<-.ra_boccia_team;bo<-.ra_boccia_bool
 list(configure=list(prior_yellow=list(type="array",minItems=2,maxItems=2,items=.ra_boccia_i(0,100)),forfeit_margin=.ra_boccia_i(6,1000000)),start_match=empty,play_jack=list(valid=bo,elapsed_ms=.ra_boccia_i(0,210000)),play_ball=list(ball=.ra_boccia_i(0,11),board=.ra_boccia_board,elapsed_ms=.ra_boccia_i(0,210000)),elapse=list(milliseconds=.ra_boccia_i(1,210000)),correct_clock=list(team=tm,milliseconds=.ra_boccia_i(0,210000)),decline_remaining=empty,retract_release=list(balls=list(type="array",minItems=1,maxItems=6,items=.ra_boccia_i(0,11)),elapsed_ms=.ra_boccia_i(0,210000)),award_penalty=list(team=tm),confirm_board=empty,correct_board=list(board=.ra_boccia_board),resolve_penalty=list(scored=bo,elapsed_ms=.ra_boccia_i(0,60000)),confirm_end=empty,start_tiebreak=list(first_team=tm),replay_end=list(offender=.ra_boccia_i(0,1,TRUE),jack_thrower=tm,clocks_ms=.ra_boccia_clocks),card=list(team=tm,color=list(type="enum",values=list("yellow","red"))),forfeit=list(team=tm),set_forfeit_margin=list(points=.ra_boccia_i(6,1000000)),confirm_result=empty,confirm_no_contest=empty)
}
.ra_boccia_fresh<-function() lapply(1:12,function(i) list(status="unplayed",rank=NULL,retracted=FALSE))
.ra_boccia_reset<-function(d,first,tiebreak=FALSE) {
 d$balls<-.ra_boccia_fresh();d$clocks_ms<-list(210000,210000);d$turn<-first;d$equal_chain<-FALSE;d$pending_penalties<-list(0L,0L);d["penalty_first"]<-list(NULL);d$end_points<-list(0L,0L);d$phase<-if(tiebreak) "play" else "jack";d
}
.ra_boccia_remaining<-function(d,t) Filter(function(i) i%/%6==t && d$balls[[i+1L]]$status=="unplayed",0:11)
.ra_boccia_near<-function(d) vapply(0:1,function(t) {values<-vapply(Filter(function(i) i%/%6==t && d$balls[[i+1L]]$status=="live",0:11),function(i) d$balls[[i+1L]]$rank,numeric(1));if(length(values)>0) min(values) else 99},numeric(1))
.ra_boccia_points<-function(d) {
 n<-.ra_boccia_near(d)
 if(all(n==99)) return(list(0L,0L))
 if(n[1]==n[2]) return(lapply(0:1,function(t) sum(vapply(0:11,function(i) {b<-d$balls[[i+1L]];i%/%6==t && b$status=="live" && b$rank==n[t+1L]},logical(1)))))
 w<-if(n[1]<n[2]) 0L else 1L
 lapply(0:1,function(t) if(t!=w) 0L else sum(vapply(0:11,function(i) {b<-d$balls[[i+1L]];b$status=="live" && i%/%6==t && b$rank<n[2L-t]},logical(1))))
}
.ra_boccia_settle<-function(d,last) {
 left<-vapply(0:1,function(t) length(.ra_boccia_remaining(d,t))>0,logical(1))
 if(!any(left)) {d$phase<-"end_review";d$equal_chain<-FALSE;return(d)}
 n<-.ra_boccia_near(d);p<-unlist(.ra_boccia_points(d))
 if(n[1]==n[2] && n[1]!=99 && p[1]==p[2]) {nxt<-if(d$equal_chain) 1L-last else last;d$equal_chain<-TRUE} else {
  d$equal_chain<-FALSE
  nxt<-if(n[1]==n[2] && n[1]!=99) {if(p[1]<p[2]) 0L else 1L} else if(all(n==99)) last else if(n[1]>n[2]) 0L else 1L
 }
 d$turn<-if(left[nxt+1L]) nxt else 1L-nxt;d$phase<-"play";d
}
.ra_boccia_expire<-function(d,t) {
 for(i in .ra_boccia_remaining(d,t)) d$balls[[i+1L]]<-list(status="dead",rank=NULL,retracted=FALSE)
 if(d$phase=="jack") {if(length(.ra_boccia_remaining(d,1L-t))>0 && d$clocks_ms[[2L-t]]>0) d$turn<-1L-t else d$phase<-"end_review"} else d<-.ra_boccia_settle(d,t)
 d
}
.ra_boccia_spend<-function(d,amount) {t<-d$turn+1L;if(amount>d$clocks_ms[[t]]) .ra_fail("Elapsed time exceeds the active side allowance");d$clocks_ms[[t]]<-d$clocks_ms[[t]]-amount;d}
.ra_boccia_validate<-function(d) {
 bad<-function(m) .ra_fail(m,code="INVALID_STATE")
 for(b in d$balls) if((b$status=="live")!=(!is.null(b$rank)) || (b$retracted && b$status!="dead")) bad("Ball rank and retraction status disagree")
 if(d$phase=="setup" && (d$ends_completed!=0 || any(unlist(d$scores)!=0) || any(vapply(d$balls,function(b) b$status!="unplayed",logical(1))) || any(unlist(d$forfeited)))) bad("Initial match must be empty")
 if(d$phase=="jack" && any(vapply(d$balls,function(b) b$status=="live",logical(1)))) bad("Coloured balls cannot precede the valid jack")
 if(d$phase %in% c("jack","play") && (length(.ra_boccia_remaining(d,d$turn))==0 || d$clocks_ms[[d$turn+1L]]==0)) bad("The next side needs a ball and available time")
 if(d$phase %in% c("end_review","penalty","end_ready") && any(vapply(d$balls,function(b) b$status=="unplayed",logical(1)))) bad("Resolve all regular balls before end scoring")
 if(d$phase %in% c("jack","play") && any(vapply(0:1,function(t) d$clocks_ms[[t+1L]]==0 && length(.ra_boccia_remaining(d,t))>0,logical(1)))) bad("Expired time cannot leave unplayed balls")
 if(d$phase=="play" && all(vapply(0:1,function(t) length(.ra_boccia_remaining(d,t))>0,logical(1)))) {
  n<-.ra_boccia_near(d);p<-unlist(.ra_boccia_points(d))
  if(n[1]!=n[2] && d$turn!=(if(n[1]>n[2]) 0 else 1)) bad("The side farther from the jack must play")
  if(n[1]==n[2] && p[1]!=p[2] && d$turn!=(if(p[1]<p[2]) 0 else 1)) bad("The side with fewer equally near balls must play")
 }
 if(d$phase %in% c("match_review","finished") && !any(unlist(d$forfeited))) {
  p<-unlist(if(d$tiebreak) d$end_points else d$scores)
  if(d$ends_completed!=4 || p[1]==p[2] || is.null(d$winner) || d$winner!=(if(p[1]>p[2]) 0 else 1)) bad("The completed match score must determine the winner")
 }
 if(d$no_contest && !all(unlist(d$forfeited))) bad("No contest requires both forfeits")
 if(d$phase %in% c("penalty","end_ready","match_review","finished") && !any(unlist(d$forfeited)) && any(unlist(d$end_points)<unlist(.ra_boccia_points(d)))) bad("End points cannot be below the confirmed board score")
 if(d$phase=="penalty" && d$pending_penalties[[d$turn+1L]]==0) bad("The next penalty ball must exist")
 if(d$phase=="end_ready" && any(unlist(d$pending_penalties)>0)) bad("Resolve every penalty ball before confirming the end")
 if(any(unlist(d$pending_penalties)>0) && is.null(d$penalty_first)) bad("The first penalty recipient is required")
 if(d$tiebreak && (d$ends_completed!=4 || d$scores[[1]]!=d$scores[[2]])) bad("Tiebreak follows four regulation ends")
 if(d$phase=="tiebreak_setup" && (d$ends_completed!=4 || d$scores[[1]]!=d$scores[[2]])) bad("A tied regulation match is required")
 if(d$tiebreak && is.null(d$tiebreak_first)) bad("Tiebreak first side is missing")
 if(d$phase %in% c("match_review","finished") && !any(unlist(d$forfeited)) && is.null(d$winner) && !d$no_contest) bad("Match result has not been decided")
 if(d$phase=="finished" && all(unlist(d$forfeited)) && !d$no_contest) bad("Double forfeits require an official no-contest decision")
 for(t in 1:2) if(d$yellow[[t]]>0 && d$yellow[[t]]+d$prior_yellow[[t]]>=2 && !d$forfeited[[t]]) bad("Accumulated yellow cards require forfeiture")
 invisible(TRUE)
}
.ra_boccia_apply<-function(state,action) {
 d<-state$data;k<-action$type;p<-action$params;phase<-d$phase;t<-d$turn;schemas<-.ra_boccia_schemas()
 if(!(k %in% names(schemas))) .ra_fail("Unknown boccia event")
 .ra_fields(p,schemas[[k]],"/params")
 if(k=="configure") {
  if(phase!="setup") .ra_fail("Configure competition history before the match")
  d$prior_yellow<-p$prior_yellow;d$forfeit_margin<-p$forfeit_margin
 } else if(k=="start_match") {
  if(phase!="setup") .ra_fail("Match already started")
  pending<-d$pending_penalties;first<-d$penalty_first;d<-.ra_boccia_reset(d,0L);d$pending_penalties<-pending;d["penalty_first"]<-list(first)
 } else if(k=="play_jack") {
  if(phase!="jack") .ra_fail("No jack delivery is pending")
  d<-.ra_boccia_spend(d,p$elapsed_ms)
  if(p$valid) d$phase<-"play" else d$turn<-1L-t
  if(d$clocks_ms[[t+1L]]==0) {d$turn<-t;d<-.ra_boccia_expire(d,t)} else if(!p$valid && length(.ra_boccia_remaining(d,1L-t))==0) d$turn<-t
 } else if(k=="play_ball") {
  j<-p$ball;new<-p$board
  if(phase!="play" || !(j %in% .ra_boccia_remaining(d,t))) .ra_fail("Play one unplayed ball of the indicated side")
  for(i in 0:11) {
   old<-d$balls[[i+1L]];b<-new[[i+1L]]
   if(i==j) {if(b$status=="unplayed") .ra_fail("The played ball must resolve")} else if(old$status %in% c("unplayed","dead")) {if(!isTRUE(all.equal(b,old))) .ra_fail("Unplayed and dead balls cannot move back into play")} else if(old$status=="live" && (b$status=="unplayed" || b$retracted)) .ra_fail("A live ball can remain live or become dead after the shot")
  }
  d<-.ra_boccia_spend(d,p$elapsed_ms);d$balls<-new
  if(d$clocks_ms[[t+1L]]==0) for(i in .ra_boccia_remaining(d,t)) d$balls[[i+1L]]<-list(status="dead",rank=NULL,retracted=FALSE)
  d<-.ra_boccia_settle(d,t)
 } else if(k=="retract_release") {
  if(phase!="play" || anyDuplicated(unlist(p$balls))>0 || any(!(unlist(p$balls) %in% .ra_boccia_remaining(d,t)))) .ra_fail("Retract distinct unplayed balls released by the indicated side")
  d<-.ra_boccia_spend(d,p$elapsed_ms)
  for(i in p$balls) d$balls[[i+1L]]<-list(status="dead",rank=NULL,retracted=TRUE)
  if(d$clocks_ms[[t+1L]]==0) for(i in .ra_boccia_remaining(d,t)) d$balls[[i+1L]]<-list(status="dead",rank=NULL,retracted=FALSE)
  d<-.ra_boccia_settle(d,t)
 } else if(k=="elapse") {
  if(!(phase %in% c("jack","play"))) .ra_fail("Regular play clock is not active")
  d<-.ra_boccia_spend(d,p$milliseconds);if(d$clocks_ms[[t+1L]]==0) d<-.ra_boccia_expire(d,t)
 } else if(k=="correct_clock") {
  if(!(phase %in% c("jack","play"))) .ra_fail("Correct clocks during regular play")
  d$clocks_ms[[p$team+1L]]<-p$milliseconds;if(p$milliseconds==0) d<-.ra_boccia_expire(d,p$team)
 } else if(k=="decline_remaining") {
  if(phase!="play") .ra_fail("Decline balls only during coloured-ball play")
  d<-.ra_boccia_expire(d,t)
 } else if(k=="award_penalty") {
  if(!(phase %in% c("setup","jack","play","end_review","penalty","end_ready"))) .ra_fail("No open end can receive a penalty ball")
  q<-p$team;d$pending_penalties[[q+1L]]<-d$pending_penalties[[q+1L]]+1L
  if(is.null(d$penalty_first)) d$penalty_first<-q
  if(phase=="end_ready") {d$phase<-"penalty";d$turn<-q}
 } else if(k=="correct_board") {
  if(phase!="end_review" || any(vapply(p$board,function(b) b$status=="unplayed",logical(1)))) .ra_fail("Correct a resolved end before scoring")
  d$balls<-p$board
 } else if(k=="confirm_board") {
  if(phase!="end_review") .ra_fail("Resolve all coloured balls first")
  d$end_points<-.ra_boccia_points(d)
  if(any(unlist(d$pending_penalties)>0)) {d$phase<-"penalty";d$turn<-if(d$pending_penalties[[d$penalty_first+1L]]>0) d$penalty_first else 1L-d$penalty_first} else d$phase<-"end_ready"
 } else if(k=="resolve_penalty") {
  if(phase!="penalty") .ra_fail("No penalty ball is pending")
  d$end_points[[t+1L]]<-d$end_points[[t+1L]]+as.integer(p$scored);d$pending_penalties[[t+1L]]<-d$pending_penalties[[t+1L]]-1L
  if(d$pending_penalties[[2L-t]]>0) d$turn<-1L-t else if(d$pending_penalties[[t+1L]]==0) d$phase<-"end_ready"
 } else if(k=="confirm_end") {
  if(phase!="end_ready") .ra_fail("Review regular scoring and resolve all penalty balls first")
  if(d$tiebreak) {
   if(d$end_points[[1]]!=d$end_points[[2]]) {d$winner<-if(d$end_points[[1]]>d$end_points[[2]]) 0L else 1L;d$phase<-"match_review"} else {d$tiebreak_first<-1L-d$tiebreak_first;d<-.ra_boccia_reset(d,d$tiebreak_first,TRUE)}
  } else {
   d$scores<-as.list(unlist(d$scores)+unlist(d$end_points));d$ends_completed<-d$ends_completed+1L
   if(d$ends_completed<4) d<-.ra_boccia_reset(d,d$ends_completed%%2L) else if(d$scores[[1]]==d$scores[[2]]) d$phase<-"tiebreak_setup" else {d$winner<-if(d$scores[[1]]>d$scores[[2]]) 0L else 1L;d$phase<-"match_review"}
  }
 } else if(k=="start_tiebreak") {
  if(phase!="tiebreak_setup") .ra_fail("No initial tiebreak is pending")
  d$tiebreak<-TRUE;d$tiebreak_first<-p$first_team;d<-.ra_boccia_reset(d,p$first_team,TRUE)
 } else if(k=="replay_end") {
  if(!(phase %in% c("jack","play","end_review"))) .ra_fail("Only an unresolved disrupted end can restart")
  for(i in 1:12) if(!d$balls[[i]]$retracted) d$balls[[i]]<-list(status="unplayed",rank=NULL,retracted=FALSE)
  if(!is.null(p$offender)) d$pending_penalties[[p$offender+1L]]<-0L
  d$clocks_ms<-p$clocks_ms;d$turn<-p$jack_thrower;d$equal_chain<-FALSE;d$phase<-if(d$tiebreak) "play" else "jack"
  for(q in 0:1) if(d$clocks_ms[[q+1L]]==0) for(i in .ra_boccia_remaining(d,q)) d$balls[[i+1L]]<-list(status="dead",rank=NULL,retracted=FALSE)
  if(length(.ra_boccia_remaining(d,d$turn))==0) {
   if(d$phase=="jack") {if(length(.ra_boccia_remaining(d,1L-d$turn))>0) d$turn<-1L-d$turn else d$phase<-"end_review"} else d<-.ra_boccia_settle(d,d$turn)
  }
 } else if(k %in% c("card","forfeit")) {
  q<-p$team
  if(d$forfeited[[q+1L]]) .ra_fail("Side has already forfeited")
  if(k=="card" && p$color=="yellow") d$yellow[[q+1L]]<-d$yellow[[q+1L]]+1L
  if(k=="forfeit" || identical(p$color,"red") || d$yellow[[q+1L]]+d$prior_yellow[[q+1L]]>=2) {d$forfeited[[q+1L]]<-TRUE;d$phase<-"match_review";d["winner"]<-list(if(all(unlist(d$forfeited))) NULL else 1L-q)}
 } else if(k=="set_forfeit_margin") d$forfeit_margin<-p$points else if(k=="confirm_no_contest") {
  if(phase!="match_review" || !all(unlist(d$forfeited))) .ra_fail("Official no-contest decision requires both sides to have forfeited")
  d$phase<-"finished";d$no_contest<-TRUE;d["winner"]<-list(NULL)
 } else if(k=="confirm_result") {
  if(phase!="match_review" || all(unlist(d$forfeited))) .ra_fail("Resolve the match result before confirmation")
  d$phase<-"finished"
 };d
}
.ra_boccia_legal<-function(state) {
 d<-state$data;phase<-d$phase;names<-switch(phase,setup=c("configure","start_match"),jack=c("play_jack","elapse","correct_clock","replay_end"),play=c("play_ball","retract_release","elapse","correct_clock","decline_remaining","replay_end"),end_review=c("correct_board","confirm_board","replay_end"),penalty="resolve_penalty",end_ready="confirm_end",tiebreak_setup="start_tiebreak",match_review=if(all(unlist(d$forfeited))) "confirm_no_contest" else "confirm_result",character())
 if(phase!="finished") names<-c(names,"card","forfeit","set_forfeit_margin")
 if(phase %in% c("setup","jack","play","end_review","penalty","end_ready")) names<-c(names,"award_penalty")
 schemas<-.ra_boccia_schemas()
 list(mode="event_types",actions=list(),action_types=lapply(names,function(k) list(type=k,params_schema=schemas[[k]],preconditions=list("End phase, indicated side, available balls and official judgment must permit the event."),external_observations=list("Official proximity ranks, elapsed playing time, valid delivery or sanction."))),constraints=list(phase=phase,next_team=d$turn,remaining_balls=lapply(0:1,function(t) as.list(.ra_boccia_remaining(d,t))),provisional_points=.ra_boccia_points(d),pending_penalties=d$pending_penalties),completeness=FALSE,reason="official_boccia_observations")
}
.ra_boccia_outcome<-function(state,pack) {
 d<-state$data;done<-d$phase=="finished";w<-d$winner;scores<-d$scores
 if(sum(unlist(d$forfeited))==1) {scores<-list(0L,0L);scores[[w+1L]]<-d$forfeit_margin}
 list(status=if(done) "terminal" else if(d$phase %in% c("end_review","match_review")) "awaiting_adjudication" else "ongoing",result_kind=if(done) {if(d$no_contest) "no_contest" else "win"} else "none",winners=if(done && !is.null(w)) list(state$players[[w+1L]]$id) else list(),losers=if(done && !is.null(w)) list(state$players[[2L-w]]$id) else list(),placements=if(done && !is.null(w)) lapply(0:1,function(i) list(player_id=state$players[[i+1L]]$id,place=if(i==w) 1L else 2L,score=scores[[i+1L]])) else list(),reason=if(done) "boccia_confirmed" else "ongoing",evidence_required=list(),rule_id=if(done) paste0(pack$ruleset_id,"@",pack$ruleset_version) else NULL)
}
