.ra_handball_team<-list(type="integer",minimum=0,maximum=1)
.ra_handball_player<-list(type="integer",minimum=0,maximum=13)
.ra_handball_capacity<-function(d,t) 7-sum(vapply(d$reductions,function(r) r$team==t,logical(1)))
.ra_handball_active<-function(d,t) as.list(which(unlist(d$players[[t+1L]])=="active")-1L)
.ra_handball_validate<-function(d) {
 bad<-function(m) .ra_fail(m,code="INVALID_STATE")
 for(t in 0:1) {
  if(length(.ra_handball_active(d,t))>.ra_handball_capacity(d,t)) bad("Active lineup exceeds the reduced team capacity")
  if(sum(unlist(d$warned[[t+1L]]))>3) bad("A team may receive at most three player warnings")
  for(i in 0:13) {
   s<-d$players[[t+1L]][[i+1L]];rr<-Filter(function(r) r$team==t && !is.null(r$player) && r$player==i,d$reductions)
   if(length(rr)>1) bad("One player cannot create simultaneous independent reductions")
   if((s=="suspended" && length(rr)==0) || (length(rr)>0 && !s %in% c("suspended","dq"))) bad("Suspension and eligibility disagree")
   if(d$suspensions[[t+1L]][[i+1L]]==3 && s!="dq") bad("A third suspension disqualifies the player")
  }
 }
 for(r in d$reductions) if(r$unserved && !r$remaining_ms %in% c(120000,240000)) bad("Unserved reduction must retain its full duration")
 if(d$phase=="live" && (d$remaining_ms==0 || !is.null(d$pending) || any(vapply(0:1,function(t) length(.ra_handball_active(d,t))==0,logical(1))))) bad("Live play requires time, active teams and no pending throw")
 if(!is.null(d$pending)) {
  p<-d$pending
  if(d$phase!="stopped" || d$players[[p$team+1L]][[p$thrower+1L]]!="active") bad("A pending throw requires an eligible thrower and stopped clock")
  if(p$kind=="free_throw" && d$remaining_ms!=0) bad("Only an end-of-half free throw is tracked separately")
 }
 if(d$phase=="pre" && (d$half!=1 || d$remaining_ms!=1800000 || any(unlist(d$score)!=0) || length(d$reductions)>0)) bad("Pre-match state precedes playing time and scoring")
 if(d$phase=="interval" && (d$half!=1 || d$remaining_ms!=0 || !is.null(d$pending))) bad("Interval follows the completed first half")
 if(d$phase=="finished") {
  if(d$reason=="regulation") {
   w<-if(d$score[[1]]==d$score[[2]]) NULL else as.integer(d$score[[2]]>d$score[[1]])
   if(d$half!=2 || d$remaining_ms!=0 || !identical(d$winner,w)) bad("Regulation result must match the final score")
  } else if(d$reason=="concession") {if(is.null(d$winner)) bad("Concession requires a winner")} else if(!d$reason %in% c("official_result","no_contest")) bad("Finished match needs its reason")
 } else if(!is.null(d$winner) || !is.null(d$reason)) bad("Nonterminal state cannot declare a result")
}
.ra_handball_schemas<-function(d) {
 phase<-d$phase;s<-list();empty<-setNames(list(),character());team<-.ra_handball_team;player<-.ra_handball_player;np<-c(player,list(nullable=TRUE))
 if(phase=="finished") return(s)
 if(phase=="abandoned") return(list(official_result=list(winner=c(team,list(nullable=TRUE)))))
 if(phase=="pre") s$start_match<-empty
 if(phase %in% c("pre","live","stopped","interval")) s<-c(s,list(substitute=list(team=team,outgoing=player,incoming=player),enter=list(team=team,player=player),leave=list(team=team,player=player)))
 if(phase=="interval") s$start_second_half<-empty
 if(phase=="live") s<-c(s,list(elapse=list(milliseconds=list(type="integer",minimum=1,maximum=1800000)),stop_clock=empty,goal=list(team=team,player=player)))
 if(phase=="stopped" && is.null(d$pending)) {if(d$remaining_ms>0) s$resume_play<-empty else s$end_half<-empty}
 if(phase %in% c("live","stopped")) {
  for(k in c("caution","suspend","disqualify")) s[[k]]<-list(team=team,player=player,remove_player=np)
  s$additional_misconduct<-list(team=team,player=player,disqualify=list(type="boolean"));s$team_reduction<-list(team=team,remove_player=np)
  if(is.null(d$pending)) s<-c(s,list(grant_timeout=list(team=team),award_throw=list(team=team,thrower=player,kind=list(type="enum",values=list("seven_metre","free_throw"))))) else s<-c(s,list(resolve_throw=list(scored=list(type="boolean")),replace_thrower=list(player=player)))
 }
 c(s,list(abandon=empty,concede=list(team=team)))
}
.ra_handball_reduce<-function(d,t,i,remove) {
 if(!is.null(i) && any(vapply(d$reductions,function(r) r$team==t && !is.null(r$player) && r$player==i,logical(1)))) .ra_fail("Use additional misconduct for an unserved player reduction")
 d$reductions[[length(d$reductions)+1L]]<-list(team=t,player=i,remaining_ms=120000,unserved=TRUE)
 if(length(.ra_handball_active(d,t))>.ra_handball_capacity(d,t)) {
  if(is.null(remove) || d$players[[t+1L]][[remove+1L]]!="active") .ra_fail("Nominate an active teammate to implement the reduction")
  d$players[[t+1L]][[remove+1L]]<-"bench"
 } else if(!is.null(remove)) .ra_fail("No additional player needs to leave")
 d$phase<-"stopped";d
}
.ra_handball_apply<-function(state,action) {
 d<-state$data;k<-action$type;p<-action$params;s<-.ra_handball_schemas(d)
 if(!k %in% names(s)) .ra_fail("Action is not permitted in this phase")
 .ra_fields(p,s[[k]],"/params");t<-p$team
 if(k=="start_match") {
  if(any(vapply(0:1,function(j) length(.ra_handball_active(d,j))<5,logical(1)))) .ra_fail("Both teams need at least five players to start")
  d$phase<-"stopped"
 } else if(k=="start_second_half") {d$half<-2L;d$remaining_ms<-1800000;d$phase<-"stopped";d$timeouts<-list(FALSE,FALSE)} else if(k=="resume_play") {
  if(any(vapply(0:1,function(j) length(.ra_handball_active(d,j))==0,logical(1)))) .ra_fail("A team without an active player cannot resume")
  d$phase<-"live"
  for(j in seq_along(d$reductions)) d$reductions[[j]]$unserved<-FALSE
 } else if(k=="stop_clock") d$phase<-"stopped" else if(k=="elapse") {
  ms<-p$milliseconds
  if(ms>d$remaining_ms) .ra_fail("Playing time cannot pass the half boundary")
  d$remaining_ms<-d$remaining_ms-ms;remaining<-list()
  for(r in d$reductions) {
   r$remaining_ms<-r$remaining_ms-ms
   if(r$remaining_ms>0) remaining[[length(remaining)+1L]]<-r else if(!is.null(r$player) && d$players[[r$team+1L]][[r$player+1L]]=="suspended") d$players[[r$team+1L]][[r$player+1L]]<-"bench"
  }
  d$reductions<-remaining
  if(d$remaining_ms==0) d$phase<-"stopped"
 } else if(k=="goal") {
  if(d$players[[t+1L]][[p$player+1L]]!="active") .ra_fail("Only an eligible active player can score")
  d$score[[t+1L]]<-d$score[[t+1L]]+1L
 } else if(k %in% c("substitute","enter","leave")) {
  if(k!="enter") {
   old<-if(k=="substitute") p$outgoing else p$player
   if(d$players[[t+1L]][[old+1L]]!="active") .ra_fail("The outgoing player must be active")
   if(!is.null(d$pending) && d$pending$team==t && d$pending$thrower==old) .ra_fail("Replace the pending thrower before removing that player")
   d$players[[t+1L]][[old+1L]]<-"bench"
  }
  if(k!="leave") {
   new<-if(k=="substitute") p$incoming else p$player
   if(d$players[[t+1L]][[new+1L]]!="bench" || length(.ra_handball_active(d,t))>=.ra_handball_capacity(d,t)) .ra_fail("Entry requires an eligible bench player and an available slot")
   if(k=="substitute" && new==old) .ra_fail("Substitution requires different players")
   d$players[[t+1L]][[new+1L]]<-"active"
  }
 } else if(k=="grant_timeout") {
  if(d$timeouts[[t+1L]] || d$remaining_ms==0) .ra_fail("One granted team timeout is available in each half")
  d$timeouts[[t+1L]]<-TRUE;d$phase<-"stopped"
 } else if(k=="award_throw") {
  if(d$players[[t+1L]][[p$thrower+1L]]!="active" || (p$kind=="free_throw" && d$remaining_ms!=0)) .ra_fail("Use an active thrower; only an end-of-half free throw is recorded")
  d$pending<-p;d$phase<-"stopped"
 } else if(k=="resolve_throw") {
  if(p$scored) d$score[[d$pending$team+1L]]<-d$score[[d$pending$team+1L]]+1L
  d["pending"]<-list(NULL)
 } else if(k=="replace_thrower") {
  if(d$players[[d$pending$team+1L]][[p$player+1L]]!="active") .ra_fail("Replacement thrower must be active")
  d$pending$thrower<-p$player
 } else if(k %in% c("caution","suspend","disqualify")) {
  i<-p$player
  if(d$players[[t+1L]][[i+1L]] %in% c("dq","suspended")) .ra_fail("Player is already unavailable; use the additional-misconduct event where applicable")
  if(!is.null(d$pending) && d$pending$team==t && d$pending$thrower==i) .ra_fail("Replace the pending thrower before imposing this sanction")
  if(k=="caution" && !d$warned[[t+1L]][[i+1L]] && sum(unlist(d$warned[[t+1L]]))<3 && d$suspensions[[t+1L]][[i+1L]]==0) {
   if(!is.null(p$remove_player)) .ra_fail("A warning does not reduce the team")
   d$warned[[t+1L]][[i+1L]]<-TRUE
  } else {
   if(k!="disqualify") d$suspensions[[t+1L]][[i+1L]]<-d$suspensions[[t+1L]][[i+1L]]+1L
   d$players[[t+1L]][[i+1L]]<-if(k=="disqualify" || d$suspensions[[t+1L]][[i+1L]]==3) "dq" else "suspended"
   d<-.ra_handball_reduce(d,t,i,p$remove_player)
  }
 } else if(k=="additional_misconduct") {
  i<-p$player;j<-which(vapply(d$reductions,function(r) r$team==t && !is.null(r$player) && r$player==i,logical(1)))
  if(length(j)!=1 || !d$reductions[[j]]$unserved || d$reductions[[j]]$remaining_ms!=120000) .ra_fail("Additional misconduct must precede resumption after the initial reduction")
  if(d$players[[t+1L]][[i+1L]]!="dq") d$suspensions[[t+1L]][[i+1L]]<-min(3L,d$suspensions[[t+1L]][[i+1L]]+1L)
  if(p$disqualify || d$suspensions[[t+1L]][[i+1L]]==3) d$players[[t+1L]][[i+1L]]<-"dq"
  d$reductions[[j]]$remaining_ms<-240000
 } else if(k=="team_reduction") d<-.ra_handball_reduce(d,t,NULL,p$remove_player) else if(k=="end_half") {
  if(d$half==1) d$phase<-"interval" else {d$phase<-"finished";d$reason<-"regulation";d["winner"]<-list(if(d$score[[1]]==d$score[[2]]) NULL else as.integer(d$score[[2]]>d$score[[1]]))}
 } else if(k=="abandon") {d$phase<-"abandoned";d["pending"]<-list(NULL)} else if(k=="concede") {d$phase<-"finished";d$winner<-1L-t;d$reason<-"concession";d["pending"]<-list(NULL)} else if(k=="official_result") {d$phase<-"finished";d["winner"]<-list(p$winner);d$reason<-if(is.null(p$winner)) "no_contest" else "official_result"}
 d
}
.ra_handball_legal<-function(state) {
 d<-state$data;s<-.ra_handball_schemas(d)
 list(mode="event_types",actions=list(),action_types=lapply(names(s),function(k) list(type=k,params_schema=s[[k]],preconditions=list("Phase, eligible lineup, team capacity and remaining allowances must permit the event."),external_observations=list("Official goals, throws, clock, discipline and timeout decisions."))),constraints=list(phase=d$phase,half=d$half,remaining_ms=d$remaining_ms,capacity=lapply(0:1,function(t) .ra_handball_capacity(d,t)),active_players=lapply(0:1,function(t) .ra_handball_active(d,t)),pending_throw=d$pending),completeness=FALSE,reason="official_handball_events")
}
.ra_handball_outcome<-function(state,pack) {
 d<-state$data;done<-d$phase=="finished";w<-d$winner;draw<-done && d$reason=="regulation" && is.null(w)
 list(status=if(done) "terminal" else if(d$phase=="abandoned") "awaiting_adjudication" else "ongoing",result_kind=if(done) {if(draw) "draw" else if(is.null(w)) "no_contest" else "win"} else "none",winners=if(!is.null(w)) list(state$players[[w+1L]]$id) else list(),losers=if(!is.null(w)) list(state$players[[2L-w]]$id) else list(),placements=list(),reason=if(done) d$reason else if(d$phase=="abandoned") "official_result_required" else "ongoing",evidence_required=if(d$phase=="abandoned") list("Official result decision.") else list(),rule_id=if(done) paste0(pack$ruleset_id,"@",pack$ruleset_version) else NULL)
}
