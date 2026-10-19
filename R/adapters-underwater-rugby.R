.ra_uwrugby_team<-list(type="integer",minimum=0,maximum=1)
.ra_uwrugby_player<-list(type="integer",minimum=0,maximum=14)
.ra_uwrugby_active<-function(d,t) as.list(which(unlist(d$players[[t+1L]])=="active")-1L)
.ra_uwrugby_capacity<-function(d,t) 6L-sum(vapply(d$penalties,function(r) r$team==t,logical(1)))
.ra_uwrugby_eligible<-function(d,t,i) d$players[[t+1L]][[i+1L]] %in% c("active","bench")
.ra_uwrugby_penalty<-function(d,t,i,double=FALSE,kind="time") {
 if(!.ra_uwrugby_eligible(d,t,i)) .ra_fail("Penalty server must be an eligible match player")
 d$players[[t+1L]][[i+1L]]<-"penalty";d$restart_team<-1L-t
 d$penalties[[length(d$penalties)+1L]]<-list(team=t,server=i,kind=kind,remaining_ms=if(kind=="expulsion") 300000L else 120000L,segments=if(double) 2L else 1L,started=FALSE)
 if(double) d$final_warning[[t+1L]][[i+1L]]<-TRUE
 d
}
.ra_uwrugby_release<-function(d,index) {
 r<-d$penalties[[index]]
 if(r$segments>1) {d$penalties[[index]]$segments<-r$segments-1L;d$penalties[[index]]$remaining_ms<-120000L} else {d$players[[r$team+1L]][[r$server+1L]]<-"bench";d$penalties<-d$penalties[-index]}
 d
}
.ra_uwrugby_goal<-function(d,t) {
 d$score[[t+1L]]<-d$score[[t+1L]]+1L
 counts<-vapply(0:1,function(j) sum(vapply(d$penalties,function(r) r$team==j && r$kind=="time",logical(1))),integer(1))
 if(counts[[2L-t]]>counts[[t+1L]]) d<-.ra_uwrugby_release(d,which(vapply(d$penalties,function(r) r$team==1L-t && r$kind=="time",logical(1)))[[1]])
 d$restart_team<-1L-t;d
}
.ra_uwrugby_tick<-function(d,ms) {
 for(i in rev(seq_along(d$penalties))) {
  if(!d$penalties[[i]]$started) next
  left<-ms;removed<-FALSE
  while(left>=d$penalties[[i]]$remaining_ms) {
   left<-left-d$penalties[[i]]$remaining_ms;last<-d$penalties[[i]]$segments==1
   d<-.ra_uwrugby_release(d,i)
   if(last) {removed<-TRUE;break}
  }
  if(!removed) d$penalties[[i]]$remaining_ms<-d$penalties[[i]]$remaining_ms-left
 }
 d
}
.ra_uwrugby_flush<-function(d) {
 if(!is.null(d$delayed)) {
  p<-d$delayed;d["delayed"]<-list(NULL)
  if(!is.null(p$server)) d<-.ra_uwrugby_penalty(d,p$team,p$server,p$double)
  if(p$throw) d$pending<-list(team=1L-p$team,attacker=NULL,defender=NULL,remaining_ms=45000L)
  d$restart_team<-1L-p$team
 }
 d
}
.ra_uwrugby_validate<-function(d) {
 bad<-function(m) .ra_fail(m,code="INVALID_STATE");seen<-character()
 for(r in d$penalties) {
  key<-paste(r$team,r$server)
  if(key %in% seen || d$players[[r$team+1L]][[r$server+1L]]!="penalty") bad("Each penalty has one distinct suspended server")
  seen<-c(seen,key);duration<-if(r$kind=="expulsion") 300000L else 120000L
  if(r$remaining_ms>duration || (!r$started && r$remaining_ms!=duration) || (r$kind=="expulsion" && r$segments!=1)) bad("Penalty duration or consecutive segments are inconsistent")
 }
 for(t in 0:1) {
  if(.ra_uwrugby_capacity(d,t)<0) bad("More than six simultaneous team reductions")
  if(sum(unlist(d$players[[t+1L]])=="reserve")>3) bad("At most three registered reserves")
  for(i in 0:14) if((d$players[[t+1L]][[i+1L]]=="penalty")!=(paste(t,i) %in% seen)) bad("Penalty status needs its serving record")
  for(c in 1:19) if(sum(vapply(d$warnings[[t+1L]],function(x) x[[c]],logical(1)))>=2 && !d$team_warnings[[t+1L]][[c]]) bad("Two player warnings of one category imply a team warning")
 }
 if(d$phase=="live" && (d$clock_ms==0 || !is.null(d$pending) || any(vapply(0:1,function(t) {n<-length(.ra_uwrugby_active(d,t));n==0 || n>.ra_uwrugby_capacity(d,t)},logical(1))))) bad("Live play needs time and eligible reduced lineups")
 if(d$phase=="throw" && (is.null(d$pending) || d$pending$remaining_ms==0)) bad("A running throw needs its clock and participants")
 if(!is.null(d$pending)) {
  p<-d$pending
  if(!d$phase %in% c("stopped","throw","timeout")) bad("Pending penalty throw requires an interruption or throw phase")
  if(is.null(p$attacker)!=is.null(p$defender)) bad("Throw participants are nominated together")
  if(!is.null(p$attacker) && (!.ra_uwrugby_eligible(d,p$team,p$attacker) || !.ra_uwrugby_eligible(d,1L-p$team,p$defender))) bad("Throw participants must be eligible")
  if(d$phase=="throw" && is.null(p$attacker)) bad("Running throw needs nominated participants")
 }
 if(!is.null(d$delayed)) {
  p<-d$delayed
  if(d$phase!="live" || (is.null(p$server) && !p$throw) || (!is.null(p$server) && !.ra_uwrugby_eligible(d,p$team,p$server))) bad("Delayed decision requires live play and eligible server")
 }
 if(d$phase=="pre" && (d$half!=1 || d$clock_ms!=900000 || any(unlist(d$score)!=0) || length(d$penalties)>0)) bad("Pre-match state precedes scoring and penalties")
 if(d$phase=="interval" && (d$half!=1 || d$clock_ms!=0)) bad("Interval follows the first half")
 if(d$phase %in% c("timeout","interval") && d$break_ms==0) bad("Break phase needs remaining break time")
 if(d$phase=="finished") {
  if(identical(d$reason,"regulation")) {
   w<-if(d$score[[1]]==d$score[[2]]) NULL else as.integer(d$score[[2]]>d$score[[1]])
   if(d$half!=2 || d$clock_ms!=0 || !is.null(d$pending) || !identical(d$winner,w)) bad("Regulation result must match completed time and score")
  } else if(identical(d$reason,"concession")) {if(is.null(d$winner)) bad("Concession needs a winner")} else if(is.null(d$reason) || !d$reason %in% c("official_result","no_contest")) bad("Finished match requires a result reason")
 } else if(!is.null(d$winner) || !is.null(d$reason)) bad("Unfinished match cannot declare a result")
}
.ra_uwrugby_schemas<-function(d) {
 phase<-d$phase;s<-list();empty<-setNames(list(),character());T<-.ra_uwrugby_team;P<-.ra_uwrugby_player;NT<-c(T,list(nullable=TRUE));B<-list(type="boolean");C<-list(type="integer",minimum=0,maximum=18)
 if(phase=="finished") return(s)
 if(phase=="abandoned") return(list(official_result=list(winner=NT)))
 if(phase=="pre") s$start_match<-empty
 if(phase %in% c("pre","live","stopped","interval")) s$set_lineup<-list(team=T,players=list(type="array",minItems=0,maxItems=6,items=P))
 if(phase=="live") s<-c(s,list(goal=list(team=T,player=P),stop=list(restart_team=NT),signal_delayed=list(team=T,server=c(P,list(nullable=TRUE)),double=B,throw=B)))
 if(phase %in% c("live","throw")) s$elapse<-list(milliseconds=list(type="integer",minimum=1,maximum=900000))
 if(phase %in% c("live","stopped")) {
  s<-c(s,list(time_penalty=list(team=T,server=P,double=B),expel=list(team=T,player=P,server=P,repeated=B),warn=list(team=T,player=P,category=C),team_warning=list(team=T,category=C),final_warning=list(team=T,player=P)))
  s$additional_penalty<-list(team=T,server=P)
  s$expel_serving<-list(team=T,player=P,server=P,retain_time_penalty=B,replacement_server=c(P,list(nullable=TRUE)))
  if(is.null(d$pending)) s$award_throw<-list(team=T)
 }
 if(phase=="stopped") {
  s<-c(s,list(reserve_substitution=list(team=T,outgoing=P,incoming=P,authorized=B),timeout=list(team=T)))
  if(!is.null(d$pending)) s$start_throw<-list(attacker=P,defender=P) else if(d$clock_ms>0) s$restart<-list(team=NT) else s<-c(s,list(close_half=empty,late_goal=list(team=T,player=P)))
 }
 if(phase=="throw") s$resolve_throw<-list(result=list(type="enum",values=list("goal","save","attacker_foul","defender_foul","defender_surface_foul")))
 if(phase %in% c("interval","timeout")) s$elapse_break<-list(milliseconds=list(type="integer",minimum=1,maximum=300000))
 c(s,list(abandon=empty,concede=list(team=T)))
}
.ra_uwrugby_apply<-function(state,action) {
 d<-state$data;k<-action$type;p<-action$params;ss<-.ra_uwrugby_schemas(d)
 if(!k %in% names(ss)) .ra_fail("Action is not permitted in this phase")
 .ra_fields(p,ss[[k]],"/params");t<-p$team
 if(k=="start_match") {
  if(any(vapply(0:1,function(j) length(.ra_uwrugby_active(d,j))!=6,logical(1)))) .ra_fail("Each team must start with six eligible players")
  d$phase<-"stopped"
 } else if(k=="set_lineup") {
  ids<-p$players
  if(anyDuplicated(unlist(ids)) || length(ids)>.ra_uwrugby_capacity(d,t) || any(vapply(ids,function(i) !.ra_uwrugby_eligible(d,t,i),logical(1)))) .ra_fail("Lineup exceeds capacity or includes an ineligible player")
  for(i in .ra_uwrugby_active(d,t)) d$players[[t+1L]][[i+1L]]<-"bench"
  for(i in ids) d$players[[t+1L]][[i+1L]]<-"active"
  if(d$phase=="live" && length(ids)==0) .ra_fail("A team without active players requires an official stoppage")
 } else if(k=="restart") {
  if(!identical(p$team,d$restart_team)) .ra_fail("Restart entitlement must match the official decision")
  if(any(vapply(0:1,function(j) {n<-length(.ra_uwrugby_active(d,j));n==0 || n>.ra_uwrugby_capacity(d,j)},logical(1)))) .ra_fail("Correct the reduced lineup before restarting")
  d$phase<-"live";d["restart_team"]<-list(NULL);d$late_used<-FALSE
  for(i in seq_along(d$penalties)) d$penalties[[i]]$started<-TRUE
 } else if(k=="stop") {d<-.ra_uwrugby_flush(d);d$phase<-"stopped";d["restart_team"]<-list(p$restart_team)} else if(k=="signal_delayed") {
  if(!is.null(d$delayed) || (is.null(p$server) && !p$throw) || (is.null(p$server) && p$double)) .ra_fail("One pending time-penalty or penalty-throw decision is required")
  if(!is.null(p$server) && !.ra_uwrugby_eligible(d,t,p$server)) .ra_fail("Delayed server must be eligible")
  d$delayed<-p
 } else if(k %in% c("goal","late_goal")) {
  if(d$players[[t+1L]][[p$player+1L]]!="active") .ra_fail("The scorer must be active")
  if(k=="late_goal") {if(d$late_used) .ra_fail("The period-ending goal has already been recorded");d$late_used<-TRUE}
  if(!is.null(d$delayed)) {
   if(d$delayed$team==t) .ra_fail("Stop and resolve the delayed decision before an offending-team goal")
   d["delayed"]<-list(NULL)
  }
  d<-.ra_uwrugby_goal(d,t);d$phase<-"stopped"
 } else if(k=="elapse") {
  ms<-p$milliseconds;throw<-d$phase=="throw"
  if(ms>if(throw) d$pending$remaining_ms else d$clock_ms) .ra_fail("Clock event exceeds the current playing segment")
  d$clock_ms<-max(0L,d$clock_ms-ms);d<-.ra_uwrugby_tick(d,ms)
  if(throw) {
   d$pending$remaining_ms<-d$pending$remaining_ms-ms
   if(d$pending$remaining_ms==0) {d["pending"]<-list(NULL);d$phase<-"stopped";d["restart_team"]<-list(NULL)}
  } else if(d$clock_ms==0) {d<-.ra_uwrugby_flush(d);d$phase<-"stopped"}
 } else if(k %in% c("time_penalty","expel","warn","team_warning","final_warning")) {
  if(!is.null(d$pending) && !is.null(d$pending$attacker)) .ra_fail("Complete the nominated throw before an unrelated disciplinary decision")
  d<-.ra_uwrugby_flush(d);d$phase<-"stopped"
  if(k=="time_penalty") d<-.ra_uwrugby_penalty(d,t,p$server,p$double) else if(k=="expel") {
   i<-p$player;server<-p$server
   if(d$players[[t+1L]][[i+1L]]=="excluded" || i==server || (p$repeated && !d$final_warning[[t+1L]][[i+1L]])) .ra_fail("Repeat expulsion needs a final warning and a different eligible server")
   if(any(vapply(d$penalties,function(r) r$team==t && r$server==i,logical(1)))) .ra_fail("Resolve the serving player penalty before a separate expulsion decision")
   d$players[[t+1L]][[i+1L]]<-"excluded";d<-.ra_uwrugby_penalty(d,t,server,kind="expulsion")
  } else if(k=="final_warning") {
   if(!.ra_uwrugby_eligible(d,t,p$player)) .ra_fail("Final warning requires an eligible player")
   d$final_warning[[t+1L]][[p$player+1L]]<-TRUE
  } else if(k=="team_warning") d$team_warnings[[t+1L]][[p$category+1L]]<-TRUE else {
   i<-p$player;c<-p$category+1L
   if(!.ra_uwrugby_eligible(d,t,i)) .ra_fail("Warning requires an eligible player")
   if(d$team_warnings[[t+1L]][[c]] || d$warnings[[t+1L]][[i+1L]][[c]]) d<-.ra_uwrugby_penalty(d,t,i) else {
    d$warnings[[t+1L]][[i+1L]][[c]]<-TRUE
    if(sum(vapply(d$warnings[[t+1L]],function(x) x[[c]],logical(1)))>=2) d$team_warnings[[t+1L]][[c]]<-TRUE
   }
  }
 } else if(k=="additional_penalty") {
  rr<-which(vapply(d$penalties,function(r) r$team==t && r$server==p$server,logical(1)))
  if(length(rr)!=1 || d$penalties[[rr]]$kind!="time" || d$penalties[[rr]]$started || d$penalties[[rr]]$segments!=1) .ra_fail("Additional misconduct extends one unstarted time penalty to two segments")
  d$penalties[[rr]]$segments<-2L;d$final_warning[[t+1L]][[p$server+1L]]<-TRUE;d$phase<-"stopped"
 } else if(k=="expel_serving") {
  i<-p$player;rr<-which(vapply(d$penalties,function(r) r$team==t && r$server==i,logical(1)))
  if(length(rr)!=1 || d$penalties[[rr]]$kind!="time" || p$server==i) .ra_fail("Nominate a serving time-penalty player and a different expulsion server")
  j<-p$replacement_server
  if(p$retain_time_penalty) {
   if(is.null(j) || j==p$server || !.ra_uwrugby_eligible(d,t,j)) .ra_fail("Retained time penalty requires a separate eligible replacement server")
   d$penalties[[rr]]$server<-j;d$players[[t+1L]][[j+1L]]<-"penalty"
  } else {
   if(!is.null(j)) .ra_fail("A superseded time penalty needs no replacement server")
   d$penalties<-d$penalties[-rr]
  }
  d$players[[t+1L]][[i+1L]]<-"excluded";d<-.ra_uwrugby_penalty(d,t,p$server,kind="expulsion");d$phase<-"stopped";d<-.ra_uwrugby_flush(d)
 } else if(k=="award_throw") {
  d<-.ra_uwrugby_flush(d)
  if(!is.null(d$pending)) .ra_fail("The delayed decision already awarded a penalty throw")
  d$pending<-list(team=t,attacker=NULL,defender=NULL,remaining_ms=45000L);d$phase<-"stopped"
 } else if(k=="start_throw") {
  t<-d$pending$team
  if(!.ra_uwrugby_eligible(d,t,p$attacker) || !.ra_uwrugby_eligible(d,1L-t,p$defender)) .ra_fail("Throw participants must be eligible match players")
  d$pending$attacker<-p$attacker;d$pending$defender<-p$defender;d$pending$remaining_ms<-45000L;d$phase<-"throw"
 } else if(k=="resolve_throw") {
  q<-d$pending;t<-q$team;result<-p$result;d$phase<-"stopped"
  if(result %in% c("defender_foul","defender_surface_foul")) {
   d<-.ra_uwrugby_penalty(d,1L-t,q$defender,result=="defender_surface_foul")
   d$pending["attacker"]<-list(NULL);d$pending["defender"]<-list(NULL);d$pending$remaining_ms<-45000L
  } else {d["pending"]<-list(NULL);d["restart_team"]<-list(NULL);if(result=="goal") d<-.ra_uwrugby_goal(d,t)}
 } else if(k=="reserve_substitution") {
  a<-p$outgoing;b<-p$incoming
  if(!p$authorized || d$reserve_changes[[t+1L]]>=3 || !.ra_uwrugby_eligible(d,t,a) || d$players[[t+1L]][[b+1L]]!="reserve") .ra_fail("Reserve substitution needs authorization, an eligible pair and a remaining allowance")
  if(!is.null(d$pending) && !is.null(d$pending$attacker)) .ra_fail("Do not change nominated throw participants")
  d$players[[t+1L]][[b+1L]]<-d$players[[t+1L]][[a+1L]];d$players[[t+1L]][[a+1L]]<-"reserve";d$reserve_changes[[t+1L]]<-d$reserve_changes[[t+1L]]+1L
 } else if(k=="timeout") {
  if(d$timeouts[[t+1L]] || d$clock_ms==0) .ra_fail("One timeout per team and half is available during an existing interruption")
  d$timeouts[[t+1L]]<-TRUE;d$phase<-"timeout";d$break_ms<-60000L
 } else if(k=="elapse_break") {
  if(p$milliseconds>d$break_ms) .ra_fail("Break elapsed time exceeds its remaining duration")
  d$break_ms<-d$break_ms-p$milliseconds
  if(d$break_ms==0) {
   if(d$phase=="interval") {d$half<-2L;d$clock_ms<-900000L;d$timeouts<-list(FALSE,FALSE);d$late_used<-FALSE;d["restart_team"]<-list(NULL)}
   d$phase<-"stopped"
  }
 } else if(k=="close_half") {
  if(d$half==1) {d$phase<-"interval";d$break_ms<-300000L} else {d$phase<-"finished";d["winner"]<-list(if(d$score[[1]]==d$score[[2]]) NULL else as.integer(d$score[[2]]>d$score[[1]]));d$reason<-"regulation"}
 } else if(k=="abandon") {d$phase<-"abandoned";d["pending"]<-list(NULL);d["delayed"]<-list(NULL)} else if(k=="concede") {d$phase<-"finished";d$winner<-1L-t;d$reason<-"concession";d["pending"]<-list(NULL);d["delayed"]<-list(NULL)} else if(k=="official_result") {d$phase<-"finished";d["winner"]<-list(p$winner);d$reason<-if(is.null(p$winner)) "no_contest" else "official_result"}
 d
}
.ra_uwrugby_legal<-function(state) {
 d<-state$data;s<-.ra_uwrugby_schemas(d)
 list(mode="event_types",actions=list(),action_types=lapply(names(s),function(k) list(type=k,params_schema=s[[k]],preconditions=list("Eligible participants, reduced capacity, phase and remaining allowances must permit the event."),external_observations=list("Official goals, infringements, restart decisions and elapsed effective time."))),constraints=list(phase=d$phase,half=d$half,clock_ms=d$clock_ms,score=d$score,capacity=lapply(0:1,function(t) .ra_uwrugby_capacity(d,t)),penalties=d$penalties,pending=d$pending,restart_team=d$restart_team,reserve_changes=d$reserve_changes,timeouts=d$timeouts),completeness=FALSE,reason="official_underwater_rugby_events")
}
.ra_uwrugby_outcome<-function(state,pack) .ra_handball_outcome(state,pack)
