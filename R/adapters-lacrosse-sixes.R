.ra_lacrosse_i<-function(a,b) list(type="integer",minimum=a,maximum=b)
.ra_lacrosse_e<-function(...) list(type="enum",values=list(...))
.ra_lacrosse_foul_schema<-function() list(team=.ra_lacrosse_i(0,1),player=c(.ra_lacrosse_i(0,11),list(nullable=TRUE)),server=.ra_lacrosse_i(0,11),kind=.ra_lacrosse_e("minor","major","expulsion"),timed=list(type="boolean"))
.ra_lacrosse_active<-function(d,t) which(unlist(d$status[[t+1L]])=="active")-1L
.ra_lacrosse_capacity<-function(d,t) 6L-sum(unlist(d$penalty_ms[[t+1L]])>0)
.ra_lacrosse_finish<-function(d,w,r) {d$phase<-"finished";d["winner"]<-list(w);d$reason<-r;d$clock_running<-FALSE;d$break_ms<-0L;d}
.ra_lacrosse_restart<-function(d,t) {d["restart_team"]<-list(t);if(d$period>=5 || (d$period==4 && d$clock_ms<=120000)) d$clock_running<-FALSE;if(!d$phase %in% c("period_review","abandoned")) d$phase<-"restart";d}
.ra_lacrosse_penalty<-function(d,p) {
 t<-p$team+1L;i<-p$player;server<-p$server+1L;kind<-p$kind
 n<-if(kind=="minor") {if(p$timed) 30000L else 0L} else if(kind=="major") 60000L else 120000L
 if(!is.null(i) && d$status[[t]][[i+1L]]=="excluded") .ra_fail("Excluded player cannot receive another participation penalty")
 if(kind=="minor" && n==0) return(d)
 delegated<-is.null(i) || (!is.null(d$goalkeeper[[t]]) && i==d$goalkeeper[[t]]) || kind=="expulsion"
 if(delegated) {
  home<-d$in_home[[t]];available<-!is.null(home) && d$status[[t]][[home+1L]] %in% c("active","bench") && (kind!="expulsion" || is.null(i) || home!=i)
  if(available && p$server!=home) .ra_fail("The designated in-home must serve the delegated penalty when available")
  if((!is.null(d$goalkeeper[[t]]) && p$server==d$goalkeeper[[t]]) || !d$status[[t]][[server]] %in% c("active","bench") || (kind=="expulsion" && !is.null(i) && p$server==i)) .ra_fail("Delegated penalty needs an eligible non-goalkeeper server")
 } else if(p$server!=i) .ra_fail("A field player serves their own minor or major penalty")
 if(kind=="major" && !is.null(i)) {d$major_fouls[[t]][[i+1L]]<-d$major_fouls[[t]][[i+1L]]+1L;if(d$major_fouls[[t]][[i+1L]]==3) d$status[[t]][[i+1L]]<-"excluded"}
 if(kind=="expulsion" && !is.null(i)) {n<-n+d$penalty_ms[[t]][[i+1L]];d$penalty_ms[[t]][[i+1L]]<-0L;d$penalty_started[[t]][[i+1L]]<-FALSE;d$status[[t]][[i+1L]]<-"excluded"}
 if(d$status[[t]][[server]]!="excluded") d$status[[t]][[server]]<-"penalty"
 d$penalty_ms[[t]][[server]]<-d$penalty_ms[[t]][[server]]+n
 if(!is.null(i) && d$status[[t]][[i+1L]]=="excluded") for(role in c("captain","goalkeeper","in_home")) if(!is.null(d[[role]][[t]]) && d[[role]][[t]]==i) d[[role]][t]<-list(NULL)
 if(.ra_lacrosse_capacity(d,t-1L)<1) {d$phase<-"abandoned";d$clock_running<-FALSE}
 d
}
.ra_lacrosse_flush<-function(d,goal_team=NULL) {
 pending<-d$delayed;d$delayed<-list();retained<-FALSE
 for(p in pending) {
  if(!is.null(goal_team) && p$team!=goal_team && p$kind=="minor") next
  d<-.ra_lacrosse_penalty(d,p)
  if(!is.null(goal_team) && p$team!=goal_team) retained<-TRUE
 }
 list(data=d,retained=retained)
}
.ra_lacrosse_goal<-function(d,t) {
 d$score[[t+1L]]<-d$score[[t+1L]]+1L;fl<-.ra_lacrosse_flush(d,t);d<-fl$data;d$clock_running<-FALSE
 if(d$period>=5) d<-.ra_lacrosse_finish(d,t,"sudden_victory") else if(d$phase!="abandoned") d<-.ra_lacrosse_restart(d,if(fl$retained) t else 1L-t)
 d
}
.ra_lacrosse_validate<-function(d) {
 bad<-function(m) .ra_fail(m,code="INVALID_STATE")
 for(t in 1:2) {
  na<-length(.ra_lacrosse_active(d,t-1L));if(na>6 || (d$phase=="play" && na>max(0,.ra_lacrosse_capacity(d,t-1L)))) bad("Lineup exceeds capacity after time-serving penalties")
  for(i in 1:12) {
   st<-d$status[[t]][[i]];n<-d$penalty_ms[[t]][[i]]
   if((n>0 && !st %in% c("penalty","excluded")) || (st=="penalty" && n==0)) bad("Penalty clocks and player status must agree")
   if(n==0 && d$penalty_started[[t]][[i]]) bad("An inactive penalty clock cannot be marked started")
   if(d$major_fouls[[t]][[i]]==3 && st!="excluded") bad("Three major fouls remove the player for the game")
  }
  g<-d$goalkeeper[[t]];h<-d$in_home[[t]];cc<-d$captain[[t]]
  if(!is.null(g) && d$status[[t]][[g+1L]]!="active") bad("Designated goalkeeper must be active")
  for(role in list(h,cc)) if(!is.null(role) && ((!is.null(g) && role==g) || d$status[[t]][[role+1L]]=="excluded")) bad("In-home and captain must be eligible non-goalkeepers")
  if(d$phase=="play" && (is.null(g) || is.null(cc) || is.null(h))) bad("Live play requires goalkeeper, captain and in-home")
 }
 if(d$clock_ms>(if(d$period<=4) 480000 else 240000)) bad("Clock exceeds the current period")
 if(d$phase=="period_review" && d$clock_ms!=0) bad("Period review requires an expired clock")
 if(d$phase=="play" && d$clock_ms==0) bad("Play cannot continue after period expiry")
 if((d$phase %in% c("interval","timeout"))!=(d$break_ms>0)) bad("Break clock must agree with phase")
 if(d$phase=="finished") {
  if(is.null(d$reason) || (d$reason=="no_contest")!=is.null(d$winner)) bad("Result needs a winner or no contest")
  if(d$reason %in% c("regulation","sudden_victory","forfeit") && d$score[[d$winner+1L]]<=d$score[[2L-d$winner]]) bad("Winner must lead the score")
 } else if(!is.null(d$winner) || !is.null(d$reason)) bad("Unfinished game cannot have a result")
}
.ra_lacrosse_schemas<-function(d) {
 ph<-d$phase;s<-list();ii<-.ra_lacrosse_i;ee<-.ra_lacrosse_e;t<-ii(0,1);p<-ii(0,11);b<-list(type="boolean");nt<-c(t,list(nullable=TRUE));empty<-setNames(list(),character());foul<-.ra_lacrosse_foul_schema()
 if(ph=="finished") return(s)
 if(ph=="abandoned") return(list(official_result=list(winner=nt)))
 if(ph %in% c("pre_game","ot_toss")) s$choose_alternate<-list(team=t)
 if(ph=="draw") s<-c(s,list(draw=list(winner=t),redraw=empty))
 if(ph %in% c("restart","stopped")) s$restart<-list(team=t)
 if(ph=="play") s<-c(s,list(goal=list(team=t,player=p),stop=list(team=nt,official_timeout=b),signal_foul=foul,minor_turnover=list(team=t),shot_clock_violation=list(team=t)))
 if(ph=="period_review") s<-c(s,list(late_goal=list(team=t,player=p),close_period=list(retain_team=nt)))
 if(!ph %in% c("pre_game","ot_toss","interval","timeout","period_review")) s$elapse<-list(milliseconds=ii(1,480000))
 if(ph %in% c("play","restart","stopped","draw","pre_game","interval")) s$timeout<-list(team=t,authorized=b)
 if(ph %in% c("interval","timeout")) s$elapse_break<-list(milliseconds=ii(1,300000))
 if(length(d$delayed)>0) s$resolve_delayed<-list(team=nt)
 if(!ph %in% c("timeout","interval")) s$assess_fouls<-list(fouls=list(type="array",minItems=1L,maxItems=12L,items=list(type="object",properties=foul,required=as.list(names(foul)))),prior_team=nt,coincidental=b)
 c(s,list(set_lineup=list(team=t,players=list(type="array",minItems=1L,maxItems=6L,items=p)),appoint=list(team=t,role=ee("goalkeeper","captain","in_home"),player=p),withdraw=list(team=t,player=p),concede=list(team=t),abandon=empty))
}
.ra_lacrosse_apply<-function(state,action) {
 d<-state$data;p<-action$params;k<-action$type;s<-.ra_lacrosse_schemas(d)
 if(!k %in% names(s)) .ra_fail("Action is not permitted in this phase")
 .ra_fields(p,s[[k]],"/params")
 if(k=="choose_alternate") {
  if(d$period==1 && any(vapply(0:1,function(t) length(.ra_lacrosse_active(d,t))!=6,logical(1)))) .ra_fail("Six active players per team are required to start")
  d$alternate<-p$team;d$phase<-if(is.null(d$restart_team)) "draw" else "restart"
 } else if(k %in% c("draw","restart")) {
  if(any(vapply(0:1,function(t) length(.ra_lacrosse_active(d,t))>.ra_lacrosse_capacity(d,t),logical(1)))) .ra_fail("Reduce the lineup to its available penalty capacity before restarting")
  if(any(vapply(c("goalkeeper","captain","in_home"),function(role) any(vapply(d[[role]],is.null,logical(1))),logical(1)))) .ra_fail("Required team roles must be appointed")
  if(k=="restart" && !is.null(d$restart_team) && p$team!=d$restart_team) .ra_fail("Only the entitled team can restart")
  d$penalty_started<-lapply(d$penalty_ms,function(row) lapply(row,function(x) x>0))
  d$phase<-"play";d$clock_running<-TRUE;d["restart_team"]<-list(NULL);d$timeout_since_restart<-FALSE;d$redraws<-0L
 } else if(k=="redraw") {
  d$redraws<-d$redraws+1L
  if(d$redraws==2) {team<-d$alternate;d$alternate<-1L-team;d<-.ra_lacrosse_restart(d,team);d$redraws<-0L}
 } else if(k=="elapse") {
  n<-p$milliseconds;if(!d$clock_running || n>d$clock_ms) .ra_fail("Elapsed time requires a running clock and cannot exceed the period")
  d$clock_ms<-d$clock_ms-n
  for(t in 1:2) for(i in 1:12) {if(d$penalty_started[[t]][[i]]) d$penalty_ms[[t]][[i]]<-max(0L,d$penalty_ms[[t]][[i]]-n);if(d$penalty_ms[[t]][[i]]==0) d$penalty_started[[t]][[i]]<-FALSE;if(d$penalty_ms[[t]][[i]]==0 && d$status[[t]][[i]]=="penalty") d$status[[t]][[i]]<-"bench"}
  if(d$clock_ms==0) {d$phase<-"period_review";d$clock_running<-FALSE;d$late_used<-FALSE}
 } else if(k %in% c("goal","late_goal")) {
  if(!p$player %in% .ra_lacrosse_active(d,p$team) || (k=="late_goal" && d$late_used)) .ra_fail("Goal needs an active player and an unresolved pre-buzzer shot")
  if(k=="late_goal") d$late_used<-TRUE
  d<-.ra_lacrosse_goal(d,p$team)
 } else if(k=="stop") {
  d<-.ra_lacrosse_flush(d)$data;d<-.ra_lacrosse_restart(d,p$team)
  if(p$official_timeout || d$period>=5 || (d$period==4 && d$clock_ms<=120000)) d$clock_running<-FALSE
 } else if(k %in% c("minor_turnover","shot_clock_violation")) {
  d<-.ra_lacrosse_flush(d)$data;d<-.ra_lacrosse_restart(d,1L-p$team)
  if(d$period>=5 || (d$period==4 && d$clock_ms<=120000)) d$clock_running<-FALSE
 } else if(k=="signal_foul") {
  if(p$kind=="minor" && !p$timed) .ra_fail("A delayed defensive minor must be a time-serving foul")
  if(any(vapply(d$delayed,function(x) x$team!=p$team,logical(1)))) .ra_fail("An opposing foul ends the delayed situation; resolve the batch officially")
  d$delayed<-c(d$delayed,list(p))
 } else if(k=="resolve_delayed") {d<-.ra_lacrosse_flush(d)$data;d<-.ra_lacrosse_restart(d,p$team)} else if(k=="assess_fouls") {
  fs<-p$fouls;totals<-c(0L,0L);teams<-unique(vapply(fs,function(x) x$team,numeric(1)))
  cancelled<-p$coincidental && length(d$delayed)==0 && all(vapply(fs,function(x) x$kind=="minor",logical(1))) && length(teams)==2
  if(length(d$delayed)>0) .ra_fail("Resolve the delayed penalty batch before another assessment")
  if(!cancelled) for(x in fs) {d<-.ra_lacrosse_penalty(d,x);totals[x$team+1L]<-totals[x$team+1L]+(if(x$kind=="minor") {if(x$timed) 30000L else 0L} else if(x$kind=="major") 60000L else 120000L)}
  if(d$phase!="abandoned") {
   if(totals[1]!=totals[2]) team<-as.integer(totals[1]>totals[2]) else if(length(teams)==1) team<-1L-fs[[1]]$team else if(!is.null(p$prior_team)) team<-p$prior_team else {team<-d$alternate;d$alternate<-1L-team}
   d<-.ra_lacrosse_restart(d,team)
  }
  if(d$period>=5 || (d$period==4 && d$clock_ms<=120000)) d$clock_running<-FALSE
 } else if(k=="close_period") {
  if(length(d$delayed)>0) .ra_fail("Resolve delayed fouls before closing the period")
  if(d$period==4 && d$score[[1]]!=d$score[[2]]) d<-.ra_lacrosse_finish(d,as.integer(d$score[[2]]>d$score[[1]]),"regulation") else {
   retained<-if(.ra_lacrosse_capacity(d,0)!=.ra_lacrosse_capacity(d,1)) p$retain_team else NULL
   d$phase<-"interval";d$break_ms<-if(d$period==2) 300000L else 120000L;d["restart_team"]<-list(retained);d$clock_running<-FALSE
  }
 } else if(k=="timeout") {
  t<-p$team+1L;limit<-if(d$period>=5) 1L else 2L
  if(!p$authorized || d$timeout_since_restart || d$timeouts[[t]]>=limit) .ra_fail("Timeout needs official entitlement, remaining allowance and resumed play")
  d$timeouts[[t]]<-d$timeouts[[t]]+1L;d$resume_phase<-d$phase;d$resume_clock<-d$clock_running;d$resume_break_ms<-d$break_ms;d$phase<-"timeout";d$break_ms<-30000L;d$clock_running<-FALSE;d$timeout_since_restart<-TRUE
 } else if(k=="elapse_break") {
  if(p$milliseconds>d$break_ms) .ra_fail("Elapsed time exceeds the break")
  d$break_ms<-d$break_ms-p$milliseconds
  if(d$break_ms==0) {
   if(d$phase=="timeout") {d$phase<-d$resume_phase;d$clock_running<-d$resume_clock;d$break_ms<-d$resume_break_ms} else {
    d$period<-d$period+1L;d$clock_ms<-if(d$period<=4) 480000L else 240000L
    if(d$period==3 || d$period>=5) d$timeouts<-list(0L,0L)
    d$timeout_since_restart<-FALSE;d$late_used<-FALSE;d$redraws<-0L;d$phase<-if(d$period==5) "ot_toss" else if(!is.null(d$restart_team)) "restart" else "draw"
   }
  }
 } else if(k=="set_lineup") {
  t<-p$team+1L;line<-unlist(p$players)
  if(anyDuplicated(line)>0 || length(line)>.ra_lacrosse_capacity(d,t-1L) || any(vapply(line,function(i) !d$status[[t]][[i+1L]] %in% c("active","bench"),logical(1)))) .ra_fail("Lineup must respect eligibility and unexpired penalty places")
  g<-d$goalkeeper[[t]];if(!is.null(g) && !g %in% line) .ra_fail("Keep the designated goalkeeper active until a new goalkeeper is appointed")
  for(i in 0:11) if(d$status[[t]][[i+1L]] %in% c("active","bench")) d$status[[t]][[i+1L]]<-if(i %in% line) "active" else "bench"
 } else if(k=="appoint") {
  t<-p$team+1L;i<-p$player;role<-p$role
  if(!d$status[[t]][[i+1L]] %in% c("active","bench")) .ra_fail("Role needs an eligible player")
  if(role=="goalkeeper") {if(!i %in% .ra_lacrosse_active(d,t-1L) || i %in% unlist(list(d$captain[[t]],d$in_home[[t]]))) .ra_fail("Goalkeeper must be active and cannot hold captain or in-home role")} else if(!is.null(d$goalkeeper[[t]]) && i==d$goalkeeper[[t]]) .ra_fail("Goalkeeper cannot be captain or in-home")
  d[[role]][[t]]<-i
 } else if(k=="withdraw") {
  t<-p$team+1L;i<-p$player
  if(!d$status[[t]][[i+1L]] %in% c("active","bench")) .ra_fail("Only an eligible player may withdraw")
  d$status[[t]][[i+1L]]<-"excluded"
  for(role in c("goalkeeper","captain","in_home")) if(!is.null(d[[role]][[t]]) && d[[role]][[t]]==i) d[[role]][t]<-list(NULL)
  if(d$phase=="play") {d$phase<-"stopped";d$clock_running<-FALSE}
 } else if(k=="concede") {d$score<-list(0L,0L);d$score[[2L-p$team]]<-1L;d<-.ra_lacrosse_finish(d,1L-p$team,"forfeit")} else if(k=="abandon") {d$phase<-"abandoned";d$clock_running<-FALSE;d$break_ms<-0L} else if(k=="official_result") d<-.ra_lacrosse_finish(d,p$winner,if(is.null(p$winner)) "no_contest" else "official_result")
 d
}
.ra_lacrosse_legal<-function(state) {
 d<-state$data;s<-.ra_lacrosse_schemas(d)
 list(mode="event_types",actions=list(),action_types=lapply(names(s),function(k) list(type=k,params_schema=s[[k]],preconditions=list("Period, clock, eligible lineup and penalty capacity must permit the event."),external_observations=list("Official goals, foul classifications, penalty servers, restart and timeout entitlement."))),constraints=list(phase=d$phase,period=d$period,clock_ms=d$clock_ms,score=d$score,capacity=lapply(0:1,function(t) .ra_lacrosse_capacity(d,t)),penalty_ms=d$penalty_ms,major_fouls=d$major_fouls,restart_team=d$restart_team,timeouts=d$timeouts,alternate=d$alternate),completeness=FALSE,reason="official_lacrosse_sixes_events")
}
.ra_lacrosse_outcome<-function(state,pack) .ra_pelota_outcome(state,pack)
