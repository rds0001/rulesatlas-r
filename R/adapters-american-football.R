.ra_amfootball_i<-function(a,b) list(type="integer",minimum=a,maximum=b)
.ra_amfootball_e<-function(...) list(type="enum",values=list(...))
.ra_amfootball_finish<-function(d,w,r) {d$phase<-"finished";d["winner"]<-list(w);d$reason<-r;d$clock_running<-FALSE;d$break_ms<-0L;d}
.ra_amfootball_series<-function(d,t,s) {d$phase<-"ready";d$offense<-t;d$spot<-s;d$line_to_gain<-min(100,s+10);d$down<-1L;d}
.ra_amfootball_kick<-function(d,t,s=35) {d$phase<-"kickoff";d$offense<-t;d$spot<-s;d$clock_running<-FALSE;d}
.ra_amfootball_settle<-function(d) {if(d$quarter<5 && d$clock_ms==0 && d$phase %in% c("ready","kickoff") && !d$extension) {d$resume_phase<-d$phase;d$phase<-"period_end";d$clock_running<-FALSE};d}
.ra_amfootball_end_series<-function(d) {
 if(d$ot_series==0) {d$ot_series<-1L;d<-.ra_amfootball_series(d,1L-d$ot_first,75)} else {
  if(d$score[[1]]!=d$score[[2]]) return(.ra_amfootball_finish(d,as.integer(d$score[[2]]>d$score[[1]]),"overtime"))
  old<-d$ot_round;d$ot_round<-old+1L;d$ot_series<-0L;d$timeouts<-list(0L,0L);d$phase<-"ot_choice"
  if(old %in% c(2,4)) {d$phase<-"interval";d$resume_phase<-"ot_choice";d$break_ms<-120000L};return(d)
 }
 if(d$ot_round>=3) {d$phase<-"try_ready";d$spot<-97};d
}
.ra_amfootball_after_score<-function(d,t) {d<-if(d$quarter==5) .ra_amfootball_end_series(d) else .ra_amfootball_kick(d,t);.ra_amfootball_settle(d)}
.ra_amfootball_td<-function(d,t) {
 d$score[[t+1L]]<-d$score[[t+1L]]+6L
 if(d$quarter==5 && t!=d$offense) return(.ra_amfootball_finish(d,t,"overtime"))
 d$offense<-t;d$phase<-"try_ready";d$spot<-97;d$clock_running<-FALSE
 deciding<-(d$quarter==4 && d$clock_ms==0) || (d$quarter==5 && d$ot_series==1)
 if(deciding && abs(d$score[[1]]-d$score[[2]])>2) d<-.ra_amfootball_after_score(d,t);d
}
.ra_amfootball_elapse<-function(d,n) {if(n>d$clock_ms || (d$quarter==5 && n>0)) .ra_fail("Elapsed time exceeds the timed period");d$clock_ms<-d$clock_ms-n;d$clock_running<-FALSE;d$sit_out<-rep(list(as.list(rep(FALSE,50))),2);d}
.ra_amfootball_next_down<-function(d) {
 if(d$spot>=d$line_to_gain) d<-.ra_amfootball_series(d,d$offense,d$spot) else if(d$down<4) d$down<-d$down+1L else if(d$quarter==5) d<-.ra_amfootball_end_series(d) else d<-.ra_amfootball_series(d,1L-d$offense,100-d$spot);d
}
.ra_amfootball_validate<-function(d) {
 bad<-function(m) .ra_fail(m,code="INVALID_STATE")
 for(t in 1:2) {if(sum(unlist(d$status[[t]])=="active")>11) bad("At most eleven players may be active")
  for(i in 1:50) {if(d$unsporting[[t]][[i]]==2 && d$status[[t]][[i]]!="excluded") bad("Second unsportsmanlike act requires exclusion");if(d$sit_out[[t]][[i]] && d$status[[t]][[i]]=="active") bad("Restricted player cannot be active")}}
 if(d$quarter==5 && (d$clock_ms!=0 || d$clock_running || d$ot_round==0)) bad("Overtime requires an untimed extra period")
 if(d$phase %in% c("ready","try_ready","kickoff") && !(d$spot>0 && d$spot<100)) bad("Restart spot must lie inside the field")
 if(d$phase=="ready" && d$line_to_gain<=d$spot) bad("An earned first down must establish a new line to gain")
 if(d$phase=="period_end" && (d$clock_ms!=0 || d$extension)) bad("Period end requires expiry without an extension")
 if((d$phase %in% c("timeout","interval"))!=(d$break_ms>0)) bad("Break clock must agree with phase")
 if(d$clock_running && d$phase!="ready") bad("Game clock may run between scrimmage downs only")
 if(d$phase=="finished") {
  if(is.null(d$reason) || (d$reason %in% c("draw","no_contest"))!=is.null(d$winner)) bad("Result must identify a winner or draw")
  if(d$reason %in% c("regulation","overtime") && d$score[[d$winner+1L]]<=d$score[[2L-d$winner]]) bad("Winner must lead")
  if(identical(d$reason,"draw") && d$score[[1]]!=d$score[[2]]) bad("Draw requires equal scores")
 } else if(!is.null(d$winner) || !is.null(d$reason)) bad("Unfinished match cannot have a result")
 invisible(TRUE)
}
.ra_amfootball_schemas<-function(d) {
 I<-.ra_amfootball_i;E<-.ra_amfootball_e;T<-I(0,1);P<-I(0,49);B<-list(type="boolean");SP<-list(type="number",minimum=0,maximum=100);ph<-d$phase;s<-setNames(list(),character())
 if(ph=="finished") return(s)
 if(ph=="abandoned") return(list(official_result=list(winner=c(T,list(nullable=TRUE)))))
 if(ph=="pre_game") s$choose_start<-list(kicking_team=T)
 if(ph=="halftime_choice") s$choose_second_half<-list(kicking_team=T)
 if(ph=="ot_choice") s$choose_overtime<-list(first=T)
 if(ph=="ready") s$play<-list(result=E("advance","incomplete","turnover","new_series","regained","touchdown","defensive_touchdown","safety","defensive_safety","touchback","field_goal","missed_field_goal"),spot=SP,elapsed_ms=I(0,720000))
 if(ph=="kickoff") s$free_kick_result<-list(result=E("return","recovery","touchback","return_touchdown","safety"),spot=SP,elapsed_ms=I(0,720000))
 if(ph=="try_ready") {s$try_result<-list(result=E("kick","touchdown","miss","defensive_touchdown","safety","defensive_safety"));s$forego_try<-setNames(list(),character())}
 if(ph %in% c("ready","kickoff","try_ready","period_end")) {
  s$penalty<-list(team=T,base_spot=SP,yards=E(0,5,10,15),effect=E("repeat","loss","first","keep"),live=B,extend=B,elapsed_ms=I(0,720000))
  s$official_enforcement<-list(team=T,spot=SP,down=I(1,4),line_to_gain=SP,restart=E("ready","kickoff","try_ready"),extend=B,elapsed_ms=I(0,720000))
 }
 if(ph %in% c("ready","try_ready","kickoff")) s$timeout<-list(team=T)
 if(ph=="ready" && d$quarter<5) {s$official_clock<-list(running=B);s$elapse_between<-list(milliseconds=I(1,720000));s$clock_runoff<-list(team=T,avoid_with_timeout=B)}
 if(ph=="period_end") s$close_period<-setNames(list(),character())
 if(ph %in% c("timeout","interval")) s$elapse_break<-list(milliseconds=I(1,1200000))
 s$set_lineup<-list(team=T,players=list(type="array",minItems=1,maxItems=11,items=P));s$sanction<-list(team=T,player=P,kind=E("unsporting","disqualify"));s$miss_next_down<-list(team=T,player=P);s$concede<-list(team=T);s$abandon<-setNames(list(),character());s
}
.ra_amfootball_apply<-function(state,action) {
 d<-state$data;p<-action$params;k<-action$type;s<-.ra_amfootball_schemas(d)
 if(!k %in% names(s)) .ra_fail("Action is not permitted in this phase")
 .ra_fields(p,s[[k]],"/params");t<-d$offense
 if(k %in% c("play","try_result","free_kick_result") && any(vapply(d$status,function(x) !any(unlist(x)=="active"),logical(1)))) .ra_fail("Both teams require eligible players")
 if(k %in% c("choose_start","choose_second_half")) d<-.ra_amfootball_kick(d,p$kicking_team)
 else if(k=="choose_overtime") {d$ot_first<-p$first;d$ot_series<-0L;d<-.ra_amfootball_series(d,p$first,75);if(d$ot_round>=3) {d$phase<-"try_ready";d$spot<-97}}
 else if(k=="play") {
  if(d$quarter<5 && d$clock_ms==0 && !d$extension) .ra_fail("Expired period needs an authorized untimed down")
  r<-p$result;spot<-p$spot;oldspot<-d$spot;d<-.ra_amfootball_elapse(d,p$elapsed_ms);d$extension<-FALSE
  if(r %in% c("advance","turnover","new_series","regained") && !(spot>0 && spot<100)) .ra_fail("Use a scoring or touchback event at a goal line")
  if(r=="incomplete" && spot!=oldspot) .ra_fail("Incomplete pass retains the scrimmage spot")
  if((r %in% c("touchdown","defensive_touchdown","field_goal") && spot!=100) || (r %in% c("safety","defensive_safety","touchback") && spot!=0)) .ra_fail("Scoring event requires the matching goal-line spot")
  if(r %in% c("advance","incomplete")) {d$spot<-spot;d<-.ra_amfootball_next_down(d)}
  else if(r %in% c("turnover","touchback","missed_field_goal")) {if(d$quarter==5) d<-.ra_amfootball_end_series(d) else d<-.ra_amfootball_series(d,1L-t,if(r=="touchback") 20 else if(r=="missed_field_goal") max(20,100-oldspot) else spot)}
  else if(r=="new_series") d<-.ra_amfootball_series(d,t,spot)
  else if(r=="regained") {if(d$quarter==5) d<-.ra_amfootball_end_series(d) else d<-.ra_amfootball_series(d,t,spot)}
  else if(r %in% c("touchdown","defensive_touchdown")) d<-.ra_amfootball_td(d,if(r=="touchdown") t else 1L-t)
  else if(r=="field_goal") {d$score[[t+1L]]<-d$score[[t+1L]]+3L;d<-.ra_amfootball_after_score(d,t)}
  else if(r %in% c("safety","defensive_safety")) {w<-if(r=="safety") 1L-t else t;d$score[[w+1L]]<-d$score[[w+1L]]+2L;if(d$quarter==5) {if(w!=t) d<-.ra_amfootball_finish(d,w,"overtime") else d<-.ra_amfootball_end_series(d)} else d<-.ra_amfootball_kick(d,1L-w,20)}
  d<-.ra_amfootball_settle(d)
 } else if(k=="free_kick_result") {
  r<-p$result;spot<-p$spot;d<-.ra_amfootball_elapse(d,p$elapsed_ms);d$extension<-FALSE
  if(r %in% c("return","recovery") && !(spot>0 && spot<100)) .ra_fail("Returned or recovered kick needs an interior spot")
  if((r=="return_touchdown" && spot!=100) || (r %in% c("touchback","safety") && spot!=0)) .ra_fail("Kick scoring event requires a goal-line spot")
  if(r=="return_touchdown") d<-.ra_amfootball_td(d,1L-t) else if(r=="safety") {d$score[[t+1L]]<-d$score[[t+1L]]+2L;d<-.ra_amfootball_kick(d,1L-t,20)} else d<-.ra_amfootball_series(d,if(r=="recovery") t else 1L-t,if(r=="touchback") 20 else spot)
  d<-.ra_amfootball_settle(d)
 } else if(k=="try_result") {
  d$extension<-FALSE;r<-p$result;d$sit_out<-rep(list(as.list(rep(FALSE,50))),2)
  if(r=="kick" && !(d$quarter==5 && d$ot_round>=2)) d$score[[t+1L]]<-d$score[[t+1L]]+1L else if(r=="touchdown") d$score[[t+1L]]<-d$score[[t+1L]]+2L else if(r=="defensive_touchdown") d$score[[2L-t]]<-d$score[[2L-t]]+2L else if(r=="safety") d$score[[2L-t]]<-d$score[[2L-t]]+1L else if(r=="defensive_safety") d$score[[t+1L]]<-d$score[[t+1L]]+1L
  d<-.ra_amfootball_after_score(d,t)
 } else if(k=="forego_try") {
  deciding<-(d$quarter==4 && d$clock_ms==0) || (d$quarter==5 && d$ot_series==1)
  if(!deciding || !(d$score[[t+1L]]-d$score[[2L-t]]) %in% c(1,2)) .ra_fail("Only a deciding try with a one- or two-point lead may be declined")
  d<-.ra_amfootball_after_score(d,t)
 } else if(k %in% c("penalty","official_enforcement")) {
  ph<-d$phase;live<-if(k=="penalty") p$live else TRUE
  if(p$elapsed_ms>0 && (!live || ph %in% c("try_ready","period_end"))) .ra_fail("No game time may elapse on this enforcement")
  if(p$elapsed_ms>0) d<-.ra_amfootball_elapse(d,p$elapsed_ms);d$clock_running<-FALSE
  if(p$extend && (d$quarter==5 || d$clock_ms>0 || !live)) .ra_fail("Extension requires an accepted live foul at timed-period expiry")
  if(k=="official_enforcement") {
   if(!(p$spot>0 && p$spot<100) || (p$restart=="ready" && p$line_to_gain<=p$spot)) .ra_fail("Official enforcement must establish a valid restart")
   d$offense<-p$team;d$spot<-p$spot;d$down<-p$down;d$line_to_gain<-p$line_to_gain;d$phase<-p$restart;d$extension<-p$extend
  } else {
   eff<-p$effect;base<-p$base_spot;team<-p$team
   if(!(base>0 && base<100)) .ra_fail("Goal-line enforcement requires an official scoring decision")
   if((eff=="first" && team==t) || (eff=="loss" && (team!=t || !live)) || (eff=="repeat" && !live) || (eff=="keep" && live)) .ra_fail("Down effect conflicts with the offending side or classification")
   if(ph %in% c("kickoff","try_ready") && eff=="first") .ra_fail("Special restart does not award a scrimmage first down")
   shift<-min(p$yards,(if(team==t) base else 100-base)/2);d$spot<-if(team==t) base-shift else base+shift;d$extension<-p$extend
   if(ph=="period_end") d$phase<-d$resume_phase
   if(ph=="try_ready" && eff=="loss") d<-.ra_amfootball_after_score(d,t) else if(!ph %in% c("kickoff","try_ready")) {if(eff=="first" || d$spot>=d$line_to_gain) d<-.ra_amfootball_series(d,t,d$spot) else if(eff=="loss") d<-.ra_amfootball_next_down(d)}
  };d<-.ra_amfootball_settle(d)
 } else if(k=="official_clock") {if(p$running && d$clock_ms==0) .ra_fail("Expired clock cannot run");d$clock_running<-p$running}
 else if(k=="elapse_between") {if(!d$clock_running) .ra_fail("Official game clock is stopped");if(p$milliseconds>d$clock_ms) .ra_fail("Elapsed time exceeds the timed period");d$clock_ms<-d$clock_ms-p$milliseconds;d<-.ra_amfootball_settle(d)}
 else if(k=="clock_runoff") {
  if(!d$quarter %in% c(2,4) || d$clock_ms>=60000) .ra_fail("Ten-second runoff is restricted to the final minute")
  if(p$avoid_with_timeout) {if(d$timeouts[[p$team+1L]]>=3) .ra_fail("No timeout remains to avoid the runoff");d$timeouts[[p$team+1L]]<-d$timeouts[[p$team+1L]]+1L} else d$clock_ms<-max(0,d$clock_ms-10000)
  d$clock_running<-FALSE;d<-.ra_amfootball_settle(d)
 } else if(k=="close_period") {
  q<-d$quarter
  if(q<4) {d$quarter<-q+1L;d$clock_ms<-720000L;d$clock_running<-FALSE;d$phase<-"interval";d$break_ms<-if(q==2) 1200000L else 60000L;if(q==2) {d$timeouts<-list(0L,0L);d$resume_phase<-"halftime_choice"}}
  else if(d$score[[1]]!=d$score[[2]]) d<-.ra_amfootball_finish(d,as.integer(d$score[[2]]>d$score[[1]]),"regulation") else if(d$allow_draw) d<-.ra_amfootball_finish(d,NULL,"draw") else {d$quarter<-5L;d$ot_round<-1L;d$ot_series<-0L;d$timeouts<-list(0L,0L);d$phase<-"ot_choice";d$clock_running<-FALSE}
 } else if(k=="timeout") {
  team<-p$team;cap<-if(d$quarter==5) 1 else 3
  if(d$timeouts[[team+1L]]>=cap) .ra_fail("No charged timeout remains")
  d$timeouts[[team+1L]]<-d$timeouts[[team+1L]]+1L;d$resume_phase<-d$phase;d$phase<-"timeout";d$break_ms<-60000L;d$clock_running<-FALSE
 } else if(k=="elapse_break") {if(p$milliseconds>d$break_ms) .ra_fail("Elapsed time exceeds this break");d$break_ms<-d$break_ms-p$milliseconds;if(d$break_ms==0) d$phase<-d$resume_phase}
 else if(k=="set_lineup") {
  team<-p$team+1L;line<-unlist(p$players)
  if(anyDuplicated(line) || any(vapply(line,function(i) d$status[[team]][[i+1L]]=="excluded" || d$sit_out[[team]][[i+1L]],logical(1)))) .ra_fail("Lineup requires distinct eligible players")
  for(i in 1:50) if(d$status[[team]][[i]]!="excluded") d$status[[team]][[i]]<-if((i-1L) %in% line) "active" else "bench"
 } else if(k=="sanction") {
  team<-p$team+1L;i<-p$player+1L
  if(d$status[[team]][[i]]=="excluded") .ra_fail("Player is already excluded")
  if(p$kind=="unsporting") d$unsporting[[team]][[i]]<-d$unsporting[[team]][[i]]+1L
  if(p$kind=="disqualify" || d$unsporting[[team]][[i]]==2) d$status[[team]][[i]]<-"excluded"
 } else if(k=="miss_next_down") {team<-p$team+1L;i<-p$player+1L;if(d$status[[team]][[i]]!="active") .ra_fail("Restriction requires an active player");d$status[[team]][[i]]<-"bench";d$sit_out[[team]][[i]]<-TRUE;d$clock_running<-FALSE}
 else if(k=="concede") d<-.ra_amfootball_finish(d,1L-p$team,"concession")
 else if(k=="abandon") {d$phase<-"abandoned";d$clock_running<-FALSE;d$break_ms<-0L}
 else if(k=="official_result") d<-.ra_amfootball_finish(d,p$winner,if(is.null(p$winner)) "no_contest" else "official_result")
 d
}
.ra_amfootball_legal<-function(state) {d<-state$data;s<-.ra_amfootball_schemas(d);list(mode="event_types",actions=list(),action_types=unname(lapply(names(s),function(k) list(type=k,params_schema=s[[k]],preconditions=list("Official completed event and current phase must agree."),external_observations=list("Final dead-ball result, legal scoring, official enforcement and clock rulings.")))),constraints=d[c("phase","quarter","clock_ms","score","offense","spot","line_to_gain","down","extension","ot_round","ot_series","timeouts")],completeness=FALSE,reason="official_tackle_football_protocol")}
