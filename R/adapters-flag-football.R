.ra_flag_i<-function(a,b) list(type="integer",minimum=a,maximum=b)
.ra_flag_e<-function(...) list(type="enum",values=list(...))
.ra_flag_active<-function(d,t) which(unlist(d$status[[t+1L]])=="active")-1L
.ra_flag_finish<-function(d,w,r) {d$phase<-"finished";d["winner"]<-list(w);d$reason<-r;d$clock_running<-FALSE;d$break_ms<-0L;d}
.ra_flag_series<-function(d,t,spot=5) {d$offense<-t;d$spot<-spot;d$down<-1L;d$middle_gained<-spot>25;d$phase<-"ready";d$try_value<-0L;d}
.ra_flag_end_ot<-function(d) {
 if(d$ot_series==0) {d$ot_series<-1L;d<-.ra_flag_series(d,1L-d$ot_first,25)} else {
  if(d$score[[1]]!=d$score[[2]]) return(.ra_flag_finish(d,as.integer(d$score[[2]]>d$score[[1]]),"overtime"))
  d$ot_round<-d$ot_round+1L;d$ot_series<-0L;d<-.ra_flag_series(d,d$ot_first,45)
 }
 if(d$ot_round>=2) {d$phase<-"try_ready";d$try_value<-1L;d$spot<-45} else d$middle_gained<-TRUE
 d$clock_running<-FALSE;d
}
.ra_flag_settle<-function(d) {
 if(d$half<=2 && d$clock_ms==0 && d$phase=="ready" && !d$extension) {d$phase<-"period_end";d$clock_running<-FALSE}
 if(d$half<=2 && d$clock_ms<=120000 && !d$notified) {d$notified<-TRUE;d$mercy_clock<-abs(d$score[[1]]-d$score[[2]])>=30;d$clock_running<-FALSE}
 d
}
.ra_flag_elapse<-function(d,n) {if(n>d$clock_ms || (d$half==3 && n>0)) .ra_fail("Elapsed time exceeds the timed period");d$clock_ms<-d$clock_ms-n;d}
.ra_flag_down_end<-function(d) {
 if(d$down==4) {if(d$half==3) d<-.ra_flag_end_ot(d) else d<-.ra_flag_series(d,1L-d$offense)} else d$down<-d$down+1L
 d
}
.ra_flag_distance<-function(d,t,base,yards) {
 distance<-if(t==d$offense) base else 50-base;shift<-min(yards,distance/2);d$spot<-if(t==d$offense) base-shift else base+shift;d
}
.ra_flag_carry<-function(d) {for(item in d$carry) d<-.ra_flag_distance(d,item$team,d$spot,item$yards);d$carry<-list();if(d$spot>25) d$middle_gained<-TRUE;d}
.ra_flag_validate<-function(d) {
 bad<-function(m) .ra_fail(m,code="INVALID_STATE")
 for(t in 1:2) {
  if(length(.ra_flag_active(d,t-1L))>5) bad("At most five players may be active")
  for(i in 1:15) {
   if(d$unsporting[[t]][[i]]==2 && d$status[[t]][[i]]!="excluded") bad("Second unsportsmanlike act requires exclusion")
   if(d$sit_out[[t]][[i]] && d$status[[t]][[i]]=="active") bad("A player ordered to miss a down cannot be active")
  }
 }
 if(d$half==3 && (d$clock_ms!=0 || d$ot_round<1 || is.null(d$ot_first) || d$clock_running)) bad("Overtime is untimed and requires a recorded toss")
 if(d$phase %in% c("ready","try_ready") && !(d$spot>0 && d$spot<50)) bad("Next snap must be inside the field of play")
 if(d$phase=="try_ready" && !d$try_value %in% c(1,2)) bad("A try needs a fixed point value")
 if(d$phase=="try_choice" && d$try_value!=0) bad("Try choice cannot already have a declared value")
 if(d$phase=="period_end" && (d$clock_ms!=0 || d$extension)) bad("Period end requires expiry and no outstanding extension")
 if((d$phase %in% c("timeout","interval"))!=(d$break_ms>0)) bad("Break clock must agree with phase")
 if(d$phase=="finished") {
  if(is.null(d$reason) || (d$reason %in% c("draw","no_contest"))!=is.null(d$winner)) bad("Result must identify winner, draw or no contest")
  if(d$reason %in% c("regulation","overtime") && d$score[[d$winner+1L]]<=d$score[[2L-d$winner]]) bad("Winner must lead the score")
  if(d$reason=="draw" && d$score[[1]]!=d$score[[2]]) bad("Draw requires equal scores")
 } else if(!is.null(d$winner) || !is.null(d$reason)) bad("Unfinished match cannot have a result")
}
.ra_flag_schemas<-function(d) {
 ph<-d$phase;s<-list();ii<-.ra_flag_i;ee<-.ra_flag_e;t<-ii(0,1);p<-ii(0,14);b<-list(type="boolean");spot<-list(type="number",minimum=0,maximum=50);empty<-setNames(list(),character())
 if(ph=="finished") return(s)
 if(ph=="abandoned") return(list(official_result=list(winner=c(t,list(nullable=TRUE)))))
 if(ph=="pre_game") s$choose_start<-list(team=t)
 if(ph=="ready") s$play<-list(result=ee("advance","incomplete","interception","touchdown","defensive_touchdown","safety","touchback"),spot=spot,elapsed_ms=ii(0,1200000),out_of_bounds=b,forward_pass=b)
 if(ph %in% c("ready","try_choice","try_ready") && d$half<=2) {s$elapse_between<-list(milliseconds=ii(1,1200000));s$timeout<-list(team=t)}
 if(ph=="try_choice") s$choose_try<-list(points=ii(1,2))
 if(ph=="try_ready") s$try_result<-list(result=ee("success","miss","defensive_touchdown","offense_safety","defense_safety"),elapsed_ms=ii(0,1200000))
 if(ph %in% c("ready","try_ready","period_end")) s$penalty<-list(team=t,base_spot=spot,yards=ee(0,5,10),effect=ee("repeat","loss","first","keep"),live=b)
 if(ph %in% c("try_choice","interval","period_end")) s$carry_penalty<-list(team=t,yards=ee(5,10))
 if(ph=="period_end") s$close_period<-empty
 if(ph %in% c("timeout","interval")) s$elapse_break<-list(milliseconds=ii(1,120000))
 if(ph=="ot_toss") s$choose_overtime<-list(first=t)
 if(ph!="pre_game") s<-c(s,list(set_lineup=list(team=t,players=list(type="array",minItems=4L,maxItems=5L,items=p)),sanction=list(team=t,player=p,kind=ee("unsporting","flagrant")),miss_next_down=list(team=t,player=p),official_clock=list(running=b)))
 c(s,list(concede=list(team=t),abandon=empty))
}
.ra_flag_apply<-function(state,action) {
 d<-state$data;p<-action$params;k<-action$type;s<-.ra_flag_schemas(d)
 if(!k %in% names(s)) .ra_fail("Action is not permitted in this phase")
 .ra_fields(p,s[[k]],"/params")
 if(k=="choose_start") {d$first_team<-p$team;d<-.ra_flag_series(d,p$team)} else if(k=="play") {
  if(any(vapply(0:1,function(t) length(.ra_flag_active(d,t))<4,logical(1)))) .ra_fail("Each team needs at least four active eligible players")
  if(d$half<=2 && d$clock_ms==0 && !d$extension) .ra_fail("Expired period requires an authorized untimed down")
  r<-p$result;old<-d$offense;oldspot<-d$spot;n<-p$elapsed_ms
  if(oldspot>=45 && !p$forward_pass && r %in% c("advance","touchdown")) .ra_fail("A play from the no-running zone requires a forward pass beyond scrimmage")
  if((r %in% c("touchdown","defensive_touchdown") && p$spot!=50) || (r %in% c("safety","touchback") && p$spot!=0)) .ra_fail("Scoring events must use the matching goal-line spot")
  if(r=="incomplete" && p$spot!=oldspot) .ra_fail("Incomplete pass retains the previous scrimmage spot")
  if(r %in% c("advance","interception") && !(p$spot>0 && p$spot<50)) .ra_fail("Use a scoring or touchback event at the goal line")
  d<-.ra_flag_elapse(d,n);d$extension<-FALSE;d$clock_running<-d$half<=2 && d$clock_ms>0;d$sit_out<-rep(list(rep(list(FALSE),15)),2);first<-FALSE
  if(r %in% c("advance","incomplete")) {
   d$spot<-p$spot
   if(!d$middle_gained && d$spot>25) {d$middle_gained<-TRUE;d$down<-1L;first<-TRUE} else {previous<-d$offense;d<-.ra_flag_down_end(d);first<-d$offense!=previous}
  } else if(r %in% c("interception","touchback")) {
   if(d$half==3) d<-.ra_flag_end_ot(d) else d<-.ra_flag_series(d,1L-old,if(r=="interception") p$spot else 5)
   first<-TRUE
  } else if(r %in% c("touchdown","defensive_touchdown")) {
   w<-if(r=="touchdown") old else 1L-old;d$score[[w+1L]]<-d$score[[w+1L]]+6L
   if(d$half==3 && d$ot_series==0 && w!=old) d<-.ra_flag_finish(d,w,"overtime") else {d$offense<-w;d$phase<-"try_choice";d$try_value<-0L}
  } else if(r=="safety") {
   d$score[[2L-old]]<-d$score[[2L-old]]+2L
   if(d$half==3) {if(d$ot_series==0) d<-.ra_flag_finish(d,1L-old,"overtime") else d<-.ra_flag_end_ot(d)} else d<-.ra_flag_series(d,1L-old)
   first<-TRUE
  }
  if(d$clock_ms<=120000 && !d$mercy_clock && (first || p$out_of_bounds || r!="advance")) d$clock_running<-FALSE
  d<-.ra_flag_settle(d)
 } else if(k=="elapse_between") {
  if(!d$clock_running) .ra_fail("The official game clock is stopped")
  d<-.ra_flag_settle(.ra_flag_elapse(d,p$milliseconds))
 } else if(k=="choose_try") {d$try_value<-p$points;d$spot<-if(p$points==1) 45 else 40;d$phase<-"try_ready";d<-.ra_flag_carry(d)} else if(k=="try_result") {
  if(any(vapply(0:1,function(t) length(.ra_flag_active(d,t))<4,logical(1)))) .ra_fail("Each team needs four active players for a try")
  if(p$elapsed_ms>0 && !d$clock_running) .ra_fail("Try elapsed time requires a running game clock")
  d<-.ra_flag_elapse(d,p$elapsed_ms);r<-p$result;old<-d$offense;d$sit_out<-rep(list(rep(list(FALSE),15)),2)
  if(r=="success") d$score[[old+1L]]<-d$score[[old+1L]]+d$try_value else if(r=="defensive_touchdown") d$score[[2L-old]]<-d$score[[2L-old]]+2L else if(r=="offense_safety") d$score[[2L-old]]<-d$score[[2L-old]]+1L else if(r=="defense_safety") d$score[[old+1L]]<-d$score[[old+1L]]+1L
  if(d$half==3) {if(d$ot_round>=2 && d$ot_series==0 && r %in% c("defensive_touchdown","offense_safety")) d<-.ra_flag_finish(d,1L-old,"overtime") else d<-.ra_flag_end_ot(d)} else {d<-.ra_flag_series(d,1L-old);d<-.ra_flag_carry(d);d<-.ra_flag_settle(d)}
 } else if(k=="penalty") {
  t<-p$team;eff<-p$effect;live<-p$live;old<-d$offense;is_try<-d$phase=="try_ready"
  if((eff=="first" && t==old) || (eff=="loss" && (t!=old || !live)) || (eff=="repeat" && !live) || (eff=="keep" && live)) .ra_fail("Down effect must agree with offending side and live/dead classification")
  if(!(p$base_spot>0 && p$base_spot<50)) .ra_fail("Goal-line foul awards require an official scoring decision")
  d<-.ra_flag_distance(d,t,p$base_spot,p$yards)
  if(d$clock_ms<=120000 && !d$mercy_clock) d$clock_running<-FALSE
  if(!is_try) {
   if(eff=="first" || (!d$middle_gained && d$spot>25)) {d$down<-1L;d$middle_gained<-d$middle_gained || d$spot>25} else if(eff=="loss") d<-.ra_flag_down_end(d)
   if(live && eff!="loss" && d$half<=2 && d$clock_ms==0) {d$extension<-TRUE;d$phase<-"ready"}
  } else if(eff=="loss") {if(d$half==3) d<-.ra_flag_end_ot(d) else d<-.ra_flag_series(d,1L-old)}
  d<-.ra_flag_settle(d)
 } else if(k=="carry_penalty") d$carry<-c(d$carry,list(list(team=p$team,yards=p$yards))) else if(k=="close_period") {
  if(d$half==1) {
   d$half<-2L;d$clock_ms<-1200000L;d$timeouts<-list(0L,0L);d$notified<-FALSE;d$mercy_clock<-FALSE;d$clock_running<-FALSE;d$break_ms<-120000L;d<-.ra_flag_series(d,1L-d$first_team);d<-.ra_flag_carry(d);d$phase<-"interval"
  } else if(d$score[[1]]!=d$score[[2]]) d<-.ra_flag_finish(d,as.integer(d$score[[2]]>d$score[[1]]),"regulation") else if(d$allow_draw) d<-.ra_flag_finish(d,NULL,"draw") else {d$phase<-"interval";d$break_ms<-120000L;d$interval_overtime<-TRUE;d$clock_running<-FALSE}
 } else if(k=="choose_overtime") {d$half<-3L;d$ot_first<-p$first;d$ot_round<-1L;d$ot_series<-0L;d<-.ra_flag_series(d,p$first,25);d$middle_gained<-TRUE;d<-.ra_flag_carry(d)} else if(k=="timeout") {
  t<-p$team+1L;if(d$timeouts[[t]]>=2) .ra_fail("No charged timeout remains in this half")
  d$timeouts[[t]]<-d$timeouts[[t]]+1L;d$resume_phase<-d$phase;d$phase<-"timeout";d$break_ms<-60000L;d$clock_running<-FALSE
 } else if(k=="elapse_break") {
  if(p$milliseconds>d$break_ms) .ra_fail("Elapsed time exceeds the current break")
  d$break_ms<-d$break_ms-p$milliseconds
  if(d$break_ms==0) {
   d$phase<-if(d$phase=="timeout") d$resume_phase else if(d$interval_overtime) "ot_toss" else "ready"
   if(d$phase=="ready") d<-.ra_flag_carry(d)
  }
 } else if(k=="set_lineup") {
  t<-p$team+1L;line<-unlist(p$players)
  if(anyDuplicated(line)>0 || any(vapply(line,function(i) d$status[[t]][[i+1L]]=="excluded" || d$sit_out[[t]][[i+1L]],logical(1)))) .ra_fail("Lineup needs distinct eligible players")
  for(i in 0:14) if(d$status[[t]][[i+1L]]!="excluded") d$status[[t]][[i+1L]]<-if(i %in% line) "active" else "bench"
 } else if(k=="sanction") {
  t<-p$team+1L;i<-p$player+1L
  if(d$status[[t]][[i]]=="excluded") .ra_fail("Player is already excluded")
  if(p$kind=="unsporting") d$unsporting[[t]][[i]]<-d$unsporting[[t]][[i]]+1L
  if(p$kind=="flagrant" || d$unsporting[[t]][[i]]==2) d$status[[t]][[i]]<-"excluded"
 } else if(k=="miss_next_down") {
  t<-p$team+1L;i<-p$player+1L
  if(d$status[[t]][[i]]!="active") .ra_fail("Removal requires an active player")
  d$status[[t]][[i]]<-"bench";d$sit_out[[t]][[i]]<-TRUE;d$clock_running<-FALSE
 } else if(k=="official_clock") {
  if(p$running && (d$half==3 || d$clock_ms==0 || !d$phase %in% c("ready","try_choice","try_ready"))) .ra_fail("Clock cannot run in this phase")
  d$clock_running<-p$running
 } else if(k=="concede") d<-.ra_flag_finish(d,1L-p$team,"concession") else if(k=="abandon") {d$phase<-"abandoned";d$clock_running<-FALSE;d$break_ms<-0L} else if(k=="official_result") d<-.ra_flag_finish(d,p$winner,if(is.null(p$winner)) "no_contest" else "official_result")
 d
}
.ra_flag_legal<-function(state) {
 d<-state$data;s<-.ra_flag_schemas(d)
 list(mode="event_types",actions=list(),action_types=lapply(names(s),function(k) list(type=k,params_schema=s[[k]],preconditions=list("Phase, down, official clock and eligible lineup must permit the event."),external_observations=list("Completed downs, official spots, penalty classifications, timing and sanctions."))),constraints=list(phase=d$phase,score=d$score,half=d$half,clock_ms=d$clock_ms,offense=d$offense,spot=d$spot,down=d$down,middle_gained=d$middle_gained,try_value=d$try_value,ot_round=d$ot_round,ot_series=d$ot_series,extension=d$extension),completeness=FALSE,reason="official_flag_football_downs")
}
.ra_flag_outcome<-function(state,pack) {o<-.ra_pelota_outcome(state,pack);if(identical(state$data$reason,"draw")) o$result_kind<-"draw";o}
