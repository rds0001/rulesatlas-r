.ra_canoepolo_int<-function(a,b) list(type="integer",minimum=a,maximum=b)
.ra_canoepolo_active<-function(d,t) as.list(which(unlist(d$status[[t+1L]])=="active")-1L)
.ra_canoepolo_capacity<-function(d,t) 5L-sum(unlist(d$ejected[[t+1L]]))-sum(vapply(d$penalties,function(r) r$team==t,logical(1)))-sum(unlist(d$status[[t+1L]])=="waiting")
.ra_canoepolo_release<-function(d,j) {
 r<-d$penalties[[j]];d$penalties<-d$penalties[-j]
 if(d$status[[r$team+1L]][[r$player+1L]]=="suspended") d$status[[r$team+1L]][[r$player+1L]]<-"bench"
 d
}
.ra_canoepolo_tick<-function(d,ms) {
 d$clock_ms<-max(0L,d$clock_ms-ms)
 for(j in rev(seq_along(d$penalties))) {
  d$penalties[[j]]$remaining_ms<-d$penalties[[j]]$remaining_ms-ms
  if(d$penalties[[j]]$remaining_ms<=0) d<-.ra_canoepolo_release(d,j)
 }
 d
}
.ra_canoepolo_reduced<-function(d) {
 if(any(vapply(0:1,function(t) length(.ra_canoepolo_active(d,t))<3 || .ra_canoepolo_capacity(d,t)<3 || sum(unlist(d$status[[t+1L]]) %in% c("active","bench","waiting"))<3,logical(1)))) {
  d$phase<-"abandoned";d$clock_running<-FALSE;d["pending"]<-list(NULL);d["restart"]<-list(NULL);d["staff_pending"]<-list(NULL)
 }
 d
}
.ra_canoepolo_sanction<-function(d,t,i,level,ejection) {
 if(d$status[[t+1L]][[i+1L]]=="excluded") .ra_fail("Player is already excluded")
 if(!ejection && level<=d$cards[[t+1L]][[i+1L]]) .ra_fail("Sanction level must advance green, yellow, red")
 d$penalties<-Filter(function(r) !(r$team==t && r$player==i),d$penalties)
 d$cards[[t+1L]][[i+1L]]<-if(ejection) 3L else level
 if(ejection) d$ejected[[t+1L]][[i+1L]]<-TRUE else d$penalties[[length(d$penalties)+1L]]<-list(team=t,player=i,remaining_ms=120000L)
 d$status[[t+1L]][[i+1L]]<-if(ejection || level==3) "excluded" else "suspended"
 d$clock_running<-FALSE
 if(d$phase %in% c("live","review")) {d$phase<-"restart";d$restart<-list(kind="free_shot",team=1L-t)}
 .ra_canoepolo_reduced(d)
}
.ra_canoepolo_goal<-function(d,t) {
 d$score[[t+1L]]<-d$score[[t+1L]]+1L
 rr<-which(vapply(d$penalties,function(r) r$team==1L-t,logical(1)))
 if(length(rr)>0) d<-.ra_canoepolo_release(d,rr[[1]])
 d$clock_running<-FALSE;d$phase<-"restart";d$restart<-list(kind="centre",team=1L-t);d["pending"]<-list(NULL)
 if(d$period>2) {d$phase<-"finished";d$winner<-t;d$reason<-"sudden_goal";d["restart"]<-list(NULL)}
 d
}
.ra_canoepolo_check_lineups<-function(d) {
 if(any(vapply(0:1,function(t) {n<-length(.ra_canoepolo_active(d,t));n<3 || n>.ra_canoepolo_capacity(d,t)},logical(1)))) .ra_fail("Restart requires three to five eligible players within reduced capacity")
 if(!is.null(d$staff_pending)) .ra_fail("Ejected team official must leave before restarting")
}
.ra_canoepolo_validate<-function(d) {
 bad<-function(m) .ra_fail(m,code="INVALID_STATE");seen<-character()
 for(r in d$penalties) {
  key<-paste(r$team,r$player)
  if(key %in% seen || !d$status[[r$team+1L]][[r$player+1L]] %in% c("suspended","excluded") || d$ejected[[r$team+1L]][[r$player+1L]]) bad("Timed sanction needs one non-ejected suspended or excluded player")
  seen<-c(seen,key)
 }
 for(t in 0:1) {
  for(i in 0:7) {
   s<-d$status[[t+1L]][[i+1L]]
   if(s=="suspended" && !paste(t,i) %in% seen) bad("Suspension requires its timer")
   if((d$cards[[t+1L]][[i+1L]]==3)!=(s=="excluded")) bad("Red sanction level and exclusion disagree")
   if(d$ejected[[t+1L]][[i+1L]] && s!="excluded") bad("Ejection excludes the player permanently")
  }
  if(!d$phase %in% c("pre","abandoned","finished") && .ra_canoepolo_capacity(d,t)<3) bad("Fewer than three available places requires official adjudication")
 }
 if(d$period>2 && (!d$knockout || d$clock_ms>300000)) bad("Overtime requires knockout mode and five-minute periods")
 if(d$phase=="interval" && d$clock_ms>0) bad("Interval requires a completed period")
 if(d$period>2 && !d$phase %in% c("finished","abandoned") && d$score[[1]]!=d$score[[2]]) bad("Ongoing sudden-goal overtime must remain tied")
 if(d$clock_running && (!d$phase %in% c("live","restart") || d$clock_ms==0)) bad("Running clock requires an active period")
 if(d$phase=="live" && (!is.null(d$restart) || !is.null(d$pending) || !d$clock_running)) bad("Live play requires running clock and no pending restart")
 if(d$phase=="live" && any(vapply(0:1,function(t) {n<-length(.ra_canoepolo_active(d,t));n<3 || n>.ra_canoepolo_capacity(d,t)},logical(1)))) bad("Live lineup must fit reduced capacity")
 if(d$phase=="restart" && is.null(d$restart)) bad("Restart phase requires its entitlement")
 if(!is.null(d$restart) && (d$restart$kind=="referee_ball")!=is.null(d$restart$team)) bad("Only a neutral referees ball has no entitled team")
 if((d$phase=="penalty")!=(!is.null(d$pending))) bad("Penalty phase requires its pending team")
 if(d$phase=="review" && d$clock_ms!=0) bad("Period review follows the horn")
 if((d$phase=="interval")!=(d$break_ms>0)) bad("Interval needs its remaining break duration")
 if(d$phase=="pre" && (d$period!=1 || d$clock_ms!=600000 || any(unlist(d$score)!=0) || length(d$penalties)>0)) bad("Pre-match state precedes play and scoring")
 if(d$phase=="finished") {
  if(identical(d$reason,"regulation")) {
   w<-if(d$score[[1]]==d$score[[2]]) NULL else as.integer(d$score[[2]]>d$score[[1]])
   if(d$period!=2 || d$clock_ms!=0 || !identical(d$winner,w) || (is.null(w) && d$knockout)) bad("Regulation result must match completed time, score and format")
  } else if(identical(d$reason,"sudden_goal")) {
   if(d$period<3 || is.null(d$winner) || d$score[[d$winner+1L]]!=d$score[[2L-d$winner]]+1) bad("Sudden goal must break an overtime tie")
  } else if(identical(d$reason,"concession")) {if(is.null(d$winner)) bad("Concession requires a winner")} else if(is.null(d$reason) || !d$reason %in% c("official_result","no_contest")) bad("Finished match needs a result reason")
 } else if(!is.null(d$winner) || !is.null(d$reason)) bad("Ongoing match cannot declare a result")
}
.ra_canoepolo_schemas<-function(d) {
 ph<-d$phase;s<-list();empty<-setNames(list(),character());T<-.ra_canoepolo_int(0,1);P<-.ra_canoepolo_int(0,7);NT<-c(T,list(nullable=TRUE));NP<-c(P,list(nullable=TRUE));B<-list(type="boolean")
 if(ph=="finished") return(s)
 if(ph=="abandoned") return(list(official_result=list(winner=NT)))
 if(ph=="pre") s$start_match<-empty
 if(ph=="start") s$sprint_start<-empty
 if(ph %in% c("pre","start","live","restart","penalty","review","interval")) s$set_lineup<-list(team=T,players=list(type="array",minItems=3,maxItems=5,items=P))
 if(d$clock_running) s$elapse<-list(milliseconds=.ra_canoepolo_int(1,600000))
 if(ph=="live") s<-c(s,list(goal=list(team=T,player=NP),restricted_exit=list(team=T,player=P)))
 if(ph %in% c("live","restart")) s$award_restart<-list(kind=list(type="enum",values=list("free_throw","free_shot","sideline","goal_line","corner","centre","referee_ball")),team=NT,stop_clock=B)
 if(ph %in% c("live","restart","review","penalty")) {
  s<-c(s,list(sanction=list(team=T,player=P,level=.ra_canoepolo_int(1,3),ejection=B),staff_misconduct=list(team=T,severe=B)))
  if(is.null(d$pending)) s$award_penalty<-list(team=T,offender=P,level=.ra_canoepolo_int(1,3),ejection=B)
 }
 if(!d$clock_running) s$clear_exit<-list(team=T,player=P)
 if(!is.null(d$staff_pending)) s$confirm_staff_departure<-list(complied=B)
 if(ph=="restart") {
  if(d$clock_ms>0) s$take_restart<-list(team=NT,player=NP,result=list(type="enum",values=list("play","direct_goal","direct_miss")),milliseconds=.ra_canoepolo_int(0,5000)) else s<-c(s,list(close_period=empty,late_goal=list(team=T,player=NP)))
 }
 if(ph=="review") s<-c(s,list(close_period=empty,late_goal=list(team=T,player=NP)))
 if(ph=="penalty") s$take_penalty<-list(shooter=P,goalkeeper=P,result=list(type="enum",values=list("goal","miss","retake","offensive_fault")),milliseconds=.ra_canoepolo_int(0,5000))
 if(ph=="interval") s$elapse_break<-list(milliseconds=.ra_canoepolo_int(1,180000))
 c(s,list(abandon=empty,concede=list(team=T)))
}
.ra_canoepolo_apply<-function(state,action) {
 d<-state$data;k<-action$type;p<-action$params;ss<-.ra_canoepolo_schemas(d)
 if(!k %in% names(ss)) .ra_fail("Action is not permitted in this phase")
 .ra_fields(p,ss[[k]],"/params");t<-p$team
 if(k=="start_match") {
  if(any(vapply(0:1,function(j) length(.ra_canoepolo_active(d,j))!=5,logical(1)))) .ra_fail("Five active players per team are required at match start")
  d$phase<-"start"
 } else if(k=="sprint_start") {.ra_canoepolo_check_lineups(d);d$phase<-"live";d$clock_running<-TRUE;d$late_used<-FALSE;d["restart"]<-list(NULL)} else if(k=="set_lineup") {
  ids<-p$players
  if(anyDuplicated(unlist(ids)) || length(ids)>.ra_canoepolo_capacity(d,t) || any(vapply(ids,function(i) !d$status[[t+1L]][[i+1L]] %in% c("active","bench"),logical(1)))) .ra_fail("Lineup requires distinct eligible players within capacity")
  for(i in .ra_canoepolo_active(d,t)) d$status[[t+1L]][[i+1L]]<-"bench"
  for(i in ids) d$status[[t+1L]][[i+1L]]<-"active"
 } else if(k=="elapse") {
  if(p$milliseconds>d$clock_ms) .ra_fail("Clock cannot pass the period boundary")
  d<-.ra_canoepolo_tick(d,p$milliseconds)
  if(d$clock_ms==0) {d$phase<-"review";d$clock_running<-FALSE;d["restart"]<-list(NULL)}
 } else if(k %in% c("goal","late_goal")) {
  if(!is.null(p$player) && !p$player %in% unlist(.ra_canoepolo_active(d,t))) .ra_fail("Recorded scorer must be an eligible active player")
  if(k=="late_goal") {if(d$late_used) .ra_fail("Period-ending goal already recorded");d$late_used<-TRUE}
  d<-.ra_canoepolo_goal(d,t)
 } else if(k=="restricted_exit") {
  if(!p$player %in% unlist(.ra_canoepolo_active(d,t))) .ra_fail("Only an active player can leave under a restricted-entry ruling")
  d$status[[t+1L]][[p$player+1L]]<-"waiting";d<-.ra_canoepolo_reduced(d)
 } else if(k=="clear_exit") {
  if(d$status[[t+1L]][[p$player+1L]]!="waiting") .ra_fail("Player has no pending restricted exit")
  d$status[[t+1L]][[p$player+1L]]<-"bench"
 } else if(k=="award_restart") {
  if((p$kind=="referee_ball")!=is.null(t)) .ra_fail("Only a referees ball has neutral entitlement")
  d$phase<-"restart";d$restart<-list(kind=p$kind,team=t)
  if(p$stop_clock) d$clock_running<-FALSE
 } else if(k=="take_restart") {
  .ra_canoepolo_check_lineups(d);r<-d$restart
  if(!identical(t,r$team) || (is.null(t) && !is.null(p$player)) || (!is.null(t) && (is.null(p$player) || !p$player %in% unlist(.ra_canoepolo_active(d,t))))) .ra_fail("Restart requires the entitled active thrower or a neutral referees ball")
  if(p$result!="play" && r$kind!="free_shot") .ra_fail("Only a free shot permits a direct attempt at goal")
  if(p$milliseconds>d$clock_ms) .ra_fail("An ordinary restart must be taken before the period ends")
  d<-.ra_canoepolo_tick(d,p$milliseconds);d$phase<-if(d$clock_ms>0) "live" else "review";d$clock_running<-d$clock_ms>0;d["restart"]<-list(NULL)
  if(p$result=="direct_goal") d<-.ra_canoepolo_goal(d,t)
 } else if(k=="sanction") d<-.ra_canoepolo_sanction(d,t,p$player,p$level,p$ejection) else if(k=="award_penalty") {
  d<-.ra_canoepolo_sanction(d,1L-t,p$offender,p$level,p$ejection)
  if(d$phase!="abandoned") {d$phase<-"penalty";d$pending<-t;d["restart"]<-list(NULL);d$clock_running<-FALSE}
 } else if(k=="take_penalty") {
  .ra_canoepolo_check_lineups(d);t<-d$pending
  if(!p$shooter %in% unlist(.ra_canoepolo_active(d,t)) || !p$goalkeeper %in% unlist(.ra_canoepolo_active(d,1L-t))) .ra_fail("Penalty shot requires an active shooter and defending goalkeeper")
  d<-.ra_canoepolo_tick(d,p$milliseconds)
  if(p$result=="retake") {} else if(p$result=="goal") d<-.ra_canoepolo_goal(d,t) else if(p$result=="offensive_fault") {d$phase<-"restart";d["pending"]<-list(NULL);d$restart<-list(kind="free_throw",team=1L-t);d$clock_running<-FALSE} else {d$phase<-if(d$clock_ms>0) "live" else "review";d["pending"]<-list(NULL);d$clock_running<-d$clock_ms>0}
 } else if(k=="staff_misconduct") {
  if(!p$severe && !d$staff_warning[[t+1L]]) d$staff_warning[[t+1L]]<-TRUE else {
   if(!is.null(d$staff_pending)) .ra_fail("Resolve the previous staff departure first")
   d$staff_warning[[t+1L]]<-TRUE;d$staff_pending<-t;d$clock_running<-FALSE
   if(d$phase=="live") {d$phase<-"restart";d$restart<-list(kind="free_throw",team=1L-t)}
  }
 } else if(k=="confirm_staff_departure") {
  d["staff_pending"]<-list(NULL)
  if(!p$complied) {d$phase<-"abandoned";d["pending"]<-list(NULL);d["restart"]<-list(NULL);d$clock_running<-FALSE}
 } else if(k=="close_period") {
  if(!is.null(d$staff_pending)) .ra_fail("Resolve the official staff departure decision")
  if(d$period==2 && (d$score[[1]]!=d$score[[2]] || !d$knockout)) {d$phase<-"finished";d["winner"]<-list(if(d$score[[1]]==d$score[[2]]) NULL else as.integer(d$score[[2]]>d$score[[1]]));d$reason<-"regulation";d["restart"]<-list(NULL)} else {d$phase<-"interval";d$break_ms<-if(d$period<=2) 180000L else 60000L;d["restart"]<-list(NULL)}
 } else if(k=="elapse_break") {
  if(p$milliseconds>d$break_ms) .ra_fail("Elapsed time exceeds the interval")
  d$break_ms<-d$break_ms-p$milliseconds
  if(d$break_ms==0) {d$clock_ms<-if(d$period==1) 600000L else 300000L;d$period<-d$period+1L;d$phase<-"start";d$late_used<-FALSE}
 } else if(k=="abandon") {d$phase<-"abandoned";d$clock_running<-FALSE;d$break_ms<-0L;d["pending"]<-list(NULL);d["restart"]<-list(NULL);d["staff_pending"]<-list(NULL)} else if(k=="concede") {d$phase<-"finished";d$winner<-1L-t;d$reason<-"concession";d$clock_running<-FALSE;d$break_ms<-0L;d["pending"]<-list(NULL);d["restart"]<-list(NULL);d["staff_pending"]<-list(NULL)} else if(k=="official_result") {d$phase<-"finished";d["winner"]<-list(p$winner);d$reason<-if(is.null(p$winner)) "no_contest" else "official_result"}
 d
}
.ra_canoepolo_legal<-function(state) {
 d<-state$data;s<-.ra_canoepolo_schemas(d)
 list(mode="event_types",actions=list(),action_types=lapply(names(s),function(k) list(type=k,params_schema=s[[k]],preconditions=list("Restart entitlement, eligible lineup, sanction level and current clock phase must permit the event."),external_observations=list("Official goals, fouls, penalty-shot outcomes, restart decisions and elapsed playing time."))),constraints=list(phase=d$phase,period=d$period,clock_ms=d$clock_ms,clock_running=d$clock_running,score=d$score,capacity=lapply(0:1,function(t) .ra_canoepolo_capacity(d,t)),penalties=d$penalties,restart=d$restart,pending=d$pending,cards=d$cards),completeness=FALSE,reason="official_2023_canoe_polo_events")
}
.ra_canoepolo_outcome<-function(state,pack) .ra_handball_outcome(state,pack)
