.ra_waterpolo_i<-function(a,b) list(type="integer",minimum=a,maximum=b)
.ra_waterpolo_e<-function(...) list(type="enum",values=list(...))
.ra_waterpolo_eq<-function(a,b) if(is.null(a)||is.null(b)) is.null(a)&&is.null(b) else a==b
.ra_waterpolo_reduction<-function(d,t,p) {js<-which(vapply(d$exclusions,function(r) r$team==t&&r$player==p,logical(1)));if(length(js)) js[[1]] else NULL}
.ra_waterpolo_capacity<-function(d,t) 7L-sum(vapply(d$exclusions,function(r) r$team==t,logical(1)))
.ra_waterpolo_eligible<-function(d,t,p) d$status[[t+1L]][[p+1L]]=="eligible"
.ra_waterpolo_mark_release<-function(d,team=NULL,goal=FALSE,changed=FALSE) {
 for(j in seq_along(d$exclusions)) {r<-d$exclusions[[j]];if(r$kind=="ordinary"&&(goal||.ra_waterpolo_eq(r$team,team)||(changed&&r$paired))) d$exclusions[[j]]$ready<-TRUE};d
}
.ra_waterpolo_entitlement<-function(d,t,reset=TRUE) {
 changed<-!is.null(d$attack)&&d$attack!=t;d$attack<-t;if(reset) d$shot_ms<-30000L;.ra_waterpolo_mark_release(d,t,changed=changed)
}
.ra_waterpolo_restart<-function(d,kind,t,direct=FALSE) {d$phase<-"restart";d$restart<-kind;d["attack"]<-list(t);d$direct_allowed<-direct;d$second_touch<-FALSE;d}
.ra_waterpolo_scored<-function(d,t) {d$score[[t+1L]]<-d$score[[t+1L]]+1L;d<-.ra_waterpolo_mark_release(d,goal=TRUE);d$shot_ms<-30000L;.ra_waterpolo_restart(d,"centre",1L-t)}
.ra_waterpolo_remove_active<-function(d,t,p) {
 d$active[[t+1L]]<-Filter(function(x) x!=p,d$active[[t+1L]])
 if(.ra_waterpolo_eq(d$keeper[[t+1L]],p)) d$keeper[t+1L]<-list(NULL);d
}
.ra_waterpolo_add_exclusion<-function(d,t,p,kind="ordinary",dismiss=FALSE,paired=FALSE) {
 z<-t+1L;q<-p+1L
 if(d$status[[z]][[q]]=="dismissed") .ra_fail("Player is already permanently excluded")
 d$fouls[[z]][[q]]<-d$fouls[[z]][[q]]+1L;d$status[[z]][[q]]<-if(dismiss||d$fouls[[z]][[q]]>=3) "dismissed" else "excluded"
 d$exclusions<-Filter(function(r) !(r$team==t&&r$player==p),d$exclusions)
 d$exclusions[[length(d$exclusions)+1L]]<-list(team=t,player=p,kind=kind,ms=if(kind=="brutality") 240000L else 20000L,ready=FALSE,cleared=FALSE,paired=paired)
 .ra_waterpolo_remove_active(d,t,p)
}
.ra_waterpolo_keeper_ok<-function(d,t) {
 ps<-unlist(d$active[[t+1L]]);k<-d$keeper[[t+1L]]
 if(!is.null(k)&&!k %in% ps) return(FALSE)
 if(length(ps)==7L&&is.null(k)) return(FALSE)
 if(!is.null(k)&&!k %in% c(0,12)&&any(vapply(c(0L,12L),function(p) .ra_waterpolo_eligible(d,t,p),logical(1)))) return(FALSE)
 if(any(vapply(c(0L,12L),function(p) p %in% ps&&! .ra_waterpolo_eq(p,k),logical(1)))&&any(vapply(1:11,function(p) .ra_waterpolo_eligible(d,t,p)&&!p %in% ps,logical(1)))) return(FALSE)
 TRUE
}
.ra_waterpolo_validate<-function(d) {
 bad<-function(m) .ra_fail(m,code="INVALID_STATE");seen<-character()
 for(r in d$exclusions) {
  key<-paste(r$team,r$player,sep=":")
  if(key %in% seen||!d$status[[r$team+1L]][[r$player+1L]] %in% c("excluded","dismissed")) bad("Each reduction requires one excluded player")
  seen<-c(seen,key)
  if(r$kind=="brutality"&&d$status[[r$team+1L]][[r$player+1L]]!="dismissed") bad("Brutality excludes the player permanently")
  if(r$ms==0&&!r$ready) bad("Expired exclusions must be eligible for official release")
  if(r$kind=="brutality"&&r$ms>0&&r$ready) bad("Brutality cannot be released before four playing minutes")
 }
 for(t in 0:1) {
  z<-t+1L
  if(anyDuplicated(unlist(d$active[[z]]))||any(vapply(d$active[[z]],function(p) !.ra_waterpolo_eligible(d,t,p),logical(1)))) bad("Active players must be distinct and eligible")
  if(d$phase=="live"&&length(d$active[[z]])>.ra_waterpolo_capacity(d,t)) bad("Lineup exceeds available playing places")
  if(!.ra_waterpolo_keeper_ok(d,t)) bad("Goalkeeper designation and available field-player substitutes must be respected")
  for(p in 0:12) {s<-d$status[[z]][[p+1L]];if(s=="excluded"&&!paste(t,p,sep=":") %in% seen) bad("Temporary exclusion requires its reduction");if(d$fouls[[z]][[p+1L]]>=3&&s!="dismissed") bad("Three personal fouls exclude a player for the match")}
 }
 if(d$phase=="live"&&(is.null(d$attack)||d$period_ms==0||d$shot_ms==0)) bad("Live play requires an attack entitlement and both clocks")
 if(d$phase=="siren_review"&&d$period_ms>0&&d$shot_ms>0) bad("Siren review requires an expired clock")
 if(d$phase=="period_review"&&d$period_ms>0) bad("Period review follows an expired period")
 if((d$phase=="penalty")!=(length(d$penalty_queue)>0)) bad("Penalty phase requires at least one awarded throw")
 if(d$phase %in% c("interval","timeout")&&d$pause_ms==0) bad("Break phase requires remaining time")
 if(d$phase=="finished"&&d$reason=="regulation") {
  w<-if(d$score[[1]]==d$score[[2]]) NULL else as.integer(d$score[[2]]>d$score[[1]])
  if(d$period!=4||d$period_ms>0||!.ra_waterpolo_eq(d$winner,w)) bad("Regulation result must match four completed periods")
 } else if(d$phase!="finished"&&!is.null(d$winner)) bad("Only a finished game has a winner")
 invisible(NULL)
}
.ra_waterpolo_tick<-function(d,ms) {
 if(d$phase %in% c("interval","timeout")) {
  if(ms>d$pause_ms) .ra_fail("Advance only to the end of this break")
  d$pause_ms<-d$pause_ms-ms
  if(d$pause_ms==0) {
   if(d$phase=="interval") {d$period<-d$period+1L;d$phase<-"start";d$period_ms<-480000L;d["attack"]<-list(NULL);d$shot_ms<-30000L;d$restart<-"sprint";d$second_touch<-FALSE;d$direct_allowed<-FALSE} else d<-.ra_waterpolo_restart(d,if(d$timeout_corner) "corner" else "timeout",d$attack,d$timeout_corner)
  };return(d)
 }
 if(ms>min(d$period_ms,d$shot_ms)) .ra_fail("Advance only to the next game or attack-clock signal")
 d$period_ms<-d$period_ms-ms;d$shot_ms<-d$shot_ms-ms
 for(j in seq_along(d$exclusions)) {d$exclusions[[j]]$ms<-max(0L,d$exclusions[[j]]$ms-ms);if(d$exclusions[[j]]$ms==0) d$exclusions[[j]]$ready<-TRUE}
 if(d$period_ms==0||d$shot_ms==0) d$phase<-"siren_review";d
}
.ra_waterpolo_schemas<-function(d) {
 i<-.ra_waterpolo_i;e<-.ra_waterpolo_e;t<-i(0,1);p<-i(0,12);nt<-t;nt$nullable<-TRUE;np<-p;np$nullable<-TRUE;b<-list(type="boolean");empty<-setNames(list(),character());ph<-d$phase;out<-empty
 if(ph=="finished") return(out)
 if(ph=="abandoned") return(list(official_result=list(winner=nt)))
 out<-list(abandon=empty,reentry_restriction=list(team=t,player=p,restricted=b))
 if(ph!="penalty") out$lineup<-list(team=t,players=list(type="array",minItems=0,maxItems=7,items=p),keeper=np)
 if(ph=="pre") out$start_match<-empty
 if(ph=="start") out$sprint_winner<-list(team=t)
 if(ph %in% c("live","timeout","interval")) out$advance<-list(ms=i(1,480000))
 if(ph=="live") out<-c(out,list(turnover=empty,second_touch=empty,goal=list(team=t,own_goal=b),shot_result=list(result=e("goal","attack_rebound","defence_rebound","corner","goal_throw")),ordinary_foul=list(team=t,outside_six=b),official_restart=list(team=nt,kind=e("neutral","goal_throw","corner","sideline","delay"),outside_six=b)))
 if(ph=="restart") out$take_restart<-list(team=t)
 if(ph %in% c("live","restart")) out$timeout_request<-list(team=t)
 if(ph %in% c("live","restart","interval","timeout","period_review","penalty")) out<-c(out,list(personal_foul=list(team=t,player=p,kind=e("exclusion","penalty","misconduct","brutality","immediate_dismissal","penalty_dismissal","illegal_entry","excluded_interference","penalty_interference")),staff_card=list(team=t,staff=i(0,2),card=e("yellow","red")),team_caution=list(team=t)))
 if(ph %in% c("live","restart")) out$simultaneous_exclusions<-list(players=list(type="array",minItems=2,maxItems=2,items=p),brutality=b)
 if(!ph %in% c("pre","finished","abandoned")) out<-c(out,list(clear_reentry=list(team=t,player=p),release_exclusion=list(team=t,player=p)))
 if(ph=="penalty") out$bench_for_reduction<-list(team=t,player=p)
 if(ph=="penalty") out<-c(out,list(take_penalty=list(player=p,elapsed_ms=i(0,480000),result=e("goal","attack_rebound","defence_rebound","goal_throw","attacker_violation","retake")),replace_for_penalty=list(team=t,outgoing=np,incoming=p,keeper=np),decline_penalty=empty))
 if(ph=="siren_review") out$penalty_at_signal<-list(team=t,player=p)
 if(ph=="siren_review") out$resolve_siren<-list(result=e("no_goal","goal","penalty"),released_before_signal=b,attacker_touch_after_signal=b,offender=np)
 if(ph=="period_review") out$end_period<-empty
 out
}
.ra_waterpolo_apply<-function(state,action) {
 d<-state$data;k<-action$type;a<-action$params;ph<-d$phase;ss<-.ra_waterpolo_schemas(d)
 if(!k %in% names(ss)) .ra_fail("Action is not permitted in this phase")
 .ra_fields(a,ss[[k]],"/params")
 if(k=="penalty_at_signal") {k<-"personal_foul";a$kind<-"penalty"}
 if(k=="advance") d<-.ra_waterpolo_tick(d,a$ms)
 else if(k=="lineup") {
  t<-a$team;ps<-a$players
  if(anyDuplicated(unlist(ps))||length(ps)>.ra_waterpolo_capacity(d,t)||any(vapply(ps,function(p) !.ra_waterpolo_eligible(d,t,p),logical(1)))) .ra_fail("Lineup needs distinct eligible players within available places")
  d$active[[t+1L]]<-ps;d$keeper[t+1L]<-list(a$keeper)
  if(!.ra_waterpolo_keeper_ok(d,t)) .ra_fail("Goalkeeper and substitute roles are not satisfied")
 } else if(k=="reentry_restriction") {
  t<-a$team;p<-a$player
  if(!d$status[[t+1L]][[p+1L]] %in% c("eligible","restricted")||(!a$restricted&&ph=="live")) .ra_fail("Official reentry clearance requires a stoppage and no disciplinary exclusion")
  d$status[[t+1L]][[p+1L]]<-if(a$restricted) "restricted" else "eligible"
  if(a$restricted) d<-.ra_waterpolo_remove_active(d,t,p)
 } else if(k=="start_match") d$phase<-"start"
 else if(k=="sprint_winner") {d$phase<-"live";d$attack<-a$team;d$shot_ms<-30000L;d$second_touch<-FALSE;d$direct_allowed<-FALSE
 } else if(k=="take_restart") {
  if(!is.null(d$attack)&&a$team!=d$attack) .ra_fail("Restart belongs to the entitled team")
  d$attack<-a$team;d$phase<-"live";d$second_touch<-FALSE
 } else if(k=="second_touch") d$second_touch<-TRUE
 else if(k=="turnover") {d<-.ra_waterpolo_entitlement(d,1L-d$attack);d$direct_allowed<-TRUE;d$second_touch<-TRUE
 } else if(k=="goal") {
  if(!a$own_goal&&!(d$direct_allowed||d$second_touch)) .ra_fail("This restart needs another eligible player touch before scoring")
  if(a$team!=d$attack&&!a$own_goal) .ra_fail("The goal must be credited to the attack team unless an own goal is recorded")
  d<-.ra_waterpolo_scored(d,a$team)
 } else if(k=="shot_result") {
  t<-d$attack;result<-a$result
  if(result=="goal") {if(!(d$direct_allowed||d$second_touch)) .ra_fail("Direct scoring is not permitted from this restart");d<-.ra_waterpolo_scored(d,t)
  } else if(result=="attack_rebound") d$shot_ms<-20000L
  else if(result=="defence_rebound") {d<-.ra_waterpolo_entitlement(d,1L-t);d$direct_allowed<-TRUE;d$second_touch<-TRUE
  } else if(result=="corner") {d$shot_ms<-20000L;d<-.ra_waterpolo_restart(d,"corner",t,TRUE)} else {d<-.ra_waterpolo_entitlement(d,1L-t);d<-.ra_waterpolo_restart(d,"goal_throw",1L-t,TRUE)}
 } else if(k=="ordinary_foul") {
  t<-1L-a$team;same<-.ra_waterpolo_eq(d$attack,t);d<-.ra_waterpolo_entitlement(d,t,!same);d<-.ra_waterpolo_restart(d,"free",t,a$outside_six)
 } else if(k=="official_restart") {
  t<-a$team;kind<-a$kind
  if((kind=="neutral")!=is.null(t)) .ra_fail("Only neutral throws have no entitled team")
  if(!is.null(t)) {same<-.ra_waterpolo_eq(d$attack,t);d<-.ra_waterpolo_entitlement(d,t,!same||kind=="goal_throw")}
  if(kind=="corner") d$shot_ms<-20000L else if(kind=="neutral") d$shot_ms<-30000L
  d<-.ra_waterpolo_restart(d,kind,t,kind %in% c("corner","goal_throw"))
 } else if(k=="timeout_request") {
  t<-a$team
  if(!.ra_waterpolo_eq(t,d$attack)) {d$phase<-"penalty";d$penalty_queue<-list(1L-t);d["penalty_resume"]<-list(NULL)
  } else if(d$timeouts[[t+1L]]>=2) {d<-.ra_waterpolo_entitlement(d,1L-t);d<-.ra_waterpolo_restart(d,"timeout",1L-t)
  } else {d$timeouts[[t+1L]]<-d$timeouts[[t+1L]]+1L;d$phase<-"timeout";d$pause_ms<-60000L;d$timeout_corner<-d$restart=="corner"&&ph=="restart"}
 } else if(k=="personal_foul") {
  t<-a$team;p<-a$player;z<-t+1L;q<-p+1L;kind<-a$kind;other<-1L-t;interval<-ph %in% c("interval","timeout","period_review")||(ph=="restart"&&d$restart=="centre")
  if(d$status[[z]][[q]]=="dismissed") .ra_fail("Player is already permanently excluded")
  if(kind=="immediate_dismissal"||(kind=="misconduct"&&interval)) {
   d$fouls[[z]][[q]]<-d$fouls[[z]][[q]]+1L;d$status[[z]][[q]]<-"dismissed";d$exclusions<-Filter(function(r) !(r$team==t&&r$player==p),d$exclusions);d<-.ra_waterpolo_remove_active(d,t,p)
  } else if(kind=="penalty") {
   d$fouls[[z]][[q]]<-d$fouls[[z]][[q]]+1L
   if(d$fouls[[z]][[q]]>=3) {d$status[[z]][[q]]<-"dismissed";d$exclusions<-Filter(function(r) !(r$team==t&&r$player==p),d$exclusions);d$forced_replacements[[z]]<-d$forced_replacements[[z]]+1L;d<-.ra_waterpolo_remove_active(d,t,p)}
   d$phase<-"penalty";d$penalty_queue<-list(other);d["penalty_resume"]<-list(NULL)
  } else if(kind=="excluded_interference") {
   if(is.null(.ra_waterpolo_reduction(d,t,p))) .ra_fail("Excluded-player interference requires an existing exclusion")
   d$fouls[[z]][[q]]<-d$fouls[[z]][[q]]+1L
   if(d$fouls[[z]][[q]]>=3) d$status[[z]][[q]]<-"dismissed"
   d$phase<-"penalty";d$penalty_queue<-list(other);d["penalty_resume"]<-list(NULL)
  } else {
   brut<-kind=="brutality";d<-.ra_waterpolo_add_exclusion(d,t,p,if(brut) "brutality" else "ordinary",kind %in% c("misconduct","brutality","penalty_dismissal","penalty_interference"))
   penalty<-kind %in% c("penalty_dismissal","penalty_interference")||(kind=="illegal_entry"&&! .ra_waterpolo_eq(t,d$attack))||(brut&&ph=="live")
   if(penalty) {d$phase<-"penalty";d$penalty_queue<-list(other);d["penalty_resume"]<-list(NULL)} else if(!interval&&ph!="penalty"&&!(brut&&ph!="live")) {
    same<-.ra_waterpolo_eq(d$attack,other);d<-.ra_waterpolo_entitlement(d,other,!same);if(same) d$shot_ms<-max(20000L,d$shot_ms);d<-.ra_waterpolo_restart(d,"free",other)
   }
  }
 } else if(k=="simultaneous_exclusions") {
  old<-d$attack;brut<-a$brutality
  for(t in 0:1) d<-.ra_waterpolo_add_exclusion(d,t,a$players[[t+1L]],if(brut) "brutality" else "ordinary",brut,!brut)
  if(brut&&ph=="live") {if(is.null(old)) .ra_fail("Simultaneous live brutalities require the existing attack entitlement");d$phase<-"penalty";d$penalty_queue<-list(old,1L-old);d$penalty_resume<-old} else if(is.null(old)) {d$shot_ms<-30000L;d<-.ra_waterpolo_restart(d,"neutral",NULL)} else d<-.ra_waterpolo_restart(d,"free",old)
 } else if(k=="clear_reentry") {
  j<-.ra_waterpolo_reduction(d,a$team,a$player);if(is.null(j)) .ra_fail("Player has no exclusion to clear");d$exclusions[[j]]$cleared<-TRUE
 } else if(k=="release_exclusion") {
  t<-a$team;p<-a$player;j<-.ra_waterpolo_reduction(d,t,p);interval<-ph %in% c("interval","timeout","period_review")||(ph=="restart"&&d$restart=="centre")
  if(is.null(j)||!d$exclusions[[j]]$ready||!(d$exclusions[[j]]$cleared||interval)) .ra_fail("Exclusion release requires its rule trigger and reentry clearance")
  d$exclusions<-d$exclusions[-j];if(d$status[[t+1L]][[p+1L]]=="excluded") d$status[[t+1L]][[p+1L]]<-"eligible"
 } else if(k=="bench_for_reduction") {
  t<-a$team;p<-a$player
  if(length(d$active[[t+1L]])<=.ra_waterpolo_capacity(d,t)||!p %in% unlist(d$active[[t+1L]])) .ra_fail("Only an excess active player can be removed for this reduction")
  d<-.ra_waterpolo_remove_active(d,t,p)
 } else if(k=="replace_for_penalty") {
  t<-a$team;z<-t+1L;out<-a$outgoing;inc<-a$incoming
  if(inc %in% unlist(d$active[[z]])||!.ra_waterpolo_eligible(d,t,inc)) .ra_fail("Replacement must be an eligible bench player")
  if(is.null(out)) {if(d$forced_replacements[[z]]==0) .ra_fail("No immediate third-foul replacement is pending");d$forced_replacements[[z]]<-d$forced_replacements[[z]]-1L} else {
   if(!is.null(d$keeper[[z]])||!inc %in% c(0,12)||!out %in% unlist(d$active[[z]])) .ra_fail("Only the required replacement goalkeeper may be brought in during a pending penalty")
   d$active[[z]]<-Filter(function(x) x!=out,d$active[[z]])
  }
  d$active[[z]]<-c(d$active[[z]],list(inc));d$keeper[z]<-list(a$keeper)
  if(length(d$active[[z]])>.ra_waterpolo_capacity(d,t)||!.ra_waterpolo_keeper_ok(d,t)) .ra_fail("Penalty replacement violates capacity or goalkeeper roles")
 } else if(k=="decline_penalty") {
  if(d$period!=4||d$period_ms<=0||d$period_ms>60000||length(d$penalty_queue)!=1L||!is.null(d$penalty_resume)) .ra_fail("A single penalty can be exchanged for a free throw only in the final minute")
  t<-d$penalty_queue[[1]];d$penalty_queue<-list();d$forced_replacements<-list(0L,0L);d<-.ra_waterpolo_entitlement(d,t);d<-.ra_waterpolo_restart(d,"timeout",t)
 } else if(k=="take_penalty") {
  t<-d$penalty_queue[[1]];p<-a$player;result<-a$result
  if(any(vapply(0:1,function(q) length(d$active[[q+1L]])>.ra_waterpolo_capacity(d,q),logical(1)))) .ra_fail("Correct the reduced lineups before the penalty throw")
  if(!p %in% unlist(d$active[[t+1L]])) .ra_fail("Penalty taker must be an eligible active player")
  ms<-a$elapsed_ms
  if(ms>d$period_ms) .ra_fail("Penalty elapsed time cannot exceed the remaining period")
  d$period_ms<-d$period_ms-ms
  for(j in seq_along(d$exclusions)) {d$exclusions[[j]]$ms<-max(0L,d$exclusions[[j]]$ms-ms);if(d$exclusions[[j]]$ms==0) d$exclusions[[j]]$ready<-TRUE}
  if(result=="retake") return(d)
  if(result=="goal") {d$score[[t+1L]]<-d$score[[t+1L]]+1L;d<-.ra_waterpolo_mark_release(d,goal=TRUE)}
  d$penalty_queue<-d$penalty_queue[-1L];d$forced_replacements<-list(0L,0L)
  if(length(d$penalty_queue)==0L) {
   if(d$period_ms==0) d$phase<-"period_review" else if(!is.null(d$penalty_resume)) {t<-d$penalty_resume;d["penalty_resume"]<-list(NULL);d<-.ra_waterpolo_entitlement(d,t);d<-.ra_waterpolo_restart(d,"timeout",t)} else if(result=="goal") {d<-.ra_waterpolo_entitlement(d,1L-t);d<-.ra_waterpolo_restart(d,"centre",1L-t)} else if(result=="attack_rebound") {d<-.ra_waterpolo_entitlement(d,t,FALSE);d$phase<-"live";d$shot_ms<-20000L;d$direct_allowed<-TRUE;d$second_touch<-TRUE} else if(result=="defence_rebound") {d<-.ra_waterpolo_entitlement(d,1L-t);d$phase<-"live";d$direct_allowed<-TRUE;d$second_touch<-TRUE} else {d<-.ra_waterpolo_entitlement(d,1L-t);d<-.ra_waterpolo_restart(d,if(result=="attacker_violation") "free" else "goal_throw",1L-t,result!="attacker_violation")}
  }
 } else if(k=="resolve_siren") {
  result<-a$result;t<-d$attack;ended<-d$period_ms==0
  if(result!="no_goal"&&(!a$released_before_signal||a$attacker_touch_after_signal||!(d$direct_allowed||d$second_touch))) .ra_fail("Late scoring decision requires a legally released shot before the signal without subsequent attacking contact")
  if(result=="goal") d<-.ra_waterpolo_scored(d,t) else if(result=="penalty") {
   p<-a$offender;z<-2L-t
   if(is.null(p)||!.ra_waterpolo_eligible(d,1L-t,p)) .ra_fail("Late penalty requires an eligible defending offender")
   d$fouls[[z]][[p+1L]]<-d$fouls[[z]][[p+1L]]+1L
   if(d$fouls[[z]][[p+1L]]>=3) {d$status[[z]][[p+1L]]<-"dismissed";d$forced_replacements[[z]]<-d$forced_replacements[[z]]+1L;d<-.ra_waterpolo_remove_active(d,1L-t,p)}
   d$phase<-"penalty";d$penalty_queue<-list(t);d["penalty_resume"]<-list(NULL)
  } else if(!ended) {d<-.ra_waterpolo_entitlement(d,1L-t);d<-.ra_waterpolo_restart(d,"free",1L-t)}
  if(ended&&result!="penalty") d$phase<-"period_review"
 } else if(k=="end_period") {
  if(d$period==4) {d$phase<-"finished";d["winner"]<-list(if(d$score[[1]]==d$score[[2]]) NULL else as.integer(d$score[[2]]>d$score[[1]]));d$reason<-"regulation"} else {d$phase<-"interval";d$pause_ms<-if(d$period==2) 180000L else 120000L}
 } else if(k=="staff_card") {
  t<-a$team+1L;p<-a$staff+1L
  if(d$staff_red[[t]][[p]]) .ra_fail("Staff member is already dismissed")
  if(a$card=="yellow") {if(p!=1L) .ra_fail("The head coach receives the preliminary staff caution");if(d$staff_yellow[[t]][[p]]) .ra_fail("A cautioned staff member requires a red card for further misconduct");d$staff_yellow[[t]][[p]]<-TRUE} else d$staff_red[[t]][[p]]<-TRUE
 } else if(k=="team_caution") d$team_warned[[a$team+1L]]<-TRUE
 else if(k=="abandon") {d$phase<-"abandoned";d$penalty_queue<-list()}
 else if(k=="official_result") {d$phase<-"finished";d["winner"]<-list(a$winner);d$reason<-if(is.null(a$winner)) "no_contest" else "official"}
 d
}
.ra_waterpolo_legal<-function(state) {
 d<-state$data;ss<-.ra_waterpolo_schemas(d)
 list(mode="event_types",actions=list(),action_types=lapply(names(ss),function(k) list(type=k,params_schema=ss[[k]],preconditions=list("Clock, restart, personal fouls, exclusion release and player roles must permit this official event."),external_observations=list("Official restart, shot result, sanction classification and reentry clearance."))),constraints=list(phase=d$phase,restart=d$restart,period=d$period,period_ms=d$period_ms,shot_ms=d$shot_ms,attack=d$attack,score=d$score,capacity=lapply(0:1,function(t) .ra_waterpolo_capacity(d,t)),fouls=d$fouls,exclusions=d$exclusions,timeouts=d$timeouts,direct_allowed=d$direct_allowed,second_touch=d$second_touch,penalty_queue=d$penalty_queue),completeness=FALSE,reason="official_regulation_league_events_without_ball_motion")
}
.ra_waterpolo_outcome<-function(state,pack) .ra_handball_outcome(state,pack)
