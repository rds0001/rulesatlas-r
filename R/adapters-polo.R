.ra_polo_i<-function(a,b) list(type="integer",minimum=a,maximum=b)
.ra_polo_e<-function(...) list(type="enum",values=list(...))
.ra_polo_a<-function(x,n) list(type="array",minItems=n,maxItems=n,items=x)
.ra_polo_equal<-function(a,b) if(is.null(a)||is.null(b)) is.null(a)&&is.null(b) else a==b
.ra_polo_hits<-c("p2","p3","p4","p5a","p5b","p6")
.ra_polo_totals<-function(d) {
 s<-2L*unlist(d$goals)
 if(d$handicap) {diff<-d$rating_peak[[1]]-d$rating_peak[[2]];num<-abs(diff)*d$chukkas;half<-2L*(num %/% 6L)+as.integer(num%%6L!=0L);t<-if(diff>0) 2L else 1L;s[[t]]<-s[[t]]+half};as.list(s)
}
.ra_polo_available<-function(d,t,p) {t<-t+1L;p<-p+1L;!(d$red[[t]][[p]]||d$sinbin_ms[[t]][[p]]>0||d$restricted[[t]][[p]])}
.ra_polo_finish<-function(d,w,reason) {d$phase<-"finished";d$clock_running<-FALSE;d["winner"]<-list(w);d$reason<-reason;d}
.ra_polo_golden<-function(d) d$chukka>d$chukkas||(d$chukka==d$chukkas&&d$tail)
.ra_polo_score<-function(d,t) {
 d$goals[[t+1L]]<-d$goals[[t+1L]]+1L;d$ends_reversed<-!d$ends_reversed;d$restart<-"throw_in";d["restart_team"]<-list(NULL);d$restart_delay_ms<-0L
 if(.ra_polo_golden(d)) d<-.ra_polo_finish(d,t,"golden_goal") else if(d$tail) {d$phase<-"horn";d$clock_running<-FALSE} else {d$phase<-"restart";d$clock_running<-TRUE};d
}
.ra_polo_pause<-function(d,restart,team=NULL,whistle=TRUE) {
 d$phase<-"restart";d$restart<-restart;d["restart_team"]<-list(team);d$restart_delay_ms<-0L
 if(whistle) d$clock_running<-FALSE
 if(d$tail&&d$chukka<d$chukkas) {d$phase<-"horn";d$clock_running<-FALSE};d
}
.ra_polo_validate<-function(d) {
 bad<-function(m) .ra_fail(m,code="INVALID_STATE")
 if(d$tail&&d$remaining_ms>30000) bad("Second-bell tail is at most thirty seconds")
 if(d$chukka>d$chukkas&&!d$knockout) bad("Extra chukkas require a result-required match")
 for(t in 0:1) {
  z<-t+1L
  if(anyDuplicated(unlist(d$slots[[z]]))) bad("Four distinct players must occupy the team slots")
  if(anyDuplicated(unlist(d$active[[z]]))||any(vapply(d$active[[z]],function(p) !p %in% unlist(d$slots[[z]])||!.ra_polo_available(d,t,p),logical(1)))) bad("Active players must occupy eligible team slots")
  if(d$rating_peak[[z]]<sum(unlist(d$handicaps[[z]])[unlist(d$slots[[z]])+1L])) bad("Recorded handicap peak cannot be below the current team total")
  for(p in 1:8) if(d$permanent_gap[[z]][[p]]&&!d$red[[z]][[p]]) bad("A permanent unreplaceable gap requires a red flag")
  if(anyDuplicated(unlist(d$played[[z]]))||any(!unlist(d$slots[[z]]) %in% unlist(d$played[[z]]))) bad("Current slots must be included in player participation")
 }
 if((d$restart %in% c(.ra_polo_hits,"hit_in","sideline","dropped_ball"))!=!is.null(d$restart_team)) bad("Restart entitlement must match the restart type")
 if(d$phase %in% c("horn","interval","abandoned","finished")&&d$clock_running) bad("This phase stops the clock")
 if(d$phase=="live"&&!d$clock_running) bad("Live play requires a running clock")
 if(d$phase=="interval"&&d$pause_ms==0) bad("Interval needs remaining break time")
 if(d$phase=="finished") {s<-.ra_polo_totals(d);expected<-if(s[[1]]==s[[2]]) NULL else as.integer(s[[2]]>s[[1]]);if(d$reason=="regulation"&&! .ra_polo_equal(d$winner,expected)) bad("Result must agree with goals including handicap")} else if(!is.null(d$winner)) bad("Only a finished match has a winner")
 invisible(NULL)
}
.ra_polo_advance<-function(d,ms) {
 if(d$phase=="interval") {
  if(ms>d$pause_ms) .ra_fail("Advance only to the end of the interval")
  d$pause_ms<-d$pause_ms-ms
  if(d$pause_ms==0) {d$chukka<-d$chukka+1L;d$phase<-"restart";d$remaining_ms<-420000L;d$tail<-FALSE;d$at_start<-TRUE;d$clock_running<-FALSE};return(d)
 }
 if(!d$clock_running) .ra_fail("The match clock is stopped")
 if(ms>d$remaining_ms) .ra_fail("Advance only to the next bell")
 d$remaining_ms<-d$remaining_ms-ms;d$at_start<-FALSE
 for(t in 1:2) d$sinbin_ms[[t]]<-lapply(d$sinbin_ms[[t]],function(n) max(0L,n-ms))
 if(d$phase=="restart") d$restart_delay_ms<-d$restart_delay_ms+ms
 if(d$remaining_ms==0) {
  s<-.ra_polo_totals(d)
  if(d$tail||(d$chukka<d$chukkas&&d$phase=="restart")||(d$chukka==d$chukkas&&s[[1]]!=s[[2]])) {if(d$phase=="live") {d$restart<-"throw_in";d["restart_team"]<-list(NULL)};d$phase<-"horn";d$clock_running<-FALSE} else {d$tail<-TRUE;d$remaining_ms<-30000L}
 };d
}
.ra_polo_schemas<-function(d) {
 i<-.ra_polo_i;e<-.ra_polo_e;a<-.ra_polo_a;t<-i(0,1);p<-i(0,7);nt<-t;nt$nullable<-TRUE;empty<-setNames(list(),character());ph<-d$phase;out<-empty
 if(ph=="finished") return(out)
 if(ph=="abandoned") return(list(resume_match=empty,official_result=list(winner=nt)))
 out<-list(abandon=empty,lineup=list(team=t,players=list(type="array",minItems=0,maxItems=4,items=p)),restrict_player=list(team=t,player=p,restricted=list(type="boolean")),substitute=list(team=t,slots=a(p,4),approved=list(type="boolean")),flag=list(team=t,player=p,kind=e("yellow","double_yellow","red","red_unpermitted_exit")),correct_score=list(goals=a(i(0,999),2)))
 if(ph %in% c("live","restart","interval")) out$advance<-list(ms=i(1,420000))
 pen<-list(team=t,penalty=e("p1","p2","p3","p4","p5a","p5b","p6"))
 if(ph=="live") out<-c(out,list(goal=list(team=t),official_stop=list(restart=e("throw_in","hit_in","sideline","dropped_ball"),team=nt,whistle=list(type="boolean")),award_penalty=pen))
 if(ph %in% c("restart","horn")) out$award_penalty<-pen
 if(ph=="restart") {
  if(d$restart %in% .ra_polo_hits) out$penalty_attempt<-list(player=p,wait_ms=i(0,20001),result=e("goal","backline","in_play","defender_violation","defender_prevents_goal","attacker_violation","both_violate")) else out$restart_play<-list(team=nt,wait_ms=i(0,25001))
 }
 if(ph=="live"&&!is.null(d$restricted_half_shot)) out$defender_played<-empty
 if(ph=="horn") out$close_chukka<-empty
 out
}
.ra_polo_apply<-function(state,action) {
 d<-state$data;k<-action$type;a<-action$params;ph<-d$phase;ss<-.ra_polo_schemas(d)
 if(!k %in% names(ss)) .ra_fail("Action is not permitted in this phase")
 .ra_fields(a,ss[[k]],"/params")
 if(k=="advance") d<-.ra_polo_advance(d,a$ms)
 else if(k=="lineup") {
  t<-a$team;ps<-a$players
  if(anyDuplicated(unlist(ps))||any(vapply(ps,function(p) !p %in% unlist(d$slots[[t+1L]])||!.ra_polo_available(d,t,p),logical(1)))) .ra_fail("Lineup requires distinct eligible players in the four team slots")
  d$active[[t+1L]]<-ps
 } else if(k=="restrict_player") {
  t<-a$team+1L;p<-a$player;d$restricted[[t]][[p+1L]]<-a$restricted
  if(a$restricted) d$active[[t]]<-Filter(function(x) x!=p,d$active[[t]])
 } else if(k=="substitute") {
  t<-a$team;z<-t+1L;new<-a$slots;old<-d$slots[[z]]
  if(!a$approved||anyDuplicated(unlist(new))) .ra_fail("A substitute requires explicit tournament approval and distinct slots")
  if(any(vapply(new,function(p) !p %in% unlist(old)&&(!.ra_polo_available(d,t,p)||(p %in% unlist(d$played[[z]])&&!d$at_start)),logical(1)))) .ra_fail("Returning substituted players may rejoin only at a chukka start; all entrants must be eligible")
  for(j in 1:4) {before<-old[[j]]+1L;after<-new[[j]]+1L;if(before!=after&&(d$sinbin_ms[[z]][[before]]>0||d$permanent_gap[[z]][[before]])) .ra_fail("A sin-binned or permanently unreplaceable player cannot be substituted")}
  d$slots[[z]]<-new;d$active[[z]]<-Filter(function(p) .ra_polo_available(d,t,p),new);d$played[[z]]<-c(d$played[[z]],Filter(function(p) !p %in% unlist(d$played[[z]]),new));d$rating_peak[[z]]<-max(d$rating_peak[[z]],sum(unlist(d$handicaps[[z]])[unlist(new)+1L]))
 } else if(k=="flag") {
  t<-a$team+1L;p<-a$player+1L;kind<-a$kind
  if(d$red[[t]][[p]]) .ra_fail("Player is already dismissed")
  if(startsWith(kind,"red")) {d$red[[t]][[p]]<-TRUE;d$yellow[[t]][[p]]<-d$yellow[[t]][[p]]+2L;d$sinbin_ms[[t]][[p]]<-if(kind=="red") 120000L else 0L;d$permanent_gap[[t]][[p]]<-kind=="red_unpermitted_exit"} else {d$yellow[[t]][[p]]<-d$yellow[[t]][[p]]+if(kind=="yellow") 1L else 2L;if(d$yellow[[t]][[p]]>=2) d$sinbin_ms[[t]][[p]]<-d$sinbin_ms[[t]][[p]]+120000L}
  if(!.ra_polo_available(d,t-1L,p-1L)) d$active[[t]]<-Filter(function(x) x!=p-1L,d$active[[t]])
 } else if(k=="goal") {d<-.ra_polo_score(d,a$team);d["restricted_half_shot"]<-list(NULL)
 } else if(k=="official_stop") {
  if((a$restart!="throw_in")!=!is.null(a$team)) .ra_fail("Only a neutral throw-in has no entitled team")
  d<-.ra_polo_pause(d,a$restart,a$team,a$whistle);d["restricted_half_shot"]<-list(NULL)
 } else if(k=="award_penalty") {
  t<-a$team;pen<-a$penalty;d["restricted_half_shot"]<-list(NULL);d$moved_up_yards<-0L
  if(pen=="p1") {d<-.ra_polo_score(d,t);if(d$phase!="finished") {d$restart<-"p5b";d$restart_team<-t;d$clock_running<-FALSE;if(ph=="horn"&&d$chukka<d$chukkas) d$phase<-"horn" else if(d$chukka>=d$chukkas&&d$remaining_ms<5000) {d$remaining_ms<-5000L;d$phase<-"restart"}}} else {
   normal_tail<-d$tail&&d$chukka<d$chukkas;d<-.ra_polo_pause(d,pen,t,TRUE)
   if(normal_tail||(ph=="horn"&&d$chukka<d$chukkas)) d$phase<-"horn" else if(d$chukka>=d$chukkas&&d$remaining_ms<5000) {d$remaining_ms<-5000L;d$phase<-"restart"}
  }
 } else if(k=="restart_play") {
  if(!.ra_polo_equal(a$team,d$restart_team)) .ra_fail("Restart must respect team entitlement")
  limit<-if(d$restart=="hit_in") 10000L else if(d$restart %in% c("sideline","dropped_ball")) 5000L else 25000L
  if(a$wait_ms>limit) {if(d$restart=="hit_in") d<-.ra_polo_pause(d,"throw_in",NULL,TRUE) else if(d$restart=="throw_in") .ra_fail("A delayed throw-in requires the official penalty decision") else d<-.ra_polo_pause(d,"p5a",1L-d$restart_team,TRUE)} else {d$phase<-"live";d$clock_running<-TRUE;d$at_start<-FALSE;d$restart_delay_ms<-0L}
 } else if(k=="penalty_attempt") {
  t<-d$restart_team;p<-a$player;kind<-d$restart;result<-a$result
  if(!p %in% unlist(d$active[[t+1L]])) .ra_fail("Penalty taker must be an eligible active player")
  if(a$wait_ms>20000) result<-"attacker_violation"
  if(result=="attacker_violation") {d<-.ra_polo_pause(d,"p5a",1L-t,TRUE);d$moved_up_yards<-0L
  } else if(result %in% c("defender_violation","both_violate")) {if(result=="defender_violation"&&kind %in% c("p5a","p5b")) d$moved_up_yards<-d$moved_up_yards+30L;d$clock_running<-FALSE
  } else if(result %in% c("goal","defender_prevents_goal")) d<-.ra_polo_score(d,t)
  else if(result=="backline") {d<-.ra_polo_pause(d,"hit_in",1L-t,FALSE);d$clock_running<-TRUE}
  else if(kind=="p2") d<-.ra_polo_pause(d,"p5a",1L-t,TRUE)
  else {d$phase<-"live";d$clock_running<-TRUE;d["restricted_half_shot"]<-list(if(kind %in% c("p3","p4","p6")) t else NULL)}
  d$at_start<-FALSE
 } else if(k=="defender_played") d["restricted_half_shot"]<-list(NULL)
 else if(k=="correct_score") d$goals<-a$goals
 else if(k=="close_chukka") {
  s<-.ra_polo_totals(d);different<-s[[1]]!=s[[2]]
  if(d$chukka>=d$chukkas&&(different||!d$knockout)) d<-.ra_polo_finish(d,if(!different) NULL else as.integer(s[[2]]>s[[1]]),"regulation") else {
   if(d$chukka==(d$chukkas+1L)%/%2L&&sum(unlist(d$goals))==0) d$ends_reversed<-!d$ends_reversed
   if(d$chukka>d$chukkas) d$ends_reversed<-!d$ends_reversed
   d$phase<-"interval";d$pause_ms<-if(d$chukka==(d$chukkas+1L)%/%2L||d$chukka>=d$chukkas) 300000L else 180000L
  }
 } else if(k=="abandon") {d$saved_phase<-d$phase;d$saved_running<-d$clock_running;d$phase<-"abandoned";d$clock_running<-FALSE
 } else if(k=="resume_match") {d$phase<-d$saved_phase;d$clock_running<-d$saved_running
 } else if(k=="official_result") d<-.ra_polo_finish(d,a$winner,if(is.null(a$winner)) "no_contest" else "official")
 d
}
.ra_polo_legal<-function(state) {
 d<-state$data;ss<-.ra_polo_schemas(d)
 list(mode="event_types",actions=list(),action_types=lapply(names(ss),function(k) list(type=k,params_schema=ss[[k]],preconditions=list("Chukka, clock, restart entitlement, player eligibility and official substitution approval must permit the event."),external_observations=list("Official goals, fouls, flags, restart outcomes and tournament eligibility approval."))),constraints=list(phase=d$phase,chukka=d$chukka,remaining_ms=d$remaining_ms,tail=d$tail,score_half_goals=.ra_polo_totals(d),restart=d$restart,restart_team=d$restart_team,sinbin_ms=d$sinbin_ms,rating_peak=d$rating_peak,restricted_half_shot=d$restricted_half_shot,moved_up_yards=d$moved_up_yards),completeness=FALSE,reason="official_outdoor_polo_events_without_trajectory_or_horse_simulation")
}
.ra_polo_outcome<-function(state,pack) .ra_handball_outcome(state,pack)
