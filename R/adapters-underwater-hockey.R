.ra_uwhockey_equal<-function(a,b) if(is.null(a)||is.null(b)) is.null(a)&&is.null(b) else a==b
.ra_uwhockey_i<-function(a,b) list(type="integer",minimum=a,maximum=b)
.ra_uwhockey_e<-function(...) list(type="enum",values=list(...))
.ra_uwhockey_a<-function(s,a,b) list(type="array",minItems=a,maxItems=b,items=s)
.ra_uwhockey_capacity<-function(d,t) max(0L,6L-sum(unlist(d$dismissed[[t+1L]]))-sum(vapply(1:10,function(p) d$penalties[[t+1L]][[p]]>0||d$pending_ms[[t+1L]][[p]]>0||d$release_ready[[t+1L]][[p]],logical(1))))
.ra_uwhockey_eligible<-function(d,t,p) {t<-t+1L;p<-p+1L;!(d$dismissed[[t]][[p]]||d$penalties[[t]][[p]]>0||d$pending_ms[[t]][[p]]>0||d$release_ready[[t]][[p]]||d$restricted[[t]][[p]])}
.ra_uwhockey_stop_clock<-function(d) d$period==5||(d$period %in% c(2,4)&&d$remaining_ms<=120000)
.ra_uwhockey_restart<-function(d,kind="equal",team=NULL,force_stop=FALSE) {
 d$phase<-"restart";d$restart_kind<-kind;d["restart_team"]<-list(team);d$restart_wait_ms<-0L;d$contact_ms<-0L;d$goal_review<-FALSE
 if(force_stop||(kind!="goal"&&.ra_uwhockey_stop_clock(d))) d$clock_running<-FALSE;d
}
.ra_uwhockey_finish<-function(d,w,reason) {d$phase<-"finished";d["winner"]<-list(w);d$reason<-reason;d$clock_running<-FALSE;d["shot_team"]<-list(NULL);d}
.ra_uwhockey_scored<-function(d,t) {
 d$scores[[t+1L]]<-d$scores[[t+1L]]+1L
 if(d$period==5) return(.ra_uwhockey_finish(d,t,"sudden_death_goal"))
 d<-.ra_uwhockey_restart(d,"goal");d$clock_running<-TRUE;d
}
.ra_uwhockey_validate<-function(d) {
 bad<-function(m) .ra_fail(m,code="INVALID_STATE")
 if(d$period>2&&!d$knockout) bad("Overtime requires knockout rules")
 if(d$period %in% c(3,4)&&d$remaining_ms>300000) bad("Overtime halves last five minutes")
 if(d$period==5&&d$remaining_ms!=0) bad("Sudden death has no countdown limit")
 for(t in 0:1) {
  z<-t+1L
  if(anyDuplicated(unlist(d$active[[z]]))) bad("Active player ids must be distinct")
  if(any(vapply(d$active[[z]],function(p) !.ra_uwhockey_eligible(d,t,p),logical(1)))) bad("Only eligible players may be active")
  if(d$phase %in% c("live","await_contact")&&length(d$active[[z]])>.ra_uwhockey_capacity(d,t)) bad("Active lineup exceeds the available team places")
  for(p in 1:10) {
   ms<-d$penalties[[z]][[p]];ready<-d$release_ready[[z]][[p]];pending<-d$pending_ms[[z]][[p]]
   if((ready&&(ms>0||pending>0))||(d$return_blocked[[z]][[p]]&&!ready)) bad("Penalty timer, release and start flags disagree")
   if(d$dismissed[[z]][[p]]&&(ms>0||ready||pending>0)) bad("Dismissal replaces the player timed exclusion")
   if(anyDuplicated(unlist(d$cautions[[z]][[p]]))) bad("Individual caution categories must be distinct")
  }
  if(anyDuplicated(unlist(d$team_cautions[[z]]))) bad("Team caution categories must be distinct")
 }
 if((d$phase=="penalty_shot")!=!is.null(d$shot_team)) bad("Penalty-shot phase requires an attacking team")
 if(d$restart_kind=="advantage"&&is.null(d$restart_team)) bad("Advantage restart requires an entitled team")
 if(d$restart_kind!="advantage"&&!is.null(d$restart_team)) bad("Neutral restart has no entitled team")
 if(d$phase %in% c("interval","team_timeout","penalty_shot","horn","abandoned","finished")&&d$clock_running) bad("This phase stops the game clock")
 if(d$phase %in% c("live","await_contact")&&!d$clock_running) bad("Active play requires a running match clock")
 if(d$phase=="restart"&&.ra_uwhockey_stop_clock(d)&&d$restart_kind!="goal"&&d$clock_running) bad("Late ordinary stoppage must stop the clock")
 if(d$phase=="horn"&&(d$remaining_ms!=0||d$period==5)) bad("Horn requires an expired timed period")
 if(d$phase=="team_timeout"&&(d$period>2||d$pause_ms<=0)) bad("Team timeout requires remaining regulation timeout time")
 if(d$phase=="interval"&&(d$period==5||d$pause_ms<=0)) bad("Interval requires time before the next period")
 if(d$phase=="finished") {
  expected<-if(d$scores[[1]]==d$scores[[2]]) NULL else as.integer(d$scores[[2]]>d$scores[[1]])
  if(d$reason=="regulation"&&! .ra_uwhockey_equal(d$winner,expected)) bad("Result must agree with the final goals")
 } else if(!is.null(d$winner)) bad("Only a finished match can have a winner")
 invisible(NULL)
}
.ra_uwhockey_tick<-function(d,ms) {
 if(d$phase %in% c("interval","team_timeout")) {
  if(ms>d$pause_ms) .ra_fail("Advance only to the end of this break")
  d$pause_ms<-d$pause_ms-ms
  if(d$pause_ms==0) {
   if(d$phase=="interval") {d$period<-d$period+1L;d$remaining_ms<-if(d$period==2) 900000L else if(d$period %in% c(3,4)) 300000L else 0L;if(d$period==2) d$timeouts<-list(0L,0L);d<-.ra_uwhockey_restart(d,"start",force_stop=TRUE)} else d<-.ra_uwhockey_restart(d,d$restart_kind,d$restart_team,TRUE)
  };return(d)
 }
 if(!d$clock_running) .ra_fail("The game clock is stopped")
 if(d$period!=5&&ms>d$remaining_ms) .ra_fail("Advance only to the period horn")
 if(d$phase=="restart") {
  if(d$restart_kind=="goal"&&d$restart_wait_ms+ms>30000) .ra_fail("Goal restart must occur within thirty seconds")
  if(d$restart_kind!="goal"&&d$period %in% c(2,4)&&d$remaining_ms>120000&&d$remaining_ms-ms<120000) .ra_fail("Stop the clock exactly at the final two-minute boundary")
 }
 if(d$phase=="await_contact"&&d$restart_kind=="advantage"&&d$contact_ms+ms>5000) .ra_fail("Resolve the five-second advantage-puck deadline first")
 for(t in 1:2) for(p in 1:10) if(d$penalties[[t]][[p]]>0) {
  d$penalties[[t]][[p]]<-max(0L,d$penalties[[t]][[p]]-ms)
  if(d$penalties[[t]][[p]]==0&&d$pending_ms[[t]][[p]]==0) {d$release_ready[[t]][[p]]<-TRUE;d$return_blocked[[t]][[p]]<-d$phase=="restart"}
 }
 if(d$phase=="restart") d$restart_wait_ms<-d$restart_wait_ms+ms
 if(d$phase=="await_contact") d$contact_ms<-d$contact_ms+ms
 if(d$period!=5) d$remaining_ms<-d$remaining_ms-ms
 if(d$period!=5&&d$remaining_ms==0) {d$phase<-"horn";d$clock_running<-FALSE;d$horn_review_open<-TRUE
 } else if(d$phase=="await_contact"&&d$restart_kind=="advantage"&&d$contact_ms==5000) d<-.ra_uwhockey_restart(d,"equal") else if(d$phase=="restart"&&d$restart_kind!="goal"&&.ra_uwhockey_stop_clock(d)) d$clock_running<-FALSE
 d
}
.ra_uwhockey_schemas<-function(d) {
 i<-.ra_uwhockey_i;e<-.ra_uwhockey_e;a<-.ra_uwhockey_a;t<-i(0,1);p<-i(0,9);nt<-t;nt$nullable<-TRUE;empty<-setNames(list(),character());ph<-d$phase;out<-empty
 if(ph=="finished") return(out)
 if(ph=="abandoned") return(list(official_result=list(winner=nt)))
 out<-list(abandon=empty,lineup=list(team=t,players=a(p,0,6)),restrict_player=list(team=t,player=p,restricted=list(type="boolean")))
 if(ph %in% c("live","await_contact","restart","interval","team_timeout")) out$advance<-list(ms=i(1,900000))
 if(ph=="restart") out<-c(out,list(whistle=empty,team_timeout=list(team=t)))
 if(ph=="await_contact") out$complete_restart<-list(team=t)
 if(ph %in% c("live","await_contact")) out$official_stop<-list(kind=e("equal","advantage"),team=nt,stop_time=list(type="boolean"))
 if(ph %in% c("live","restart","horn")) out<-c(out,list(goal=list(team=t),award_shot=list(team=t)))
 if(ph %in% c("restart","horn","team_timeout","interval","penalty_shot")) out<-c(out,list(sanction=list(team=t,player=p,category=i(0,23),kind=e("caution","one_minute","two_minutes","five_minutes","dismissal"),team_caution=list(type="boolean")),dismiss_staff=list(team=t,staff=i(0,3))))
 if(ph %in% c("live","await_contact","restart")) out$release_player<-list(team=t,player=p)
 if(ph=="penalty_shot") out$resolve_shot<-list(attackers=a(p,2,2),defender=p,first_contact_ms=i(0,5001),result=e("goal","defended","attacker_foul","defender_foul","no_contact"))
 if(ph=="horn") out$close_period<-empty
 out
}
.ra_uwhockey_apply<-function(state,action) {
 d<-state$data;k<-action$type;a<-action$params;ss<-.ra_uwhockey_schemas(d);ph<-d$phase
 if(!k %in% names(ss)) .ra_fail("Action is not permitted in this phase")
 .ra_fields(a,ss[[k]],"/params")
 if(k=="advance") d<-.ra_uwhockey_tick(d,a$ms)
 else if(k=="lineup") {
  t<-a$team;ps<-a$players
  if(anyDuplicated(unlist(ps))||length(ps)>.ra_uwhockey_capacity(d,t)||any(vapply(ps,function(p) !.ra_uwhockey_eligible(d,t,p),logical(1)))) .ra_fail("Lineup requires distinct eligible players within the available team places")
  d$active[[t+1L]]<-ps
 } else if(k=="restrict_player") {
  t<-a$team+1L;p<-a$player;d$restricted[[t]][[p+1L]]<-a$restricted
  if(a$restricted) d$active[[t]]<-Filter(function(x) x!=p,d$active[[t]])
 } else if(k=="whistle") {
  if(any(vapply(0:1,function(t) length(d$active[[t+1L]])>.ra_uwhockey_capacity(d,t),logical(1)))) .ra_fail("Remove excess players before the restart")
  if(!d$started&&any(lengths(d$active)<4)) .ra_fail("Match start requires at least four players per team")
  d$phase<-"await_contact";d$clock_running<-TRUE;d$contact_ms<-0L;d$started<-TRUE;d$goal_review<-FALSE
  for(t in 1:2) for(p in 1:10) {d$penalties[[t]][[p]]<-d$penalties[[t]][[p]]+d$pending_ms[[t]][[p]];d$pending_ms[[t]][[p]]<-0L}
 } else if(k=="complete_restart") {
  if(d$restart_kind=="advantage"&&a$team!=d$restart_team) .ra_fail("The entitled team must take the advantage puck first")
  d$phase<-"live";d$contact_ms<-0L;d$return_blocked<-rep(list(as.list(rep(FALSE,10))),2)
 } else if(k=="official_stop") {
  if((a$kind=="advantage")!=!is.null(a$team)) .ra_fail("Only an advantage restart identifies an entitled team")
  d<-.ra_uwhockey_restart(d,a$kind,a$team,a$stop_time);d$goal_review<-TRUE
 } else if(k=="goal") {
  if(ph=="restart"&&!d$goal_review) .ra_fail("No unresolved official scoring review")
  if(ph=="horn"&&!d$horn_review_open) .ra_fail("The horn scoring review is already resolved")
  d<-.ra_uwhockey_scored(d,a$team)
  if(ph=="horn") {d$phase<-"horn";d$clock_running<-FALSE;d$horn_review_open<-FALSE}
 } else if(k=="award_shot") {
  if(ph=="restart"&&!d$goal_review) .ra_fail("No unresolved official scoring review")
  if(ph=="horn"&&!d$horn_review_open) .ra_fail("The horn scoring review is already resolved")
  d$phase<-"penalty_shot";d$clock_running<-FALSE;d$shot_team<-a$team;d$horn_review_open<-FALSE
 } else if(k=="resolve_shot") {
  t<-d$shot_team;ps<-a$attackers;q<-a$defender;result<-a$result
  if(anyDuplicated(unlist(ps))||any(vapply(ps,function(p) !.ra_uwhockey_eligible(d,t,p),logical(1)))||!.ra_uwhockey_eligible(d,1L-t,q)) .ra_fail("Penalty shot requires two distinct eligible attackers and one eligible defender")
  if((result=="no_contact")!=(a$first_contact_ms>5000)) .ra_fail("A failed five-second contact deadline is a defended penalty shot")
  if(result %in% c("goal","defender_foul")) d$scores[[t+1L]]<-d$scores[[t+1L]]+1L
  d["shot_team"]<-list(NULL)
  if(d$period==5&&result %in% c("goal","defender_foul")) d<-.ra_uwhockey_finish(d,t,"sudden_death_goal") else if(d$period!=5&&d$remaining_ms==0) {d$phase<-"horn";d$horn_review_open<-FALSE} else d<-.ra_uwhockey_restart(d,"start",force_stop=TRUE)
 } else if(k=="team_timeout") {
  t<-a$team+1L
  if(d$period>2||d$timeouts[[t]]>0) .ra_fail("One timeout per team per regulation half; none in overtime")
  d$timeouts[[t]]<-d$timeouts[[t]]+1L;d$phase<-"team_timeout";d$clock_running<-FALSE;d$pause_ms<-60000L
 } else if(k=="sanction") {
  t<-a$team+1L;p<-a$player+1L;c<-a$category;kind<-a$kind
  if(d$dismissed[[t]][[p]]) .ra_fail("A dismissed player cannot receive another match sanction")
  d$infractions[[t]][[p]][[c+1L]]<-d$infractions[[t]][[p]][[c+1L]]+1L
  if(a$team_caution&&!c %in% unlist(d$team_cautions[[t]])) d$team_cautions[[t]]<-c(d$team_cautions[[t]],list(c))
  if(kind=="caution") {if(!c %in% unlist(d$cautions[[t]][[p]])) d$cautions[[t]][[p]]<-c(d$cautions[[t]][[p]],list(c))} else {
   if(kind=="dismissal") {d$dismissed[[t]][[p]]<-TRUE;d$penalties[[t]][[p]]<-0L;d$pending_ms[[t]][[p]]<-0L;d$release_ready[[t]][[p]]<-FALSE;d$return_blocked[[t]][[p]]<-FALSE} else {
    d$pending_ms[[t]][[p]]<-d$pending_ms[[t]][[p]]+switch(kind,one_minute=60000L,two_minutes=120000L,five_minutes=300000L);d$release_ready[[t]][[p]]<-FALSE;d$return_blocked[[t]][[p]]<-FALSE
   }
   d$active[[t]]<-Filter(function(x) x!=p-1L,d$active[[t]])
  }
 } else if(k=="dismiss_staff") {
  t<-a$team+1L;p<-a$staff+1L
  if(d$staff_dismissed[[t]][[p]]) .ra_fail("Team official is already dismissed")
  d$staff_dismissed[[t]][[p]]<-TRUE
 } else if(k=="release_player") {
  t<-a$team+1L;p<-a$player+1L
  if(!d$release_ready[[t]][[p]]||d$return_blocked[[t]][[p]]||d$restricted[[t]][[p]]) .ra_fail("Return requires expired penalty, official release and any required restart contact")
  d$release_ready[[t]][[p]]<-FALSE;d$active[[t]]<-c(d$active[[t]],list(p-1L))
  if(length(d$active[[t]])>.ra_uwhockey_capacity(d,t-1L)) .ra_fail("Return would exceed available team places")
 } else if(k=="close_period") {
  period<-d$period;tied<-d$scores[[1]]==d$scores[[2]]
  if(period==2&&(!d$knockout||!tied)) d<-.ra_uwhockey_finish(d,if(tied) NULL else as.integer(d$scores[[2]]>d$scores[[1]]),"regulation") else if(period==4&&!tied) d<-.ra_uwhockey_finish(d,as.integer(d$scores[[2]]>d$scores[[1]]),"regulation") else {d$phase<-"interval";d$pause_ms<-if(period %in% c(1,2)) 180000L else 60000L}
 } else if(k=="abandon") {d$phase<-"abandoned";d$clock_running<-FALSE;d["shot_team"]<-list(NULL)} else if(k=="official_result") d<-.ra_uwhockey_finish(d,a$winner,if(is.null(a$winner)) "no_contest" else "official")
 d
}
.ra_uwhockey_legal<-function(state) {
 d<-state$data;ss<-.ra_uwhockey_schemas(d)
 list(mode="event_types",actions=list(),action_types=lapply(names(ss),function(k) list(type=k,params_schema=ss[[k]],preconditions=list("Phase, official clock, player eligibility and restart entitlement must permit the event."),external_observations=list("Official scoring, infringements, sanction severity, completed substitution or restart contact."))),constraints=list(phase=d$phase,period=d$period,remaining_ms=d$remaining_ms,clock_running=d$clock_running,scores=d$scores,capacity=lapply(0:1,function(t) .ra_uwhockey_capacity(d,t)),penalties=d$penalties,release_ready=d$release_ready,restart_kind=d$restart_kind,restart_team=d$restart_team,timeouts=d$timeouts,shot_team=d$shot_team),completeness=FALSE,reason="official_match_events_without_puck_or_movement_simulation")
}
.ra_uwhockey_outcome<-function(state,pack) .ra_handball_outcome(state,pack)
