.ra_korfball_i<-function(a,b) list(type="integer",minimum=a,maximum=b)
.ra_korfball_e<-function(...) list(type="enum",values=list(...))
.ra_korfball_active<-function(d,t) which(unlist(d$zones[[t+1L]])>=0)-1L
.ra_korfball_attack<-function(d,t,i) d$zones[[t+1L]][[i+1L]]==d$attack_zone
.ra_korfball_playable<-function(d) {
 for(z in 0:1) {
  a<-lapply(0:1,function(t) vapply(0:1,function(g) sum(unlist(d$zones[[t+1L]])==(if(t==0) z else 1L-z) & unlist(d$gender[[t+1L]])==g),numeric(1)))
  if(any(vapply(a,function(x) sum(x)<3 || max(x)>2,logical(1))) || sum(pmin(a[[1]],a[[2]]))<3) return(FALSE)
 }
 TRUE
}
.ra_korfball_finish<-function(d,w,r) {d$phase<-"finished";d["winner"]<-list(w);d$reason<-r;d$break_ms<-0L;d}
.ra_korfball_score<-function(d,t) {
 d$score[[t+1L]]<-d$score[[t+1L]]+1L
 if(sum(unlist(d$score))%%2==0) d$attack_zone<-1L-d$attack_zone
 d$restart_team<-1L-t;d$restart_kind<-"throw_off";d["taker"]<-list(NULL);d$free_faults<-0L
 if(d$period==5) d<-.ra_korfball_finish(d,t,"golden_goal") else if(d$clock_ms>0) d$phase<-"stopped"
 d
}
.ra_korfball_validate<-function(d) {
 bad<-function(m) .ra_fail(m,code="INVALID_STATE")
 if(d$attack_zone!=(sum(unlist(d$score))%/%2)%%2) bad("Attacking groups switch after every two goals")
 for(t in 1:2) {
  if(sum(unlist(d$gender[[t]])==0)!=7) bad("Registered roster requires seven players of each gender")
  for(i in 1:14) {
   if(d$zones[[t]][[i]]>=0 && (d$cards[[t]][[i]]==2 || d$cooldown_ms[[t]][[i]]>0 || d$withdrawn[[t]][[i]])) bad("Unavailable player cannot occupy a zone")
   if(d$cooldown_ms[[t]][[i]]>0 && d$cards[[t]][[i]]!=1) bad("Temporary bench restriction requires a yellow card")
  }
  for(z in 0:1) if(any(vapply(0:1,function(g) sum(unlist(d$zones[[t]])==z & unlist(d$gender[[t]])==g)>2,logical(1)))) bad("Each group permits two players of each gender")
  for(z in 0:1) for(g in 0:1) {
   slots<-z*4L+g*2L+c(1L,2L)
   if(sum(unlist(d$missing[[t]])[slots])+sum(unlist(d$zones[[t]])==z & unlist(d$gender[[t]])==g)!=2) bad("Recorded vacancies must match missing group places")
  }
  cc<-d$captains[[t]]
  if(!is.null(cc) && (d$cards[[t]][[cc+1L]]==2 || d$withdrawn[[t]][[cc+1L]])) bad("Captain must remain eligible for the match")
 }
 if(d$phase=="play" && (!.ra_korfball_playable(d) || any(vapply(d$captains,is.null,logical(1))))) bad("Live play requires valid matched groups and captains")
 if(d$clock_ms>(if(d$period==5) 600000 else 750000)) bad("Clock exceeds the declared period")
 if(d$phase %in% c("play","stopped","timeout") && d$clock_ms==0 && d$restart_kind!="penalty") bad("Expired period must enter review")
 if(d$phase=="review" && d$clock_ms!=0) bad("Period review requires an expired clock")
 if((d$phase %in% c("timeout","interval"))!=(d$break_ms>0)) bad("Break clock must agree with phase")
 if(d$phase %in% c("shoot_setup","shootout") && (d$period!=5 || d$clock_ms!=0 || d$score[[1]]!=d$score[[2]])) bad("Shoot-out follows tied golden goal")
 if(d$phase=="shootout") {
  if(is.null(d$shoot_first) || any(vapply(d$shoot_order,function(x) length(x)==0 || anyDuplicated(unlist(x))>0,logical(1)))) bad("Shoot-out requires fixed unique orders")
  n<-unlist(d$shoot_taken);f<-d$shoot_first+1L
  if(!n[f] %in% c(n[3L-f],n[3L-f]+1L) || any(unlist(d$shoot_score)>n)) bad("Shoot-out attempts and goals are inconsistent")
  if(n[1]==n[2] && d$shoot_score[[1]]!=d$shoot_score[[2]]) bad("Decisive equal-length pair must end the shoot-out")
 }
 if(d$phase=="finished") {
  if(is.null(d$reason) || (d$reason=="no_contest")!=is.null(d$winner)) bad("Result needs winner or no-contest")
  if(d$reason %in% c("regulation","golden_goal") && d$score[[d$winner+1L]]<=d$score[[2L-d$winner]]) bad("Winner must lead the match score")
 } else if(!is.null(d$winner) || !is.null(d$reason)) bad("Unfinished match cannot have a result")
}
.ra_korfball_schemas<-function(d) {
 ph<-d$phase;s<-list();ii<-.ra_korfball_i;ee<-.ra_korfball_e;t<-ii(0,1);p<-ii(0,13);b<-list(type="boolean");empty<-setNames(list(),character());np<-c(p,list(nullable=TRUE))
 if(ph=="finished") return(s)
 if(ph=="abandoned") return(list(official_result=list(winner=c(t,list(nullable=TRUE)))))
 if(ph %in% c("stopped","review")) {
  s$award_restart<-list(team=t,kind=ee("restart","free_pass","penalty","out_ball"),player=np)
  if(d$restart_kind=="penalty") s$penalty<-list(team=t,player=p,result=ee("goal","miss","retake","attacker_fault"))
 }
 if(ph=="stopped") {
  if(d$restart_kind!="penalty") s$restart<-list(team=t,player=p,milliseconds=ii(0,4000))
  if(d$restart_kind=="free_pass") s$free_pass_fault<-list(team=t)
  s$timeout<-list(team=t)
 }
 if(ph=="play") s<-c(s,list(elapse=list(milliseconds=ii(1,750000)),goal=list(team=t,player=p),stop=empty,shot_clock_violation=list(team=t)))
 if(ph=="review") s<-c(s,list(late_goal=list(team=t,player=p),close_period=list(next_throw_off=t)))
 if(ph %in% c("timeout","interval")) s$elapse_break<-list(milliseconds=ii(1,600000))
 if(ph=="shoot_setup") s$set_shootout<-list(first=t,orders=list(type="array",minItems=2L,maxItems=2L,items=list(type="array",minItems=1L,maxItems=8L,items=p)))
 if(ph=="shootout") s$shoot_penalty<-list(team=t,player=p,scored=b)
 if(!ph %in% c("play","shootout","shoot_setup")) s<-c(s,list(substitute=list(team=t,out=p,"in"=p),replace_missing=list(team=t,slot=ii(0,7),player=p),appoint_captain=list(team=t,player=p)))
 if(!ph %in% c("shootout","shoot_setup")) s<-c(s,list(card=list(team=t,player=p,color=ee("yellow","red")),withdraw=list(team=t,player=p),coach_card=list(team=t,color=ee("yellow","red"))))
 c(s,list(abandon=empty,concede=list(team=t)))
}
.ra_korfball_tick<-function(d,n) {
 d$clock_ms<-d$clock_ms-n
 for(a in 1:2) {d$cooldown_ms[[a]]<-lapply(d$cooldown_ms[[a]],function(x) max(0L,x-n));d$coach_ms[[a]]<-max(0L,d$coach_ms[[a]]-n)}
 d
}
.ra_korfball_apply<-function(state,action) {
 d<-state$data;k<-action$type;p<-action$params;s<-.ra_korfball_schemas(d);t<-p$team;i<-p$player;a<-if(is.null(t)) NULL else t+1L;j<-if(is.null(i)) NULL else i+1L
 if(!k %in% names(s)) .ra_fail("Action is not permitted in this phase")
 .ra_fields(p,s[[k]],"/params")
 if(k=="restart") {
  if(t!=d$restart_team || !i %in% .ra_korfball_active(d,t) || (d$restart_kind %in% c("throw_off","free_pass") && !.ra_korfball_attack(d,t,i)) || !.ra_korfball_playable(d) || any(vapply(d$captains,is.null,logical(1)))) .ra_fail("Restart requires entitled player, valid groups and captains")
  n<-p$milliseconds;if(n>d$clock_ms) .ra_fail("Restart duration exceeds remaining period")
  d<-.ra_korfball_tick(d,n);d$phase<-if(d$clock_ms>0) "play" else "review";d$timeout_used_stop<-FALSE;d$return_blocked<-rep(list(as.list(rep(FALSE,14))),2)
 } else if(k=="elapse") {
  n<-p$milliseconds;if(n>d$clock_ms) .ra_fail("Elapsed time exceeds remaining period")
  d<-.ra_korfball_tick(d,n)
  if(d$clock_ms==0) {d$phase<-"review";d$late_used<-FALSE;d$restart_kind<-"none";d["taker"]<-list(NULL)}
 } else if(k %in% c("goal","late_goal")) {
  if(!.ra_korfball_attack(d,t,i) || (k=="late_goal" && d$late_used)) .ra_fail("Goal requires an active attacker and an unresolved pre-buzzer shot")
  if(k=="late_goal") d$late_used<-TRUE
  d<-.ra_korfball_score(d,t)
 } else if(k=="stop") {d$phase<-"stopped";d$restart_kind<-"restart";d["taker"]<-list(NULL)} else if(k=="shot_clock_violation") {d$phase<-"stopped";d$restart_team<-1L-t;d$restart_kind<-"restart";d["taker"]<-list(NULL);d$free_faults<-0L} else if(k=="award_restart") {
  if(p$kind %in% c("penalty","free_pass") && (is.null(i) || !.ra_korfball_attack(d,t,i))) .ra_fail("Penalty needs the officially designated active attacker")
  if(d$phase=="review" && p$kind!="penalty") .ra_fail("Only an outstanding penalty extends an expired period")
  d$restart_team<-t;d$restart_kind<-p$kind;d["taker"]<-list(if(p$kind %in% c("penalty","free_pass")) i else NULL);d$free_faults<-0L
 } else if(k=="free_pass_fault") {
  if(t==d$restart_team) {d$restart_team<-1L-t;d$restart_kind<-"restart";d$free_faults<-0L} else {
   d$free_faults<-d$free_faults+1L
   if(d$free_faults==2) d$restart_kind<-"penalty"
  }
 } else if(k=="penalty") {
  if(t!=d$restart_team || !.ra_korfball_attack(d,t,i) || (!is.null(d$taker) && i!=d$taker)) .ra_fail("Use the entitled penalty taker")
  rr<-p$result
  if(rr=="goal") d<-.ra_korfball_score(d,t) else if(rr!="retake") {d$restart_kind<-"restart";d$restart_team<-if(rr=="attacker_fault") 1L-t else t;d["taker"]<-list(NULL);d$phase<-if(d$clock_ms==0) "review" else "stopped"}
 } else if(k=="close_period") {
  if(d$restart_kind=="penalty") .ra_fail("Resolve the outstanding penalty first")
  q<-d$period
  if(q==4 && d$score[[1]]!=d$score[[2]]) d<-.ra_korfball_finish(d,as.integer(d$score[[2]]>d$score[[1]]),"regulation") else if(q==5) d$phase<-"shoot_setup" else {
   d$period<-q+1L;d$clock_ms<-if(q==4) 600000L else 750000L;d$phase<-"interval";d$break_ms<-if(q==2) 600000L else 60000L;d$restart_team<-if(q==2) 1L else p$next_throw_off;d$restart_kind<-"throw_off";d["taker"]<-list(NULL);d$late_used<-FALSE;d$timeout_used_stop<-FALSE
  }
 } else if(k=="timeout") {
  if(d$timeouts[[a]]>=2 || d$timeout_used_stop || d$coach_ms[[a]]>0) .ra_fail("Timeout allowance, restart or coaching restriction prevents this timeout")
  d$timeouts[[a]]<-d$timeouts[[a]]+1L;d$phase<-"timeout";d$break_ms<-60000L;d$timeout_used_stop<-TRUE
 } else if(k=="elapse_break") {
  if(p$milliseconds>d$break_ms) .ra_fail("Elapsed time exceeds break")
  d$break_ms<-d$break_ms-p$milliseconds;if(d$break_ms==0) d$phase<-"stopped"
 } else if(k=="substitute") {
  o<-p$out+1L;inc<-p[["in"]]+1L
  if(!p$out %in% .ra_korfball_active(d,t) || d$zones[[a]][[inc]]>=0 || d$cards[[a]][[inc]]==2 || d$withdrawn[[a]][[inc]] || d$cooldown_ms[[a]][[inc]]>0 || d$return_blocked[[a]][[inc]] || d$gender[[a]][[o]]!=d$gender[[a]][[inc]] || d$substitutions[[a]]>=8 || any(unlist(d$missing[[a]])) || d$coach_ms[[a]]>0) .ra_fail("Substitution is not permitted for this roster or allowance")
  d$zones[[a]][[inc]]<-d$zones[[a]][[o]];d$zones[[a]][[o]]<- -1L;d$return_blocked[[a]][[o]]<-TRUE;d$substitutions[[a]]<-d$substitutions[[a]]+1L
  if(!is.null(d$taker) && d$taker==p$out && d$restart_team==t) d["taker"]<-list(NULL)
 } else if(k=="replace_missing") {
  slot<-p$slot+1L;z<-p$slot%/%4;g<-(p$slot%%4)%/%2
  if(!d$missing[[a]][[slot]] || d$zones[[a]][[j]]>=0 || d$gender[[a]][[j]]!=g || d$cards[[a]][[j]]==2 || d$withdrawn[[a]][[j]] || d$cooldown_ms[[a]][[j]]>0 || d$return_blocked[[a]][[j]]) .ra_fail("Replacement must fill a recorded vacancy with an eligible matching player")
  d$missing[[a]][[slot]]<-FALSE;d$zones[[a]][[j]]<-z
 } else if(k=="appoint_captain") {
  if(!is.null(d$captains[[a]]) || !i %in% .ra_korfball_active(d,t)) .ra_fail("A replacement captain must be an active player after permanent departure")
  d$captains[[a]]<-i
 } else if(k %in% c("card","withdraw")) {
  if(d$cards[[a]][[j]]==2 || d$withdrawn[[a]][[j]]) .ra_fail("Player is already permanently unavailable")
  if(k=="withdraw") d$withdrawn[[a]][[j]]<-TRUE else {d$cards[[a]][[j]]<-if(p$color=="red") 2L else d$cards[[a]][[j]]+1L;d$cooldown_ms[[a]][[j]]<-if(d$cards[[a]][[j]]==1) 240000L else 0L}
  z<-d$zones[[a]][[j]]
  if(z>=0) {
   g<-d$gender[[a]][[j]];slot<-z*4L+g*2L+1L;if(d$missing[[a]][[slot]]) slot<-slot+1L
   d$missing[[a]][[slot]]<-TRUE;d$zones[[a]][[j]]<- -1L;d$substitutions[[a]]<-d$substitutions[[a]]+1L
   if(d$restart_team==t && !is.null(d$taker) && d$taker==i) d["taker"]<-list(NULL)
  }
  if((k=="withdraw" || d$cards[[a]][[j]]==2) && !is.null(d$captains[[a]]) && d$captains[[a]]==i) d$captains[a]<-list(NULL)
  if(d$phase=="play") {d$phase<-"stopped";d$restart_kind<-"restart"}
 } else if(k=="coach_card") {
  if(d$coach_cards[[a]]==2) .ra_fail("Coaching staff is already excluded")
  d$coach_cards[[a]]<-if(p$color=="red") 2L else d$coach_cards[[a]]+1L;d$coach_ms[[a]]<-240000L
  if(d$phase=="play") {d$phase<-"stopped";d$restart_kind<-"restart"}
 } else if(k=="set_shootout") {
  for(aa in 0:1) if(!identical(sort(unlist(p$orders[[aa+1L]])),sort(.ra_korfball_active(d,aa)))) .ra_fail("Shoot-out order must contain every on-court player exactly once")
  d$shoot_first<-p$first;d$shoot_order<-p$orders;d$phase<-"shootout"
 } else if(k=="shoot_penalty") {
  f<-d$shoot_first+1L;n<-unlist(d$shoot_taken);turn<-if(n[f]==n[3L-f]) f-1L else 2L-f
  if(t!=turn || i!=d$shoot_order[[a]][[n[a]%%length(d$shoot_order[[a]])+1L]]) .ra_fail("Follow alternating teams and the registered player sequence")
  d$shoot_taken[[a]]<-n[a]+1L;d$shoot_score[[a]]<-d$shoot_score[[a]]+as.integer(p$scored)
  if(d$shoot_taken[[1]]==d$shoot_taken[[2]] && d$shoot_score[[1]]!=d$shoot_score[[2]]) d<-.ra_korfball_finish(d,as.integer(d$shoot_score[[2]]>d$shoot_score[[1]]),"shootout")
 } else if(k=="abandon") {d$phase<-"abandoned";d$break_ms<-0L} else if(k=="concede") d<-.ra_korfball_finish(d,1L-t,"concession") else if(k=="official_result") d<-.ra_korfball_finish(d,p$winner,if(is.null(p$winner)) "no_contest" else "official_result")
 d
}
.ra_korfball_legal<-function(state) {
 d<-state$data;s<-.ra_korfball_schemas(d)
 list(mode="event_types",actions=list(),action_types=lapply(names(s),function(k) list(type=k,params_schema=s[[k]],preconditions=list("Phase, roster eligibility and event-specific allowances must permit the event."),external_observations=list("Official goals, restart decisions, shot-clock violations and sanctions."))),constraints=list(phase=d$phase,score=d$score,period=d$period,clock_ms=d$clock_ms,attack_zone=d$attack_zone,restart_team=d$restart_team,restart_kind=d$restart_kind,substitutions=d$substitutions,cooldown_ms=d$cooldown_ms,playable=.ra_korfball_playable(d),shoot_score=d$shoot_score),completeness=FALSE,reason="official_korfball_match_events")
}
.ra_korfball_outcome<-function(state,pack) .ra_pelota_outcome(state,pack)
