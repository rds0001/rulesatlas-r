.ra_icehockey_active<-function(d,t) as.list(which(unlist(d$status[[t+1L]])=="active")-1L)
.ra_icehockey_measured<-function(d) {
 out<-integer();seen<-list(integer(),integer());reductions<-c(0L,0L)
 for(j in seq_along(d$penalties)) {r<-d$penalties[[j]];t<-r$team+1L;i<-r$offender;if(!i %in% seen[[t]] && (r$offset || reductions[t]<2)) {seen[[t]]<-c(seen[[t]],i);out<-c(out,j);if(!r$offset) reductions[t]<-reductions[t]+1L}}
 out
}
.ra_icehockey_capacity<-function(d,t) 6-sum(vapply(.ra_icehockey_measured(d),function(j) d$penalties[[j]]$team==t && !d$penalties[[j]]$offset,logical(1)))
.ra_icehockey_blocked<-function(d,t,i) any(vapply(d$penalties,function(r) r$team==t && (i==r$server || i==r$offender && (!d$goalkeepers[[t+1L]][[i+1L]] || d$status[[t+1L]][[i+1L]]=="penalty")),logical(1))) || d$personal_ms[[t+1L]][[i+1L]]>0 || d$held[[t+1L]][[i+1L]]
.ra_icehockey_release<-function(d,stopped) {
 for(t in 0:1) for(i in 0:21) {
  a<-t+1L;b<-i+1L
  if(stopped || .ra_icehockey_capacity(d,t)>4 && !d$goalkeepers[[a]][[b]] && !d$personal_wait[[a]][[b]]) d$held[[a]][[b]]<-FALSE
  if(stopped) d$personal_wait[[a]][[b]]<-FALSE
  if(d$status[[a]][[b]]=="penalty" && !.ra_icehockey_blocked(d,t,i)) d$status[[a]][[b]]<-"bench"
 };d
}
.ra_icehockey_stop<-function(d) {d$phase<-"stopped";.ra_icehockey_release(d,TRUE)}
.ra_icehockey_validate<-function(d) {
 bad<-function(m) .ra_fail(m,code="INVALID_STATE")
 for(t in 0:1) {
  a<-.ra_icehockey_active(d,t);z<-t+1L
  if(sum(unlist(d$goalkeepers[[z]]))!=2) bad("The roster has exactly two registered goalkeepers")
  if(length(a)>6 || sum(vapply(a,function(i) d$goalkeepers[[z]][[i+1L]],logical(1)))>1) bad("At most six active players and one goalkeeper are permitted")
  if(d$phase=="play" && (length(a)>.ra_icehockey_capacity(d,t) || sum(vapply(a,function(i) !d$goalkeepers[[z]][[i+1L]],logical(1)))<3)) bad("Live lineup must satisfy penalty capacity and minimum strength")
  for(i in 0:21) {
   b<-i+1L
   if(.ra_icehockey_blocked(d,t,i) && !d$status[[z]][[b]] %in% c("penalty","excluded")) bad("Penalized players cannot be available")
   if(d$status[[z]][[b]]=="penalty" && !.ra_icehockey_blocked(d,t,i)) bad("Penalty status requires a clock or return restriction")
   if((d$majors[[z]][[b]]>1 || d$personals[[z]][[b]]>1) && d$status[[z]][[b]]!="excluded") bad("Repeated major or personal misconduct excludes the player")
  }
  i<-d$captain[[z]]
  if(!is.null(i) && (d$goalkeepers[[z]][[i+1L]] || d$status[[z]][[i+1L]]=="excluded")) bad("Captain must be an eligible field player")
 }
 for(r in d$penalties) if(d$status[[r$team+1L]][[r$server+1L]]!="penalty") bad("Penalty server must be unavailable")
 if(d$phase=="play" && (d$clock_ms==0 || !is.null(d$pending_team))) bad("Play requires remaining time and no pending penalty shot")
 if(d$phase=="interval" && (d$period==3 || d$clock_ms!=0)) bad("Interval must follow an unfinished regulation match")
 if(d$phase=="pre" && (d$period!=1 || d$clock_ms!=1200000 || any(unlist(d$score)!=0) || length(d$penalties)>0)) bad("Pre-game state must precede scoring and penalties")
 if(!is.null(d$pending_team) && !d$phase %in% c("stopped","timeout")) bad("Penalty shot requires a stopped match")
 if(d$phase=="finished") {
  if(identical(d$reason,"regulation")) {w<-if(d$score[[1]]==d$score[[2]]) NULL else as.integer(d$score[[2]]>d$score[[1]]);if(d$period!=3 || d$clock_ms!=0 || !identical(d$winner,w)) bad("Regulation outcome must match score and completed time")}
  else if(identical(d$reason,"concession")) {if(is.null(d$winner)) bad("Forfeit requires a winner")}
  else if(is.null(d$reason) || !d$reason %in% c("official_result","no_contest")) bad("Terminal state requires a result reason")
 } else if(!is.null(d$winner) || !is.null(d$reason)) bad("Only a finished match has a result")
}
.ra_icehockey_schemas<-function(d) {
 ph<-d$phase;s<-list();empty<-setNames(list(),character());T<-list(type="integer",minimum=0,maximum=1);P<-list(type="integer",minimum=0,maximum=21);B<-list(type="boolean");kind<-list(type="enum",values=as.list(c("minor","double_minor","major","misconduct","minor_misconduct","game_misconduct","major_game")))
 if(ph=="finished") return(s)
 if(ph=="abandoned") return(list(official_result=list(winner=c(T,list(nullable=TRUE)))))
 s<-list(set_lineup=list(team=T,players=list(type="array",minItems=0,maxItems=6,items=P)),appoint_captain=list(team=T,player=P),withdraw=list(team=T,player=P),abandon=empty,concede=list(team=T))
 if(ph=="pre") s$start_match<-empty
 if(ph=="interval") s$elapse_break<-list(milliseconds=list(type="integer",minimum=1,maximum=900000))
 if(ph %in% c("play","stopped")) {
  s$timeout<-list(team=T)
  s$late_differential<-list(team=T,player=P,server=P,opponent=P,opponent_kind=list(type="enum",values=list("minor","double_minor")),game_misconduct=B)
  if(ph=="stopped" && !is.null(d$icing_team)) s$allow_icing_change<-empty
  pair<-list(players=list(type="array",minItems=2,maxItems=2,items=P),kind=list(type="enum",values=as.list(c("minor","double_minor","major"))))
  s$coincidental<-list(pairs=list(type="array",minItems=1,maxItems=10,items=list(type="object",properties=pair,required=as.list(names(pair)))))
  s$penalty<-list(team=T,player=P,server=P,kind=kind)
  if(is.null(d$pending_team)) {
   s$timeout<-list(team=T);s$award_shot<-list(team=T)
   if(ph=="play") s<-c(s,list(elapse=list(milliseconds=list(type="integer",minimum=1,maximum=1200000)),stop_play=list(icing_team=c(T,list(nullable=TRUE))),goal=list(team=T,player=P,own_goal=B),signal_penalty=list(team=T,player=P,server=P,kind=kind),signal_shot=list(team=T)))
   else if(d$clock_ms>0) s$restart<-empty else s$close_period<-empty
  } else s$resolve_shot<-list(player=P,scored=B)
 }
 if(ph=="timeout") s$elapse_break<-list(milliseconds=list(type="integer",minimum=1,maximum=30000))
 s
}
.ra_icehockey_impose<-function(d,p,offset=FALSE,coincident=FALSE) {
 t<-p$team+1L;i<-p$player+1L;server<-p$server+1L;kind<-p$kind
 if(d$status[[t]][[i]]=="excluded") .ra_fail("Excluded player cannot incur another match reduction")
 if(kind %in% c("major","major_game")) d$majors[[t]][[i]]<-d$majors[[t]][[i]]+1L
 if(kind %in% c("misconduct","minor_misconduct")) d$personals[[t]][[i]]<-d$personals[[t]][[i]]+1L
 excluded<-kind %in% c("game_misconduct","major_game") || d$majors[[t]][[i]]>=2 || d$personals[[t]][[i]]>=2
 duration<-if(kind %in% c("major","major_game")) 300000L else 120000L
 count<-if(kind=="double_minor") 2L else if(kind %in% c("misconduct","game_misconduct")) 0L else 1L
 delegated<-d$goalkeepers[[t]][[i]] || excluded || kind=="minor_misconduct" || d$personal_ms[[t]][[i]]>0
 if(delegated && (count>0 || (kind=="misconduct" && !excluded))) {
  if(server==i || d$goalkeepers[[t]][[server]] || !d$status[[t]][[server]] %in% c("active","bench")) .ra_fail("This sanction needs a separate eligible skater server")
 } else if(!excluded && server!=i) .ra_fail("The offender must serve this penalty")
 if(excluded) {
  d$status[[t]][[i]]<-"excluded";d$personal_ms[[t]][[i]]<-0L;d$personal_running[[t]][[i]]<-FALSE;d$held[[t]][[i]]<-FALSE;d$personal_wait[[t]][[i]]<-FALSE
  if(!is.null(d$captain[[t]]) && d$captain[[t]]==i-1L) d$captain[t]<-list(NULL)
  d$penalties<-Filter(function(r) !(r$team==t-1L && r$offender==i-1L && r$offset),d$penalties)
  for(j in seq_along(d$penalties)) if(d$penalties[[j]]$team==t-1L && d$penalties[[j]]$offender==i-1L) {
   if(server==i || d$goalkeepers[[t]][[server]] || !d$status[[t]][[server]] %in% c("active","bench","penalty")) .ra_fail("Unserved penalties need an eligible replacement server")
   d$penalties[[j]]$server<-server-1L;d$status[[t]][[server]]<-"penalty"
  }
 } else if(!d$goalkeepers[[t]][[i]]) d$status[[t]][[i]]<-"penalty"
 if(kind %in% c("misconduct","minor_misconduct") && !excluded) {
  personal<-if(d$goalkeepers[[t]][[i]]) server else i
  d$personal_ms[[t]][[personal]]<-600000L;d$personal_wait[[t]][[personal]]<-TRUE;d$personal_running[[t]][[personal]]<-FALSE;d$status[[t]][[personal]]<-"penalty"
 }
 if(count>0) {d$status[[t]][[server]]<-"penalty";for(j in seq_len(count)) d$penalties[[length(d$penalties)+1L]]<-list(team=t-1L,offender=i-1L,server=server-1L,ms=duration,minor=duration==120000,offset=offset,coincident=coincident)}
 d
}
.ra_icehockey_flush<-function(d,cancel_minor=FALSE) {
 pending<-d$delayed;d$delayed<-list()
 if(cancel_minor) {
  ix<-which(vapply(pending,function(p) p$kind %in% c("minor","double_minor","minor_misconduct"),logical(1)))
  if(length(ix)) {j<-tail(ix,1);if(pending[[j]]$kind=="double_minor") pending[[j]]$kind<-"minor" else if(pending[[j]]$kind=="minor_misconduct") {pending[[j]]$kind<-"misconduct";if(!d$goalkeepers[[pending[[j]]$team+1L]][[pending[[j]]$player+1L]]) pending[[j]]$server<-pending[[j]]$player} else pending<-pending[-j]}
 }
 for(p in pending) d<-.ra_icehockey_impose(d,p);d
}
.ra_icehockey_expire<-function(d,indices) {
 for(j in indices) {r<-d$penalties[[j]];for(i in unique(c(r$offender,r$server))) if(d$status[[r$team+1L]][[i+1L]]=="penalty") {d$held[[r$team+1L]][[i+1L]]<-TRUE;if(r$coincident) d$personal_wait[[r$team+1L]][[i+1L]]<-TRUE}}
 if(length(indices)>0) d$penalties<-d$penalties[-indices]
 .ra_icehockey_release(d,FALSE)
}
.ra_icehockey_elapse<-function(d,ms) {
 while(ms>0) {
  indices<-.ra_icehockey_measured(d);running<-list()
  for(t in 0:1) for(i in 0:21) {
   if(!any(vapply(d$penalties,function(r) r$team==t && r$offender==i,logical(1)))) d$personal_running[[t+1L]][[i+1L]]<-d$personal_ms[[t+1L]][[i+1L]]>0
   else if(any(vapply(indices,function(j) d$penalties[[j]]$team==t && d$penalties[[j]]$offender==i,logical(1)))) d$personal_running[[t+1L]][[i+1L]]<-FALSE
   if(d$personal_ms[[t+1L]][[i+1L]]>0 && d$personal_running[[t+1L]][[i+1L]]) running[[length(running)+1L]]<-c(t+1L,i+1L)
  }
  step<-min(c(ms,vapply(indices,function(j) d$penalties[[j]]$ms,numeric(1)),vapply(running,function(z) d$personal_ms[[z[1]]][[z[2]]],numeric(1))));ms<-ms-step
  for(j in indices) d$penalties[[j]]$ms<-d$penalties[[j]]$ms-step
  for(z in running) {d$personal_ms[[z[1]]][[z[2]]]<-d$personal_ms[[z[1]]][[z[2]]]-step;if(d$personal_ms[[z[1]]][[z[2]]]==0) {d$personal_running[[z[1]]][[z[2]]]<-FALSE;d$held[[z[1]]][[z[2]]]<-TRUE;d$personal_wait[[z[1]]][[z[2]]]<-TRUE}}
  d<-.ra_icehockey_expire(d,indices[vapply(indices,function(j) d$penalties[[j]]$ms==0,logical(1))])
 };d
}
.ra_icehockey_apply<-function(state,action) {
 d<-state$data;k<-action$type;p<-action$params;s<-.ra_icehockey_schemas(d)
 if(!k %in% names(s)) .ra_fail("Action is not permitted in this phase")
 .ra_fields(p,s[[k]],"/params");t<-p$team;i<-p$player
 if(k=="start_match") {
  if(any(vapply(0:1,function(j) length(.ra_icehockey_active(d,j))!=6 || sum(vapply(.ra_icehockey_active(d,j),function(x) d$goalkeepers[[j+1L]][[x+1L]],logical(1)))!=1 || is.null(d$captain[[j+1L]]),logical(1)))) .ra_fail("Start requires five field players, one goalkeeper and a captain per team")
  d<-.ra_icehockey_stop(d)
 } else if(k=="set_lineup") {
  a<-unlist(p$players)
  if(!is.null(d$icing_team) && d$icing_team==t && !setequal(a,unlist(.ra_icehockey_active(d,t)))) .ra_fail("Icing team cannot change its lineup before the faceoff")
  if(anyDuplicated(a)>0 || length(a)>.ra_icehockey_capacity(d,t) || sum(vapply(a,function(x) d$goalkeepers[[t+1L]][[x+1L]],logical(1)))>1) .ra_fail("Lineup violates capacity, uniqueness or goalkeeper limit")
  if(any(vapply(a,function(x) !d$status[[t+1L]][[x+1L]] %in% c("active","bench"),logical(1)))) .ra_fail("Unavailable player cannot enter")
  if(d$phase=="play" && sum(vapply(a,function(i) !d$goalkeepers[[t+1L]][[i+1L]],logical(1)))<3) .ra_fail("At least three skaters are required during play")
  for(x in 0:21) if(d$status[[t+1L]][[x+1L]] %in% c("active","bench")) d$status[[t+1L]][[x+1L]]<-if(x %in% a) "active" else "bench"
 } else if(k=="appoint_captain") {
  if(d$goalkeepers[[t+1L]][[i+1L]] || !d$status[[t+1L]][[i+1L]] %in% c("active","bench")) .ra_fail("Captain must be an available field player")
  d$captain[[t+1L]]<-i
 } else if(k=="designate_goalkeeper") {
  if(d$phase=="play" || d$status[[t+1L]][[i+1L]]!="bench" || (!is.null(d$captain[[t+1L]]) && d$captain[[t+1L]]==i)) .ra_fail("A new goalkeeper must be an available bench player at an interruption")
  d$goalkeepers[[t+1L]][[i+1L]]<-TRUE
 } else if(k=="withdraw") {
  if(!d$status[[t+1L]][[i+1L]] %in% c("active","bench")) .ra_fail("Only available players can withdraw")
  d$status[[t+1L]][[i+1L]]<-"excluded"
  if(!is.null(d$captain[[t+1L]]) && d$captain[[t+1L]]==i) d$captain[t+1L]<-list(NULL)
  if(d$phase=="play") d<-.ra_icehockey_stop(d)
 } else if(k=="restart") {
  if(length(d$delayed)>0 || !is.null(d$delayed_shot)) .ra_fail("Resolve delayed decisions before restarting")
  if(any(vapply(0:1,function(j) sum(vapply(.ra_icehockey_active(d,j),function(i) !d$goalkeepers[[j+1L]][[i+1L]],logical(1)))<3 || length(.ra_icehockey_active(d,j))>.ra_icehockey_capacity(d,j) || is.null(d$captain[[j+1L]]),logical(1)))) .ra_fail("Restart requires legal lineups and captains")
  d$phase<-"play";d["icing_team"]<-list(NULL)
 } else if(k=="elapse") {
  ms<-p$milliseconds;if(ms>d$clock_ms) .ra_fail("Time cannot cross the period boundary")
  d<-.ra_icehockey_elapse(d,ms);d$clock_ms<-d$clock_ms-ms
  if(d$clock_ms==0) {d<-.ra_icehockey_flush(d);d["pending_team"]<-list(d$delayed_shot);d["delayed_shot"]<-list(NULL);d<-.ra_icehockey_stop(d)}
 } else if(k=="stop_play") {
  d["icing_team"]<-list(p$icing_team)
d<-.ra_icehockey_flush(d);d["pending_team"]<-list(d$delayed_shot);d["delayed_shot"]<-list(NULL);d<-.ra_icehockey_stop(d)
 } else if(k=="penalty") {d<-.ra_icehockey_impose(d,p);d<-.ra_icehockey_stop(d)
 } else if(k=="allow_icing_change") d["icing_team"]<-list(NULL)
 else if(k=="late_differential") {
  if(d$period!=3 || d$clock_ms>300000) .ra_fail("Major/minor differential applies only in the last five minutes")
  other<-1L-t;opp<-p$opponent;small<-if(p$opponent_kind=="minor") 120000L else 240000L
  if(d$goalkeepers[[other+1L]][[opp+1L]] || !d$status[[other+1L]][[opp+1L]] %in% c("active","bench")) .ra_fail("Opposing coincidental offender must be an available skater")
  d<-.ra_icehockey_impose(d,list(team=t,player=i,server=p$server,kind=if(p$game_misconduct) "major_game" else "major"))
  d$penalties[[length(d$penalties)]]$ms<-300000L-small
  if(d$status[[t+1L]][[i+1L]]!="excluded") d$penalties[[length(d$penalties)+1L]]<-list(team=t,offender=i,server=p$server,ms=small,minor=FALSE,offset=TRUE,coincident=TRUE)
  d<-.ra_icehockey_impose(d,list(team=other,player=opp,server=opp,kind=p$opponent_kind),TRUE,TRUE);d<-.ra_icehockey_stop(d)
 } else if(k=="coincidental") {
  pairs<-p$pairs;ids<-list(integer(),integer())
  for(pair in pairs) for(team in 0:1) {player<-pair$players[[team+1L]];if(player %in% ids[[team+1L]] || d$goalkeepers[[team+1L]][[player+1L]] || !d$status[[team+1L]][[player+1L]] %in% c("active","bench")) .ra_fail("Coincidental pairs require distinct available skaters");ids[[team+1L]]<-c(ids[[team+1L]],player)}
  offset<-!(length(pairs)==1 && pairs[[1]]$kind=="minor" && !length(d$penalties))
  for(pair in pairs) for(team in 0:1) {player<-pair$players[[team+1L]];if(pair$kind=="major" && d$majors[[team+1L]][[player+1L]]>0) .ra_fail("Repeated-major exclusion must be assessed with an appointed server");d<-.ra_icehockey_impose(d,list(team=team,player=player,server=player,kind=pair$kind),offset,TRUE)}
  d<-.ra_icehockey_stop(d)
 } else if(k=="signal_penalty") {
  if(length(d$delayed)>0 && d$delayed[[1]]$team!=t) .ra_fail("A delayed call can concern only one offending team")
  .ra_icehockey_impose(d,p);d$delayed[[length(d$delayed)+1L]]<-p[c("team","player","server","kind")]
 } else if(k=="signal_shot") {
  if(!is.null(d$delayed_shot)) .ra_fail("A penalty shot is already delayed")
  d$delayed_shot<-t
 } else if(k=="goal") {
  scorer<-if(p$own_goal) 1L-t else t
  if(d$status[[scorer+1L]][[i+1L]]!="active") .ra_fail("Goal attribution requires an active player")
  disallowed<-length(d$delayed)>0 && d$delayed[[1]]$team==t && !p$own_goal;released<-FALSE
  if(!disallowed) {
   d$score[[t+1L]]<-d$score[[t+1L]]+1L
   if(.ra_icehockey_capacity(d,1L-t)<.ra_icehockey_capacity(d,t)) {
    js<-Filter(function(j) {r<-d$penalties[[j]];r$team==1L-t && r$minor && !r$coincident},.ra_icehockey_measured(d))
    if(length(js)>0) {j<-js[which.min(vapply(js,function(j) d$penalties[[j]]$ms,numeric(1)))];d<-.ra_icehockey_expire(d,j);released<-TRUE}
   }
  }
  d<-.ra_icehockey_flush(d,!disallowed && !released && length(d$delayed)>0 && d$delayed[[1]]$team!=t)
  d["pending_team"]<-list(if(!disallowed && !is.null(d$delayed_shot) && d$delayed_shot==t) NULL else d$delayed_shot);d["delayed_shot"]<-list(NULL);d<-.ra_icehockey_stop(d)
 } else if(k=="award_shot") {d<-.ra_icehockey_flush(d);d$pending_team<-t;d["delayed_shot"]<-list(NULL);d<-.ra_icehockey_stop(d)
 } else if(k=="resolve_shot") {
  t<-d$pending_team
  if(!d$status[[t+1L]][[i+1L]] %in% c("active","bench") || d$goalkeepers[[t+1L]][[i+1L]]) .ra_fail("Penalty shooter must be an available field player")
  if(p$scored) d$score[[t+1L]]<-d$score[[t+1L]]+1L
  d["pending_team"]<-list(NULL)
 } else if(k=="timeout") {
  if(!is.null(d$icing_team) && d$icing_team==t) .ra_fail("Icing team cannot take a timeout before the faceoff")
  if(d$timeouts[[t+1L]]) .ra_fail("One timeout per team and match is available")
  if(d$phase=="play") .ra_fail("Record the official interruption before taking the timeout")
  d$timeouts[[t+1L]]<-TRUE;d$phase<-"timeout";d$break_ms<-30000L
 } else if(k=="elapse_break") {
  ms<-p$milliseconds;if(ms>d$break_ms) .ra_fail("Time cannot cross the break boundary")
  d$break_ms<-d$break_ms-ms
  if(d$break_ms==0) {if(d$phase=="interval") {d$period<-d$period+1L;d$clock_ms<-1200000L};d<-.ra_icehockey_stop(d)}
 } else if(k=="close_period") {
  if(length(d$delayed)>0 || !is.null(d$delayed_shot)) .ra_fail("Outstanding official decisions must be resolved")
  if(d$period<3) {d$phase<-"interval";d$break_ms<-900000L} else {d$phase<-"finished";d$reason<-"regulation";d["winner"]<-list(if(d$score[[1]]==d$score[[2]]) NULL else as.integer(d$score[[2]]>d$score[[1]]))}
 } else if(k=="abandon") {d$phase<-"abandoned";d["pending_team"]<-list(NULL)
 } else if(k=="concede") {d$phase<-"finished";d$winner<-1L-t;d$reason<-"concession";d["pending_team"]<-list(NULL);d$score<-lapply(0:1,function(j) if(j==1L-t) 5L else 0L)
 } else if(k=="official_result") {d$phase<-"finished";d["winner"]<-list(p$winner);d$reason<-if(is.null(p$winner)) "no_contest" else "official_result"}
 d
}
.ra_icehockey_legal<-function(state) {
 d<-state$data;s<-.ra_icehockey_schemas(d)
 list(mode="event_types",actions=list(),action_types=lapply(names(s),function(k) list(type=k,params_schema=s[[k]],preconditions=list("Phase, eligible players, penalty queue and remaining time must permit the event."),external_observations=list("Official goals, infringements, interruptions and penalty-shot results."))),constraints=list(phase=d$phase,period=d$period,clock_ms=d$clock_ms,score=d$score,capacity=lapply(0:1,function(t) .ra_icehockey_capacity(d,t)),penalties=d$penalties,personal_ms=d$personal_ms,pending_team=d$pending_team,icing_team=d$icing_team),completeness=FALSE,reason="official_ice_hockey_events")
}
.ra_icehockey_outcome<-function(state,pack) .ra_handball_outcome(state,pack)
