.ra_takraw_int<-function(a,b) list(type="integer",minimum=a,maximum=b)
.ra_takraw_wins<-function(d) lapply(0:1,function(t) sum(vapply(d$sets,function(s) s$winner==t,logical(1))))
.ra_takraw_complete<-function(s) max(unlist(s))>=21 && (abs(s[[1]]-s[[2]])>=2 || max(unlist(s))==25)
.ra_takraw_server<-function(d) {
 if(is.null(d$starter)) return(NULL)
 n<-sum(unlist(d$points));as.integer((d$starter+if(n<40) n%/%3 else 13+n-40)%%2)
}
.ra_takraw_eligible<-function(d,t) as.list(which(!unlist(d$red[[t+1L]]) & unlist(d$available[[t+1L]]))-1L)
.ra_takraw_ready<-function(d) all(vapply(0:1,function(t) all(vapply(d$lineup[[t+1L]],function(i) !is.null(i) && i %in% unlist(.ra_takraw_eligible(d,t)),logical(1))),logical(1)))
.ra_takraw_finish_set<-function(d,w,reason) {
 d$sets[[length(d$sets)+1L]]<-list(score=d$points,winner=w,reason=reason);d$phase<-"interval";d$break_ms<-120000L
 if(.ra_takraw_wins(d)[[w+1L]]==2) {d$phase<-"finished";d$winner<-w;d$reason<-"two_sets";d$break_ms<-0L}
 d
}
.ra_takraw_point<-function(d,w) {
 d$points[[w+1L]]<-d$points[[w+1L]]+1L;d$phase<-"ready"
 if(.ra_takraw_complete(d$points)) d<-.ra_takraw_finish_set(d,w,"points") else if(length(d$sets)==2 && !d$changed_ends && max(unlist(d$points))==11) d$phase<-"change_ends"
 d
}
.ra_takraw_replace<-function(d,t,slot,i) {
 if(d$substitutions[[t+1L]]>=2 || !i %in% unlist(.ra_takraw_eligible(d,t)) || i %in% unlist(d$lineup[[t+1L]])) .ra_fail("Replacement needs an eligible reserve and an unused set substitution")
 d$lineup[[t+1L]][[slot+1L]]<-i;d$substitutions[[t+1L]]<-d$substitutions[[t+1L]]+1L;d
}
.ra_takraw_validate<-function(d) {
 bad<-function(m) .ra_fail(m,code="INVALID_STATE")
 for(t in 0:1) {
  ids<-unlist(d$lineup[[t+1L]])
  if(anyDuplicated(ids) || any(vapply(ids,function(i) !i %in% unlist(.ra_takraw_eligible(d,t)),logical(1)))) bad("Lineup must contain distinct eligible players")
  if(any(vapply(0:4,function(i) d$yellow[[t+1L]][[i+1L]]==2 && !d$red[[t+1L]][[i+1L]],logical(1)))) bad("Second match caution requires exclusion")
 }
 for(j in seq_along(d$sets)) {
  s<-d$sets[[j]];prior<-d$sets[seq_len(j-1L)]
  if(any(vapply(0:1,function(t) sum(vapply(prior,function(x) x$winner==t,logical(1)))>=2,logical(1)))) bad("No set follows the second set victory")
  if(s$reason=="points") {
   hi<-max(unlist(s$score));lo<-min(unlist(s$score))
   if(!((hi==21 && lo<=19) || (hi>=22 && hi<=24 && lo==hi-2) || (hi==25 && lo %in% c(23,24))) || s$score[[s$winner+1L]]!=hi) bad("Completed set must stop at its first winning score")
  } else if(.ra_takraw_complete(s$score)) bad("Forfeited set cannot already be decided by points")
 }
 if(d$phase=="choose_service" && (!is.null(d$starter) || length(d$sets)>0 || any(unlist(d$points)!=0))) bad("Initial service choice precedes all play")
 if(!d$phase %in% c("choose_service","abandoned","finished") && is.null(d$starter)) bad("Started match needs initial set service entitlement")
 if(d$phase %in% c("ready","rally","stopped","timeout","change_ends") && (.ra_takraw_complete(d$points) || max(unlist(.ra_takraw_wins(d)))==2)) bad("Active set cannot already be decided")
 if(length(d$sets)==2 && d$phase %in% c("ready","rally","timeout") && max(unlist(d$points))>=11 && !d$changed_ends) bad("Complete the deciding-set end change before continuing")
 if(d$phase=="rally" && !.ra_takraw_ready(d)) bad("Rally requires three eligible players per team")
 if(d$phase=="interval" && (length(d$sets)==0 || !identical(d$points,d$sets[[length(d$sets)]]$score) || max(unlist(.ra_takraw_wins(d)))==2)) bad("Interval follows a completed nonfinal set")
 if(d$phase=="change_ends" && (length(d$sets)!=2 || max(unlist(d$points))!=11 || d$changed_ends)) bad("Third-set end change occurs at eleven points")
 if((d$phase %in% c("timeout","interval"))!=(d$break_ms>0)) bad("Break clock and phase disagree")
 if(d$phase=="timeout" && d$break_ms>60000) bad("Timeout lasts at most one minute")
 if(d$phase=="finished") {
  if(is.null(d$reason) || (d$reason=="no_contest")!=is.null(d$winner)) bad("Finished match needs its declared result")
  if(d$reason=="two_sets" && (.ra_takraw_wins(d)[[d$winner+1L]]!=2 || !identical(d$points,d$sets[[length(d$sets)]]$score))) bad("Match winner must have two completed sets")
 } else if(!is.null(d$winner) || !is.null(d$reason) || max(unlist(.ra_takraw_wins(d)))==2) bad("Ongoing match cannot declare a result")
}
.ra_takraw_schemas<-function(d) {
 ph<-d$phase;s<-list();empty<-setNames(list(),character());T<-.ra_takraw_int(0,1);P<-.ra_takraw_int(0,4)
 if(ph=="finished") return(s)
 if(ph=="abandoned") return(list(official_result=list(winner=c(T,list(nullable=TRUE)))))
 if(ph=="choose_service") s$choose_service<-list(team=T)
 if(ph=="ready") s<-c(s,list(serve=list(team=T,player=P),timeout=list(team=T),service_delay=list(team=T)))
 if(ph=="rally") s<-c(s,list(fault=list(team=T),replay=empty,stop=empty))
 if(ph=="stopped") s$resume<-empty
 if(ph %in% c("ready","stopped","timeout","choose_service")) s<-c(s,list(substitute=list(team=T,slot=.ra_takraw_int(0,2),incoming=P),assign_roles=list(team=T,players=list(type="array",minItems=3,maxItems=3,items=P))))
 if(ph %in% c("timeout","interval")) s$elapse_break<-list(milliseconds=.ra_takraw_int(1,120000))
 if(ph=="change_ends") s$change_ends<-empty
 if(ph %in% c("ready","rally","stopped","timeout","change_ends")) s<-c(s,list(sanction=list(team=T,player=P,kind=list(type="enum",values=list("yellow","red"))),unavailable=list(team=T,player=P)))
 if(ph %in% c("interval","choose_service","ready","stopped")) s$restore_availability<-list(team=T,player=P)
 c(s,list(abandon=empty,concede=list(team=T)))
}
.ra_takraw_remove<-function(d,t,i,red=FALSE) {
 if(red) d$red[[t+1L]][[i+1L]]<-TRUE else d$available[[t+1L]][[i+1L]]<-FALSE
 if(i %in% unlist(d$lineup[[t+1L]])) {
  slot<-which(vapply(d$lineup[[t+1L]],function(x) !is.null(x) && x==i,logical(1)))
  d$lineup[[t+1L]][slot]<-list(NULL)
  reserves<-setdiff(unlist(.ra_takraw_eligible(d,t)),unlist(d$lineup[[t+1L]]))
  if(d$substitutions[[t+1L]]>=2 || length(reserves)==0) d<-.ra_takraw_finish_set(d,1L-t,"insufficient_players") else {d$phase<-"stopped";d$break_ms<-0L}
 } else if(d$phase=="rally") d$phase<-"stopped"
 d
}
.ra_takraw_apply<-function(state,action) {
 d<-state$data;k<-action$type;p<-action$params;ss<-.ra_takraw_schemas(d)
 if(!k %in% names(ss)) .ra_fail("Action is not permitted in this phase")
 .ra_fields(p,ss[[k]],"/params");t<-p$team
 if(k=="choose_service") {
  if(!.ra_takraw_ready(d)) .ra_fail("Three eligible players per team are required")
  d$starter<-t;d$phase<-"ready"
 } else if(k=="serve") {
  if(t!=.ra_takraw_server(d) || !identical(p$player,d$lineup[[t+1L]][[1]]) || !.ra_takraw_ready(d)) .ra_fail("Entitled Tekong and complete eligible lineups are required")
  d$phase<-"rally"
 } else if(k=="fault") d<-.ra_takraw_point(d,1L-t) else if(k=="replay") d$phase<-"ready" else if(k=="stop") d$phase<-"stopped" else if(k=="resume") {
  if(!.ra_takraw_ready(d)) .ra_fail("Replace missing players before resuming")
  d$phase<-if(length(d$sets)==2 && max(unlist(d$points))==11 && !d$changed_ends) "change_ends" else "ready"
 } else if(k=="service_delay") {
  if(t!=.ra_takraw_server(d)) .ra_fail("Service delay concerns the entitled serving regu")
  if(d$delay_warned[[t+1L]]) d<-.ra_takraw_point(d,1L-t) else d$delay_warned[[t+1L]]<-TRUE
 } else if(k=="substitute") d<-.ra_takraw_replace(d,t,p$slot,p$incoming) else if(k=="assign_roles") {
  if(!identical(sort(unlist(p$players)),sort(unlist(d$lineup[[t+1L]])))) .ra_fail("Role reassignment must preserve the three current players")
  d$lineup[[t+1L]]<-p$players
 } else if(k=="timeout") {
  if(d$timeouts[[t+1L]]) .ra_fail("Only one timeout per team per set")
  d$timeouts[[t+1L]]<-TRUE;d$phase<-"timeout";d$break_ms<-60000L
 } else if(k=="elapse_break") {
  if(p$milliseconds>d$break_ms) .ra_fail("Elapsed time exceeds the remaining break")
  d$break_ms<-d$break_ms-p$milliseconds
  if(d$break_ms==0) {
   if(d$phase=="interval") {d$points<-list(0L,0L);d$starter<-1L-d$starter;d$substitutions<-list(0L,0L);d$timeouts<-list(FALSE,FALSE);d$changed_ends<-FALSE}
   d$phase<-"ready"
   short<-Filter(function(j) length(.ra_takraw_eligible(d,j))<3,as.list(0:1))
   if(length(short)==2) d$phase<-"abandoned" else if(length(short)==1) d<-.ra_takraw_finish_set(d,1L-short[[1]],"insufficient_players")
  }
 } else if(k=="change_ends") {d$phase<-"ready";d$changed_ends<-TRUE} else if(k=="sanction") {
  i<-p$player
  if(d$red[[t+1L]][[i+1L]]) .ra_fail("Player is already excluded")
  if(p$kind=="yellow") d$yellow[[t+1L]][[i+1L]]<-d$yellow[[t+1L]][[i+1L]]+1L
  if(p$kind=="red" || d$yellow[[t+1L]][[i+1L]]==2) d<-.ra_takraw_remove(d,t,i,red=TRUE) else if(d$phase=="rally") d$phase<-"stopped"
 } else if(k=="unavailable") {
  i<-p$player
  if(d$red[[t+1L]][[i+1L]] || !d$available[[t+1L]][[i+1L]]) .ra_fail("Player is already unavailable")
  d<-.ra_takraw_remove(d,t,i)
 } else if(k=="restore_availability") {
  i<-p$player
  if(d$red[[t+1L]][[i+1L]] || d$available[[t+1L]][[i+1L]]) .ra_fail("Only a non-excluded unavailable player can be restored")
  d$available[[t+1L]][[i+1L]]<-TRUE
 } else if(k=="abandon") {d$phase<-"abandoned";d$break_ms<-0L} else if(k=="concede") {d$phase<-"finished";d$winner<-1L-t;d$reason<-"concession";d$break_ms<-0L} else if(k=="official_result") {d$phase<-"finished";d["winner"]<-list(p$winner);d$reason<-if(is.null(p$winner)) "no_contest" else "official_result"}
 d
}
.ra_takraw_legal<-function(state) {
 d<-state$data;s<-.ra_takraw_schemas(d)
 list(mode="event_types",actions=list(),action_types=lapply(names(s),function(k) list(type=k,params_schema=s[[k]],preconditions=list("Service entitlement, phase, eligible lineup and remaining set allowances must permit the event."),external_observations=list("Official rally faults, delays, availability, sanctions and toss choice."))),constraints=list(phase=d$phase,points=d$points,sets_won=.ra_takraw_wins(d),server=.ra_takraw_server(d),lineup=d$lineup,substitutions=d$substitutions,timeouts=d$timeouts,break_ms=d$break_ms),completeness=FALSE,reason="official_2016_regu_events")
}
.ra_takraw_outcome<-function(state,pack) .ra_pelota_outcome(state,pack)
