.ra_fistball_i<-function(a,b) list(type="integer",minimum=a,maximum=b)
.ra_fistball_active<-function(d,t) which(unlist(d$status[[t+1L]])=="active")-1L
.ra_fistball_capacity<-function(d,t) 5L-sum(unlist(d$lost_place[[t+1L]]))-sum(unlist(d$suspensions[[t+1L]])>0)
.ra_fistball_complete<-function(s) max(unlist(s))>=11 && (abs(s[[1]]-s[[2]])>=2 || max(unlist(s))==15)
.ra_fistball_wins<-function(d) vapply(0:1,function(t) sum(vapply(d$sets,function(s) s[[t+1L]]>s[[2L-t]],logical(1))),numeric(1))
.ra_fistball_point<-function(d,w) {
 d$points[[w+1L]]<-d$points[[w+1L]]+1L;d$server<-1L-w;d$phase<-"ready";d$has_rally<-TRUE
 for(t in 1:2) for(i in 1:10) {
  n<-d$suspensions[[t]][[i]]
  if(n>0) {d$suspensions[[t]][[i]]<-n-1L;if(n==1) d$status[[t]][[i]]<-"bench"}
 }
 if(.ra_fistball_complete(d$points)) {
  d$sets<-c(d$sets,list(d$points));d$phase<-"interval"
  if(.ra_fistball_wins(d)[w+1L]==3) {d$phase<-"finished";d$winner<-w;d$reason<-"three_sets"}
 } else if(length(d$sets)==4 && !d$decider_changed && max(unlist(d$points))==6) d$phase<-"change_ends"
 d
}
.ra_fistball_validate<-function(d) {
 bad<-function(m) .ra_fail(m,code="INVALID_STATE")
 for(t in 0:1) {
  if(length(.ra_fistball_active(d,t))>.ra_fistball_capacity(d,t)) bad("Lineup exceeds available places after exclusions")
  for(i in 1:10) {
   st<-d$status[[t+1L]][[i]]
   if((st=="suspended")!=(d$suspensions[[t+1L]][[i]]>0)) bad("Suspension status and remaining points must agree")
   if(d$lost_place[[t+1L]][[i]] && st!="excluded") bad("Only excluded on-court players remove a permanent place")
  }
  captain<-d$captains[[t+1L]]
  if(!is.null(captain) && !d$status[[t+1L]][[captain+1L]] %in% c("active","bench")) bad("Captain must remain eligible")
  if(!d$phase %in% c("finished","abandoned") && .ra_fistball_capacity(d,t)<4) bad("Two simultaneous missing places require abandonment")
 }
 for(j in seq_along(d$sets)) {
  s<-d$sets[[j]];hi<-max(unlist(s));lo<-min(unlist(s))
  if(!((hi==11 && lo<=9) || (hi>=12 && hi<=14 && lo==hi-2) || (hi==15 && lo %in% c(13,14)))) bad("Completed set must stop at its first winning score")
  if(any(vapply(0:1,function(t) sum(vapply(head(d$sets,j-1L),function(x) x[[t+1L]]>x[[2L-t]],logical(1)))>=3,logical(1)))) bad("No set follows the third set victory")
 }
 if(d$phase=="choose_server" && (!is.null(d$server) || !length(d$sets) %in% c(0,4) || any(unlist(d$points)!=0))) bad("Toss is available only before first or deciding set")
 if(d$phase=="interval" && (length(d$sets)==0 || !identical(d$points,tail(d$sets,1)[[1]]) || max(.ra_fistball_wins(d))==3)) bad("Interval follows a completed nonfinal set")
 if(d$phase %in% c("ready","rally","stopped","timeout","change_ends")) {
  if(is.null(d$server) || .ra_fistball_complete(d$points)) bad("Active set requires a server and unfinished score")
  if(any(vapply(0:1,function(t) length(.ra_fistball_active(d,t))<4,logical(1)))) bad("Play requires at least four active players")
 }
 if(d$phase=="rally" && any(vapply(d$captains,is.null,logical(1)))) bad("Each team needs an appointed captain")
 if(d$phase=="change_ends" && (length(d$sets)!=4 || max(unlist(d$points))!=6 || d$decider_changed)) bad("Deciding-set end change occurs at the first six points")
 if((d$phase=="timeout")!=(d$timeout_ms>0)) bad("Timeout clock must agree with phase")
 if(d$phase=="finished") {
  if(is.null(d$reason) || (d$reason=="no_contest")!=is.null(d$winner)) bad("Result must identify a winner or no contest")
  if(d$reason=="three_sets" && (.ra_fistball_wins(d)[d$winner+1L]!=3 || !identical(d$points,tail(d$sets,1)[[1]]))) bad("Normal victory requires three completed sets")
 } else if(!is.null(d$winner) || !is.null(d$reason) || max(.ra_fistball_wins(d))==3) bad("An ongoing match cannot have a result")
}
.ra_fistball_schemas<-function(d) {
 ph<-d$phase;s<-list();i<-.ra_fistball_i;t<-i(0,1);p<-i(0,9);empty<-setNames(list(),character())
 if(ph=="finished") return(s)
 if(ph=="abandoned") return(list(official_result=list(winner=c(t,list(nullable=TRUE)))))
 if(ph=="choose_server") s$choose_server<-list(team=t)
 if(ph=="ready") {s$serve<-list(team=t,player=p);s$timeout<-list(team=t)}
 if(ph=="rally") {s$fault<-list(team=t);s$replay<-empty}
 if(ph %in% c("ready","rally")) s$stop<-empty
 if(ph=="stopped") s$resume<-empty
 if(ph=="timeout") s$elapse_timeout<-list(milliseconds=i(1,30000))
 if(ph=="interval") s$next_set<-empty
 if(ph=="change_ends") s$change_ends<-empty
 if(ph!="rally") {s$substitute<-list(team=t,out=p,"in"=p);s$enter<-list(team=t,player=p);s$appoint_captain<-list(team=t,player=p)}
 s$sanction<-list(team=t,player=p,kind=list(type="enum",values=list("caution","suspension","expulsion")))
 if(ph %in% c("ready","rally","stopped")) {s$delay<-list(team=t);s$substitution_fault<-list(team=t)}
 c(s,list(concede=list(team=t),abandon=empty))
}
.ra_fistball_apply<-function(state,action) {
 d<-state$data;k<-action$type;p<-action$params;s<-.ra_fistball_schemas(d);t<-p$team
 if(!k %in% names(s)) .ra_fail("Action is not permitted in this phase")
 .ra_fields(p,s[[k]],"/params")
 if(k=="choose_server") {d$server<-t;d$set_starter<-t;d$phase<-"ready"} else if(k=="serve") {
  if(t!=d$server || !p$player %in% .ra_fistball_active(d,t) || any(vapply(d$captains,is.null,logical(1)))) .ra_fail("Service requires an active entitled player and appointed captains")
  d$phase<-"rally"
 } else if(k=="fault") d<-.ra_fistball_point(d,1L-t) else if(k=="replay") {d$phase<-"ready";d$has_rally<-TRUE} else if(k=="stop") d$phase<-"stopped" else if(k=="resume") d$phase<-"ready" else if(k=="timeout") {
  if(!d$has_rally || d$timeouts[[t+1L]]) .ra_fail("One timeout per team and set is available after a rally")
  d$timeouts[[t+1L]]<-TRUE;d$phase<-"timeout";d$timeout_ms<-30000L
 } else if(k=="elapse_timeout") {
  if(p$milliseconds>d$timeout_ms) .ra_fail("Elapsed time exceeds the timeout")
  d$timeout_ms<-d$timeout_ms-p$milliseconds
  if(d$timeout_ms==0) d$phase<-"ready"
 } else if(k=="next_set") {
  deciding<-length(d$sets)==4;starter<-if(deciding) NULL else 1L-d$set_starter
  d$points<-list(0L,0L);d["server"]<-list(starter);d["set_starter"]<-list(starter);d$phase<-if(deciding) "choose_server" else "ready";d$timeouts<-list(FALSE,FALSE);d$has_rally<-FALSE;d$decider_changed<-FALSE
 } else if(k=="change_ends") {d$server<-1L-d$server;d$decider_changed<-TRUE;d$phase<-"ready"} else if(k=="substitute") {
  if(!p$out %in% .ra_fistball_active(d,t) || d$status[[t+1L]][[p[["in"]]+1L]]!="bench") .ra_fail("Substitution requires an active outgoing and eligible incoming player")
  d$status[[t+1L]][[p$out+1L]]<-"bench";d$status[[t+1L]][[p[["in"]]+1L]]<-"active"
 } else if(k=="enter") {
  if(d$status[[t+1L]][[p$player+1L]]!="bench" || length(.ra_fistball_active(d,t))>=.ra_fistball_capacity(d,t)) .ra_fail("No eligible completion place is available")
  d$status[[t+1L]][[p$player+1L]]<-"active"
 } else if(k=="appoint_captain") {
  if(!d$status[[t+1L]][[p$player+1L]] %in% c("active","bench")) .ra_fail("Captain must be eligible")
  d$captains[[t+1L]]<-p$player
 } else if(k=="sanction") {
  idx<-p$player+1L;kind<-p$kind;st<-d$status[[t+1L]][[idx]]
  if(st=="excluded") .ra_fail("Excluded player cannot receive another match sanction")
  if(kind=="caution") {
   if(d$cautions[[t+1L]][[idx]]) .ra_fail("A player cannot receive a duplicate caution")
   d$cautions[[t+1L]][[idx]]<-TRUE
  } else if(kind=="suspension") {
   if(st!="active") .ra_fail("Point-counted suspension requires an on-court player")
   d$status[[t+1L]][[idx]]<-"suspended";d$suspensions[[t+1L]][[idx]]<-10L
  } else {
   d$lost_place[[t+1L]][[idx]]<-st %in% c("active","suspended");d$status[[t+1L]][[idx]]<-"excluded";d$suspensions[[t+1L]][[idx]]<-0L
  }
  if(kind!="caution" && !is.null(d$captains[[t+1L]]) && d$captains[[t+1L]]==p$player) d$captains[t+1L]<-list(NULL)
  if(.ra_fistball_capacity(d,t)<4 || length(.ra_fistball_active(d,t))<4) {d$phase<-"abandoned";d$timeout_ms<-0L} else if(d$phase=="rally") d$phase<-"stopped"
 } else if(k %in% c("delay","substitution_fault")) {
  if(k=="delay" && !d$delay_warned[[t+1L]]) d$delay_warned[[t+1L]]<-TRUE else d<-.ra_fistball_point(d,1L-t)
 } else if(k=="concede") {d$phase<-"finished";d$winner<-1L-t;d$reason<-"concession";d$timeout_ms<-0L} else if(k=="abandon") {d$phase<-"abandoned";d$timeout_ms<-0L} else if(k=="official_result") {d$phase<-"finished";d["winner"]<-list(p$winner);d$reason<-if(is.null(p$winner)) "no_contest" else "official_result"}
 d
}
.ra_fistball_legal<-function(state) {
 d<-state$data;s<-.ra_fistball_schemas(d)
 list(mode="event_types",actions=list(),action_types=lapply(names(s),function(k) list(type=k,params_schema=s[[k]],preconditions=list("Set, service, timeout and player eligibility must permit the event."),external_observations=list("Official rally faults, sanctions, referee stoppages and toss results."))),constraints=list(phase=d$phase,server=d$server,points=d$points,sets_won=as.list(.ra_fistball_wins(d)),active=lapply(0:1,function(t) as.list(.ra_fistball_active(d,t))),capacity=lapply(0:1,function(t) .ra_fistball_capacity(d,t)),captains=d$captains,suspensions=d$suspensions,timeout_ms=d$timeout_ms),completeness=FALSE,reason="official_2019_fistball_events")
}
.ra_fistball_outcome<-function(state,pack) .ra_pelota_outcome(state,pack)
