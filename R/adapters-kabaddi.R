.ra_kabaddi_int<-function(a,b) list(type="integer",minimum=a,maximum=b)
.ra_kabaddi_active<-function(d,t) Filter(function(i) d$status[[t+1L]][[i+1L]]=="active",d$slots[[t+1L]])
.ra_kabaddi_presence<-function(d,t) sum(vapply(d$slots[[t+1L]],function(i) d$status[[t+1L]][[i+1L]]!="out",logical(1)))
.ra_kabaddi_revive<-function(d,t,n) {
 for(j in seq_len(min(n,length(d$out_queue[[t+1L]])))) {
  i<-d$out_queue[[t+1L]][[1]];d$out_queue[[t+1L]]<-d$out_queue[[t+1L]][-1]
  d$status[[t+1L]][[i+1L]]<-if(d$suspension_ms[[t+1L]][[i+1L]]>0) "suspended" else "active"
 }
 d
}
.ra_kabaddi_put_out<-function(d,t,i) {
 if(d$status[[t+1L]][[i+1L]]!="active") .ra_fail("Only an active player can be declared out")
 d$status[[t+1L]][[i+1L]]<-"out";d$out_queue[[t+1L]][[length(d$out_queue[[t+1L]])+1L]]<-i;d
}
.ra_kabaddi_tick<-function(d,ms) {
 d$clock_ms<-max(0L,d$clock_ms-ms)
 for(t in 0:1) for(i in 0:11) if(d$status[[t+1L]][[i+1L]]=="suspended") {
  d$suspension_ms[[t+1L]][[i+1L]]<-max(0L,d$suspension_ms[[t+1L]][[i+1L]]-ms)
  if(d$suspension_ms[[t+1L]][[i+1L]]==0) d$status[[t+1L]][[i+1L]]<-if(i %in% unlist(d$slots[[t+1L]])) "active" else "bench"
 }
 d
}
.ra_kabaddi_all_out<-function(d,t) {
 d$score[[2L-t]]<-d$score[[2L-t]]+2L;d<-.ra_kabaddi_revive(d,t,length(d$out_queue[[t+1L]]));d$phase<-"reentry";d$reentry_team<-t;d$reentry_ms<-0L;d
}
.ra_kabaddi_check_all_out<-function(d) {
 for(t in 0:1) if(length(.ra_kabaddi_active(d,t))==0 && !any(vapply(d$slots[[t+1L]],function(i) d$status[[t+1L]][[i+1L]]=="suspended",logical(1))) && length(d$out_queue[[t+1L]])>0) return(.ra_kabaddi_all_out(d,t))
 if(any(vapply(0:1,function(t) all(vapply(d$slots[[t+1L]],function(i) d$status[[t+1L]][[i+1L]]=="excluded",logical(1))),logical(1)))) d$phase<-"abandoned"
 d
}
.ra_kabaddi_validate<-function(d) {
 bad<-function(m) .ra_fail(m,code="INVALID_STATE")
 for(t in 0:1) {
  if(length(unique(unlist(d$slots[[t+1L]])))!=7) bad("Each team must retain seven distinct playing places")
  q<-unlist(d$out_queue[[t+1L]]);out<-which(unlist(d$status[[t+1L]])=="out")-1L
  if(anyDuplicated(q) || !setequal(q,out)) bad("Out queue must contain every out player exactly once")
  for(i in 0:11) {
   s<-d$status[[t+1L]][[i+1L]];sl<-i %in% unlist(d$slots[[t+1L]]);ms<-d$suspension_ms[[t+1L]][[i+1L]]
   if((s %in% c("active","out") && !sl) || (s %in% c("bench","retired") && sl)) bad("Playing places and player status disagree")
   if((s=="suspended" && ms==0) || (ms>0 && !s %in% c("suspended","out"))) bad("Suspension clock must belong to a suspended or pending-revival player")
   if(s=="out" && !ms %in% c(0,120000)) bad("Out-player suspension starts only at revival")
   if(d$yellow[[t+1L]][[i+1L]]==2 && s!="excluded") bad("Second yellow excludes the player for the rest of the match")
  }
 }
 if(d$phase=="pre" && (!is.null(d$first_raider) || d$half!=1 || d$clock_ms!=1200000 || any(unlist(d$score)!=0) || any(vapply(d$out_queue,length,integer(1))>0))) bad("Pre-match state precedes toss and play")
 if(!d$phase %in% c("pre","abandoned","finished") && (is.null(d$first_raider) || is.null(d$next_team))) bad("Started match requires raid entitlement")
 if(d$phase=="interval" && (d$half!=1 || d$clock_ms!=0)) bad("Halftime follows completed first-half play")
 if((d$phase %in% c("interval","timeout"))!=(d$break_ms>0)) bad("Break duration and phase disagree")
 if((d$phase=="reentry")!=(!is.null(d$reentry_team))) bad("All-out reentry phase needs the returning team")
 if(d$phase=="finished") {
  if(identical(d$reason,"regulation")) {
   w<-if(d$score[[1]]==d$score[[2]]) NULL else as.integer(d$score[[2]]>d$score[[1]])
   if(d$half!=2 || d$clock_ms!=0 || !identical(d$winner,w)) bad("Regulation result must match completed time and score")
  } else if(identical(d$reason,"concession")) {if(is.null(d$winner)) bad("Concession requires a winner")} else if(is.null(d$reason) || !d$reason %in% c("official_result","no_contest")) bad("Finished match requires a result reason")
 } else if(!is.null(d$winner) || !is.null(d$reason)) bad("Ongoing match cannot declare a result")
}
.ra_kabaddi_schemas<-function(d) {
 ph<-d$phase;s<-list();empty<-setNames(list(),character());T<-.ra_kabaddi_int(0,1);P<-.ra_kabaddi_int(0,11);B<-list(type="boolean")
 if(ph=="finished") return(s)
 if(ph=="abandoned") return(list(official_result=list(winner=c(T,list(nullable=TRUE)))))
 if(ph=="pre") s$start_match<-list(first_team=T)
 if(ph=="ready") {
  if(d$clock_ms>0) {
   f<-list(player=P,reason=list(type="enum",values=list("touch","self_out","foul")))
   s$raid<-list(team=T,raider=P,duration_ms=.ra_kabaddi_int(1,30000),result=list(type="enum",values=list("returned","caught","self_out","cant","timeout")),baulk_valid=B,bonus=B,defenders_out=list(type="array",minItems=0,maxItems=7,items=list(type="object",properties=f,required=as.list(names(f)))),technical_points=list(type="array",minItems=2,maxItems=2,items=.ra_kabaddi_int(0,7)))
   s<-c(s,list(elapse=list(milliseconds=.ra_kabaddi_int(1,1200000)),timeout=list(team=T),official_stop=empty))
  } else s$close_half<-empty
  s$declare_remaining_out<-list(team=T)
 }
 if(ph %in% c("ready","stopped")) {
  pair<-list(outgoing=P,incoming=P)
  s<-c(s,list(substitute=list(team=T,pairs=list(type="array",minItems=1,maxItems=7,items=list(type="object",properties=pair,required=as.list(names(pair)))),exception_authorized=B),sanction=list(team=T,player=P,kind=list(type="enum",values=list("green","yellow","red"))),technical_point=list(team=T)))
 }
 if(ph=="stopped") s$resume<-empty
 if(ph %in% c("interval","timeout")) s$elapse_break<-list(milliseconds=.ra_kabaddi_int(1,300000))
 if(ph=="reentry") s<-c(s,list(confirm_reentry=empty,elapse_reentry=list(milliseconds=.ra_kabaddi_int(1,60000))))
 c(s,list(abandon=empty,concede=list(team=T)))
}
.ra_kabaddi_apply<-function(state,action) {
 d<-state$data;k<-action$type;p<-action$params;ss<-.ra_kabaddi_schemas(d)
 if(!k %in% names(ss)) .ra_fail("Action is not permitted in this phase")
 .ra_fields(p,ss[[k]],"/params");t<-p$team
 if(k=="start_match") {
  if(any(vapply(0:1,function(j) length(.ra_kabaddi_active(d,j))!=7,logical(1)))) .ra_fail("Both teams require seven active starters")
  d$phase<-"ready";d$first_raider<-p$first_team;d$next_team<-p$first_team
 } else if(k=="raid") {
  i<-p$raider;other<-1L-t;outs<-p$defenders_out;ids<-lapply(outs,function(r) r$player)
  if(t!=d$next_team || !i %in% unlist(.ra_kabaddi_active(d,t)) || length(.ra_kabaddi_active(d,other))==0) .ra_fail("Raid requires the entitled active raider and an active opposing team")
  d<-.ra_kabaddi_tick(d,p$duration_ms)
  if(anyDuplicated(unlist(ids)) || any(vapply(ids,function(j) !j %in% unlist(.ra_kabaddi_active(d,other)),logical(1)))) .ra_fail("Out defenders must be distinct active opponents")
  if(p$bonus && .ra_kabaddi_presence(d,other)<6) .ra_fail("Bonus requires at least six defenders including disciplinary absences")
  if(p$bonus && !p$baulk_valid) .ra_fail("A qualifying bonus crossing also crosses the baulk line")
  raider_out<-p$result!="returned"
  if(raider_out && any(vapply(outs,function(r) r$reason=="touch",logical(1)))) .ra_fail("Touched defenders are out only when the raider returns safely")
  if(!p$baulk_valid && !any(vapply(outs,function(r) r$reason=="touch",logical(1)))) raider_out<-TRUE
  if(p$result=="timeout" && p$duration_ms!=30000) .ra_fail("Raid time expiry requires thirty seconds")
  productive<-length(ids)>0 || p$bonus || any(unlist(p$technical_points)>0) || raider_out
  if(!productive && d$empty_raids[[t+1L]]==2) {raider_out<-TRUE;productive<-TRUE}
  count_at_catch<-.ra_kabaddi_presence(d,other)-length(ids)
  for(j in ids) d<-.ra_kabaddi_put_out(d,other,j)
  if(raider_out) d<-.ra_kabaddi_put_out(d,t,i)
  d$score[[t+1L]]<-d$score[[t+1L]]+length(ids)+as.integer(p$bonus)+p$technical_points[[t+1L]]
  d$score[[other+1L]]<-d$score[[other+1L]]+if(raider_out) {if(count_at_catch<=3) 2L else 1L} else 0L
  d$score[[other+1L]]<-d$score[[other+1L]]+p$technical_points[[other+1L]]
  d<-.ra_kabaddi_revive(d,t,length(ids));d<-.ra_kabaddi_revive(d,other,as.integer(raider_out))
  d$empty_raids[[t+1L]]<-if(productive) 0L else d$empty_raids[[t+1L]]+1L
  d$next_team<-other;d<-.ra_kabaddi_check_all_out(d)
 } else if(k=="elapse") {
  if(p$milliseconds>d$clock_ms) .ra_fail("Between-raid clock cannot cross the half boundary")
  d<-.ra_kabaddi_tick(d,p$milliseconds)
 } else if(k=="declare_remaining_out") {
  ids<-.ra_kabaddi_active(d,t)
  if(length(ids)<1 || length(ids)>2 || any(vapply(d$slots[[t+1L]],function(i) d$status[[t+1L]][[i+1L]]=="suspended",logical(1)))) .ra_fail("Voluntary all-out requires one or two remaining active players and no suspended playing place")
  for(i in ids) d<-.ra_kabaddi_put_out(d,t,i)
  d$score[[2L-t]]<-d$score[[2L-t]]+length(ids);d<-.ra_kabaddi_revive(d,1L-t,length(ids));d<-.ra_kabaddi_all_out(d,t)
 } else if(k=="substitute") {
  pairs<-p$pairs;old<-lapply(pairs,function(r) r$outgoing);new<-lapply(pairs,function(r) r$incoming)
  if(anyDuplicated(unlist(old)) || anyDuplicated(unlist(new)) || any(vapply(old,function(i) !i %in% unlist(.ra_kabaddi_active(d,t)),logical(1))) || any(vapply(new,function(i) d$status[[t+1L]][[i+1L]]!="bench",logical(1)))) .ra_fail("Substitution batch requires distinct active outgoing and eligible reserve players")
  if(d$substitutions[[t+1L]]>=5 && !p$exception_authorized) .ra_fail("Five substitution occasions have been exhausted")
  if(p$exception_authorized && d$substitutions[[t+1L]]<5) .ra_fail("Exceptional replacement is used only after the ordinary allowance is exhausted")
  for(j in seq_along(old)) {
   a<-old[[j]];b<-new[[j]];slot<-which(unlist(d$slots[[t+1L]])==a)
   d$slots[[t+1L]][[slot]]<-b;d$status[[t+1L]][[a+1L]]<-if(p$exception_authorized) "retired" else "bench";d$status[[t+1L]][[b+1L]]<-"active"
  }
  if(!p$exception_authorized) d$substitutions[[t+1L]]<-d$substitutions[[t+1L]]+1L
 } else if(k=="sanction") {
  i<-p$player;kind<-p$kind;status<-d$status[[t+1L]][[i+1L]]
  if(status %in% c("excluded","retired")) .ra_fail("Player is no longer eligible for a new match sanction")
  if(kind=="green" && (d$green[[t+1L]][[i+1L]] || d$yellow[[t+1L]][[i+1L]]>0)) kind<-"yellow"
  if(kind=="green") d$green[[t+1L]][[i+1L]]<-TRUE else {
   if(kind=="yellow") {d$yellow[[t+1L]][[i+1L]]<-d$yellow[[t+1L]][[i+1L]]+1L;d$score[[2L-t]]<-d$score[[2L-t]]+1L}
   if(kind=="red" || d$yellow[[t+1L]][[i+1L]]==2) {
    d$status[[t+1L]][[i+1L]]<-"excluded";d$suspension_ms[[t+1L]][[i+1L]]<-0L
    d$out_queue[[t+1L]]<-Filter(function(j) j!=i,d$out_queue[[t+1L]])
   } else {d$suspension_ms[[t+1L]][[i+1L]]<-120000L;if(status!="out") d$status[[t+1L]][[i+1L]]<-"suspended"}
   d<-.ra_kabaddi_check_all_out(d)
  }
 } else if(k=="technical_point") d$score[[t+1L]]<-d$score[[t+1L]]+1L else if(k=="official_stop") d$phase<-"stopped" else if(k=="resume") d$phase<-"ready" else if(k=="timeout") {
  if(d$timeouts[[t+1L]]>=2) .ra_fail("Two team timeouts per half are available")
  d$timeouts[[t+1L]]<-d$timeouts[[t+1L]]+1L;d$phase<-"timeout";d$break_ms<-30000L
 } else if(k=="elapse_break") {
  if(p$milliseconds>d$break_ms) .ra_fail("Elapsed time exceeds the remaining break")
  d$break_ms<-d$break_ms-p$milliseconds
  if(d$break_ms==0) {
   if(d$phase=="interval") {d$half<-2L;d$clock_ms<-1200000L;d$next_team<-1L-d$first_raider;d$empty_raids<-list(0L,0L);d$timeouts<-list(0L,0L)}
   d$phase<-"ready"
  }
 } else if(k=="close_half") {
  if(d$half==1) {d$phase<-"interval";d$break_ms<-300000L} else {d$phase<-"finished";d["winner"]<-list(if(d$score[[1]]==d$score[[2]]) NULL else as.integer(d$score[[2]]>d$score[[1]]));d$reason<-"regulation"}
 } else if(k=="confirm_reentry") {d$phase<-"ready";d["reentry_team"]<-list(NULL);d$reentry_ms<-0L} else if(k=="elapse_reentry") {
  ms<-p$milliseconds
  if(d$reentry_ms+ms>60000) .ra_fail("All-out return clock cannot exceed one minute")
  before<-d$reentry_ms;d$reentry_ms<-d$reentry_ms+ms;other<-1L-d$reentry_team
  d$score[[other+1L]]<-d$score[[other+1L]]+d$reentry_ms%/%15000-before%/%15000;d<-.ra_kabaddi_tick(d,ms)
  if(d$reentry_ms==60000) {d$phase<-"finished";d$winner<-other;d$reason<-"concession";d["reentry_team"]<-list(NULL)}
 } else if(k=="abandon") {d$phase<-"abandoned";d$break_ms<-0L;d["reentry_team"]<-list(NULL)} else if(k=="concede") {d$phase<-"finished";d$winner<-1L-t;d$reason<-"concession";d$break_ms<-0L;d["reentry_team"]<-list(NULL)} else if(k=="official_result") {d$phase<-"finished";d["winner"]<-list(p$winner);d$reason<-if(is.null(p$winner)) "no_contest" else "official_result"}
 d
}
.ra_kabaddi_legal<-function(state) {
 d<-state$data;s<-.ra_kabaddi_schemas(d)
 list(mode="event_types",actions=list(),action_types=lapply(names(s),function(k) list(type=k,params_schema=s[[k]],preconditions=list("Raid entitlement, eligibility, revival order, playing time and remaining allowances must permit the event."),external_observations=list("Completed raid result, ordered defender outs, bonus crossing, technical points and disciplinary decisions."))),constraints=list(phase=d$phase,half=d$half,clock_ms=d$clock_ms,score=d$score,next_team=d$next_team,active=lapply(0:1,function(t) .ra_kabaddi_active(d,t)),out_queue=d$out_queue,empty_raids=d$empty_raids,suspension_ms=d$suspension_ms,substitutions=d$substitutions,timeouts=d$timeouts),completeness=FALSE,reason="official_completed_kabaddi_raids")
}
.ra_kabaddi_outcome<-function(state,pack) .ra_handball_outcome(state,pack)
