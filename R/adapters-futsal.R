# Futsal: official match events and their syntactic consequences.
.ra_futsal_active<-function(t) as.list(which(unlist(t$status)=="active")-1L)
.ra_futsal_available<-function(t) as.list(which(unlist(t$status)!="dismissed")-1L)
.ra_futsal_schemas<-function() {
 team<-list(type="integer",minimum=0,maximum=1);slot<-list(type="integer",minimum=0,maximum=13);b<-list(type="boolean");empty<-setNames(list(),character())
 s<-list(configure=list(knockout=b,extra_time=b),start_period=empty,end_period=empty,goal=list(team=team),card=list(team=team,player=slot,color=list(type="string",enum=list("yellow","red"))),substitute=list(team=team,out=slot),enter_player=list(team=team,player=slot),set_goalkeeper=list(team=team,player=slot),elapsed_play=list(milliseconds=list(type="integer",minimum=1,maximum=1200000)),accumulated_foul=list(team=team,penalty_area=b),award_penalty=list(team=team),resolve_kick=list(scored=b),timeout=list(team=team),end_timeout=empty,abandon_match=empty,start_shootout=list(first_team=team,excluded=list(type="array",minItems=2,maxItems=2,items=list(type="array",items=slot,maxItems=14))),exclude_kicker=list(team=team,player=slot),shootout_kick=list(team=team,player=slot,scored=b));s$substitute[["in"]]<-slot;s$set_lineup<-list(team=team,roster_size=list(type="integer",minimum=3,maximum=14),starters=list(type="array",minItems=3,maxItems=5,items=slot),goalkeeper=slot);s
}
.ra_futsal_validate<-function(d) {
 bad<-function(m) .ra_fail(m,code="INVALID_STATE");phase<-d$phase;shoot<-phase %in% c("shootout_setup","shootout") || d$shootout_started
 if(d$rules$extra_time && !d$rules$knockout) bad("Extra time requires a knockout match")
 if(d$extra_started && !(d$rules$knockout && d$rules$extra_time)) bad("Extra time is disabled")
 if(phase %in% .ra_fb_extra && !d$extra_started) bad("Extra-time phase mismatch")
 if(d$extra_started && phase %in% c("pre_match","first_half","halftime","second_half")) bad("Extra time has not begun")
 if(shoot && (!d$rules$knockout || d$goals[[1]]!=d$goals[[2]])) bad("Shoot-out requires a tied knockout match")
 if(d$shootout_started && !(phase %in% c("shootout","finished","abandoned"))) bad("Shoot-out phase mismatch")
 if(phase=="shootout" && !d$shootout_started) bad("Shoot-out has not started")
 if(!is.null(d$timeout_team) && !(phase %in% c("first_half","second_half"))) bad("Timeout requires a regulation half")
 if(!is.null(d$pending_kick) && !(phase %in% .ra_fb_play)) bad("Pending kick requires a playing period")
 if(!is.null(d$timeout_team) && !is.null(d$pending_kick)) bad("Resolve the pending kick before a timeout")
 for(t in d$teams) {
  n<-length(t$status);if(length(t$yellow)!=n || length(t$red)!=n) bad("Player vectors differ in length")
  for(i in seq_len(n)) if(t$red[[i]]!=(t$status[[i]]=="dismissed") || (t$yellow[[i]]==2 && !t$red[[i]])) bad("Card and eligibility mismatch")
  cap<-if(shoot) 5 else 5-length(t$reductions_ms)
  if(length(.ra_futsal_active(t))>cap || (phase %in% c(.ra_fb_play,.ra_fb_breaks) && length(.ra_futsal_active(t))<3)) bad("Active player count conflicts with team reductions")
  if(!is.null(t$goalkeeper) && !(t$goalkeeper %in% unlist(.ra_futsal_active(t)))) bad("Goalkeeper must be active")
  if(phase=="pre_match" && length(t$reductions_ms)>0) bad("Pre-match dismissals do not start playing-time reductions")
  if(anyDuplicated(unlist(t$eligible)) || any(!(unlist(t$eligible) %in% unlist(.ra_futsal_available(t))))) bad("Invalid shoot-out eligibility")
  if(anyDuplicated(unlist(t$taken)) || any(!(unlist(t$taken) %in% unlist(t$eligible)))) bad("Invalid kicker cycle")
  if(!shoot && (length(t$eligible)>0 || length(t$taken)>0)) bad("Kicker cycle exists before a shoot-out")
  if(d$shootout_started && !is.null(t$goalkeeper) && !(t$goalkeeper %in% unlist(t$eligible))) bad("Shoot-out goalkeeper must be eligible")
 }
 a<-unlist(d$shootout_attempts);g<-unlist(d$shootout_goals);first<-d$shootout_first+1L
 if(any(g>a) || !((a[first]-a[3L-first]) %in% c(0,1))) bad("Invalid shoot-out counters")
 if(!d$shootout_started && (any(a!=0) || any(g!=0))) bad("Shoot-out has not started")
 if(phase=="shootout" && !is.null(.ra_fb_penalty_winner(d))) bad("Shoot-out is already decided")
 if(phase=="finished" && d$rules$knockout && d$goals[[1]]==d$goals[[2]] && is.null(.ra_fb_penalty_winner(d))) bad("Knockout winner is unresolved")
 if(phase=="pre_match" && (any(unlist(d$goals)!=0) || d$extra_started || d$shootout_started || any(vapply(d$teams,function(t) t$fouls>0 || t$timeout_used,logical(1))))) bad("Pre-match counters are not empty")
 invisible(TRUE)
}
.ra_futsal_score<-function(d,team) {
 d$goals[[team]]<-d$goals[[team]]+1L;o<-3L-team
 if(length(.ra_futsal_active(d$teams[[team]]))>length(.ra_futsal_active(d$teams[[o]])) && length(d$teams[[o]]$reductions_ms)>0) d$teams[[o]]$reductions_ms<-d$teams[[o]]$reductions_ms[-which.min(unlist(d$teams[[o]]$reductions_ms))]
 d
}
.ra_futsal_apply<-function(state,action) {
 d<-state$data;k<-action$type;p<-action$params;phase<-d$phase;s<-.ra_futsal_schemas()
 if(!(k %in% names(s))) .ra_fail("Unknown futsal event")
 .ra_fields(p,s[[k]],"/params")
 if(phase %in% c("finished","abandoned")) .ra_fail("Match is not open")
 if(k=="configure") {if(phase!="pre_match") .ra_fail("Rules are fixed after kick-off");d$rules<-p
 } else if(k=="set_lineup") {
  ti<-p$team+1L;t<-d$teams[[ti]];n<-p$roster_size;starters<-unlist(p$starters)
  if(phase!="pre_match" || any(unlist(t$yellow)!=0) || any(unlist(t$red))) .ra_fail("Set the lineup before kickoff and disciplinary events")
  if(anyDuplicated(starters) || max(starters)>=n || !(p$goalkeeper %in% starters)) .ra_fail("Invalid starting lineup")
  t$status<-lapply(0:(n-1L),function(i) if(i %in% starters) "active" else "bench");t$yellow<-as.list(rep(0L,n));t$red<-as.list(rep(FALSE,n));t$goalkeeper<-p$goalkeeper;d$teams[[ti]]<-t
 } else if(k=="start_period") {
  mapping<-c(pre_match="first_half",halftime="second_half",extra_break="extra_first",extra_halftime="extra_second")
  if(!(phase %in% names(mapping)) || any(vapply(d$teams,function(t) length(.ra_futsal_active(t))<3 || is.null(t$goalkeeper),logical(1)))) .ra_fail("A new period requires three eligible players and a goalkeeper per team")
  d$phase<-unname(mapping[[phase]])
  if(phase %in% c("pre_match","halftime")) for(i in 1:2) {d$teams[[i]]$fouls<-0L;d$teams[[i]]$timeout_used<-FALSE}
 } else if(k=="end_period") {
  if(!(phase %in% .ra_fb_play) || !is.null(d$pending_kick) || !is.null(d$timeout_team)) .ra_fail("Resolve the pending kick or timeout before ending a period")
  if(phase=="first_half") d$phase<-"halftime" else if(phase=="extra_first") d$phase<-"extra_halftime" else if(d$goals[[1]]!=d$goals[[2]] || !d$rules$knockout) d$phase<-"finished" else if(phase=="second_half" && d$rules$extra_time) {d$phase<-"extra_break";d$extra_started<-TRUE} else {
   d$phase<-"shootout_setup"
   for(i in 1:2) {t<-d$teams[[i]];t$yellow<-as.list(rep(0L,length(t$status)));t$eligible<-.ra_futsal_available(t);t$taken<-list();t$reductions_ms<-list();d$teams[[i]]<-t}
  }
 } else if(k %in% c("goal","accumulated_foul","award_penalty","elapsed_play","timeout")) {
  if(!(phase %in% .ra_fb_play) || !is.null(d$timeout_team)) .ra_fail("Event requires an active playing period")
  if(k=="elapsed_play") {for(i in 1:2) d$teams[[i]]$reductions_ms<-lapply(Filter(function(x) x>p$milliseconds,d$teams[[i]]$reductions_ms),function(x) x-p$milliseconds)
  } else if(k=="accumulated_foul") {
   if(!is.null(d$pending_kick)) .ra_fail("Resolve the pending kick first")
   i<-p$team+1L;d$teams[[i]]$fouls<-d$teams[[i]]$fouls+1L
   if(p$penalty_area || d$teams[[i]]$fouls>=6) d$pending_kick<-list(team=1L-p$team,kind=if(p$penalty_area) "penalty" else "sixth_foul")
  } else if(k=="award_penalty") {
   if(!is.null(d$pending_kick)) .ra_fail("Resolve the pending kick first")
   i<-2L-p$team;d$teams[[i]]$fouls<-d$teams[[i]]$fouls+1L;d$pending_kick<-list(team=p$team,kind="penalty")
  } else if(k=="goal") {
   if(!is.null(d$pending_kick) || any(vapply(d$teams,function(t) is.null(t$goalkeeper),logical(1)))) .ra_fail("Resolve the pending kick and goalkeeper eligibility first")
   d<-.ra_futsal_score(d,p$team+1L)
  } else {
   i<-p$team+1L
   if(!(phase %in% c("first_half","second_half")) || !is.null(d$pending_kick) || d$teams[[i]]$timeout_used) .ra_fail("One timeout per regulation half is available")
   d$teams[[i]]$timeout_used<-TRUE;d$timeout_team<-p$team
  }
 } else if(k=="end_timeout") {
  if(is.null(d$timeout_team)) .ra_fail("No timeout is active")
  d["timeout_team"]<-list(NULL)
 } else if(k=="resolve_kick") {
  if(is.null(d$pending_kick) || any(vapply(d$teams,function(t) is.null(t$goalkeeper),logical(1)))) .ra_fail("A pending kick and eligible goalkeepers are required")
  if(p$scored) d<-.ra_futsal_score(d,d$pending_kick$team+1L)
  d["pending_kick"]<-list(NULL)
 } else if(k=="card") {
  ti<-p$team+1L;t<-d$teams[[ti]];i<-p$player+1L
  if(i>length(t$status) || t$red[[i]]) .ra_fail("Player is unavailable for another card")
  if(p$color=="yellow") t$yellow[[i]]<-t$yellow[[i]]+1L
  if(p$color=="red" || t$yellow[[i]]==2) {
   was_active<-t$status[[i]]=="active";t$red[[i]]<-TRUE;t$status[[i]]<-"dismissed"
   if(!is.null(t$goalkeeper) && t$goalkeeper==p$player) t["goalkeeper"]<-list(NULL)
   t$eligible<-Filter(function(x) x!=p$player,t$eligible);t$taken<-Filter(function(x) x!=p$player,t$taken)
   if(was_active && phase %in% c(.ra_fb_play,.ra_fb_breaks)) {
    t$reductions_ms[[length(t$reductions_ms)+1L]]<-120000L
    if(length(.ra_futsal_active(t))<3) {d$phase<-"abandoned";d["pending_kick"]<-list(NULL);d["timeout_team"]<-list(NULL)}
   }
  };d$teams[[ti]]<-t
 } else if(k %in% c("substitute","enter_player","set_goalkeeper")) {
  ti<-p$team+1L;t<-d$teams[[ti]];player<-if(k=="substitute") p[["in"]] else p$player;i<-player+1L;shoot<-phase %in% c("shootout_setup","shootout")
  if(i>length(t$status) || t$status[[i]]=="dismissed") .ra_fail("Player is ineligible")
  if(k=="set_goalkeeper") {
   if(shoot && player %in% unlist(t$eligible) && t$status[[i]]=="bench") {
    if(!is.null(t$goalkeeper)) t$status[[t$goalkeeper+1L]]<-"bench" else if(length(.ra_futsal_active(t))>=5) .ra_fail("Choose an active eligible goalkeeper")
    t$status[[i]]<-"active"
   }
   if(!(player %in% unlist(.ra_futsal_active(t))) || (d$shootout_started && !(player %in% unlist(t$eligible)))) .ra_fail("Goalkeeper must be active and eligible")
   t$goalkeeper<-player
  } else {
   if(shoot) .ra_fail("Normal substitutions end before the shoot-out")
   if(t$status[[i]]!="bench") .ra_fail("An available substitute must enter")
   if(k=="substitute") {
    j<-p$out+1L;if(!(p$out %in% unlist(.ra_futsal_active(t)))) .ra_fail("Only an active player can leave")
    t$status[[j]]<-"bench";if(!is.null(t$goalkeeper) && t$goalkeeper==p$out) t$goalkeeper<-player
   } else if(length(.ra_futsal_active(t))>=5-length(t$reductions_ms)) .ra_fail("No replacement slot is available")
   t$status[[i]]<-"active"
  };d$teams[[ti]]<-t
 } else if(k=="start_shootout") {
  if(phase!="shootout_setup") .ra_fail("Shoot-out is not ready")
  target<-min(vapply(d$teams,function(t) length(t$eligible),integer(1)))
  for(i in 1:2) {
   t<-d$teams[[i]];excluded<-unlist(p$excluded[[i]])
   if(length(excluded)!=length(t$eligible)-target) .ra_fail("Only the larger eligible squad is reduced to equal numbers")
   if(anyDuplicated(excluded) || any(!(excluded %in% unlist(t$eligible))) || (!is.null(t$goalkeeper) && t$goalkeeper %in% excluded)) .ra_fail("Invalid shoot-out exclusions")
   t$eligible<-Filter(function(x) !(x %in% excluded),t$eligible);d$teams[[i]]<-t
  }
  if(length(d$teams[[1]]$eligible)==0 || length(d$teams[[1]]$eligible)!=length(d$teams[[2]]$eligible) || any(vapply(d$teams,function(t) is.null(t$goalkeeper),logical(1)))) .ra_fail("Shoot-out requires equal eligible squads and goalkeepers")
  d$shootout_first<-p$first_team;d$shootout_started<-TRUE;d$phase<-"shootout"
 } else if(k=="exclude_kicker") {
  if(phase!="shootout") .ra_fail("Shoot-out is not active")
  ti<-p$team+1L;t<-d$teams[[ti]];i<-p$player
  if(length(t$eligible)<=length(d$teams[[3L-ti]]$eligible) || !(i %in% unlist(t$eligible)) || (!is.null(t$goalkeeper) && i==t$goalkeeper)) .ra_fail("Exclusion must equalize eligible squads")
  t$eligible<-Filter(function(x) x!=i,t$eligible);t$taken<-Filter(function(x) x!=i,t$taken);d$teams[[ti]]<-t
 } else if(k=="shootout_kick") {
  if(phase!="shootout") .ra_fail("Shoot-out is not active")
  a<-d$shootout_attempts;ti<-p$team+1L;t<-d$teams[[ti]];i<-p$player;expected<-if(a[[1]]==a[[2]]) d$shootout_first else 1L-d$shootout_first
  if(p$team!=expected || length(t$eligible)!=length(d$teams[[3L-ti]]$eligible) || any(vapply(d$teams,function(x) is.null(x$goalkeeper),logical(1)))) .ra_fail("Wrong turn or unresolved shoot-out eligibility")
  if(setequal(unlist(t$taken),unlist(t$eligible))) t$taken<-list()
  if(!(i %in% unlist(t$eligible)) || i %in% unlist(t$taken)) .ra_fail("Kicker is ineligible or has already kicked this cycle")
  t$taken[[length(t$taken)+1L]]<-i;d$teams[[ti]]<-t;d$shootout_attempts[[ti]]<-a[[ti]]+1L;d$shootout_goals[[ti]]<-d$shootout_goals[[ti]]+as.integer(p$scored)
  if(!is.null(.ra_fb_penalty_winner(d))) d$phase<-"finished"
 } else if(k=="abandon_match") {d$phase<-"abandoned";d["pending_kick"]<-list(NULL);d["timeout_team"]<-list(NULL)}
 d
}
.ra_futsal_legal<-function(state) {
 d<-state$data;phase<-d$phase;names<-character()
 if(!(phase %in% c("finished","abandoned"))) {names<-c("card","set_goalkeeper","abandon_match");if(!(phase %in% c("shootout_setup","shootout"))) names<-c(names,"substitute","enter_player")}
 if(phase=="pre_match") names<-c(names,"configure","set_lineup","start_period")
 if(phase %in% .ra_fb_breaks) names<-c(names,"start_period")
 if(phase %in% .ra_fb_play) {
  if(!is.null(d$timeout_team)) names<-c(names,"end_timeout") else if(!is.null(d$pending_kick)) names<-c(names,"resolve_kick","elapsed_play") else {names<-c(names,"goal","accumulated_foul","award_penalty","elapsed_play","end_period");if(phase %in% c("first_half","second_half")) names<-c(names,"timeout")}
 }
 if(phase=="shootout_setup") names<-c(names,"start_shootout")
 if(phase=="shootout") names<-c(names,"exclude_kicker","shootout_kick")
 s<-.ra_futsal_schemas()
 list(mode="event_types",actions=list(),action_types=lapply(names,function(k) list(type=k,params_schema=s[[k]],preconditions=list("Match phase, player eligibility and pending decisions must permit the event."),external_observations=list("Official period timing, playing-time elapsed, foul, timeout entitlement or goal decision."))),constraints=list(phase=phase,teams=lapply(d$teams,function(t) {nexts<-Filter(function(i) !(i %in% unlist(t$taken)),t$eligible);list(active=.ra_futsal_active(t),replacement_slots=max(0,5-length(t$reductions_ms)-length(.ra_futsal_active(t))),fouls=t$fouls,next_kickers=if(length(nexts)>0) nexts else t$eligible)})),completeness=FALSE,reason="official_futsal_events")
}
.ra_futsal_outcome<-function(state,pack) .ra_fb_outcome(state,pack)
