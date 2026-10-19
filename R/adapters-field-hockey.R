.ra_field_hockey_team<-list(type="integer",minimum=0,maximum=1)
.ra_field_hockey_player<-list(type="integer",minimum=0,maximum=17)
.ra_field_hockey_active<-function(d,t) as.list(which(unlist(d$players[[t+1L]])=="active")-1L)
.ra_field_hockey_capacity<-function(d,t) 11-sum(unlist(d$players[[t+1L]]) %in% c("suspended","dq"))
.ra_field_hockey_corner<-function(d) !is.null(d$pending) && d$pending$kind=="corner"
.ra_field_hockey_stroke<-function(d) !is.null(d$pending) && d$pending$kind=="stroke"
.ra_field_hockey_validate<-function(d) {
 bad<-function(m) .ra_fail(m,code="INVALID_STATE")
 for(t in 0:1) {
  if(length(.ra_field_hockey_active(d,t))>.ra_field_hockey_capacity(d,t)) bad("Active players exceed available team places")
  if(!is.null(d$goalkeeper[[t+1L]]) && d$players[[t+1L]][[d$goalkeeper[[t+1L]]+1L]]!="active") bad("Goalkeeper designation requires an active player")
  for(i in 1:18) if((d$players[[t+1L]][[i]]=="suspended")!=(d$penalty_ms[[t+1L]][[i]]>0)) bad("Timed penalty and player eligibility disagree")
 }
 if(d$phase=="live" && (d$remaining_ms==0 || .ra_field_hockey_stroke(d) || any(vapply(0:1,function(t) length(.ra_field_hockey_active(d,t))==0,logical(1))))) bad("Live play requires time and eligible teams without an outstanding stroke")
 if(!is.null(d$pending)) {
  p<-d$pending
  if(!d$phase %in% c("live","stopped")) bad("Outstanding penalty requires an active quarter")
  if(p$kind=="stroke" && (is.null(p$thrower) || d$players[[p$team+1L]][[p$thrower+1L]]!="active")) bad("Penalty stroke requires an eligible taker")
  if(p$kind=="corner" && !is.null(p$thrower)) bad("Corner does not nominate a stroke taker")
 }
 if(d$phase=="interval" && (d$quarter==4 || d$remaining_ms!=0 || !is.null(d$pending))) bad("Quarter interval must follow a completed nonfinal quarter")
 if(d$phase=="pre" && (d$quarter!=1 || d$remaining_ms!=900000 || any(unlist(d$score)!=0) || any(unlist(d$players) %in% c("suspended","dq")))) bad("Pre-match state precedes scoring, time and match sanctions")
 if(d$phase=="finished") {
  if(!is.null(d$pending) || is.null(d$reason)) bad("Finished match cannot retain an outstanding penalty")
  if(d$reason=="regulation") {
   w<-if(d$score[[1]]==d$score[[2]]) NULL else as.integer(d$score[[2]]>d$score[[1]])
   if(d$quarter!=4 || d$remaining_ms!=0 || !identical(d$winner,w)) bad("Regulation result must match the final score")
  }
  if(((d$reason %in% c("concession","official_result"))!=!is.null(d$winner)) && d$reason!="regulation") bad("Official result and winner disagree")
 } else if(!is.null(d$reason) || !is.null(d$winner)) bad("Only a finished match declares a result")
}
.ra_field_hockey_schemas<-function(d) {
 phase<-d$phase;s<-list();empty<-setNames(list(),character());team<-.ra_field_hockey_team;player<-.ra_field_hockey_player;np<-c(player,list(nullable=TRUE))
 if(phase=="finished") return(s)
 if(phase=="abandoned") return(list(official_result=list(winner=c(team,list(nullable=TRUE)))))
 if(phase=="pre") s$start_match<-empty
 if(phase=="interval") s$next_quarter<-empty
 s<-c(s,list(substitute=list(team=team,outgoing=player,incoming=player),enter=list(team=team,player=player),leave=list(team=team,player=player)))
 if(phase!="live") s$designate_goalkeeper<-list(team=team,player=np)
 if(phase=="live") s<-c(s,list(elapse=list(milliseconds=list(type="integer",minimum=1,maximum=900000)),stop_clock=empty,goal=list(team=team,player=player)))
 if(phase=="stopped") {
  if(d$remaining_ms>0 && !.ra_field_hockey_stroke(d)) s$resume_play<-empty
  if(d$remaining_ms==0 && is.null(d$pending)) s$end_quarter<-empty
 }
 if(phase %in% c("live","stopped")) {
  s$card<-list(team=team,player=player,colour=list(type="enum",values=list("green","yellow","red")),minutes=list(type="integer",minimum=0,maximum=20),remove_player=np)
  if(is.null(d$pending)) s$award_corner<-list(team=team)
  if(!.ra_field_hockey_stroke(d)) s$award_stroke<-list(team=team,thrower=player)
  if(.ra_field_hockey_corner(d)) {
   s$corner_result<-list(result=list(type="enum",values=list("goal","ended","repeat")),scorer=np)
   s$authorized_keeper_replacement<-list(outgoing=np,incoming=player)
  }
  if(.ra_field_hockey_stroke(d)) s<-c(s,list(stroke_result=list(scored=list(type="boolean")),replace_taker=list(player=player)))
 }
 c(s,list(abandon=empty,concede=list(team=team)))
}
.ra_field_hockey_remove<-function(d,t,i) {
 d$players[[t+1L]][[i+1L]]<-"bench"
 if(!is.null(d$goalkeeper[[t+1L]]) && d$goalkeeper[[t+1L]]==i) d$goalkeeper[t+1L]<-list(NULL)
 d
}
.ra_field_hockey_apply<-function(state,action) {
 d<-state$data;k<-action$type;p<-action$params;s<-.ra_field_hockey_schemas(d)
 if(!k %in% names(s)) .ra_fail("Action is not permitted in this phase")
 .ra_fields(p,s[[k]],"/params");t<-p$team
 if(k=="start_match") {
  if(any(vapply(0:1,function(j) length(.ra_field_hockey_active(d,j))==0,logical(1)))) .ra_fail("Both teams require active players")
  d$phase<-"stopped"
 } else if(k=="next_quarter") {d$quarter<-d$quarter+1L;d$remaining_ms<-900000;d$phase<-"stopped"} else if(k=="resume_play") {
  if(any(vapply(0:1,function(j) length(.ra_field_hockey_active(d,j))==0,logical(1)))) .ra_fail("Both teams require active players")
  d$phase<-"live"
 } else if(k=="stop_clock") d$phase<-"stopped" else if(k=="elapse") {
  ms<-p$milliseconds
  if(ms>d$remaining_ms) .ra_fail("Playing time cannot pass the quarter boundary")
  d$remaining_ms<-d$remaining_ms-ms
  for(j in 1:2) for(i in 1:18) {
   n<-d$penalty_ms[[j]][[i]]
   if(n>0) {d$penalty_ms[[j]][[i]]<-max(0,n-ms);if(n<=ms) d$players[[j]][[i]]<-"bench"}
  }
  if(d$remaining_ms==0) d$phase<-"stopped"
 } else if(k=="goal") {
  if(!is.null(d$pending) || d$players[[t+1L]][[p$player+1L]]!="active") .ra_fail("Ordinary goal needs an active scorer and no unresolved penalty")
  d$score[[t+1L]]<-d$score[[t+1L]]+1L
 } else if(k %in% c("substitute","enter","leave")) {
  old<-if(k=="substitute") p$outgoing else p$player;new<-if(k=="substitute") p$incoming else p$player;was_keeper<-!is.null(d$goalkeeper[[t+1L]]) && old==d$goalkeeper[[t+1L]]
  if(.ra_field_hockey_corner(d)) .ra_fail("Ordinary substitutions wait until the corner is completed")
  if(k!="enter") {
   if(d$players[[t+1L]][[old+1L]]!="active") .ra_fail("Outgoing player must be active")
   if(.ra_field_hockey_stroke(d) && d$pending$team==t && d$pending$thrower==old) .ra_fail("Replace the stroke taker first")
   d<-.ra_field_hockey_remove(d,t,old)
  }
  if(k!="leave") {
   if(d$players[[t+1L]][[new+1L]]!="bench" || length(.ra_field_hockey_active(d,t))>=.ra_field_hockey_capacity(d,t) || (k=="substitute" && new==old)) .ra_fail("Incoming player needs eligibility and a free team place")
   d$players[[t+1L]][[new+1L]]<-"active"
   if(k=="substitute" && was_keeper) d$goalkeeper[t+1L]<-list(new)
  }
 } else if(k=="authorized_keeper_replacement") {
  t<-1L-d$pending$team;old<-p$outgoing;new<-p$incoming
  if(!is.null(d$goalkeeper[[t+1L]]) && (is.null(old) || old!=d$goalkeeper[[t+1L]])) .ra_fail("Replace the designated defending goalkeeper")
  if(!is.null(old)) {
   if(d$players[[t+1L]][[old+1L]]!="active") .ra_fail("Outgoing player must be active")
   d<-.ra_field_hockey_remove(d,t,old)
  }
  if(d$players[[t+1L]][[new+1L]]!="bench" || length(.ra_field_hockey_active(d,t))>=.ra_field_hockey_capacity(d,t) || (!is.null(old) && new==old)) .ra_fail("Replacement goalkeeper needs eligibility and a team place")
  d$players[[t+1L]][[new+1L]]<-"active";d$goalkeeper[t+1L]<-list(new)
 } else if(k=="designate_goalkeeper") {
  if(.ra_field_hockey_corner(d)) .ra_fail("Resolve the corner before changing goalkeeper designation")
  if(!is.null(p$player) && d$players[[t+1L]][[p$player+1L]]!="active") .ra_fail("Goalkeeper must be active")
  d$goalkeeper[t+1L]<-list(p$player)
 } else if(k=="card") {
  i<-p$player;colour<-p$colour;old<-d$players[[t+1L]][[i+1L]]
  if(old=="dq" || (old=="suspended" && colour!="red")) .ra_fail("Only red can upgrade a current suspension")
  if(.ra_field_hockey_stroke(d) && d$pending$team==t && d$pending$thrower==i) .ra_fail("Replace the stroke taker first")
  if(colour=="green") {
   if(p$minutes!=2 || d$green[[t+1L]][[i+1L]]) .ra_fail("A single green card gives two minutes; further discipline requires yellow or red")
   d$green[[t+1L]][[i+1L]]<-TRUE
  } else if(colour=="yellow") {
   if(p$minutes<5 || p$minutes<=d$last_yellow_minutes[[t+1L]][[i+1L]]) .ra_fail("Yellow is at least five minutes; a repeat suspension must be longer")
   d$last_yellow_minutes[[t+1L]][[i+1L]]<-p$minutes
  } else if(p$minutes!=0) .ra_fail("Red is a permanent exclusion, not a timed penalty")
  d$players[[t+1L]][[i+1L]]<-if(colour=="red") "dq" else "suspended";d$penalty_ms[[t+1L]][[i+1L]]<-if(colour=="red") 0L else p$minutes*60000
  if(!is.null(d$goalkeeper[[t+1L]]) && d$goalkeeper[[t+1L]]==i) d$goalkeeper[t+1L]<-list(NULL)
  if(length(.ra_field_hockey_active(d,t))>.ra_field_hockey_capacity(d,t)) {
   if(is.null(p$remove_player) || d$players[[t+1L]][[p$remove_player+1L]]!="active") .ra_fail("Nominate an active teammate to implement a bench-player reduction")
   d<-.ra_field_hockey_remove(d,t,p$remove_player)
  } else if(!is.null(p$remove_player)) .ra_fail("No further player needs to leave")
  d$phase<-"stopped"
 } else if(k=="award_corner") {d$pending<-list(kind="corner",team=t,thrower=NULL);d$phase<-"stopped"} else if(k=="award_stroke") {
  if(d$players[[t+1L]][[p$thrower+1L]]!="active") .ra_fail("Stroke taker must be active")
  d$pending<-list(kind="stroke",team=t,thrower=p$thrower);d$phase<-"stopped"
 } else if(k=="corner_result") {
  if(p$result=="goal") {
   if(is.null(p$scorer) || d$players[[d$pending$team+1L]][[p$scorer+1L]]!="active") .ra_fail("A corner goal requires an active attacking scorer")
   d$score[[d$pending$team+1L]]<-d$score[[d$pending$team+1L]]+1L
  } else if(!is.null(p$scorer)) .ra_fail("A non-goal corner result cannot name a scorer")
  if(p$result!="repeat") d["pending"]<-list(NULL)
  d$phase<-"stopped"
 } else if(k=="stroke_result") {
  if(p$scored) d$score[[d$pending$team+1L]]<-d$score[[d$pending$team+1L]]+1L
  d["pending"]<-list(NULL)
 } else if(k=="replace_taker") {
  if(d$players[[d$pending$team+1L]][[p$player+1L]]!="active") .ra_fail("Replacement taker must be active")
  d$pending$thrower<-p$player
 } else if(k=="end_quarter") {
  if(d$quarter<4) d$phase<-"interval" else {d$phase<-"finished";d$reason<-"regulation";d["winner"]<-list(if(d$score[[1]]==d$score[[2]]) NULL else as.integer(d$score[[2]]>d$score[[1]]))}
 } else if(k=="abandon") {d$phase<-"abandoned";d["pending"]<-list(NULL)} else if(k=="concede") {d$phase<-"finished";d$winner<-1L-t;d$reason<-"concession";d["pending"]<-list(NULL)} else if(k=="official_result") {d$phase<-"finished";d["winner"]<-list(p$winner);d$reason<-if(is.null(p$winner)) "no_contest" else "official_result"}
 d
}
.ra_field_hockey_legal<-function(state) {
 d<-state$data;s<-.ra_field_hockey_schemas(d)
 list(mode="event_types",actions=list(),action_types=lapply(names(s),function(k) list(type=k,params_schema=s[[k]],preconditions=list("Quarter, pending penalty, eligibility and team capacity must permit the event."),external_observations=list("Official goals, cards, clock and penalty decisions."))),constraints=list(phase=d$phase,quarter=d$quarter,remaining_ms=d$remaining_ms,capacity=lapply(0:1,function(t) .ra_field_hockey_capacity(d,t)),goalkeeper=d$goalkeeper,pending_penalty=d$pending),completeness=FALSE,reason="official_field_hockey_events")
}
.ra_field_hockey_outcome<-function(state,pack) {
 d<-state$data;done<-d$phase=="finished";w<-d$winner;draw<-done && d$reason=="regulation" && is.null(w)
 list(status=if(done) "terminal" else if(d$phase=="abandoned") "awaiting_adjudication" else "ongoing",result_kind=if(done) {if(draw) "draw" else if(is.null(w)) "no_contest" else "win"} else "none",winners=if(!is.null(w)) list(state$players[[w+1L]]$id) else list(),losers=if(!is.null(w)) list(state$players[[2L-w]]$id) else list(),placements=list(),reason=if(done) d$reason else if(d$phase=="abandoned") "official_result_required" else "ongoing",evidence_required=if(d$phase=="abandoned") list("Official result decision.") else list(),rule_id=if(done) paste0(pack$ruleset_id,"@",pack$ruleset_version) else NULL)
}
