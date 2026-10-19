.ra_golf_croquet_i<-function(a,b) list(type="integer",minimum=a,maximum=b)
.ra_golf_croquet_e<-function(...) list(type="enum",values=list(...))
.ra_golf_croquet_a<-function(x,n,m=n) list(type="array",minItems=n,maxItems=m,items=x)
.ra_golf_croquet_owner<-function(d,b) (b%%2L+d$blue_owner)%%2L
.ra_golf_croquet_counts<-function(d) vapply(0:1,function(t) sum(vapply(d$hoops,function(b) .ra_golf_croquet_owner(d,b)==t,logical(1)))+d$awarded[[t+1L]],numeric(1))
.ra_golf_croquet_finish<-function(d,w,r) {d$phase<-"finished";d["winner"]<-list(w);d$reason<-r;d}
.ra_golf_croquet_advance<-function(d) {
 d$next_ball<-(d$next_ball+1L)%%4L
 while(d$lost_strokes[[.ra_golf_croquet_owner(d,d$next_ball)+1L]]>0) {
  t<-.ra_golf_croquet_owner(d,d$next_ball)+1L;d$lost_strokes[[t]]<-d$lost_strokes[[t]]-1L;d$next_ball<-(d$next_ball+1L)%%4L
 }
 d
}
.ra_golf_croquet_snapshot<-function(d) d[c("next_ball","hoops","offside","penalty_area","lost_strokes")]
.ra_golf_croquet_restore<-function(d) {for(k in names(d$last)) d[k]<-d$last[k];d["last"]<-list(NULL);d["last_actor"]<-list(NULL);d}
.ra_golf_croquet_validate<-function(d) {
 bad<-function(m) .ra_fail(m,code="INVALID_STATE")
 for(j in seq_along(d$hoops)) if(max(vapply(0:1,function(t) sum(vapply(head(d$hoops,j-1L),function(b) .ra_golf_croquet_owner(d,b)==t,logical(1))),numeric(1)))>=7) bad("No further hoop follows the winning seventh point")
 if(any(unlist(d$offside) & unlist(d$penalty_area)!="none")) bad("A directed ball cannot also be offside")
 if(d$phase=="choose_colors" && (length(d$hoops)>0 || d$next_ball!=0)) bad("Color allocation precedes play")
 if(d$phase=="finished") {
  if(is.null(d$reason) || (d$reason=="no_contest")!=is.null(d$winner)) bad("A result must identify winner or no contest")
  if(d$reason %in% c("seven_points","misconduct","disqualification") && (.ra_golf_croquet_counts(d)[d$winner+1L]!=7 || .ra_golf_croquet_counts(d)[2L-d$winner]>=7)) bad("Winning score must be seven against fewer than seven")
 } else if(!is.null(d$winner) || !is.null(d$reason) || max(.ra_golf_croquet_counts(d))>=7) bad("Ongoing game cannot contain a winning result")
 if(any(unlist(d$awarded)>0) && !d$reason %in% c("misconduct","disqualification")) bad("Awarded points require an official match award")
 if(is.null(d$last)!=is.null(d$last_actor)) bad("Last-stroke snapshot and actor must agree")
 if(!is.null(d$last)) {
  old<-d$last
  if(length(old$hoops)>length(d$hoops) || !identical(old$hoops,head(d$hoops,length(old$hoops)))) bad("Last-stroke snapshot must precede the current score")
 }
}
.ra_golf_croquet_schemas<-function(d) {
 ph<-d$phase;s<-list();ii<-.ra_golf_croquet_i;ee<-.ra_golf_croquet_e;aa<-.ra_golf_croquet_a;t<-ii(0,1);ball<-ii(0,3);b<-list(type="boolean");empty<-setNames(list(),character())
 if(ph=="finished") return(s)
 if(ph=="abandoned") return(list(official_result=list(winner=c(t,list(nullable=TRUE)))))
 if(ph=="choose_colors") return(list(choose_colors=list(blue_owner=t)))
 if(ph=="ready") {
  s<-list(stroke=list(player=t,ball=ball,hoops=aa(ball,0,13),offside=aa(b,4),fault=b,remedy=ee("none","replay","swap","leave","replace"),entered_court=b),declare_stroke=list(player=t,ball=ball),offside_decision=list(player=t,ball=ball,direction=ee("waive","D","E")),penalty_continuation=list(reason=ee("impasse","wrong_hoop","overlapping_unlawful","previous_opponent_ball"),cancel_hoops=ii(0,13)),overlap_penalty=list(player=t),misconduct=list(player=t,severity=ee("ordinary","lose_stroke","award_game","annul_last")),disqualify=list(player=t))
  if(!is.null(d$last)) s$replay_last<-list(reason=ee("outside_interference","incorrect_information","wrong_mallet"))
 }
 if(ph=="penalty_toss") s$penalty_toss<-list(first_ball=ball,area=ee("D","E"))
 c(s,list(concede=list(player=t),abandon=empty))
}
.ra_golf_croquet_apply<-function(state,action) {
 d<-state$data;p<-action$params;k<-action$type;s<-.ra_golf_croquet_schemas(d)
 if(!k %in% names(s)) .ra_fail("Action is not permitted in this phase")
 .ra_fields(p,s[[k]],"/params")
 if(k=="choose_colors") {d$blue_owner<-p$blue_owner;d$phase<-"ready"} else if(k %in% c("stroke","declare_stroke")) {
  who<-p$player;ball<-p$ball;expected<-d$next_ball;entitled<-.ra_golf_croquet_owner(d,expected)
  if(who!=entitled) .ra_fail("A stroke by the non-entitled side must be annulled, not entered as a completed turn")
  if(d$offside[[ball+1L]]) .ra_fail("Obtain the opponent offside decision before recording this stroke")
  if(k=="declare_stroke") {
   if(ball!=expected) .ra_fail("Declare the ball next in color sequence")
   d$last<-.ra_golf_croquet_snapshot(d);d$last_actor<-who
   for(b in 0:3) if(.ra_golf_croquet_owner(d,b)!=who) d$offside[[b+1L]]<-FALSE
   d<-.ra_golf_croquet_advance(d)
  } else {
   remedy<-p$remedy;fault<-p$fault;wrong<-ball!=expected
   allowed<-if(fault) c("leave","replace") else if(wrong && .ra_golf_croquet_owner(d,ball)==who) c("replay","swap") else if(wrong) "replay" else "none"
   if(!remedy %in% allowed) .ra_fail("The declared remedy does not match the fault or wrong-ball rule")
   if(length(p$hoops)==0 && any(unlist(p$offside))) .ra_fail("New offside declarations require a newly scored hoop")
   if(remedy %in% c("replay","replace") && (length(p$hoops)>0 || any(unlist(p$offside)))) .ra_fail("Replacement cancels all points and new offside declarations")
   if(fault && any(vapply(p$hoops,function(b) .ra_golf_croquet_owner(d,b)==who,logical(1)))) .ra_fail("A fault can retain points only for the non-offending side")
   if(!p$entered_court && length(p$hoops)>0) .ra_fail("A boundary stroke not entering court cannot score")
   before<-.ra_golf_croquet_snapshot(d)
   if(remedy=="replay") return(d)
   d$last<-before;d$last_actor<-who
   for(b in 0:3) if(.ra_golf_croquet_owner(d,b)!=who) d$offside[[b+1L]]<-FALSE
   if(remedy!="replace") {
    if(p$entered_court) d$penalty_area[[ball+1L]]<-"none"
    for(b in p$hoops) {
     if(max(.ra_golf_croquet_counts(d))>=7) .ra_fail("Scoring stops at the winning seventh point")
     d$hoops<-c(d$hoops,list(b))
    }
    if(length(p$hoops)>0) {
     if(any(unlist(p$offside) & unlist(d$penalty_area)!="none")) .ra_fail("Directed balls are exempt from a new offside declaration")
     d$offside<-p$offside
    }
    if(remedy=="swap") for(field in c("offside","penalty_area")) {tmp<-d[[field]][[ball+1L]];d[[field]][[ball+1L]]<-d[[field]][[expected+1L]];d[[field]][[expected+1L]]<-tmp}
   }
   d<-.ra_golf_croquet_advance(d)
   if(max(.ra_golf_croquet_counts(d))==7) d<-.ra_golf_croquet_finish(d,which(.ra_golf_croquet_counts(d)==7)[1]-1L,"seven_points")
  }
 } else if(k=="offside_decision") {
  b<-p$ball
  if(!d$offside[[b+1L]] || p$player==.ra_golf_croquet_owner(d,b)) .ra_fail("Only the offside opponent can direct or waive this ball")
  d$offside[[b+1L]]<-FALSE;if(p$direction!="waive") d$penalty_area[[b+1L]]<-p$direction
 } else if(k=="penalty_continuation") {
  n<-p$cancel_hoops
  if(n>length(d$hoops) || (p$reason %in% c("impasse","overlapping_unlawful") && n>0)) .ra_fail("This continuation cannot cancel the specified points")
  if(n>0) d$hoops<-head(d$hoops,length(d$hoops)-n)
  d$phase<-"penalty_toss";d$offside<-rep(list(FALSE),4);d["last"]<-list(NULL);d["last_actor"]<-list(NULL)
 } else if(k=="penalty_toss") {d$next_ball<-p$first_ball;d$penalty_area<-rep(list(p$area),4);d$phase<-"ready"} else if(k %in% c("misconduct","overlap_penalty","disqualify")) {
  t<-p$player;severity<-if(is.null(p$severity)) "lose_stroke" else p$severity
  if(k=="misconduct") {
   d$warnings[[t+1L]]<-d$warnings[[t+1L]]+1L
   if(severity=="ordinary") severity<-if(d$warnings[[t+1L]]>=3) "award_game" else if(d$warnings[[t+1L]]==2) "lose_stroke" else "warn"
  }
  if(k=="disqualify" || severity=="award_game") {
   w<-1L-t;d$awarded[[w+1L]]<-7L-.ra_golf_croquet_counts(d)[w+1L];d<-.ra_golf_croquet_finish(d,w,if(k=="disqualify") "disqualification" else "misconduct")
  } else if(severity=="annul_last") {
   if(is.null(d$last) || d$last_actor!=t) .ra_fail("Annulment requires the offending side last stroke")
   d<-.ra_golf_croquet_restore(d)
  } else if(severity=="lose_stroke") {
   if(.ra_golf_croquet_owner(d,d$next_ball)==t) d<-.ra_golf_croquet_advance(d) else d$lost_strokes[[t+1L]]<-d$lost_strokes[[t+1L]]+1L
  }
 } else if(k=="replay_last") d<-.ra_golf_croquet_restore(d) else if(k=="concede") d<-.ra_golf_croquet_finish(d,1L-p$player,"concession") else if(k=="abandon") d$phase<-"abandoned" else if(k=="official_result") d<-.ra_golf_croquet_finish(d,p$winner,if(is.null(p$winner)) "no_contest" else "official_result")
 d
}
.ra_golf_croquet_legal<-function(state) {
 d<-state$data;s<-.ra_golf_croquet_schemas(d)
 list(mode="event_types",actions=list(),action_types=lapply(names(s),function(k) list(type=k,params_schema=s[[k]],preconditions=list("Correct side, color sequence and remedy must agree with the recorded state."),external_observations=list("Official hoop completions, offside findings, faults, interference and opponent remedy choices."))),constraints=list(phase=d$phase,score=as.list(.ra_golf_croquet_counts(d)),hoop_in_order=length(d$hoops)+1L,next_ball=d$next_ball,next_player=.ra_golf_croquet_owner(d,d$next_ball),offside=d$offside,penalty_area=d$penalty_area,warnings=d$warnings,lost_strokes=d$lost_strokes),completeness=FALSE,reason="official_golf_croquet_stroke_events")
}
.ra_golf_croquet_outcome<-function(state,pack) .ra_pelota_outcome(state,pack)
