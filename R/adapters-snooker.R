.ra_snooker_schemas<-function() {
 i<-function(a,b) list(type="integer",minimum=a,maximum=b);b<-list(type="boolean")
 list(start=list(player=i(0,1)),nominate_colour=list(value=i(2,7)),nominate_free_ball=list(value=i(2,7)),stroke=list(reds=i(0,15),on_potted=b,free_potted=b),foul=list(points=i(4,7),reds_removed=i(0,15),miss=b,full_ball_available=b,free_ball_available=b),choose_after_foul=list(choice=list(type="string",enum=as.list(c("play","return","replace")))),choose_black_start=list(player=i(0,1)),concede=list(player=i(0,1)))
}
.ra_snooker_on<-function(d) if(d$stage=="reds") 1L else if(d$stage=="colour") d$nominated else d$clearance
.ra_snooker_finish<-function(d,w,reason) {d$phase<-"finished";d$winner<-w;d$reason<-reason;d$free_ball<-FALSE;d["free_nomination"]<-list(NULL);d$miss_count<-0L;d["pending"]<-list(NULL);d["nominated"]<-list(NULL);d}
.ra_snooker_black<-function(d) {
 if(d$scores[[1]]==d$scores[[2]]) {d$phase<-"black_toss";d$stage<-"black_tie";d$clearance<-7L;d["nominated"]<-list(NULL);d$free_ball<-FALSE;d["free_nomination"]<-list(NULL);d["pending"]<-list(NULL);d$miss_count<-0L;d} else .ra_snooker_finish(d,if(d$scores[[1]]>d$scores[[2]]) 0L else 1L,"points")
}
.ra_snooker_reset<-function(d) {d$free_ball<-FALSE;d["free_nomination"]<-list(NULL);d["nominated"]<-list(NULL);d$miss_count<-0L;d}
.ra_snooker_validate<-function(d) {
 bad<-function(m) .ra_fail(m,code="INVALID_STATE")
 if(d$stage=="reds" && d$reds==0 || d$stage %in% c("clearance","black_tie") && d$reds!=0) bad("Ball-on stage must agree with remaining reds")
 if(d$stage!="colour" && !is.null(d$nominated)) bad("Colour nomination is only needed after a red")
 if(!is.null(d$free_nomination) && (!d$free_ball || d$stage=="colour" || d$stage!="reds" && d$free_nomination<=.ra_snooker_on(d))) bad("Free ball must differ from the ball on and require an entitlement")
 if(d$free_ball && d$stage=="colour") bad("The post-red colour cannot carry a free-ball entitlement")
 if(d$phase=="ready" && (any(unlist(d$scores)!=0) || d$reds!=15 || d$stage!="reds" || d$free_ball || d$miss_count>0 || !is.null(d$pending))) bad("Unstarted frame must have its complete unscored rack")
 if((d$phase=="choice")!=!is.null(d$pending)) bad("Only foul choice retains a replacement record")
 if(!is.null(d$pending)) {r<-d$pending;if(r$offender==d$turn || r$full_ball && !r$miss || r$old_stage=="reds" && r$old_reds==0) bad("Foul choice requires the opponent and a coherent miss record")}
 if(d$phase=="black_toss" && (d$stage!="black_tie" || d$scores[[1]]!=d$scores[[2]])) bad("Respotted black requires a tied completed frame")
 if(d$stage=="black_tie" && d$clearance!=7) bad("Respotted-black stage has only the black on")
 if(d$phase=="finished") {
  if(is.null(d$winner) || is.null(d$reason)) bad("Finished frame requires a winner and reason")
  if(d$reason=="points" && (d$reds!=0 || d$clearance!=7 || !(d$stage %in% c("clearance","black_tie")) || d$scores[[d$winner+1L]]<=d$scores[[2L-d$winner]])) bad("Points victory follows the final black with unequal scores")
 } else if(!is.null(d$winner) || !is.null(d$reason)) bad("Open frame cannot retain a winner");invisible(TRUE)
}
.ra_snooker_apply<-function(state,action) {
 d<-state$data;k<-action$type;p<-action$params;s<-.ra_snooker_schemas()
 if(!(k %in% names(s))) .ra_fail("Unknown snooker event")
 .ra_fields(p,s[[k]],"/params")
 if(d$phase=="finished") .ra_fail("Frame is finished")
 if(k=="concede") return(.ra_snooker_finish(d,1L-p$player,"concession"))
 if(k=="start") {if(d$phase!="ready") .ra_fail("Frame already started");d$phase<-"turn";d$turn<-p$player;return(d)}
 if(k=="choose_black_start") {if(d$phase!="black_toss") .ra_fail("No respotted-black toss is due");d$phase<-"turn";d$turn<-p$player;return(d)}
 if(k=="choose_after_foul") {
  if(d$phase!="choice") .ra_fail("No foul choice is pending")
  r<-d$pending;choice<-p$choice;d<-.ra_snooker_reset(d)
  if(choice=="replace") {
   if(!r$miss) .ra_fail("Replacement requires an official foul-and-miss call")
   d$turn<-r$offender;d$reds<-r$old_reds;d$stage<-r$old_stage;d$clearance<-r$old_clearance;d["nominated"]<-list(r$old_nomination);d$miss_count<-if(r$full_ball) r$sequence else 0L;d$free_ball<-r$old_free_ball;d["free_nomination"]<-list(r$old_free_nomination)
  } else if(choice=="return") d$turn<-r$offender else d$free_ball<-r$free_ball
  d$phase<-"turn";d["pending"]<-list(NULL);return(d)
 }
 if(d$phase!="turn") .ra_fail("A stroke requires an open visit")
 if(k=="nominate_colour") {if(d$stage!="colour") .ra_fail("Choose a colour only after scoring a red");d$nominated<-p$value;return(d)}
 if(k=="nominate_free_ball") {
  if(!d$free_ball || d$stage=="colour" || d$stage!="reds" && p$value<=.ra_snooker_on(d)) .ra_fail("A free-ball nomination must differ from the ball on")
  d$free_nomination<-p$value;return(d)
 }
 value<-.ra_snooker_on(d);if(is.null(value)) .ra_fail("Nominate the post-red colour before adjudicating a stroke")
 if(k=="stroke") {
  if(p$reds>d$reds || p$free_potted && is.null(d$free_nomination)) .ra_fail("Pots exceed the remaining balls or free-ball entitlement")
  oldstage<-d$stage;points<-0L
  if(oldstage=="reds") {
   if(p$on_potted) .ra_fail("Red pots use their explicit count")
   points<-p$reds+as.integer(p$free_potted);d$reds<-d$reds-p$reds;if(points>0) d$stage<-"colour"
  } else {
   if(p$reds>0) .ra_fail("Reds cannot be legally potted while a colour is on")
   points<-if(p$on_potted || p$free_potted) value else 0L
   if(points>0) {if(oldstage=="colour") {d$stage<-if(d$reds>0) "reds" else "clearance";d$clearance<-2L} else if(p$on_potted && value<7) d$clearance<-d$clearance+1L}
  }
  d$scores[[d$turn+1L]]<-d$scores[[d$turn+1L]]+points;d<-.ra_snooker_reset(d)
  if(oldstage %in% c("clearance","black_tie") && value==7 && p$on_potted) d<-.ra_snooker_black(d) else if(points==0) {d$turn<-1L-d$turn;d$stage<-if(d$reds>0) "reds" else if(oldstage=="colour") "clearance" else d$stage};return(d)
 }
 if(k=="foul") {
  if(p$points<max(4L,value) || p$reds_removed>d$reds || p$full_ball_available && !p$miss) .ra_fail("Foul points and miss context must agree with the ball on and available reds")
  offender<-d$turn;sequence<-if(p$miss && p$full_ball_available) d$miss_count+1L else 0L
  r<-list(offender=offender,old_reds=d$reds,old_stage=d$stage,old_clearance=d$clearance,old_nomination=d$nominated,old_free_ball=d$free_ball,old_free_nomination=d$free_nomination,miss=p$miss,full_ball=p$full_ball_available,sequence=sequence,free_ball=p$free_ball_available)
  d$scores[[2L-offender]]<-d$scores[[2L-offender]]+p$points;d$reds<-d$reds-p$reds_removed;d<-.ra_snooker_reset(d)
  if(d$stage %in% c("clearance","black_tie") && value==7) return(.ra_snooker_black(d))
  if(sequence>=3) return(.ra_snooker_finish(d,1L-offender,"three_misses"))
  if(d$stage=="colour" || d$stage=="reds" && d$reds==0) {d$stage<-if(d$reds>0) "reds" else "clearance";d$clearance<-2L}
  d$phase<-"choice";d$turn<-1L-offender;d$pending<-r;return(d)
 }
 .ra_fail("Unknown snooker event")
}
.ra_snooker_legal<-function(state) {
 d<-state$data;phase<-d$phase;names<-character()
 if(phase=="ready") names<-"start" else if(phase=="black_toss") names<-"choose_black_start" else if(phase=="choice") names<-"choose_after_foul" else if(phase=="turn") {if(d$stage=="colour") names<-c(names,"nominate_colour");if(d$free_ball) names<-c(names,"nominate_free_ball");if(!is.null(.ra_snooker_on(d))) names<-c(names,"stroke","foul")}
 if(phase!="finished") names<-c(names,"concede")
 s<-.ra_snooker_schemas()
 list(mode="event_types",actions=list(),action_types=lapply(names,function(k) list(type=k,params_schema=s[[k]],preconditions=list("Ball-on sequence, nomination, available reds and pending referee decision must permit the event."),external_observations=list("Legal pots, foul value, miss and free-ball rulings; no ball coordinates."))),constraints=list(phase=phase,player=d$turn,ball_on=.ra_snooker_on(d),reds_remaining=d$reds,free_ball=d$free_ball,miss_warning=d$miss_count==2),completeness=FALSE,reason="official_snooker_strokes")
}
.ra_snooker_outcome<-function(state,pack) {
 d<-state$data;done<-d$phase=="finished";w<-d$winner
 list(status=if(done) "terminal" else "ongoing",result_kind=if(done) "win" else "none",winners=if(done) list(state$players[[w+1L]]$id) else list(),losers=if(done) list(state$players[[2L-w]]$id) else list(),placements=list(),reason=if(done) d$reason else "ongoing",evidence_required=list(),rule_id=if(done) paste0(pack$ruleset_id,"@",pack$ruleset_version) else NULL)
}
