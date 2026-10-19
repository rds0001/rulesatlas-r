.ra_karate_schemas<-function() {
 i<-function(a,b) list(type="integer",minimum=a,maximum=b);a<-function(x,n) list(type="array",minItems=n,maxItems=n,items=x);empty<-setNames(list(),character())
 list(start=empty,pause=empty,resume=empty,elapse=list(milliseconds=i(1,180000)),award_scores=list(points=a(i(0,3),2)),warning=list(player=i(0,1),category=i(1,2),level=i(1,4)),correct_record=list(scores=a(i(0,1000000),2),remaining_ms=i(0,180000)),hantei=list(votes=a(i(0,1),5)),disqualify=list(loser=i(0,1)),forfeit=list(loser=i(0,1)),both_disqualified=empty,confirm=empty)
}
.ra_karate_settle<-function(d) {
 d["winner"]<-list(NULL);d["reason"]<-list(NULL);a<-d$scores[[1]];b<-d$scores[[2]]
 if(abs(a-b)>=8 || (d$remaining_ms==0 && a!=b)) {d$phase<-"review";d$winner<-if(a>b) 0L else 1L;d$reason<-"points"} else if(d$remaining_ms==0) d$phase<-"decision" else d$phase<-"paused";d
}
.ra_karate_validate<-function(d) {
 bad<-function(m) .ra_fail(m,code="INVALID_STATE");terminal<-d$phase %in% c("review","finished")
 if(terminal!=!is.null(d$reason)) bad("Result phase and reason disagree")
 if(d$phase=="ready" && (any(unlist(d$scores)!=0) || any(unlist(d$penalties)!=0) || d$remaining_ms!=180000)) bad("Unstarted bout must have empty records")
 if(d$phase %in% c("active","paused") && (d$remaining_ms==0 || abs(d$scores[[1]]-d$scores[[2]])>=8)) bad("A decided period cannot remain active")
 if(d$phase=="decision" && (d$remaining_ms!=0 || d$scores[[1]]!=d$scores[[2]])) bad("Hantei requires tied scores at time expiry")
 if(!terminal) {if(!is.null(d$winner) || any(unlist(d$penalties)==4)) bad("A winning decision must enter review")} else if(d$reason=="both_disqualified") {if(!is.null(d$winner)) bad("Double disqualification has no winner")} else {
  w<-d$winner;if(is.null(w)) bad("A decisive result requires a winner")
  if(d$reason=="points" && (d$scores[[w+1L]]<=d$scores[[2L-w]] || !(abs(d$scores[[1]]-d$scores[[2]])>=8 || d$remaining_ms==0))) bad("Point victory requires an eight-point margin or a lead at time expiry")
  if(d$reason=="hantei" && (d$remaining_ms!=0 || d$scores[[1]]!=d$scores[[2]])) bad("Hantei requires a tied expired bout")
  if(d$reason=="hansoku" && !(4 %in% unlist(d$penalties[[2L-w]]))) bad("Hansoku requires a final warning-category sanction")
 };invisible(TRUE)
}
.ra_karate_available<-function(d) {
 names<-switch(d$phase,ready="start",active=c("pause","elapse","award_scores","warning"),paused=c("resume","award_scores","warning","correct_record"),decision=c("hantei","warning","correct_record"),review="confirm",character())
 if(d$phase=="review" && d$reason=="points") names<-c(names,"correct_record")
 if(d$phase!="finished") names<-c(names,"disqualify","forfeit","both_disqualified");names
}
.ra_karate_apply<-function(state,action) {
 d<-state$data;k<-action$type;p<-action$params
 if(!(k %in% .ra_karate_available(d))) .ra_fail("Event is not permitted in this bout phase")
 .ra_fields(p,.ra_karate_schemas()[[k]],"/params")
 if(k %in% c("start","resume")) d$phase<-"active" else if(k=="pause") d$phase<-"paused" else if(k=="elapse") {
  if(p$milliseconds>d$remaining_ms) .ra_fail("Elapsed time cannot cross the bout boundary")
  d$remaining_ms<-d$remaining_ms-p$milliseconds;if(d$remaining_ms==0) d<-.ra_karate_settle(d)
 } else if(k=="award_scores") {
  if(!any(unlist(p$points)>0)) .ra_fail("At least one official score is required")
  d$scores<-lapply(1:2,function(i) d$scores[[i]]+p$points[[i]]);d<-.ra_karate_settle(d)
 } else if(k=="warning") {
  i<-p$player+1L;c<-p$category
  if(p$level<=d$penalties[[i]][[c]]) .ra_fail("A warning category must progress to a higher sanction")
  d$penalties[[i]][[c]]<-p$level
  if(p$level==4) {d$phase<-"review";d$winner<-1L-p$player;d$reason<-"hansoku"} else if(d$phase=="active") d$phase<-"paused"
 } else if(k=="correct_record") {d$scores<-p$scores;d$remaining_ms<-p$remaining_ms;d<-.ra_karate_settle(d)} else if(k=="hantei") {d$phase<-"review";d$winner<-if(sum(unlist(p$votes))>=3) 1L else 0L;d$reason<-"hantei"} else if(k %in% c("disqualify","forfeit")) {d$phase<-"review";d$winner<-1L-p$loser;d$reason<-k} else if(k=="both_disqualified") {d$phase<-"review";d["winner"]<-list(NULL);d$reason<-k} else if(k=="confirm") d$phase<-"finished";d
}
.ra_karate_legal<-function(state) {
 d<-state$data;s<-.ra_karate_schemas()
 list(mode="event_types",actions=list(),action_types=lapply(.ra_karate_available(d),function(k) list(type=k,params_schema=s[[k]],preconditions=list("Official score, independent warning-category progression and clock phase must permit the event."),external_observations=list("Recognized one-, two- or three-point scores, official sanctions, elapsed active time or five decision votes."))),constraints=list(phase=d$phase,remaining_ms=d$remaining_ms,winning_margin=8L,first_score_advantage=FALSE),completeness=FALSE,reason="official_classic_kumite_rulings")
}
.ra_karate_outcome<-function(state,pack) {
 d<-state$data;done<-d$phase=="finished";w<-d$winner
 list(status=if(done) "terminal" else if(d$phase=="review") "awaiting_adjudication" else "ongoing",result_kind=if(done) {if(is.null(w)) "no_contest" else if(d$reason=="forfeit") "forfeit" else if(d$reason %in% c("hansoku","disqualify")) "disqualification" else "win"} else "none",winners=if(done && !is.null(w)) list(state$players[[w+1L]]$id) else list(),losers=if(done && !is.null(w)) list(state$players[[2L-w]]$id) else list(),placements=list(),reason=if(done) d$reason else "ongoing",evidence_required=list(),rule_id=if(done) paste0(pack$ruleset_id,"@",pack$ruleset_version) else NULL)
}
