.ra_waterski_i<-function(a,b) list(type="integer",minimum=a,maximum=b)
.ra_waterski_q<-list(type="integer",enum=as.list((0:24)[(0:24)%%4L!=3L]))
.ra_waterski_ruling<-list(quarters=.ra_waterski_q,complete=list(type="boolean"),reride=list(type="enum",values=list("none","optional","mandatory")),protected=.ra_waterski_q,cap=.ra_waterski_q,continue_allowed=list(type="boolean"))
.ra_waterski_stage<-list(type="object",properties=list(speed_kmh=.ra_waterski_i(25,58),rope_cm=.ra_waterski_i(800,1825)),required=list("speed_kmh","rope_cm"))
.ra_waterski_empty<-setNames(list(),character())
.ra_waterski_schemas<-list(configure=list(stages=list(type="array",minItems=1,maxItems=32,items=.ra_waterski_stage),order=list(type="array",minItems=8,maxItems=8,items=.ra_waterski_i(0,7))),start_pass=list(stage=.ra_waterski_i(0,31)),judge_pass=.ra_waterski_ruling,correct_ruling=.ra_waterski_ruling,accept_pass=.ra_waterski_empty,take_reride=.ra_waterski_empty,continue_at_risk=list(stage=.ra_waterski_i(0,31)),stop_run=.ra_waterski_empty,withdraw=list(participant=.ra_waterski_i(0,7)),disqualify=list(participant=.ra_waterski_i(0,7)),zero_performance=list(participant=.ra_waterski_i(0,7)),confirm_result=.ra_waterski_empty)
.ra_waterski_current<-function(d) {for(i in d$order) if(d$skiers[[i+1L]]$status %in% c("pending","skiing")) return(i);NULL}
.ra_waterski_clear<-function(d) {d["selected"]<-list(NULL);d$protected<-0L;d$cap<-24L;d["ruling"]<-list(NULL);d}
.ra_waterski_settle<-function(d) {d<-.ra_waterski_clear(d);d$phase<-if(is.null(.ra_waterski_current(d))) "review" else "ready";d}
.ra_waterski_rule_valid<-function(r) r$protected<=r$quarters && r$quarters<=r$cap && (!r$complete || r$quarters==24) && (!r$continue_allowed || (r$complete && r$reride!="none")) && (r$reride!="none" || (r$protected==0 && r$cap==24))
.ra_waterski_validate<-function(d) {
 bad<-function(m) .ra_fail(m,code="INVALID_STATE")
 if(d$phase=="setup") {
  if(length(d$stages)>0 || length(d$order)>0 || any(vapply(d$skiers,function(r) r$status!="pending" || r$next_stage!=0 || r$score_quarters!=0,logical(1)))) bad("Initial slalom event must be empty")
 } else {
  if(!length(d$stages) || !identical(as.integer(sort(unlist(d$order))),0:7)) bad("Configure progression and the complete start order")
  if(d$stages[[1]]$rope_cm!=1825 || any(vapply(d$stages,function(x) x$speed_kmh<d$stages[[length(d$stages)]]$speed_kmh && x$rope_cm!=1825,logical(1)))) bad("Rope shortening starts only at the configured maximum speed")
  if(length(d$stages)>1) for(j in 2:length(d$stages)) {
   a<-d$stages[[j-1L]];b<-d$stages[[j]]
   if(b$speed_kmh<a$speed_kmh || b$rope_cm>a$rope_cm || (b$speed_kmh==a$speed_kmh && b$rope_cm==a$rope_cm) || (b$speed_kmh!=a$speed_kmh && b$speed_kmh-a$speed_kmh!=3)) bad("Successive passes must be strictly harder")
  }
 }
 for(r in d$skiers) {
  if(r$score_quarters%%4L==3L) bad("Three-quarter buoy scores are not permitted")
  if(r$next_stage>length(d$stages) || r$score_quarters>24*length(d$stages)) bad("Score exceeds configured progression")
  if(r$status %in% c("pending","dns") && (r$next_stage!=0 || r$score_quarters!=0)) bad("Unstarted skier already has a score")
  if(r$status=="skiing" && r$next_stage>=length(d$stages)) bad("Completed progression must finish the run")
  if(r$status=="skiing" && !(24*r$next_stage<=r$score_quarters && r$score_quarters<=24*(r$next_stage+1L))) bad("Active score and next credited stage disagree")
 }
 if(d$protected>d$cap) bad("Conflicting reride score bounds")
 if(is.null(d$selected)) {
  if(d$phase %in% c("pass","pass_review") || d$protected!=0 || d$cap!=24) bad("Pass context is missing")
 } else {
  i<-.ra_waterski_current(d)
  if(is.null(i) || !d$phase %in% c("ready","pass","pass_review") || !(d$skiers[[i+1L]]$next_stage<=d$selected && d$selected<length(d$stages))) bad("Selected pass is outside the remaining progression")
 }
 if((d$phase=="pass_review")!=!is.null(d$ruling)) bad("Pending ruling requires pass review")
 if(!is.null(d$ruling) && !.ra_waterski_rule_valid(d$ruling)) bad("Inconsistent pass ruling")
 if(d$phase!="setup" && (d$phase %in% c("review","finished"))!=is.null(.ra_waterski_current(d))) bad("Resolve every skier before result review")
}
.ra_waterski_apply<-function(state,action) {
 d<-state$data;k<-action$type;p<-action$params;phase<-d$phase;i<-.ra_waterski_current(d)
 if(!k %in% names(.ra_waterski_schemas)) .ra_fail("Unknown slalom action")
 .ra_fields(p,.ra_waterski_schemas[[k]],"/params")
 if(k=="configure") {
  if(phase!="setup" || !identical(as.integer(sort(unlist(p$order))),0:7)) .ra_fail("Configure each entrant once before skiing")
  d$stages<-p$stages;d$order<-p$order;d$phase<-"ready";.ra_waterski_validate(d)
 } else if(k=="start_pass") {
  if(phase!="ready" || !(d$skiers[[i+1L]]$next_stage<=p$stage && p$stage<length(d$stages)) || (!is.null(d$selected) && p$stage!=d$selected)) .ra_fail("Start the next selected or authorized repeat pass")
  d$selected<-p$stage;d$skiers[[i+1L]]$status<-"skiing";d$phase<-"pass"
 } else if(k %in% c("judge_pass","correct_ruling")) {
  if(phase!=(if(k=="judge_pass") "pass" else "pass_review") || !.ra_waterski_rule_valid(p)) .ra_fail("Supply a consistent official pass ruling in the proper phase")
  r<-p;r$quarters<-max(d$protected,min(d$cap,r$quarters));r$complete<-r$complete && r$quarters==24;r$protected<-max(d$protected,r$protected);r$cap<-min(d$cap,r$cap)
  if(r$protected>r$cap) .ra_fail("Reride rulings conflict with existing protection")
  if(r$reride=="none") {r$protected<-0L;r$cap<-24L;r$continue_allowed<-FALSE}
  if(!r$complete) r$continue_allowed<-FALSE
  d$ruling<-r;d$phase<-"pass_review"
 } else if(k=="accept_pass") {
  if(phase!="pass_review" || d$ruling$reride=="mandatory") .ra_fail("A mandatory reride cannot be accepted")
  r<-d$skiers[[i+1L]];v<-d$ruling;r$score_quarters<-max(r$score_quarters,if(v$complete) 24*(d$selected+1L) else 24*r$next_stage+v$quarters)
  if(v$complete) r$next_stage<-d$selected+1L
  if(!v$complete || r$next_stage==length(d$stages)) r$status<-"finished"
  d$skiers[[i+1L]]<-r;d<-.ra_waterski_settle(d)
 } else if(k=="take_reride") {
  if(phase!="pass_review" || d$ruling$reride=="none") .ra_fail("No repeat has been authorized")
  d$protected<-d$ruling$protected;d$cap<-d$ruling$cap;d["ruling"]<-list(NULL);d$phase<-"ready"
 } else if(k=="continue_at_risk") {
  if(phase!="pass_review" || !d$ruling$continue_allowed || !(d$selected<p$stage && p$stage<length(d$stages))) .ra_fail("No harder at-risk pass is authorized")
  r<-d$skiers[[i+1L]];r$score_quarters<-max(r$score_quarters,24*r$next_stage+d$ruling$protected);d$skiers[[i+1L]]<-r;d<-.ra_waterski_clear(d);d$selected<-p$stage;d$phase<-"ready"
 } else if(k=="stop_run") {
  if(phase!="ready" || d$skiers[[i+1L]]$status!="skiing") .ra_fail("Stop only between attempts after skiing has begun")
  r<-d$skiers[[i+1L]];r$score_quarters<-max(r$score_quarters,24*r$next_stage+d$protected);r$status<-"finished";d$skiers[[i+1L]]<-r;d<-.ra_waterski_settle(d)
 } else if(k %in% c("withdraw","disqualify","zero_performance")) {
  j<-p$participant;r<-d$skiers[[j+1L]]
  if(phase=="setup" || r$status=="dq") .ra_fail("Entrant cannot receive this ruling now")
  if(k=="withdraw") {if(r$status!="pending") .ra_fail("Withdrawal before skiing is DNS; use stop_run after starting");r$status<-"dns"} else if(k=="disqualify") r$status<-"dq" else {r$status<-"finished";r$score_quarters<-0L;r$next_stage<-0L}
  d$skiers[[j+1L]]<-r;if(!is.null(i) && j==i) d<-.ra_waterski_settle(d)
 } else if(k=="confirm_result") {if(phase!="review") .ra_fail("Resolve all runs before confirming results");d$phase<-"finished"}
 d
}
.ra_waterski_legal<-function(state) {
 d<-state$data;names<-switch(d$phase,setup="configure",ready=c("start_pass","stop_run"),pass="judge_pass",pass_review=c("correct_ruling","accept_pass","take_reride","continue_at_risk"),review="confirm_result",character())
 if(!d$phase %in% c("setup","finished")) names<-c(names,"withdraw","disqualify","zero_performance")
 list(mode="event_types",actions=list(),action_types=lapply(names,function(k) list(type=k,params_schema=.ra_waterski_schemas[[k]],preconditions=list("Pass selection, completion, repeat entitlement and protected score bounds must permit the event."),external_observations=list("Official buoy score, continuation eligibility and reride decision."))),constraints=list(phase=d$phase,current_participant=.ra_waterski_current(d),selected_stage=d$selected,protected_quarters=d$protected,maximum_quarters=d$cap),completeness=FALSE,reason="official_slalom_pass_rulings")
}
.ra_waterski_outcome<-function(state,pack) {
 d<-state$data;t<-d$phase=="finished";ranked<-list()
 if(t) {ix<-which(vapply(d$skiers,function(r) r$status=="finished",logical(1)));if(length(ix)) {ix<-ix[order(-vapply(d$skiers[ix],function(r) r$score_quarters,numeric(1)),ix)];ranked<-lapply(ix,function(i) list(score=d$skiers[[i]]$score_quarters,i=i))}}
 winners<-lapply(Filter(function(r) r$score==ranked[[1L]]$score,ranked),function(r) state$players[[r$i]]$id)
 list(status=if(t) "terminal" else if(d$phase %in% c("pass_review","review")) "awaiting_adjudication" else "ongoing",result_kind=if(t) {if(length(ranked)) "ranking" else "no_contest"} else "none",winners=winners,losers=if(length(winners)) lapply(Filter(function(p) !p$id %in% unlist(winners),state$players),function(p) p$id) else list(),placements=lapply(ranked,function(r) list(player_id=state$players[[r$i]]$id,place=1L+sum(vapply(ranked,function(o) o$score>r$score,logical(1))),score_quarters=r$score)),reason=if(t) "slalom_confirmed" else "ongoing",evidence_required=list(),rule_id=if(t) paste0(pack$ruleset_id,"@",pack$ruleset_version) else NULL)
}
