.ra_golf_i<-function(a,b,nullable=FALSE) {x<-list(type="integer",minimum=a,maximum=b);if(nullable) x$nullable<-TRUE;x}
.ra_golf_player<-.ra_golf_i(0,3)
.ra_golf_schemas<-list(start_hole=list(player=.ra_golf_player),stroke=list(player=.ra_golf_player),penalty=list(player=.ra_golf_player,hole=.ra_golf_i(0,17),strokes=.ra_golf_i(1,2)),hole_out=list(player=.ra_golf_player),correct_hole=list(player=.ra_golf_player,hole=.ra_golf_i(0,17),strokes=.ra_golf_i(1,1000)),return_card=list(player=.ra_golf_player,scores=list(type="array",minItems=18,maxItems=18,items=.ra_golf_i(1,1000,TRUE)),certified=list(type="boolean")),withdraw=list(player=.ra_golf_player),disqualify=list(player=.ra_golf_player),confirm_result=setNames(list(),character()))
.ra_golf_schemas$unknown_penalty<-list(player=.ra_golf_player,hole=.ra_golf_i(0,17),strokes=.ra_golf_i(1,2))
.ra_golf_settled<-function(d) all(vapply(d$golfers,function(r) r$status %in% c("returned","dns","dnf","dq"),logical(1)))
.ra_golf_validate<-function(d) {
 bad<-function(m) .ra_fail(m,code="INVALID_STATE")
 for(r in d$golfers) {
  status<-r$status;n<-length(r$holes)
  if((status %in% c("playing","ready") && n>=18) || (status=="card" && n!=18)) bad("Round phase and completed holes disagree")
  if(!status %in% c("playing","dnf","dq") && r$current_strokes!=0) bad("Strokes exist outside an active hole")
  if(r$current_played && (r$current_strokes==0 || !status %in% c("playing","dnf","dq"))) bad("Played-stroke flag disagrees with active hole")
  if(status=="dns" && (n>0 || r$current_strokes!=0)) bad("DNS golfer already played")
  if(status=="returned" && (n!=18 || is.null(r$card) || any(vapply(r$card,is.null,logical(1))) || any(unlist(r$holes)>unlist(r$card)))) bad("Returned scorecard understates or omits a completed hole")
  if(!is.null(r$card) && !status %in% c("returned","dq")) bad("Scorecard exists before return")
  if(!is.null(r$card) && n!=18) bad("Only completed rounds may return cards")
  if((status=="dq")!=!is.null(r$disqualification_reason)) bad("Disqualification reason mismatch")
 }
 if((d$phase %in% c("review","finished"))!=.ra_golf_settled(d)) bad("All golfers must resolve their rounds and scorecards before review")
}
.ra_golf_apply<-function(state,action) {
 d<-state$data;k<-action$type;p<-action$params
 if(!k %in% names(.ra_golf_schemas)) .ra_fail("Unknown golf action")
 .ra_fields(p,.ra_golf_schemas[[k]],"/params")
 if(k=="confirm_result") {if(d$phase!="review") .ra_fail("Every round and scorecard must be resolved first");d$phase<-"finished";return(d)}
 j<-p$player+1L;r<-d$golfers[[j]];status<-r$status
 if(k=="start_hole") {if(status!="ready") .ra_fail("This golfer cannot start another hole");r$status<-"playing"} else if(k=="stroke") {
  if(status!="playing" || r$current_strokes>=1000) .ra_fail("Stroke requires an active hole within the supported score bound")
  r$current_strokes<-r$current_strokes+1L;r$current_played<-TRUE
 } else if(k=="penalty") {
  h<-p$hole;n<-length(r$holes)
  if(!status %in% c("ready","playing","card") || h>n || (h==n && status!="playing")) .ra_fail("Apply an official penalty to a started hole before card return")
  if(h==n) r$current_strokes<-r$current_strokes+p$strokes else r$holes[[h+1L]]<-r$holes[[h+1L]]+p$strokes
  if(max(c(unlist(r$holes),r$current_strokes))>1000) .ra_fail("Hole score exceeds the supported bound")
 } else if(k=="unknown_penalty") {
  if(status!="returned") .ra_fail("The unknown-penalty exception requires a returned certified card")
  h<-p$hole+1L;r$holes[[h]]<-r$holes[[h]]+p$strokes;r$card[[h]]<-r$card[[h]]+p$strokes
  if(max(c(unlist(r$holes),unlist(r$card)))>1000) .ra_fail("Hole score exceeds the supported bound")
 } else if(k=="hole_out") {
  if(status!="playing" || !r$current_played) .ra_fail("Holing out requires a played stroke")
  r$holes[[length(r$holes)+1L]]<-r$current_strokes;r$current_strokes<-0L;r$current_played<-FALSE;r$status<-if(length(r$holes)==18) "card" else "ready"
 } else if(k=="correct_hole") {
  if(!status %in% c("ready","playing","card") || p$hole>=length(r$holes)) .ra_fail("Correct a completed hole before returning the card")
  r$holes[[p$hole+1L]]<-p$strokes
 } else if(k=="return_card") {
  if(status!="card") .ra_fail("Complete all eighteen holes before returning the card")
  r$card<-p$scores
  reason<-if(!p$certified) "uncertified_card" else if(any(vapply(p$scores,is.null,logical(1)))) "missing_hole" else if(any(unlist(r$holes)>unlist(p$scores))) "understated_hole" else NULL
  r$status<-if(is.null(reason)) "returned" else "dq";r["disqualification_reason"]<-list(reason)
 } else if(k=="withdraw") {
  if(!status %in% c("ready","playing","card")) .ra_fail("Only an unfinished or unreturned round can be withdrawn")
  r$status<-if(status=="ready" && !length(r$holes)) "dns" else "dnf"
 } else if(k=="disqualify") {
  if(status=="dq") .ra_fail("Golfer is already disqualified")
  r$status<-"dq";r$disqualification_reason<-"official_ruling"
 }
 d$golfers[[j]]<-r;d$phase<-if(.ra_golf_settled(d)) "review" else "round";d
}
.ra_golf_legal<-function(state) {
 d<-state$data;s<-if(d$phase=="round") .ra_golf_schemas[setdiff(names(.ra_golf_schemas),"confirm_result")] else if(d$phase=="review") list(disqualify=.ra_golf_schemas$disqualify,unknown_penalty=.ra_golf_schemas$unknown_penalty,confirm_result=setNames(list(),character())) else list()
 list(mode="event_types",actions=list(),action_types=lapply(names(s),function(k) list(type=k,params_schema=s[[k]],preconditions=list("Golfer phase, hole completion and scorecard return must permit this event."),external_observations=list("Played strokes, official one- or two-stroke penalties, holed balls and certified cards."))),constraints=list(phase=d$phase,next_holes=lapply(d$golfers,function(r) if(length(r$holes)<18) length(r$holes) else NULL),target_holes=18),completeness=FALSE,reason="official_scratch_stroke_play")
}
.ra_golf_outcome<-function(state,pack) {
 d<-state$data;t<-d$phase=="finished";ranked<-list()
 if(t) for(i in 1:4) if(d$golfers[[i]]$status=="returned") ranked[[length(ranked)+1L]]<-list(score=sum(unlist(d$golfers[[i]]$card)),i=i)
 if(length(ranked)>0) ranked<-ranked[order(vapply(ranked,function(r) r$score,numeric(1)),vapply(ranked,function(r) r$i,integer(1)))]
 winners<-lapply(Filter(function(r) r$score==ranked[[1L]]$score,ranked),function(r) state$players[[r$i]]$id)
 list(status=if(t) "terminal" else if(d$phase=="review") "awaiting_adjudication" else "ongoing",result_kind=if(t) {if(length(ranked)>0) "ranking" else "no_contest"} else "none",winners=winners,losers=if(length(winners)>0) lapply(Filter(function(p) !p$id %in% unlist(winners),state$players),function(p) p$id) else list(),placements=lapply(ranked,function(r) list(player_id=state$players[[r$i]]$id,place=1L+sum(vapply(ranked,function(other) other$score<r$score,logical(1))),strokes=r$score)),reason=if(t) "round_confirmed" else if(d$phase=="review") "scorecard_review" else "ongoing",evidence_required=if(d$phase=="review") list("Official scorecard confirmation.") else list(),rule_id=if(t) paste0(pack$ruleset_id,"@",pack$ruleset_version) else NULL)
}
