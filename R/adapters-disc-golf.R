.ra_dg_card<-function() list(player=.ra_golf_player,scores=list(type="array",minItems=18,maxItems=18,items=.ra_golf_i(1,1000,TRUE)),total=.ra_golf_i(1,18000,TRUE))
.ra_dg_adjust<-function() list(player=.ra_golf_player,delta=.ra_golf_i(-1000,1000))
.ra_dg_elapse<-function() list(milliseconds=.ra_golf_i(1,1800001))
.ra_dg_done<-function(d) all(vapply(d$golfers,function(r) r$status %in% c("card","returned","dns","dnf","dq"),logical(1)))
.ra_dg_validate<-function(d) {
 .ra_golf_validate(d)
 if(.ra_dg_done(d)!=!is.null(d$card_elapsed_ms)) .ra_fail("Scorecard clock starts when the whole group finishes",code="INVALID_STATE")
 for(r in d$golfers) {
  if(r$status=="returned" && (!identical(unlist(r$card),unlist(r$holes)) || sum(unlist(r$card))+r$round_adjustment+2*(r$card_error+r$late_card)<0)) .ra_fail("Disc golf uses the corrected card plus explicit round penalties",code="INVALID_STATE")
  if(r$card_error && is.null(r$card)) .ra_fail("Card error exists before a card was submitted",code="INVALID_STATE")
  if(r$late_card && (is.null(d$card_elapsed_ms) || d$card_elapsed_ms<=1800000)) .ra_fail("Late-card penalty precedes the deadline",code="INVALID_STATE")
 }
}
.ra_dg_apply<-function(state,action) {
 d<-state$data;k<-action$type;p<-action$params
 if(k=="return_card") {
  .ra_fields(p,.ra_dg_card(),"/params");j<-p$player+1L;r<-d$golfers[[j]]
  if(r$status!="card" || !.ra_dg_done(d)) .ra_fail("Finish the group round before returning the independent scorecard")
  r$card_error<-any(vapply(p$scores,is.null,logical(1))) || !identical(unlist(p$scores),unlist(r$holes)) || is.null(p$total) || p$total!=sum(unlist(r$holes));r$card<-r$holes;r$status<-"returned";d$golfers[[j]]<-r
  d$phase<-if(.ra_golf_settled(d)) "review" else "round"
 } else if(k=="elapse_card_window") {
  .ra_fields(p,.ra_dg_elapse(),"/params")
  if(is.null(d$card_elapsed_ms) || d$card_elapsed_ms+p$milliseconds>1800001) .ra_fail("Advance the scorecard clock only up to the first expired millisecond")
  d$card_elapsed_ms<-d$card_elapsed_ms+p$milliseconds
  if(d$card_elapsed_ms>1800000) for(j in 1:4) if(d$golfers[[j]]$status=="card") d$golfers[[j]]$late_card<-TRUE
 } else if(k=="adjust_penalty") {
  .ra_fields(p,.ra_dg_adjust(),"/params");j<-p$player+1L;r<-d$golfers[[j]]
  if(r$status!="returned" || p$delta==0) .ra_fail("Director adjustment requires a returned card and a nonzero correction")
  r$round_adjustment<-r$round_adjustment+p$delta
  if(abs(r$round_adjustment)>1000 || sum(unlist(r$card))+r$round_adjustment+2*(r$card_error+r$late_card)<0) .ra_fail("Adjusted total is outside the supported score range")
  d$golfers[[j]]<-r
 } else if(k=="unknown_penalty") .ra_fail("Use the director penalty adjustment for a returned disc-golf card") else d<-.ra_golf_apply(state,action)
 if(.ra_dg_done(d) && is.null(d$card_elapsed_ms)) d$card_elapsed_ms<-0L
 d
}
.ra_dg_legal<-function(state) {
 d<-state$data;res<-.ra_golf_legal(state);types<-list()
 for(item in res$action_types) {
  if(item$type=="unknown_penalty") next
  if(item$type=="return_card") item$params_schema<-.ra_dg_card()
  types[[length(types)+1L]]<-item
 }
 if(d$phase!="finished") {
  if(!is.null(d$card_elapsed_ms) && d$card_elapsed_ms<1800001) types[[length(types)+1L]]<-list(type="elapse_card_window",params_schema=.ra_dg_elapse(),preconditions=list("The group has completed its round."),external_observations=list("Elapsed time since group completion."))
  if(any(vapply(d$golfers,function(r) r$status=="returned",logical(1)))) types[[length(types)+1L]]<-list(type="adjust_penalty",params_schema=.ra_dg_adjust(),preconditions=list("An official adjustment applies to an already returned card."),external_observations=list("Tournament director penalty addition or removal."))
 }
 res$action_types<-types;res$constraints["card_elapsed_ms"]<-list(d$card_elapsed_ms);res$reason<-"official_disc_golf_round";res
}
.ra_dg_outcome<-function(state,pack) {
 d<-state$data;t<-d$phase=="finished";ranked<-list()
 if(t) for(i in 1:4) {r<-d$golfers[[i]];if(r$status=="returned") ranked[[length(ranked)+1L]]<-list(score=sum(unlist(r$card))+r$round_adjustment+2*(r$card_error+r$late_card),i=i)}
 if(length(ranked)>0) ranked<-ranked[order(vapply(ranked,function(r) r$score,numeric(1)),vapply(ranked,function(r) r$i,integer(1)))]
 winners<-lapply(Filter(function(r) r$score==ranked[[1L]]$score,ranked),function(r) state$players[[r$i]]$id)
 list(status=if(t) "terminal" else if(d$phase=="review") "awaiting_adjudication" else "ongoing",result_kind=if(t) {if(length(ranked)>0) "ranking" else "no_contest"} else "none",winners=winners,losers=if(length(winners)>0) lapply(Filter(function(p) !p$id %in% unlist(winners),state$players),function(p) p$id) else list(),placements=lapply(ranked,function(r) list(player_id=state$players[[r$i]]$id,place=1L+sum(vapply(ranked,function(other) other$score<r$score,logical(1))),throws=r$score)),reason=if(t) "round_confirmed" else if(d$phase=="review") "scorecard_review" else "ongoing",evidence_required=if(d$phase=="review") list("Official scorecard confirmation.") else list(),rule_id=if(t) paste0(pack$ruleset_id,"@",pack$ruleset_version) else NULL)
}
