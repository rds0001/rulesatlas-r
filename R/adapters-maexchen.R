.ra_maexchen_values <- c(31,32,41,42,43,51,52,53,54,61,62,63,64,65,11,22,33,44,55,66,21)
.ra_maexchen_next <- function(d,t) {for(i in 1:3) {p<-(t+i)%%3;if(d$lives[[p+1L]]>0) return(p)}}
.ra_maexchen_validate <- function(d) {
  alive<-which(unlist(d$lives)>0)-1L
  if(!length(alive) || !d$turn %in% alive || (d$phase=="finished")!=(length(alive)==1)) .ra_fail("Lives, turn and phase mismatch",code="INVALID_STATE")
  bid<-d$bid
  if(!is.null(bid) && (!bid$claim %in% .ra_maexchen_values || !bid$actual %in% .ra_maexchen_values || !bid$player %in% alive || d$turn!=.ra_maexchen_next(d,bid$player))) .ra_fail("Invalid claim or responding player",code="INVALID_STATE")
  if((d$phase=="respond" && is.null(bid)) || (d$phase %in% c("roll","finished") && !is.null(bid))) .ra_fail("Claim and phase mismatch",code="INVALID_STATE")
  if(length(d$dice)!=if(d$phase=="claim") 2 else 0) .ra_fail("Only the claiming player holds a fresh roll",code="INVALID_STATE")
  if(d$phase=="claim" && !is.null(bid) && bid$claim==21) .ra_fail("Maexchen must be challenged",code="INVALID_STATE")
}
.ra_maexchen_apply <- function(state,action) {
  d<-state$data;p<-action$params;kind<-action$type;t<-d$turn
  if(!is.null(action$actor) && action$actor!=state$players[[t+1L]]$id) .ra_fail("Wrong actor")
  if(kind=="roll") {
    .ra_fields(p,list(dice=list(type="array",minItems=2,maxItems=2,items=list(type="integer",minimum=1,maximum=6))),"/params")
    if(!d$phase %in% c("roll","respond") || (!is.null(d$bid) && d$bid$claim==21)) .ra_fail("Cannot accept and reroll in this phase")
    d$dice<-p$dice;d$phase<-"claim";return(d)
  }
  if(kind=="claim") {
    .ra_fields(p,list(value=list(type="integer",minimum=11,maximum=66)),"/params");v<-p$value
    if(d$phase!="claim" || !v %in% .ra_maexchen_values || (!is.null(d$bid) && match(v,.ra_maexchen_values)<=match(d$bid$claim,.ra_maexchen_values))) .ra_fail("Claim must strictly increase in dice rank")
    d$bid<-list(player=t,claim=v,actual=10*max(unlist(d$dice))+min(unlist(d$dice)));d$dice<-list();d$turn<-.ra_maexchen_next(d,t);d$phase<-"respond";return(d)
  }
  if(kind!="challenge") .ra_fail("Unknown action")
  .ra_fields(p,structure(list(),names=character()),"/params")
  if(d$phase!="respond") .ra_fail("Only the next player may challenge before accepting the dice")
  bid<-d$bid;loser<-if(match(bid$actual,.ra_maexchen_values)>=match(bid$claim,.ra_maexchen_values)) t else bid$player;penalty<-if(bid$claim==21) 2 else 1
  d$lives[[loser+1L]]<-max(0,d$lives[[loser+1L]]-penalty);d$turn<-if(d$lives[[loser+1L]]>0) loser else .ra_maexchen_next(d,loser);d["bid"]<-list(NULL);d$phase<-if(sum(unlist(d$lives)>0)==1) "finished" else "roll";d
}
.ra_maexchen_legal <- function(state) {
  d<-state$data;bid<-d$bid
  if(d$phase=="claim") return(lapply(.ra_maexchen_values[if(is.null(bid)) rep(TRUE,21) else seq_along(.ra_maexchen_values)>match(bid$claim,.ra_maexchen_values)],function(v) list(type="claim",params=list(value=v))))
  actions<-if(d$phase=="respond") list(list(type="challenge",params=structure(list(),names=character()))) else list();types<-list()
  if(is.null(bid) || bid$claim!=21) types<-list(list(type="roll",params_schema=list(dice=list(type="array",minItems=2,maxItems=2,items=list(type="integer",minimum=1,maximum=6))),preconditions=list("Rolling accepts the previous claim and prevents challenging it."),external_observations=list("Two secret dice supplied by the current player to the host.")))
  list(mode="event_types",actions=actions,action_types=types,constraints=list(player=d$turn),completeness=FALSE,reason="externally_observed_dice")
}
.ra_maexchen_outcome <- function(state,pack) {
  d<-state$data;terminal<-d$phase=="finished";wins<-if(terminal) which(unlist(d$lives)>0) else integer()
  list(status=if(terminal) "terminal" else "ongoing",result_kind=if(terminal) "win" else "none",winners=lapply(wins,function(p) state$players[[p]]$id),losers=if(terminal) lapply(setdiff(1:3,wins),function(p) state$players[[p]]$id) else list(),placements=list(),reason=if(terminal) "last_player_with_lives" else "ongoing",evidence_required=list(),rule_id=if(terminal) paste0(pack$ruleset_id,"@",pack$ruleset_version) else NULL)
}
