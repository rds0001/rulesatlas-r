.ra_liars_next <- function(d,p) {for(i in 1:3) {t<-(p+i)%%3;if(d$counts[[t+1L]]>0) return(t)}}
.ra_liars_dice_validate <- function(d) {
  active<-which(unlist(d$counts)>0)-1L
  if(!length(active) || (d$phase=="finished")!=(length(active)==1)) .ra_fail("Remaining dice and game phase mismatch",code="INVALID_STATE")
  if(!d$starter %in% active || !d$turn %in% active) .ra_fail("Current and starting players must have dice",code="INVALID_STATE")
  if(d$phase=="bidding") {
    if(any(lengths(d$dice)!=unlist(d$counts))) .ra_fail("Roll sizes must match remaining dice",code="INVALID_STATE")
    bid<-d$bid
    if(is.null(bid)) {if(d$turn!=d$starter) .ra_fail("Starter must make the opening bid",code="INVALID_STATE")} else if(!bid$player %in% active || bid$quantity>sum(unlist(d$counts)) || d$turn!=.ra_liars_next(d,bid$player)) .ra_fail("Bid and current player mismatch",code="INVALID_STATE")
  } else if(sum(lengths(d$dice))>0 || !is.null(d$bid) || d$turn!=d$starter) .ra_fail("Old bids or dice remain outside a round",code="INVALID_STATE")
}
.ra_liars_dice_apply <- function(state,action) {
  d<-state$data;p<-action$params;kind<-action$type;t<-d$turn
  if(d$phase=="awaiting_roll") {
    if(kind!="roll_round") .ra_fail("Supply every remaining die for the round")
    .ra_fields(p,list(dice=list(type="array",minItems=3,maxItems=3,items=list(type="array",minItems=0,maxItems=5,items=list(type="integer",minimum=1,maximum=6)))),"/params")
    if(any(lengths(p$dice)!=unlist(d$counts))) .ra_fail("Roll sizes must match remaining dice")
    d$dice<-p$dice;d$phase<-"bidding";return(d)
  }
  if(!is.null(action$actor) && action$actor!=state$players[[t+1L]]$id) .ra_fail("Wrong actor")
  if(kind=="bid") {
    .ra_fields(p,list(quantity=list(type="integer",minimum=1,maximum=sum(unlist(d$counts))),face=list(type="integer",minimum=1,maximum=6)),"/params")
    if(!is.null(d$bid) && (p$quantity<d$bid$quantity || (p$quantity==d$bid$quantity && p$face<=d$bid$face))) .ra_fail("Bid must increase quantity or face at equal quantity")
    d$bid<-c(p,list(player=t));d$turn<-.ra_liars_next(d,t);return(d)
  }
  if(kind!="challenge") .ra_fail("Unknown action")
  .ra_fields(p,structure(list(),names=character()),"/params")
  if(is.null(d$bid)) .ra_fail("There is no bid to challenge")
  bid<-d$bid;actual<-sum(unlist(d$dice)==bid$face);loser<-if(actual>=bid$quantity) t else bid$player;d$counts[[loser+1L]]<-d$counts[[loser+1L]]-1L
  d$starter<-if(d$counts[[loser+1L]]>0) loser else .ra_liars_next(d,loser);d$turn<-d$starter;d$dice<-rep(list(list()),3);d["bid"]<-list(NULL);d$phase<-if(sum(unlist(d$counts)>0)==1) "finished" else "awaiting_roll";d
}
.ra_liars_dice_legal <- function(state) {
  d<-state$data
  if(d$phase=="awaiting_roll") return(list(mode="event_types",actions=list(),action_types=list(list(type="roll_round",params_schema=list(dice=list(type="array",minItems=3,maxItems=3,items=list(type="array",minItems=0,maxItems=5,items=list(type="integer",minimum=1,maximum=6)))),preconditions=list("Each player supplies exactly their remaining number of dice; eliminated players supply an empty list."),external_observations=list("Private dice results supplied to the authoritative host."))),constraints=list(counts=d$counts),completeness=FALSE,reason="external_round_rolls"))
  bid<-d$bid;actions<-list()
  for(q in seq_len(sum(unlist(d$counts)))) for(f in 1:6) if(is.null(bid) || q>bid$quantity || (q==bid$quantity && f>bid$face)) actions[[length(actions)+1L]]<-list(type="bid",params=list(quantity=q,face=f))
  if(!is.null(bid)) actions[[length(actions)+1L]]<-list(type="challenge",params=structure(list(),names=character()))
  actions
}
.ra_liars_dice_outcome <- function(state,pack) {
  d<-state$data;terminal<-d$phase=="finished";wins<-if(terminal) which(unlist(d$counts)>0) else integer()
  list(status=if(terminal) "terminal" else "ongoing",result_kind=if(terminal) "win" else "none",winners=lapply(wins,function(p) state$players[[p]]$id),losers=if(terminal) lapply(setdiff(1:3,wins),function(p) state$players[[p]]$id) else list(),placements=list(),reason=if(terminal) "last_player_with_dice" else "ongoing",evidence_required=list(),rule_id=if(terminal) paste0(pack$ruleset_id,"@",pack$ruleset_version) else NULL)
}
