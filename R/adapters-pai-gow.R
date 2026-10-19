.ra_pai_gow_ranks <- function(cards,low) {low<-unlist(low);list(.ra_pai_five(cards[-(low+1L)]),.ra_pai_two(cards[low+1L]))}
.ra_pai_gow_valid <- function(cards,low) {
  low<-unlist(low)
  if(length(low)!=2 || anyDuplicated(low)) return(FALSE)
  ranks<-.ra_pai_gow_ranks(cards,low);.ra_poker_compare(ranks[[1]],ranks[[2]])>=0
}
.ra_pai_gow_validate <- function(d) {
  phase<-d$phase
  if(phase=="betting") {
    if(!is.null(d$stake) || length(d$player) || length(d$banker) || !is.null(d$player_low) || !is.null(d$banker_low)) .ra_fail("Betting must precede cards and settings",code="INVALID_STATE")
    return(invisible(NULL))
  }
  if(is.null(d$stake) || d$stake%%20!=0) .ra_fail("Stake must be a multiple of twenty",code="INVALID_STATE")
  if(phase=="awaiting_deal") {
    if(length(d$player) || length(d$banker) || !is.null(d$player_low) || !is.null(d$banker_low)) .ra_fail("No cards or settings before the deal",code="INVALID_STATE")
    return(invisible(NULL))
  }
  cards<-c(unlist(d$player),unlist(d$banker))
  if(length(d$player)!=7 || length(d$banker)!=7 || anyDuplicated(cards)) .ra_fail("Two seven-card hands must contain distinct cards",code="INVALID_STATE")
  if((!is.null(d$player_low))!=(phase %in% c("banker","settled")) || (!is.null(d$banker_low))!=(phase=="settled")) .ra_fail("Hand setting and phase mismatch",code="INVALID_STATE")
  for(name in c("player","banker")) if(!is.null(d[[paste0(name,"_low")]]) && !.ra_pai_gow_valid(d[[name]],d[[paste0(name,"_low")]])) .ra_fail("Five-card high hand must rank at least as high as two-card low hand",code="INVALID_STATE")
}
.ra_pai_gow_apply <- function(state,action) {
  d<-state$data;p<-action$params;kind<-action$type
  if(d$phase=="betting") {
    if(kind!="bet") .ra_fail("Lock the wager first")
    .ra_fields(p,list(stake=list(type="integer",minimum=20,maximum=1000)),"/params")
    if(p$stake%%20!=0) .ra_fail("Stake must be a multiple of twenty for exact commission")
    d$stake<-p$stake;d$phase<-"awaiting_deal";return(d)
  }
  if(d$phase=="awaiting_deal") {
    if(kind!="deal") .ra_fail("Supply both seven-card hands")
    hand<-list(type="array",minItems=7,maxItems=7,items=list(type="integer",minimum=0,maximum=52));.ra_fields(p,list(player=hand,banker=hand),"/params")
    if(anyDuplicated(c(unlist(p$player),unlist(p$banker)))) .ra_fail("All fourteen cards must be distinct")
    d$player<-p$player;d$banker<-p$banker;d$phase<-"player";return(d)
  }
  expected<-if(d$phase=="player") "set_player" else "set_banker"
  if(kind!=expected) .ra_fail("Set the player first, then the banker")
  .ra_fields(p,list(low=list(type="array",minItems=2,maxItems=2,items=list(type="integer",minimum=0,maximum=6))),"/params");name<-d$phase
  if(!.ra_pai_gow_valid(d[[name]],p$low)) .ra_fail("Distinct low-card indices must leave a five-card hand at least as strong")
  d[[paste0(name,"_low")]]<-as.list(sort(unlist(p$low)));d$phase<-if(name=="player") "banker" else "settled";d
}
.ra_pai_gow_legal <- function(state) {
  d<-state$data;phase<-d$phase
  if(phase=="player") {
    pairs<-utils::combn(0:6,2,simplify=FALSE);pairs<-Filter(function(pair) .ra_pai_gow_valid(d$player,pair),pairs)
    return(lapply(pairs,function(pair) list(type="set_player",params=list(low=as.list(pair)))))
  }
  if(phase=="banker") return(list(mode="event_types",actions=list(),action_types=list(list(type="set_banker",params_schema=list(low=list(type="array",minItems=2,maxItems=2,items=list(type="integer",minimum=0,maximum=6))),preconditions=list("Distinct low-card indices; the remaining high hand must rank at least as high."),external_observations=list("Banker setting under the externally selected house way."))),constraints=structure(list(),names=character()),completeness=FALSE,reason="external_banker_setting"))
  if(phase=="betting") {spec<-list(stake=list(type="integer",minimum=20,maximum=1000));kind<-"bet"} else {
    hand<-list(type="array",minItems=7,maxItems=7,items=list(type="integer",minimum=0,maximum=52));spec<-list(player=hand,banker=hand);kind<-"deal"
  }
  list(mode="event_types",actions=list(),action_types=list(list(type=kind,params_schema=spec,preconditions=list("Stake is a multiple of twenty within the 1000-chip budget; both seven-card hands are distinct."),external_observations=list("Stake or externally dealt hands."))),constraints=structure(list(),names=character()),completeness=FALSE,reason="external_wager_and_cards")
}
.ra_pai_gow_outcome <- function(state,pack) {
  d<-state$data;done<-d$phase=="settled";net<-0
  if(done) {
    player<-.ra_pai_gow_ranks(d$player,d$player_low);banker<-.ra_pai_gow_ranks(d$banker,d$banker_low);won<-sum(vapply(1:2,function(i) .ra_poker_compare(player[[i]],banker[[i]])>0,logical(1)));net<-if(won==2) d$stake*19/20 else if(won==1) 0 else -d$stake
  }
  pid<-state$players[[1]]$id
  list(status=if(done) "terminal" else "ongoing",result_kind=if(done) if(net>0) "win" else if(net==0) "draw" else "other" else "none",winners=if(done && net>0) list(pid) else list(),losers=if(done && net<0) list(pid) else list(),placements=if(done) list(list(player_id=pid,place=1,balance=1000+net,net=net)) else list(),reason=if(done) "both_hands_compared_banker_wins_copies" else "ongoing",evidence_required=list(),rule_id=if(done) paste0(pack$ruleset_id,"@",pack$ruleset_version) else NULL)
}
