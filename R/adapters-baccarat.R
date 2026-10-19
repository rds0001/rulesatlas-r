.ra_baccarat_bet <- list(target=list(type="string",enum=list("player","banker","tie")),stake=list(type="integer",minimum=20,maximum=1000))
.ra_baccarat_next <- function(hands) {
  p<-hands[[1]];b<-hands[[2]]
  if(length(p)<2 || length(b)<2) return(if(length(p)==length(b)) 1L else 2L)
  pt<-sum(unlist(head(p,2)))%%10;bt<-sum(unlist(head(b,2)))%%10
  if(pt>=8 || bt>=8) return(NULL)
  if(length(p)==2 && pt<=5) return(1L)
  if(length(b)==3) return(NULL)
  if(length(p)==2) return(if(bt<=5) 2L else NULL)
  third<-p[[3]];draw<-bt<=2 || (bt==3 && third!=8) || (bt==4 && third>=2 && third<=7) || (bt==5 && third>=4 && third<=7) || (bt==6 && third %in% c(6,7))
  if(draw) 2L else NULL
}
.ra_baccarat_inspect <- function(cards) {
  hands<-list(list(),list())
  for(value in cards) {
    target<-.ra_baccarat_next(hands)
    if(is.null(target)) .ra_fail("Cards continued after the tableau ended",code="INVALID_STATE")
    hands[[target]][[length(hands[[target]])+1L]]<-value
  }
  list(hands=hands,target=.ra_baccarat_next(hands))
}
.ra_baccarat_validate <- function(d) {
  if(!is.null(d$bet) && d$bet$stake%%20!=0) .ra_fail("Stake must use twenty-chip units",code="INVALID_STATE")
  if(d$phase=="betting") {if(!is.null(d$bet) || length(d$cards)) .ra_fail("Betting state already contains a locked bet or cards",code="INVALID_STATE");return(invisible(NULL))}
  if(is.null(d$bet)) .ra_fail("A locked bet is required",code="INVALID_STATE")
  target<-.ra_baccarat_inspect(d$cards)$target
  if((d$phase=="settled")!=is.null(target)) .ra_fail("Phase does not match the drawing tableau",code="INVALID_STATE")
}
.ra_baccarat_apply <- function(state,action) {
  d<-state$data;p<-action$params
  if(d$phase=="betting") {
    if(action$type!="bet") .ra_fail("Lock a bet before dealing")
    .ra_fields(p,.ra_baccarat_bet,"/params")
    if(p$stake%%20!=0) .ra_fail("Stake must use twenty-chip units")
    d$bet<-p;d$phase<-"dealing";return(d)
  }
  if(action$type!="deal_card") .ra_fail("Only the required next card may be dealt")
  .ra_fields(p,list(hand=list(type="string",enum=list("player","banker")),value=list(type="integer",minimum=0,maximum=9)),"/params")
  target<-.ra_baccarat_inspect(d$cards)$target
  if(p$hand!=if(target==1) "player" else "banker") .ra_fail("Card was dealt to the wrong hand")
  d$cards[[length(d$cards)+1L]]<-p$value
  if(is.null(.ra_baccarat_inspect(d$cards)$target)) d$phase<-"settled"
  d
}
.ra_baccarat_legal <- function(state) {
  d<-state$data
  if(d$phase=="betting") {schemas<-list(bet=.ra_baccarat_bet);constraints<-list(available_chips=1000)} else {
    target<-.ra_baccarat_inspect(d$cards)$target;hand<-if(target==1) "player" else "banker"
    schemas<-list(deal_card=list(hand=list(type="string",enum=list(hand)),value=list(type="integer",minimum=0,maximum=9)));constraints<-list(next_hand=hand)
  }
  types<-lapply(names(schemas),function(k) list(type=k,params_schema=schemas[[k]],preconditions=list("Stakes use twenty-chip units; cards follow the mandatory punto banco tableau."),external_observations=list("A locked wager or the next officially dealt card value; tens and face cards count zero.")))
  list(mode="event_types",actions=list(),action_types=types,constraints=constraints,completeness=FALSE,reason="external_cards_mandatory_tableau")
}
.ra_baccarat_outcome <- function(state,pack) {
  d<-state$data;terminal<-d$phase=="settled";net<-0
  if(terminal) {
    hands<-.ra_baccarat_inspect(d$cards)$hands;p<-sum(unlist(hands[[1]]))%%10;b<-sum(unlist(hands[[2]]))%%10;winner<-if(p==b) "tie" else if(p>b) "player" else "banker";bet<-d$bet;stake<-bet$stake
    if(bet$target==winner) net<-if(winner=="tie") stake*8 else if(winner=="banker") stake*19/20 else stake else if(winner!="tie" || bet$target=="tie") net<- -stake
  }
  list(status=if(terminal) "terminal" else "ongoing",result_kind=if(terminal) {if(net>0) "win" else if(net==0) "draw" else "other"} else "none",winners=if(terminal && net>0) list(state$players[[1]]$id) else list(),losers=if(terminal && net<0) list(state$players[[1]]$id) else list(),placements=if(terminal) list(list(player_id=state$players[[1]]$id,place=1,balance=1000+net,net=net)) else list(),reason=if(terminal) "round_settled" else "ongoing",evidence_required=list(),rule_id=if(terminal) paste0(pack$ruleset_id,"@",pack$ruleset_version) else NULL)
}
