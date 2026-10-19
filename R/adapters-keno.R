.ra_keno_bet <- list(picks=list(type="array",minItems=1,maxItems=20,items=list(type="integer",minimum=1,maximum=80)),stake=list(type="integer",minimum=1,maximum=1000),returns=list(type="array",minItems=2,maxItems=21,items=list(type="integer",minimum=0,maximum=1000000)))
.ra_keno_validate <- function(d) {
  if(anyDuplicated(unlist(d$drawn))) .ra_fail("Drawn numbers must be distinct",code="INVALID_STATE")
  if(is.null(d$bet)) {if(length(d$drawn)) .ra_fail("Drawing requires a locked bet",code="INVALID_STATE");return(invisible(NULL))}
  b<-d$bet
  if(anyDuplicated(unlist(b$picks)) || length(b$returns)!=length(b$picks)+1) .ra_fail("Picks must be distinct and the table must cover every hit count",code="INVALID_STATE")
}
.ra_keno_apply <- function(state,action) {
  d<-state$data;p<-action$params
  if(is.null(d$bet)) {
    if(action$type!="bet") .ra_fail("Supply a bet and return table first")
    .ra_fields(p,.ra_keno_bet,"/params")
    if(anyDuplicated(unlist(p$picks)) || length(p$returns)!=length(p$picks)+1) .ra_fail("Picks must be distinct and the table must cover every hit count")
    d$bet<-p;return(d)
  }
  if(action$type!="draw") .ra_fail("Only the next draw is permitted after the bet is locked")
  .ra_fields(p,list(number=list(type="integer",minimum=1,maximum=80)),"/params")
  if(p$number %in% unlist(d$drawn)) .ra_fail("Number was already drawn")
  d$drawn[[length(d$drawn)+1L]]<-p$number;d
}
.ra_keno_legal <- function(state) {
  d<-state$data
  if(!is.null(d$bet)) return(lapply(setdiff(1:80,unlist(d$drawn)),function(n) list(type="draw",params=list(number=n))))
  list(mode="event_types",actions=list(),action_types=list(list(type="bet",params_schema=.ra_keno_bet,preconditions=list("Distinct picks and a gross-return multiplier for each hit count from zero through the number of picks."),external_observations=list("Selected numbers, abstract-chip stake and the published payout table."))),constraints=list(available_chips=1000),completeness=FALSE,reason="external_payout_table_required")
}
.ra_keno_outcome <- function(state,pack) {
  d<-state$data;terminal<-length(d$drawn)==20;net<-0;hits<-0
  if(terminal) {b<-d$bet;hits<-length(intersect(unlist(b$picks),unlist(d$drawn)));net<-b$stake*(b$returns[[hits+1L]]-1)}
  list(status=if(terminal) "terminal" else "ongoing",result_kind=if(terminal) {if(net>0) "win" else if(net==0) "draw" else "other"} else "none",winners=if(terminal && net>0) list(state$players[[1]]$id) else list(),losers=if(terminal && net<0) list(state$players[[1]]$id) else list(),placements=if(terminal) list(list(player_id=state$players[[1]]$id,place=1,balance=1000+net,net=net,hits=hits)) else list(),reason=if(terminal) "twenty_numbers_drawn" else "ongoing",evidence_required=list(),rule_id=if(terminal) paste0(pack$ruleset_id,"@",pack$ruleset_version) else NULL)
}
