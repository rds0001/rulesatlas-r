.ra_sic_bo_kinds <- as.list(c("single","double","triple","combination","total","any_triple","big","small"))
.ra_sic_bo_bet <- list(kind=list(type="string",enum=.ra_sic_bo_kinds),numbers=list(type="array",minItems=0,maxItems=2,items=list(type="integer",minimum=1,maximum=17)),stake=list(type="integer",minimum=1,maximum=1000))
.ra_sic_bo_selection <- function(kind,numbers) {
  n<-sort(as.numeric(unlist(numbers)))
  if(kind %in% c("single","double","triple") && length(n)==1 && n[1]>=1 && n[1]<=6) return(TRUE)
  if(kind=="combination" && length(n)==2 && n[1]>=1 && n[1]<n[2] && n[2]<=6) return(TRUE)
  if(kind=="total" && length(n)==1 && n[1]>=4 && n[1]<=17) return(TRUE)
  if(kind %in% c("any_triple","big","small") && !length(n)) return(TRUE)
  NULL
}
.ra_sic_bo_payout <- function(b,dice) {
  dice<-unlist(dice);k<-b$kind;n<-unlist(b$numbers);triple<-length(unique(dice))==1;total<-sum(dice);odds<-NULL
  if(k=="single") {count<-sum(dice==n[1]);if(count>0) odds<-count} else if(k=="double" && sum(dice==n[1])>=2) odds<-10 else if(k=="triple" && sum(dice==n[1])==3) odds<-180 else if(k=="combination" && all(n %in% dice)) odds<-5 else if(k=="total" && n[1]==total) odds<-c(50,18,14,12,8,6,6,6,6,8,12,14,18,50)[total-3L] else if(k=="any_triple" && triple) odds<-30 else if(k=="big" && !triple && total>=11 && total<=17) odds<-1 else if(k=="small" && !triple && total>=4 && total<=10) odds<-1
  b$stake*(if(is.null(odds)) -1 else odds)
}
.ra_sic_bo_total <- function(bets) sum(vapply(bets,function(b) b$stake,numeric(1)))
.ra_sic_bo_validate <- function(d) {
  if(.ra_sic_bo_total(d$bets)>1000) .ra_fail("Bets exceed the chip budget",code="INVALID_STATE")
  if(any(vapply(d$bets,function(b) is.null(.ra_sic_bo_selection(b$kind,b$numbers)),logical(1)))) .ra_fail("Bet does not match the bet form",code="INVALID_STATE")
  if((d$phase=="settled")!=!is.null(d$result)) .ra_fail("Result and betting phase mismatch",code="INVALID_STATE")
  if(d$phase!="betting" && !length(d$bets)) .ra_fail("A closed round must contain a bet",code="INVALID_STATE")
}
.ra_sic_bo_apply <- function(state,action) {
  d<-state$data;p<-action$params;kind<-action$type
  if(d$phase=="betting") {
    if(kind=="bet") {
      .ra_fields(p,.ra_sic_bo_bet,"/params")
      if(is.null(.ra_sic_bo_selection(p$kind,p$numbers))) .ra_fail("Bet does not match the bet form")
      if(.ra_sic_bo_total(d$bets)+p$stake>1000) .ra_fail("Bets exceed the chip budget")
      d$bets[[length(d$bets)+1L]]<-p;return(d)
    }
    if(kind=="cancel") {
      .ra_fields(p,list(index=list(type="integer",minimum=0,maximum=max(0,length(d$bets)-1))),"/params")
      if(!length(d$bets)) .ra_fail("There is no bet to cancel")
      d$bets<-d$bets[-(p$index+1L)];return(d)
    }
    if(kind=="close_bets") {
      .ra_fields(p,structure(list(),names=character()),"/params")
      if(!length(d$bets)) .ra_fail("Place at least one bet")
      d$phase<-"closed";return(d)
    }
    .ra_fail("Action is not permitted while betting")
  }
  if(kind!="settle") .ra_fail("Only settlement is permitted after betting closes")
  .ra_fields(p,list(dice=list(type="array",minItems=3,maxItems=3,items=list(type="integer",minimum=1,maximum=6))),"/params");d$result<-p$dice;d$phase<-"settled";d
}
.ra_sic_bo_legal <- function(state) {
  d<-state$data
  if(d$phase=="betting") {
    schemas<-list(bet=.ra_sic_bo_bet)
    if(length(d$bets)) schemas<-c(schemas,list(cancel=list(index=list(type="integer",minimum=0,maximum=length(d$bets)-1)),close_bets=structure(list(),names=character())))
  } else schemas<-list(settle=list(dice=list(type="array",minItems=3,maxItems=3,items=list(type="integer",minimum=1,maximum=6))))
  types<-lapply(names(schemas),function(k) list(type=k,params_schema=schemas[[k]],preconditions=list("The betting phase, bet selection and available chip budget must permit the action."),external_observations=list("Player bet selection or the official three-dice result.")))
  list(mode="event_types",actions=list(),action_types=types,constraints=list(available_chips=1000-.ra_sic_bo_total(d$bets)),completeness=FALSE,reason="guarded_bets_and_external_dice_result")
}
.ra_sic_bo_outcome <- function(state,pack) {
  d<-state$data;terminal<-d$phase=="settled";net<-0
  if(terminal) for(b in d$bets) {net<-net+.ra_sic_bo_payout(b,d$result)}
  list(status=if(terminal) "terminal" else "ongoing",result_kind=if(terminal) {if(net>0) "win" else if(net==0) "draw" else "other"} else "none",winners=if(terminal && net>0) list(state$players[[1]]$id) else list(),losers=if(terminal && net<0) list(state$players[[1]]$id) else list(),placements=if(terminal) list(list(player_id=state$players[[1]]$id,place=1,balance=1000+net,net=net)) else list(),reason=if(terminal) "round_settled" else "ongoing",evidence_required=list(),rule_id=if(terminal) paste0(pack$ruleset_id,"@",pack$ruleset_version) else NULL)
}
