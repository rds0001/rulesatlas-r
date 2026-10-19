.ra_roulette_red <- c(1,3,5,7,9,12,14,16,18,19,21,23,25,27,30,32,34,36)
.ra_roulette_kinds <- as.list(c("straight","split","street","trio","corner","first_four","six_line","dozen","column","red","black","odd","even","low","high"))
.ra_roulette_bet <- list(kind=list(type="string",enum=.ra_roulette_kinds),numbers=list(type="array",minItems=0,maxItems=6,items=list(type="integer",minimum=0,maximum=36)),stake=list(type="integer",minimum=1,maximum=1000))
.ra_roulette_selection <- function(kind,numbers) {
  n<-sort(as.numeric(unlist(numbers)));len<-length(n);answer<-function(values,odds) list(numbers=values,odds=odds)
  if(anyDuplicated(n)) return(NULL)
  if(kind=="straight" && len==1) return(answer(n,35))
  if(kind=="split" && len==2) {a<-n[1];b<-n[2];if((a==0 && b %in% 1:3) || (a>0 && (b-a==3 || (b-a==1 && (a-1)%/%3==(b-1)%/%3)))) return(answer(n,17))}
  if(kind=="street" && len==3 && n[1]%%3==1 && all(n==seq(n[1],n[1]+2))) return(answer(n,11))
  if(kind=="trio" && len==3 && (all(n==c(0,1,2)) || all(n==c(0,2,3)))) return(answer(n,11))
  if(kind=="corner" && len==4 && n[1]>0 && n[1]%%3 %in% c(1,2) && all(n==c(n[1],n[1]+1,n[1]+3,n[1]+4))) return(answer(n,8))
  if(kind=="first_four" && len==0) return(answer(0:3,8))
  if(kind=="six_line" && len==6 && n[1]%%3==1 && all(n==seq(n[1],n[1]+5))) return(answer(n,5))
  if(kind=="dozen" && len==1 && n[1] %in% 1:3) return(answer(seq((n[1]-1)*12+1,n[1]*12),2))
  if(kind=="column" && len==1 && n[1] %in% 1:3) return(answer(seq(n[1],36,by=3),2))
  if(len>0) return(NULL)
  groups<-list(red=.ra_roulette_red,black=setdiff(1:36,.ra_roulette_red),odd=seq(1,36,by=2),even=seq(2,36,by=2),low=1:18,high=19:36)
  if(kind %in% names(groups)) answer(groups[[kind]],1) else NULL
}
.ra_roulette_total <- function(bets) sum(vapply(bets,function(b) b$stake,numeric(1)))
.ra_roulette_validate <- function(d) {
  if(.ra_roulette_total(d$bets)>1000) .ra_fail("Bets exceed the chip budget",code="INVALID_STATE")
  if(any(vapply(d$bets,function(b) is.null(.ra_roulette_selection(b$kind,b$numbers)),logical(1)))) .ra_fail("Bet does not match the wheel layout",code="INVALID_STATE")
  if((d$phase=="settled")!=!is.null(d$result)) .ra_fail("Result and betting phase mismatch",code="INVALID_STATE")
  if(d$phase!="betting" && !length(d$bets)) .ra_fail("A closed round must contain a bet",code="INVALID_STATE")
}
.ra_roulette_apply <- function(state,action) {
  d<-state$data;p<-action$params;kind<-action$type
  if(d$phase=="betting") {
    if(kind=="bet") {
      .ra_fields(p,.ra_roulette_bet,"/params")
      if(is.null(.ra_roulette_selection(p$kind,p$numbers))) .ra_fail("Bet does not match the wheel layout")
      if(.ra_roulette_total(d$bets)+p$stake>1000) .ra_fail("Bets exceed the chip budget")
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
  .ra_fields(p,list(number=list(type="integer",minimum=0,maximum=36)),"/params");d$result<-p$number;d$phase<-"settled";d
}
.ra_roulette_legal <- function(state) {
  d<-state$data
  if(d$phase=="betting") {
    schemas<-list(bet=.ra_roulette_bet)
    if(length(d$bets)) schemas<-c(schemas,list(cancel=list(index=list(type="integer",minimum=0,maximum=length(d$bets)-1)),close_bets=structure(list(),names=character())))
  } else schemas<-list(settle=list(number=list(type="integer",minimum=0,maximum=36)))
  types<-lapply(names(schemas),function(k) list(type=k,params_schema=schemas[[k]],preconditions=list("The betting phase, layout selection and available chip budget must permit the action."),external_observations=list("Player bet selection or the official single-zero wheel result.")))
  list(mode="event_types",actions=list(),action_types=types,constraints=list(available_chips=1000-.ra_roulette_total(d$bets)),completeness=FALSE,reason="guarded_layout_bets_and_external_wheel_result")
}
.ra_roulette_outcome <- function(state,pack) {
  d<-state$data;terminal<-d$phase=="settled";net<-0
  if(terminal) for(b in d$bets) {selection<-.ra_roulette_selection(b$kind,b$numbers);net<-net+b$stake*(if(d$result %in% selection$numbers) selection$odds else -1)}
  list(status=if(terminal) "terminal" else "ongoing",result_kind=if(terminal) {if(net>0) "win" else if(net==0) "draw" else "other"} else "none",winners=if(terminal && net>0) list(state$players[[1]]$id) else list(),losers=if(terminal && net<0) list(state$players[[1]]$id) else list(),placements=if(terminal) list(list(player_id=state$players[[1]]$id,place=1,balance=1000+net,net=net)) else list(),reason=if(terminal) "round_settled" else "ongoing",evidence_required=list(),rule_id=if(terminal) paste0(pack$ruleset_id,"@",pack$ruleset_version) else NULL)
}
