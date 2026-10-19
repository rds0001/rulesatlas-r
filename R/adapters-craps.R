.ra_ca_ratio <- function(d) {ratio<-if(d$point %in% c(4,10)) c(2,1) else if(d$point %in% c(5,9)) c(3,2) else c(6,5);if(d$side=="pass") ratio else rev(ratio)}
.ra_ca_max <- function(d) min(1000-d$stake,d$stake*(if(d$side=="dont_pass") 6 else if(d$point %in% c(4,10)) 3 else if(d$point %in% c(5,9)) 4 else 5))
.ra_ca_result <- function(d) {
 if(d$reason=="withdrawn_dont_pass") return(0)
 total<-sum(unlist(d$dice))
 if(is.null(d$point)) {
  won<-if(d$side=="pass") total %in% c(7,11) else total %in% c(2,3)
  if(d$side=="dont_pass" && total==12) return(0)
 } else won<-if(d$side=="pass") total==d$point else total==7
 if(!won) return(-d$stake-d$odds)
 d$stake+if(!is.null(d$point)) d$odds*.ra_ca_ratio(d)[1]/.ra_ca_ratio(d)[2] else 0
}
.ra_ca_validate <- function(d) {
 bad<-function(m) .ra_fail(m,code="INVALID_STATE")
 if(d$phase=="betting") {
  if(!is.null(d$point) || !is.null(d$dice) || d$odds || !is.null(d$reason) || is.null(d$side)!=(d$stake==0)) bad("Initial betting cannot contain a point, odds or rolled result")
  return(invisible(TRUE))
 }
 if(is.null(d$side) || d$stake==0) bad("A closed contract requires a line bet")
 if(is.null(d$point)) {if(d$odds || d$phase=="point_betting") bad("Free odds require an established point")} else if(is.null(d$dice) || d$odds>.ra_ca_max(d) || d$odds%%.ra_ca_ratio(d)[2]!=0) bad("Point contracts require a prior roll and exactly payable odds within the limit")
 if(d$phase=="settled") {
  if(d$reason=="withdrawn_dont_pass") {if(d$side!="dont_pass" || is.null(d$point)) bad("Only an established Dont Pass contract may be withdrawn")} else if(d$reason=="come_out") {if(!is.null(d$point) || is.null(d$dice) || !(sum(unlist(d$dice)) %in% c(2,3,7,11,12))) bad("Come-out settlement requires a natural or craps number")} else if(d$reason=="point_resolved") {if(is.null(d$point) || is.null(d$dice) || !(sum(unlist(d$dice)) %in% c(7,d$point))) bad("Resolve a point only on that point or seven")} else bad("Unknown settlement reason")
 } else if(!is.null(d$reason) || (is.null(d$point) && !is.null(d$dice))) bad("Ongoing come-out contract cannot already contain a result")
 invisible(TRUE)
}
.ra_ca_apply <- function(state,action) {
 d<-state$data;p<-action$params;k<-action$type
 if(k=="bet") {
  .ra_fields(p,list(side=list(type="string",enum=list("pass","dont_pass")),stake=list(type="integer",minimum=1,maximum=1000)),"/params")
  if(d$phase!="betting") .ra_fail("Select or replace the line bet before the first roll")
  d$side<-p$side;d$stake<-p$stake;return(d)
 }
 if(k=="cancel") {
  .ra_fields(p,list(),"/params");if(d$phase!="betting" || !d$stake) .ra_fail("Cancel only an unrolled line bet")
  d["side"]<-list(NULL);d$stake<-0L;return(d)
 }
 if(k=="close_roll") {
  .ra_fields(p,list(),"/params");if(!(d$phase %in% c("betting","point_betting")) || !d$stake) .ra_fail("Place a line bet and close the betting window before rolling")
  d$phase<-"await_roll";return(d)
 }
 if(k=="set_odds") {
  .ra_fields(p,list(stake=list(type="integer",minimum=0,maximum=1000)),"/params");n<-p$stake
  if(d$phase!="point_betting" || n>.ra_ca_max(d) || n%%.ra_ca_ratio(d)[2]!=0) .ra_fail("Change or remove odds only between point rolls, within 3-4-5x limits and exact chip increments")
  d$odds<-n;return(d)
 }
 if(k=="withdraw_dont_pass") {
  .ra_fields(p,list(),"/params");if(d$phase!="point_betting" || d$side!="dont_pass") .ra_fail("Only Dont Pass may withdraw its established line contract between rolls")
  d$phase<-"settled";d$reason<-"withdrawn_dont_pass";return(d)
 }
 if(k!="roll") .ra_fail("Unknown action")
 .ra_fields(p,list(dice=list(type="array",minItems=2,maxItems=2,items=list(type="integer",minimum=1,maximum=6))),"/params")
 if(d$phase!="await_roll") .ra_fail("Close betting before submitting the actual two-dice result")
 d$dice<-p$dice;n<-sum(unlist(d$dice))
 if(is.null(d$point)) {
  if(n %in% c(4,5,6,8,9,10)) {d$point<-n;d$phase<-"point_betting"} else {d$phase<-"settled";d$reason<-"come_out"}
 } else if(n %in% c(7,d$point)) {d$phase<-"settled";d$reason<-"point_resolved"} else d$phase<-"point_betting"
 d
}
.ra_ca_legal <- function(state) {
 d<-state$data;schemas<-list();empty<-setNames(list(),character())
 if(d$phase=="betting") {
  schemas$bet<-list(side=list(type="string",enum=list("pass","dont_pass")),stake=list(type="integer",minimum=1,maximum=1000))
  if(d$stake) {schemas$cancel<-empty;schemas$close_roll<-empty}
 } else if(d$phase=="point_betting") {
  schemas$set_odds<-list(stake=list(type="integer",minimum=0,maximum=.ra_ca_max(d)));schemas$close_roll<-empty
  if(d$side=="dont_pass") schemas$withdraw_dont_pass<-empty
 } else schemas$roll<-list(dice=list(type="array",minItems=2,maxItems=2,items=list(type="integer",minimum=1,maximum=6)))
 list(mode="event_types",actions=list(),action_types=lapply(names(schemas),function(k) list(type=k,params_schema=schemas[[k]],preconditions=list("Contract phase, chip budget and exact odds increments must permit the action."),external_observations=list("Player betting decision or actual two-dice result."))),constraints=list(point=d$point,line_stake=d$stake,odds_stake=d$odds,odds_increment=if(!is.null(d$point)) .ra_ca_ratio(d)[2] else NULL,maximum_odds=if(!is.null(d$point)) .ra_ca_max(d) else 0),completeness=FALSE,reason="guarded_contract_and_external_dice")
}
.ra_ca_outcome <- function(state,pack) {
 d<-state$data;done<-d$phase=="settled";net<-if(done) .ra_ca_result(d) else 0
 list(status=if(done) "terminal" else "ongoing",result_kind=if(done) if(net>0) "win" else if(net==0) "draw" else "other" else "none",winners=if(done && net>0) list(state$players[[1]]$id) else list(),losers=if(done && net<0) list(state$players[[1]]$id) else list(),placements=if(done) list(list(player_id=state$players[[1]]$id,place=1L,balance=1000+net,net=net)) else list(),reason=if(done) d$reason else "ongoing",evidence_required=list(),rule_id=if(done) paste0(pack$ruleset_id,"@",pack$ruleset_version) else NULL)
}
