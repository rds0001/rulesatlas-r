.ra_sumo_decision <- list(winner=list(type="integer",minimum=0,maximum=1),reason=list(type="string",enum=list("out_of_ring","ground_contact","forbidden_action","forfeit")))
.ra_sumo_validate <- function(d) {if((d$phase %in% c("provisional","review","finished"))!=!is.null(d$decision)) .ra_fail("Decision and bout phase mismatch",code="INVALID_STATE")}
.ra_sumo_apply <- function(state,action) {
  d<-state$data;p<-action$params;kind<-action$type;phase<-d$phase
  if(kind %in% c("declare","overturn")) {
    .ra_fields(p,.ra_sumo_decision,"/params")
    if(kind=="overturn") {if(phase!="review") .ra_fail("Overturning requires an official review")} else if(!phase %in% c("ready","active","paused") || (p$reason %in% c("out_of_ring","ground_contact") && phase!="active")) .ra_fail("This decision is not permitted in the current bout phase")
    d$decision<-p;d$phase<-if(kind=="declare") "provisional" else "review";return(d)
  }
  .ra_fields(p,structure(list(),names=character()),"/params")
  transitions<-list(start=c("ready","active"),pause=c("active","paused"),resume=c("paused","active"),request_review=c("provisional","review"))
  if(kind %in% names(transitions)) {transition<-transitions[[kind]];if(phase!=transition[1]) .ra_fail("Action is not permitted in this bout phase");d$phase<-transition[2];return(d)}
  if(kind=="false_start") {if(phase!="active") .ra_fail("A false-start ruling requires an active start");d$false_starts<-d$false_starts+1L;d$phase<-"ready";return(d)}
  if(kind=="rematch") {if(phase!="review") .ra_fail("A rematch requires an official review");d$rematches<-d$rematches+1L;d["decision"]<-list(NULL);d$phase<-"ready";return(d)}
  if(kind=="confirm") {if(!phase %in% c("provisional","review")) .ra_fail("There is no decision to confirm");d$phase<-"finished";return(d)}
  .ra_fail("Unknown action")
}
.ra_sumo_legal <- function(state) {
  phase<-state$data$phase;empty<-structure(list(),names=character())
  schemas<-switch(phase,ready=list(start=empty,declare=.ra_sumo_decision),active=list(false_start=empty,pause=empty,declare=.ra_sumo_decision),paused=list(resume=empty,declare=.ra_sumo_decision),provisional=list(request_review=empty,confirm=empty),review=list(overturn=.ra_sumo_decision,rematch=empty,confirm=empty))
  types<-lapply(names(schemas),function(k) list(type=k,params_schema=schemas[[k]],preconditions=list("Physical winning observations require active play; review actions require a provisional decision."),external_observations=list("Official start, false start, interruption, winning ruling or judges\u2019 review.")))
  list(mode="event_types",actions=list(),action_types=types,constraints=list(phase=phase),completeness=FALSE,reason="externally_adjudicated_bout")
}
.ra_sumo_outcome <- function(state,pack) {
  d<-state$data;terminal<-d$phase=="finished";w<-if(terminal) d$decision$winner+1L else NULL;reason<-if(terminal) d$decision$reason else if(d$phase %in% c("provisional","review")) "awaiting_confirmation" else "ongoing"
  list(status=if(terminal) "terminal" else "ongoing",result_kind=if(terminal) {if(reason=="forfeit") "forfeit" else if(reason=="forbidden_action") "disqualification" else "win"} else "none",winners=if(terminal) list(state$players[[w]]$id) else list(),losers=if(terminal) list(state$players[[3L-w]]$id) else list(),placements=list(),reason=reason,evidence_required=list(),rule_id=if(terminal) paste0(pack$ruleset_id,"@",pack$ruleset_version) else NULL)
}
