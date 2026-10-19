.ra_mafia_faction <- function(d) {
  if(!length(d$roles)) return(NULL)
  mafia<-sum(unlist(d$alive) & unlist(d$roles)=="mafia");town<-sum(unlist(d$alive) & unlist(d$roles)=="town")
  if(mafia==0) "town" else if(mafia>=town) "mafia" else NULL
}
.ra_mafia_validate <- function(d) {
  if(d$phase=="setup") {if(length(d$roles)>0 || !all(unlist(d$alive)) || d$day!=0 || any(unlist(d$votes)!=-2)) .ra_fail("Unconfigured game contains play",code="INVALID_STATE");return(invisible(NULL))}
  if(length(d$roles)!=6 || sum(unlist(d$roles)=="mafia")!=2 || d$day<1) .ra_fail("Six players require two mafia and four townspeople",code="INVALID_STATE")
  if((d$phase=="finished")!=!is.null(.ra_mafia_faction(d))) .ra_fail("Faction result and phase mismatch",code="INVALID_STATE")
  if(d$phase!="vote") {if(any(unlist(d$votes)!=-2)) .ra_fail("Votes remain outside the voting phase",code="INVALID_STATE")} else {
    for(p in 1:6) {v<-d$votes[[p]];if((!d$alive[[p]] && v!=-2) || (v>=0 && !d$alive[[v+1L]])) .ra_fail("A dead participant cannot vote or receive a vote",code="INVALID_STATE")}
    if(all(unlist(d$votes)[unlist(d$alive)]!=-2)) .ra_fail("Completed votes must resolve",code="INVALID_STATE")
  }
}
.ra_mafia_apply <- function(state,action) {
  d<-state$data;p<-action$params;kind<-action$type
  if(d$phase=="setup") {
    if(kind!="assign_roles") .ra_fail("Assign roles first")
    .ra_fields(p,list(roles=list(type="array",minItems=6,maxItems=6,items=list(type="string",enum=list("mafia","town")))),"/params")
    if(sum(unlist(p$roles)=="mafia")!=2) .ra_fail("Exactly two mafia members are required")
    d$roles<-p$roles;d$phase<-"night";d$day<-1L;return(d)
  }
  if(d$phase=="night") {
    if(kind!="night_choice") .ra_fail("Resolve the night choice first")
    .ra_fields(p,list(target=list(type="integer",minimum=0,maximum=5,nullable=TRUE)),"/params");target<-p$target
    if(!is.null(action$actor) && !any(vapply(1:6,function(i) state$players[[i]]$id==action$actor && d$alive[[i]] && d$roles[[i]]=="mafia",logical(1)))) .ra_fail("Only living mafia can submit the night choice")
    if(!is.null(target)) {target<-target+1L;if(!d$alive[[target]] || d$roles[[target]]=="mafia") .ra_fail("Night target must be a living townsperson");d$alive[[target]]<-FALSE}
    d$phase<-if(!is.null(.ra_mafia_faction(d))) "finished" else "vote";return(d)
  }
  if(kind!="vote") .ra_fail("Submit daytime votes")
  .ra_fields(p,list(player=list(type="integer",minimum=0,maximum=5),target=list(type="integer",minimum=-1,maximum=5)),"/params");t<-p$player+1L;target<-p$target
  if(!d$alive[[t]] || d$votes[[t]]!=-2) .ra_fail("Voter is dead or already voted")
  if(target>=0 && !d$alive[[target+1L]]) .ra_fail("Vote target must be alive")
  if(!is.null(action$actor) && action$actor!=state$players[[t]]$id) .ra_fail("Wrong actor")
  d$votes[[t]]<-target
  if(all(unlist(d$votes)[unlist(d$alive)]!=-2)) {
    living<-sum(unlist(d$alive));counts<-vapply(0:5,function(i) sum(unlist(d$votes)==i),numeric(1));target<-which(counts>living/2)
    if(length(target)) d$alive[[target[1]]]<-FALSE
    d$votes<-rep(list(-2L),6)
    if(!is.null(.ra_mafia_faction(d))) d$phase<-"finished" else {d$day<-d$day+1L;d$phase<-"night"}
  }
  d
}
.ra_mafia_legal <- function(state) {
  d<-state$data
  if(d$phase=="setup") return(list(mode="event_types",actions=list(),action_types=list(list(type="assign_roles",params_schema=list(roles=list(type="array",minItems=6,maxItems=6,items=list(type="string",enum=list("mafia","town")))),preconditions=list("Exactly two mafia and four townspeople."),external_observations=list("Supplied secret roles."))),constraints=structure(list(),names=character()),completeness=FALSE,reason="external_role_assignment"))
  if(d$phase=="night") return(lapply(c(list(NULL),as.list(which(unlist(d$alive) & unlist(d$roles)=="town")-1L)),function(t) list(type="night_choice",params=list(target=t))))
  actions<-list()
  for(p in 0:5) if(d$alive[[p+1L]] && d$votes[[p+1L]]==-2) for(t in c(-1L,which(unlist(d$alive))-1L)) actions[[length(actions)+1L]]<-list(type="vote",params=list(player=p,target=t))
  actions
}
.ra_mafia_outcome <- function(state,pack) {
  d<-state$data;winner<-.ra_mafia_faction(d);terminal<-!is.null(winner)
  list(status=if(terminal) "terminal" else "ongoing",result_kind=if(terminal) "win" else "none",winners=if(terminal) lapply(which(unlist(d$roles)==winner),function(i) state$players[[i]]$id) else list(),losers=if(terminal) lapply(which(unlist(d$roles)!=winner),function(i) state$players[[i]]$id) else list(),placements=list(),reason=if(identical(winner,"town")) "mafia_eliminated" else if(identical(winner,"mafia")) "mafia_reached_parity" else "ongoing",evidence_required=list(),rule_id=if(terminal) paste0(pack$ruleset_id,"@",pack$ruleset_version) else NULL)
}
