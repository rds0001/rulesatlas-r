.ra_real_tennis_player<-list(type="integer",minimum=0,maximum=1)
.ra_real_tennis_kind<-list(type="enum",values=list("service","hazard"))
.ra_real_tennis_mark<-list(type="string",minLength=1,maxLength=100)
.ra_real_tennis_wins<-function(d) vapply(0:1,function(t) sum(vapply(d$sets,function(s) s[[t+1L]]==6,logical(1))),numeric(1))
.ra_real_tennis_due<-function(d) !d$playing_chases && length(d$chases)>0 && (length(d$chases)==2 || max(unlist(d$points))>=3)
.ra_real_tennis_settle<-function(d) {
 d$fault<-FALSE;d$phase<-"ready"
 if(length(d$chases)==0) d$playing_chases<-FALSE
 if(.ra_real_tennis_due(d)) d$phase<-"change_ends"
 d
}
.ra_real_tennis_point<-function(d,t) {
 i<-t+1L;j<-2L-t;d$points[[i]]<-d$points[[i]]+1L
 if(d$points[[i]]>=4 && d$points[[i]]-d$points[[j]]>=2) {
  d$points<-list(0L,0L);d$games[[i]]<-d$games[[i]]+1L;d$chases<-list();d$playing_chases<-FALSE
  if(d$games[[i]]==6) {
   d$sets<-c(d$sets,list(d$games));d$games<-list(0L,0L)
   if(.ra_real_tennis_wins(d)[i]==2) {d$phase<-"finished";d$winner<-t;d$reason<-"two_sets";d$fault<-FALSE;return(d)}
  }
 } else if(min(unlist(d$points))>=3) {m<-min(unlist(d$points))-3L;d$points<-lapply(d$points,function(v) v-m)}
 .ra_real_tennis_settle(d)
}
.ra_real_tennis_consume<-function(d) {if(d$playing_chases) d$chases<-d$chases[-1L];d}
.ra_real_tennis_validate<-function(d) {
 bad<-function(m) .ra_fail(m,code="INVALID_STATE")
 if(max(unlist(d$points))==4 && min(unlist(d$points))!=3) bad("Advantage requires forty for the opponent")
 for(j in seq_along(d$sets)) {
  s<-d$sets[[j]]
  if(max(unlist(s))!=6 || min(unlist(s))>=6) bad("A completed set ends at six games")
  if(any(vapply(0:1,function(t) sum(vapply(head(d$sets,j-1L),function(x) x[[t+1L]]==6,logical(1)))==2,logical(1)))) bad("No set follows match victory")
 }
 if(d$phase=="choose_server") {
  if(!is.null(d$server) || any(unlist(c(d$points,d$games))!=0) || length(d$sets)>0 || length(d$chases)>0 || d$fault) bad("Initial service choice precedes all play")
 } else if(!d$phase %in% c("finished","abandoned") && is.null(d$server)) bad("Play requires a service-side player")
 if(d$playing_chases && length(d$chases)==0) bad("Playing chases requires a pending chase")
 for(ch in d$chases) if(nchar(trimws(ch$mark))==0) bad("Chase marker cannot be blank")
 if(!d$phase %in% c("finished","abandoned")) {
  if((d$phase=="change_ends")!=.ra_real_tennis_due(d)) bad("Required change of ends must occur before service")
  if(max(.ra_real_tennis_wins(d))==2) bad("Two sets end the match")
 }
 if(d$phase %in% c("choose_server","change_ends","finished","abandoned") && d$fault) bad("Fault only persists during the current rest")
 if(d$phase=="finished") {
  if(is.null(d$reason) || (d$reason=="no_contest")!=is.null(d$winner)) bad("Result must identify a winner or no contest")
  if(d$reason=="two_sets" && (.ra_real_tennis_wins(d)[d$winner+1L]!=2 || any(unlist(c(d$games,d$points))!=0))) bad("Normal victory requires two sets")
 } else if(!is.null(d$winner) || !is.null(d$reason)) bad("Only a finished match has a result")
}
.ra_real_tennis_schemas<-function(d) {
 phase<-d$phase;s<-list();p<-.ra_real_tennis_player;k<-.ra_real_tennis_kind;m<-.ra_real_tennis_mark;empty<-setNames(list(),character())
 if(phase=="finished") return(s)
 if(phase=="abandoned") return(list(official_result=list(winner=c(p,list(nullable=TRUE)))))
 if(phase=="choose_server") s$choose_server<-list(player=p)
 if(phase=="change_ends") s$change_ends<-empty
 if(phase=="ready") {
  s$start_serve<-list(player=p)
  if(length(d$chases)>0) s$correct_chase<-list(index=list(type="integer",minimum=0,maximum=length(d$chases)-1L),kind=k,mark=m)
 }
 if(phase=="serve") s$service_decision<-list(result=list(type="enum",values=list("good","fault","let")))
 if(phase=="rally") {
  s$award_rest<-list(winner=p);s$let<-empty
  if(d$playing_chases) s$chase_result<-list(result=list(type="enum",values=list("better","worse","equal","opposite_end"))) else s$set_chase<-list(kind=k,mark=m)
 }
 c(s,list(concede=list(player=p),disqualify=list(player=p),abandon=empty))
}
.ra_real_tennis_apply<-function(state,action) {
 d<-state$data;k<-action$type;p<-action$params;s<-.ra_real_tennis_schemas(d)
 if(!k %in% names(s)) .ra_fail("Action is not permitted in this phase")
 .ra_fields(p,s[[k]],"/params")
 if(k=="choose_server") {d$server<-p$player;d$phase<-"ready"} else if(k=="change_ends") {d$server<-1L-d$server;d$playing_chases<-TRUE;d$phase<-"ready"} else if(k=="correct_chase") {
  if(nchar(trimws(p$mark))==0) .ra_fail("Chase marker cannot be blank")
  d$chases[[p$index+1L]]<-list(kind=p$kind,mark=p$mark)
 } else if(k=="start_serve") {
  if(p$player!=d$server) .ra_fail("Only the service-side player may serve")
  d$phase<-"serve"
 } else if(k=="service_decision") {
  if(p$result=="good") d$phase<-"rally" else if(p$result=="let") d$phase<-"ready" else if(d$fault) d<-.ra_real_tennis_point(.ra_real_tennis_consume(d),1L-d$server) else {d$fault<-TRUE;d$phase<-"ready"}
 } else if(k=="let") d$phase<-"ready" else if(k=="set_chase") {
  if(nchar(trimws(p$mark))==0) .ra_fail("Chase marker cannot be blank")
  d$chases<-c(d$chases,list(list(kind=p$kind,mark=p$mark)));d<-.ra_real_tennis_settle(d)
 } else if(k=="chase_result") {
  attacker<-if(d$chases[[1L]]$kind=="hazard") d$server else 1L-d$server;d<-.ra_real_tennis_consume(d)
  if(p$result=="equal") d<-.ra_real_tennis_settle(d) else d<-.ra_real_tennis_point(d,if(p$result=="better") attacker else 1L-attacker)
 } else if(k=="award_rest") d<-.ra_real_tennis_point(.ra_real_tennis_consume(d),p$winner) else if(k %in% c("concede","disqualify")) {d$phase<-"finished";d$winner<-1L-p$player;d$reason<-if(k=="concede") "concession" else "disqualification";d$fault<-FALSE} else if(k=="abandon") {d$phase<-"abandoned";d$fault<-FALSE} else if(k=="official_result") {d$phase<-"finished";d["winner"]<-list(p$winner);d$reason<-if(is.null(p$winner)) "no_contest" else "official_result"}
 d
}
.ra_real_tennis_legal<-function(state) {
 d<-state$data;s<-.ra_real_tennis_schemas(d);attacker<-NULL
 if(d$playing_chases) attacker<-if(d$chases[[1L]]$kind=="hazard") d$server else 1L-d$server
 list(mode="event_types",actions=list(),action_types=lapply(names(s),function(k) list(type=k,params_schema=s[[k]],preconditions=list("Phase, service side and ordered chase rights must permit the event."),external_observations=list("Official service decisions, chase marks, chase comparisons and rest winners."))),constraints=list(phase=d$phase,server=d$server,service_attempt=if(d$fault) 2L else 1L,chases=d$chases,playing_chases=d$playing_chases,attacker=attacker,points=d$points,games=d$games,sets_won=as.list(.ra_real_tennis_wins(d))),completeness=FALSE,reason="official_real_tennis_events")
}
.ra_real_tennis_outcome<-function(state,pack) .ra_pelota_outcome(state,pack)
