.ra_mancala_n <- function(config) if(config$mode=="oware") 6L else 9L
.ra_mancala_total <- function(config) if(config$mode=="oware") 48L else 162L
.ra_mancala_own <- function(p,config) {n<-.ra_mancala_n(config);seq(p*n,(p+1L)*n-1L)}
.ra_mancala_sow <- function(d,pit,config) {
  n<-.ra_mancala_n(config);count<-d$pits[[pit+1L]];last<-pit
  if(config$mode=="toguz" && count>1) {d$pits[[pit+1L]]<-1L;count<-count-1L} else d$pits[[pit+1L]]<-0L
  while(count>0) {
    last<-(last+1L)%%(2*n)
    if(config$mode=="oware" && last==pit) next
    owner<-if(config$mode=="toguz") which(vapply(d$tuzdyk,function(x) !is.null(x) && x==last,logical(1))) else integer()
    if(!length(owner)) d$pits[[last+1L]]<-d$pits[[last+1L]]+1L else d$scores[[owner[1]]]<-d$scores[[owner[1]]]+1L
    count<-count-1L
  }
  list(data=d,last=last)
}
.ra_mancala_pits <- function(d,config) {
  p<-d$turn;candidates<-Filter(function(i) d$pits[[i+1L]]>0,.ra_mancala_own(p,config));other<-.ra_mancala_own(1L-p,config)
  if(config$mode=="oware" && !any(unlist(d$pits[other+1L])>0)) candidates<-Filter(function(i) any(unlist(.ra_mancala_sow(d,i,config)$data$pits[other+1L])>0),candidates)
  candidates
}
.ra_mancala_tuz <- function(d,last,config) {
  p<-d$turn;other<-d$tuzdyk[[2L-p]]
  last %in% .ra_mancala_own(1L-p,config) && last%%9!=8 && d$pits[[last+1L]]==3 && is.null(d$tuzdyk[[p+1L]]) && (is.null(other) || other%%9!=last%%9)
}
.ra_mancala_collect <- function(d,config) {
  for(p in 0:1) for(i in .ra_mancala_own(p,config)) {d$scores[[p+1L]]<-d$scores[[p+1L]]+d$pits[[i+1L]];d$pits[[i+1L]]<-0L}
  d
}
.ra_mancala_validate <- function(d,config) {
  total<-.ra_mancala_total(config)
  if(sum(unlist(d$pits))+sum(unlist(d$scores))!=total) .ra_fail("Seed inventory is not conserved",code="INVALID_STATE")
  if(config$mode=="toguz") {
    for(p in 0:1) {pit<-d$tuzdyk[[p+1L]];if(!is.null(pit) && (!pit %in% .ra_mancala_own(1L-p,config) || pit%%9==8 || d$pits[[pit+1L]]!=0)) .ra_fail("Invalid tuzdyk location or seed count",code="INVALID_STATE")}
    if(!is.null(d$tuzdyk[[1]]) && !is.null(d$tuzdyk[[2]]) && d$tuzdyk[[1]]%%9==d$tuzdyk[[2]]%%9) .ra_fail("Opposing tuzdyks cannot share a pit number",code="INVALID_STATE")
  } else if(all(unlist(d$agreement)) && any(unlist(d$pits)>0)) .ra_fail("Agreed ending must collect the remaining seeds",code="INVALID_STATE")
  if(max(unlist(d$scores))<=total/2 && sum(unlist(d$scores))<total && !length(.ra_mancala_pits(d,config))) .ra_fail("An unplayable position must settle remaining seeds",code="INVALID_STATE")
}
.ra_mancala_apply <- function(state,action,config) {
  d<-state$data;p<-d$turn;params<-action$params;kind<-action$type;n<-.ra_mancala_n(config);total<-.ra_mancala_total(config)
  if(config$mode=="oware" && kind=="agree_end") {
    .ra_fields(params,list(player=list(type="integer",minimum=0,maximum=1)),"/params");t<-params$player+1L
    if(!is.null(action$actor) && action$actor!=state$players[[t]]$id) .ra_fail("Wrong actor")
    if(d$agreement[[t]]) .ra_fail("Player already agreed")
    d$agreement[[t]]<-TRUE;return(if(all(unlist(d$agreement))) .ra_mancala_collect(d,config) else d)
  }
  if(kind!="sow") .ra_fail("Unknown action")
  schema<-list(pit=list(type="integer",minimum=0,maximum=2*n-1))
  if(config$mode=="toguz") schema$claim_tuzdyk<-list(type="boolean")
  .ra_fields(params,schema,"/params");pit<-params$pit
  if(!is.null(action$actor) && action$actor!=state$players[[p+1L]]$id) .ra_fail("Wrong actor")
  if(!pit %in% .ra_mancala_pits(d,config)) .ra_fail("Pit is empty, belongs to the opponent or fails the feeding obligation")
  s<-.ra_mancala_sow(d,pit,config);d<-s$data;last<-s$last
  if(config$mode=="oware") {
    captured<-numeric();i<-last;other<-.ra_mancala_own(1L-p,config)
    while(i %in% other && d$pits[[i+1L]] %in% c(2,3)) {captured<-c(captured,i);i<-(i-1L)%%(2*n)}
    if(sum(unlist(d$pits[captured+1L]))<sum(unlist(d$pits[other+1L]))) for(i in captured) {d$scores[[p+1L]]<-d$scores[[p+1L]]+d$pits[[i+1L]];d$pits[[i+1L]]<-0L}
    d$agreement<-list(FALSE,FALSE)
  } else {
    if(params$claim_tuzdyk) {
      if(!.ra_mancala_tuz(d,last,config)) .ra_fail("This landing cannot create a tuzdyk")
      d$tuzdyk[[p+1L]]<-last;d$scores[[p+1L]]<-d$scores[[p+1L]]+3L;d$pits[[last+1L]]<-0L
    } else if(last %in% .ra_mancala_own(1L-p,config) && d$pits[[last+1L]]>0 && d$pits[[last+1L]]%%2==0) {d$scores[[p+1L]]<-d$scores[[p+1L]]+d$pits[[last+1L]];d$pits[[last+1L]]<-0L}
  }
  d$turn<-1L-p
  if(max(unlist(d$scores))<=total/2 && sum(unlist(d$scores))<total && !length(.ra_mancala_pits(d,config))) d<-.ra_mancala_collect(d,config)
  d
}
.ra_mancala_legal <- function(state,config) {
  d<-state$data;actions<-list()
  for(pit in .ra_mancala_pits(d,config)) {
    if(config$mode=="oware") actions[[length(actions)+1L]]<-list(type="sow",params=list(pit=pit)) else {
      after<-.ra_mancala_sow(d,pit,config);actions[[length(actions)+1L]]<-list(type="sow",params=list(pit=pit,claim_tuzdyk=FALSE))
      if(.ra_mancala_tuz(after$data,after$last,config)) actions[[length(actions)+1L]]<-list(type="sow",params=list(pit=pit,claim_tuzdyk=TRUE))
    }
  }
  if(config$mode=="oware") for(p in 0:1) if(!d$agreement[[p+1L]]) actions[[length(actions)+1L]]<-list(type="agree_end",params=list(player=p))
  actions
}
.ra_mancala_outcome <- function(state,pack,config) {
  d<-state$data;total<-.ra_mancala_total(config);scores<-unlist(d$scores);terminal<-max(scores)>total/2 || sum(scores)==total;tie<-scores[1]==scores[2];w<-if(scores[1]>scores[2]) 1L else 2L
  list(status=if(terminal) "terminal" else "ongoing",result_kind=if(terminal) {if(tie) "draw" else "win"} else "none",winners=if(terminal && !tie) list(state$players[[w]]$id) else list(),losers=if(terminal && !tie) list(state$players[[3L-w]]$id) else list(),placements=list(),reason=if(terminal) "majority_or_seed_exhaustion" else "ongoing",evidence_required=list(),rule_id=if(terminal) paste0(pack$ruleset_id,"@",pack$ruleset_version) else NULL)
}
