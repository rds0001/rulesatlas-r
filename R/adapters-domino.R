.ra_dom_tiles <- unlist(lapply(0:6,function(a) lapply(a:6,function(b) c(a,b))),recursive=FALSE)
.ra_dom_tile <- list(type="integer",minimum=0,maximum=27)
.ra_dom_opening <- function(d) {
  entries<-unlist(lapply(0:1,function(t) lapply(d$hands[[t+1L]],function(tile) c(tile,t))),recursive=FALSE)
  doubles<-Filter(function(e) {v<-.ra_dom_tiles[[e[1]+1L]];v[1]==v[2]},entries)
  choices<-if(length(doubles)) doubles else entries
  scores<-vapply(choices,function(e) {v<-.ra_dom_tiles[[e[1]+1L]];sum(v)*10+max(v)},numeric(1))
  choices[[which.max(scores)]]
}
.ra_dom_moves <- function(d,t) {
  if(!length(d$layout)) {
    first<-.ra_dom_opening(d)
    return(if(t==first[2]) list(list(type="play",params=list(tile=first[1],end="right"))) else list())
  }
  left<-d$layout[[1]][[1]];right<-tail(d$layout,1)[[1]][[2]];result<-list()
  for(tile in d$hands[[t+1L]]) {
    if(left %in% .ra_dom_tiles[[tile+1L]]) result[[length(result)+1L]]<-list(type="play",params=list(tile=tile,end="left"))
    if(right %in% .ra_dom_tiles[[tile+1L]]) result[[length(result)+1L]]<-list(type="play",params=list(tile=tile,end="right"))
  }
  result
}
.ra_dom_validate <- function(d) {
  if(!d$configured) {
    if(any(lengths(d$hands)>0) || length(d$stock) || length(d$layout) || d$turn!=0 || d$passes!=0) .ra_fail("Invalid unconfigured hand",code="INVALID_STATE")
    return(invisible(NULL))
  }
  played<-vapply(d$layout,function(tile) {
    v<-sort(unlist(tile));which(vapply(.ra_dom_tiles,function(x) all(x==v),logical(1)))[1]-1L
  },numeric(1))
  tiles<-c(unlist(d$hands),unlist(d$stock),played)
  if(length(tiles)!=28 || any(sort(tiles)!=0:27)) .ra_fail("Tiles must conserve the double-six set",code="INVALID_STATE")
  if(length(d$layout)>1) for(i in 2:length(d$layout)) if(d$layout[[i-1L]][[2]]!=d$layout[[i]][[1]]) .ra_fail("Domino chain does not connect",code="INVALID_STATE")
  if(!length(d$layout) && (any(lengths(d$hands)!=7) || length(d$stock)!=14 || d$turn!=.ra_dom_opening(d)[2])) .ra_fail("Opening hand mismatch",code="INVALID_STATE")
  if(!length(d$hands[[1]]) && !length(d$hands[[2]])) .ra_fail("Both hands cannot finish together",code="INVALID_STATE")
  if(d$passes>0) {
    if(length(d$stock) || length(.ra_dom_moves(d,1L-d$turn))) .ra_fail("Invalid pass state",code="INVALID_STATE")
    if(d$passes==2 && length(.ra_dom_moves(d,d$turn))) .ra_fail("A blocked hand still has a legal play",code="INVALID_STATE")
  }
}
.ra_dom_apply <- function(state,action) {
  d<-state$data;p<-action$params
  if(!d$configured) {
    if(action$type!="deal") .ra_fail("Supply the tile order first")
    .ra_fields(p,list(tiles=list(type="array",items=.ra_dom_tile,minItems=28,maxItems=28)),"/params")
    if(any(sort(unlist(p$tiles))!=0:27)) .ra_fail("Tile order must contain every tile exactly once")
    d$hands<-list(p$tiles[1:7],p$tiles[8:14]);d$stock<-p$tiles[15:28];d$configured<-TRUE;d$turn<-.ra_dom_opening(d)[2];return(d)
  }
  t<-d$turn;allowed<-.ra_dom_moves(d,t)
  if(!is.null(action$actor) && action$actor!=state$players[[t+1L]]$id) .ra_fail("Wrong actor")
  if(action$type=="play") {
    .ra_fields(p,list(tile=.ra_dom_tile,end=list(type="enum",values=list("left","right"))),"/params")
    if(!any(vapply(allowed,function(a) a$params$tile==p$tile && a$params$end==p$end,logical(1)))) .ra_fail("Tile cannot be played at that end")
    v<-.ra_dom_tiles[[p$tile+1L]];a<-v[1];b<-v[2]
    if(!length(d$layout)) d$layout<-list(as.list(v)) else if(p$end=="left") {
      end<-d$layout[[1]][[1]];d$layout<-c(list(as.list(if(a==end) c(b,a) else c(a,b))),d$layout)
    } else {
      end<-tail(d$layout,1)[[1]][[2]];d$layout[[length(d$layout)+1L]]<-as.list(if(a==end) c(a,b) else c(b,a))
    }
    d$hands[[t+1L]]<-Filter(function(x) x!=p$tile,d$hands[[t+1L]]);d$turn<-1L-t;d$passes<-0L
  } else if(action$type=="draw") {
    .ra_fields(p,structure(list(),names=character()),"/params")
    if(length(allowed) || !length(d$stock)) .ra_fail("Draw only when blocked and stock remains")
    d$hands[[t+1L]][[length(d$hands[[t+1L]])+1L]]<-d$stock[[1]];d$stock<-d$stock[-1]
  } else if(action$type=="pass") {
    .ra_fields(p,structure(list(),names=character()),"/params")
    if(length(allowed) || length(d$stock)) .ra_fail("Pass only when blocked with empty stock")
    d$passes<-d$passes+1L;d$turn<-1L-t
  } else .ra_fail("Unknown action")
  d
}
.ra_dom_legal <- function(state) {
  d<-state$data
  if(!d$configured) return(list(mode="event_types",actions=list(),action_types=list(list(type="deal",params_schema=list(tiles=list(type="array",items=.ra_dom_tile,minItems=28,maxItems=28)),preconditions=list("A permutation of all 28 canonical double-six tile IDs."),external_observations=list("An externally supplied shuffled tile order."))),constraints=structure(list(),names=character()),completeness=FALSE,reason="external_deal_required"))
  moves<-.ra_dom_moves(d,d$turn)
  if(length(moves)) moves else list(list(type=if(length(d$stock)) "draw" else "pass",params=structure(list(),names=character())))
}
.ra_dom_outcome <- function(state,pack) {
  d<-state$data;terminal<-d$configured && (any(lengths(d$hands)==0) || d$passes==2);w<-NULL
  if(terminal) {
    pips<-vapply(d$hands,function(hand) sum(vapply(hand,function(tile) sum(.ra_dom_tiles[[tile+1L]]),numeric(1))),numeric(1))
    w<-if(!length(d$hands[[1]])) 1L else if(!length(d$hands[[2]])) 2L else if(pips[1]==pips[2]) NULL else if(pips[1]<pips[2]) 1L else 2L
  }
  list(status=if(terminal) "terminal" else "ongoing",result_kind=if(terminal) {if(is.null(w)) "draw" else "win"} else "none",winners=if(is.null(w)) list() else list(state$players[[w]]$id),losers=if(is.null(w)) list() else list(state$players[[3L-w]]$id),placements=list(),reason=if(terminal) "hand_completed" else if(d$configured) "ongoing" else "awaiting_deal",evidence_required=list(),rule_id=if(terminal) paste0(pack$ruleset_id,"@",pack$ruleset_version) else NULL)
}
