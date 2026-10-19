.ra_chinese_dirs <- list(c(1,0),c(-1,0),c(0,1),c(0,-1),c(1,-1),c(-1,1))
.ra_chinese_coord_key <- function(q,r) paste0(q,":",r)
.ra_chinese_geometry <- local({
  base<-list();for(r in -8:-5) for(q in seq(-r-4,4)) base[[length(base)+1L]]<-c(q,r)
  arms<-list();arm<-base
  for(i in 1:6) {arms[[i]]<-arm;arm<-lapply(arm,function(c) c(-c[2],c[1]+c[2]))}
  all<-list();for(q in -4:4) for(r in -4:4) if(abs(q+r)<=4) all[[length(all)+1L]]<-c(q,r)
  all<-c(all,unlist(arms,recursive=FALSE));keys<-vapply(all,function(c) .ra_chinese_coord_key(c[1],c[2]),character(1));all<-all[!duplicated(keys)]
  order<-order(vapply(all,function(c) c[1],numeric(1)),vapply(all,function(c) c[2],numeric(1)));coords<-all[order];keys<-vapply(coords,function(c) .ra_chinese_coord_key(c[1],c[2]),character(1));index<-as.list(0:(length(coords)-1L));names(index)<-keys
  homes<-lapply(arms[c(1,4)],function(arm) sort(vapply(arm,function(c) index[[.ra_chinese_coord_key(c[1],c[2])]],numeric(1))))
  list(coords=coords,index=index,homes=homes)
})
.ra_chinese_index <- function(q,r) {key<-.ra_chinese_coord_key(q,r);if(key %in% names(.ra_chinese_geometry$index)) .ra_chinese_geometry$index[[key]] else NA_real_}
.ra_chinese_goal <- function(start,end,p) !start %in% .ra_chinese_geometry$homes[[2L-p]] || end %in% .ra_chinese_geometry$homes[[2L-p]]
.ra_chinese_steps <- function(d) {
  p<-d$turn;occupied<-unlist(d$pawns);result<-list()
  for(start in sort(unlist(d$pawns[[p+1L]]))) {
    c<-.ra_chinese_geometry$coords[[start+1L]]
    for(dir in .ra_chinese_dirs) {dest<-.ra_chinese_index(c[1]+dir[1],c[2]+dir[2]);if(!is.na(dest) && !dest %in% occupied && .ra_chinese_goal(start,dest,p)) result[[length(result)+1L]]<-c(start,dest)}
    fixed<-setdiff(occupied,start);seen<-start;todo<-list(start)
    while(length(todo)) {
      path<-todo[[1]];todo<-tail(todo,length(todo)-1L);here<-tail(path,1);c<-.ra_chinese_geometry$coords[[here+1L]]
      for(dir in .ra_chinese_dirs) {
        middle<-.ra_chinese_index(c[1]+dir[1],c[2]+dir[2]);dest<-.ra_chinese_index(c[1]+2*dir[1],c[2]+2*dir[2])
        if(middle %in% fixed && !is.na(dest) && !dest %in% fixed && !dest %in% seen && .ra_chinese_goal(here,dest,p)) {seen<-c(seen,dest);new<-c(path,dest);result[[length(result)+1L]]<-new;todo[[length(todo)+1L]]<-new}
      }
    }
  }
  if(length(result)) result<-result[order(vapply(result,function(p) p[1],numeric(1)),vapply(result,function(p) tail(p,1),numeric(1)),lengths(result),vapply(result,function(p) paste(sprintf("%03d",p),collapse=":"),character(1)))]
  result
}
.ra_chinese_checkers_validate <- function(d) {
  if(anyDuplicated(unlist(d$pawns))) .ra_fail("Marbles overlap",code="INVALID_STATE")
  wins<-Filter(function(p) setequal(unlist(d$pawns[[p+1L]]),.ra_chinese_geometry$homes[[2L-p]]),0:1)
  if(length(wins)>1 || (length(wins)>0 && wins[1]!=1-d$turn)) .ra_fail("Winner is not the last mover",code="INVALID_STATE")
}
.ra_chinese_checkers_apply <- function(state,action) {
  if(action$type!="move") .ra_fail("Unknown action")
  .ra_fields(action$params,list(path=list(type="array",minItems=2,maxItems=121,items=list(type="integer",minimum=0,maximum=120))),"/params")
  d<-state$data;p<-d$turn;path<-unlist(action$params$path)
  if(!is.null(action$actor) && action$actor!=state$players[[p+1L]]$id) .ra_fail("Wrong actor")
  if(!path[1] %in% unlist(d$pawns[[p+1L]]) || anyDuplicated(path)) .ra_fail("Path must start at an owned marble and not repeat holes")
  fixed<-setdiff(unlist(d$pawns),path[1])
  for(k in seq_len(length(path)-1L)) {
    a<-path[k];b<-path[k+1L];c<-.ra_chinese_geometry$coords[[a+1L]];cc<-.ra_chinese_geometry$coords[[b+1L]];delta<-cc-c
    adjacent<-any(vapply(.ra_chinese_dirs,function(dir) all(delta==dir),logical(1)));jump<-any(vapply(.ra_chinese_dirs,function(dir) all(delta==2*dir),logical(1))) && .ra_chinese_index((c[1]+cc[1])/2,(c[2]+cc[2])/2) %in% fixed
    if(b %in% fixed || !.ra_chinese_goal(a,b,p) || !((adjacent && length(path)==2) || jump)) .ra_fail("Path violates step, jump or target-camp rules")
  }
  d$pawns[[p+1L]]<-as.list(sort(c(setdiff(unlist(d$pawns[[p+1L]]),path[1]),tail(path,1))));d$turn<-1L-p;d
}
.ra_chinese_checkers_legal <- function(state) list(mode="event_types",actions=lapply(.ra_chinese_steps(state$data),function(path) list(type="move",params=list(path=as.list(path)))),action_types=list(list(type="move",params_schema=list(path=list(type="array",minItems=2,maxItems=121,items=list(type="integer",minimum=0,maximum=120))),preconditions=list("A single adjacent step or a nonrepeating sequence of occupied-hole jumps; target-camp marbles remain inside the camp."),external_observations=list())),constraints=structure(list(),names=character()),completeness=FALSE,reason="representative_routes_for_reachable_destinations")
.ra_chinese_checkers_outcome <- function(state,pack) {
  d<-state$data;wins<-Filter(function(p) setequal(unlist(d$pawns[[p+1L]]),.ra_chinese_geometry$homes[[2L-p]]),0:1);w<-if(length(wins)) wins[1] else NULL
  list(status=if(length(wins)) "terminal" else "ongoing",result_kind=if(length(wins)) "win" else "none",winners=if(is.null(w)) list() else list(state$players[[w+1L]]$id),losers=if(is.null(w)) list() else list(state$players[[2L-w]]$id),placements=list(),reason=if(length(wins)) "opposite_camp_filled" else "ongoing",evidence_required=list(),rule_id=if(length(wins)) paste0(pack$ruleset_id,"@",pack$ruleset_version) else NULL)
}
