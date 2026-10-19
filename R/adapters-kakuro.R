.ra_kakuro_run <- list(type="object",properties=list(cells=list(type="array",minItems=2,maxItems=9,items=list(type="integer",minimum=0,maximum=624)),total=list(type="integer",minimum=3,maximum=45)),required=list("cells","total"))
.ra_kakuro_runs <- list(type="array",minItems=2,maxItems=625,items=.ra_kakuro_run)
.ra_kakuro_consistent <- function(values,total) {
  values<-unlist(values);used<-values[values>0]
  if(anyDuplicated(used)) return(FALSE)
  missing<-length(values)-length(used);available<-setdiff(1:9,used)
  sum(used)+sum(head(available,missing))<=total && total<=sum(used)+sum(tail(available,missing))
}
.ra_kakuro_validate <- function(d) {
  if(!d$configured) {if(d$width!=0 || d$height!=0 || length(d$runs)+length(d$board)>0) .ra_fail("Unconfigured puzzle contains data",code="INVALID_STATE");return(invisible(NULL))}
  w<-d$width;h<-d$height;b<-d$board
  if(w<1 || h<1 || length(b)!=w*h) .ra_fail("Puzzle dimensions mismatch",code="INVALID_STATE")
  counts<-matrix(0,nrow=length(b),ncol=2)
  for(r in d$runs) {
    cells<-unlist(r$cells)
    if(any(cells>=length(b))) .ra_fail("Run cell outside board",code="INVALID_STATE")
    horizontal<-all(cells==cells[1]+seq_along(cells)-1L) && all(cells%/%w==cells[1]%/%w)
    vertical<-all(cells==cells[1]+(seq_along(cells)-1L)*w)
    if(!horizontal && !vertical) .ra_fail("Run must be contiguous and ordered",code="INVALID_STATE")
    step<-if(horizontal) 1L else w
    before<-cells[1]-step;after<-tail(cells,1)+step
    adjacent<-c(if(before>=0 && (!horizontal || before%/%w==cells[1]%/%w)) before else numeric(),if(after<length(b) && (!horizontal || after%/%w==cells[1]%/%w)) after else numeric())
    if(length(adjacent) && any(unlist(b[adjacent+1L])>=0)) .ra_fail("Runs must span the entire white segment",code="INVALID_STATE")
    for(i in cells) counts[i+1L,if(horizontal) 1L else 2L]<-counts[i+1L,if(horizontal) 1L else 2L]+1L
    if(any(unlist(b[cells+1L])<0) || !.ra_kakuro_consistent(b[cells+1L],r$total)) .ra_fail("Run digits violate the clue",code="INVALID_STATE")
  }
  if(!length(d$runs)) .ra_fail("Each white cell needs one horizontal and one vertical run",code="INVALID_STATE")
  for(i in seq_along(b)) if(any(counts[i,]!=if(b[[i]]==-1) 0 else 1)) .ra_fail("Each white cell needs one horizontal and one vertical run",code="INVALID_STATE")
}
.ra_kakuro_options <- function(d,i) {
  result<-numeric()
  for(v in 0:9) {
    if(v==d$board[[i]]) next
    ok<-TRUE
    for(r in d$runs) if((i-1L) %in% unlist(r$cells)) {
      values<-lapply(r$cells,function(j) if(j==i-1L) v else d$board[[j+1L]])
      if(!.ra_kakuro_consistent(values,r$total)) {ok<-FALSE;break}
    }
    if(ok) result<-c(result,v)
  }
  result
}
.ra_kakuro_apply <- function(state,action) {
  d<-state$data;p<-action$params
  if(!d$configured) {
    if(action$type!="set_puzzle") .ra_fail("Supply the puzzle first")
    .ra_fields(p,list(width=list(type="integer",minimum=1,maximum=25),height=list(type="integer",minimum=1,maximum=25),runs=.ra_kakuro_runs),"/params")
    b<-as.list(rep(-1,p$width*p$height))
    for(r in p$runs) for(i in r$cells) {if(i>=length(b)) .ra_fail("Run cell outside board");b[[i+1L]]<-0L}
    d$configured<-TRUE;d$width<-p$width;d$height<-p$height;d$runs<-p$runs;d$board<-b;return(d)
  }
  if(action$type!="write") .ra_fail("Unknown action")
  .ra_fields(p,list(cell=list(type="integer",minimum=0,maximum=length(d$board)-1),value=list(type="integer",minimum=0,maximum=9)),"/params");i<-p$cell+1L
  if(d$board[[i]]<0) .ra_fail("Black cells cannot be edited")
  if(!p$value %in% .ra_kakuro_options(d,i)) .ra_fail("Digit is unchanged or violates a run")
  d$board[[i]]<-p$value;d
}
.ra_kakuro_legal <- function(state) {
  d<-state$data
  if(!d$configured) return(list(mode="event_types",actions=list(),action_types=list(list(type="set_puzzle",params_schema=list(width=list(type="integer",minimum=1,maximum=25),height=list(type="integer",minimum=1,maximum=25),runs=.ra_kakuro_runs),preconditions=list("Every white cell belongs to one horizontal and one vertical run; clues have feasible distinct-digit sums."),external_observations=list("Supplied Kakuro runs and sums; other cells are black."))),constraints=structure(list(),names=character()),completeness=FALSE,reason="external_puzzle_required"))
  result<-list()
  for(i in seq_along(d$board)) if(d$board[[i]]>=0) for(v in .ra_kakuro_options(d,i)) result[[length(result)+1L]]<-list(type="write",params=list(cell=i-1L,value=v))
  result
}
.ra_kakuro_outcome <- function(state,pack) {
  d<-state$data;solved<-d$configured && !(0 %in% unlist(d$board))
  list(status=if(solved) "terminal" else "ongoing",result_kind=if(solved) "win" else "none",winners=if(solved) list(state$players[[1]]$id) else list(),losers=list(),placements=list(),reason=if(solved) "puzzle_completed" else if(d$configured) "ongoing" else "awaiting_puzzle",evidence_required=list(),rule_id=if(solved) paste0(pack$ruleset_id,"@",pack$ruleset_version) else NULL)
}
