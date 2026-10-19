.ra_nonogram_clues <- list(type="array",minItems=1,maxItems=25,items=list(type="array",minItems=0,maxItems=13,items=list(type="integer",minimum=1,maximum=25)))
.ra_nonogram_runs <- function(line) {
  result<-numeric();n<-0L
  for(v in c(unlist(line),0)) {if(v==1) n<-n+1L else if(n>0) {result<-c(result,n);n<-0L}}
  result
}
.ra_nonogram_validate <- function(d) {
  if(!d$configured) {if(length(d$rows)+length(d$columns)+length(d$board)>0) .ra_fail("Unconfigured puzzle contains data",code="INVALID_STATE");return(invisible(NULL))}
  h<-length(d$rows);w<-length(d$columns)
  if(h==0 || w==0 || length(d$board)!=h*w) .ra_fail("Puzzle dimensions mismatch",code="INVALID_STATE")
  for(c in d$rows) if(sum(unlist(c))+max(0,length(c)-1)>w) .ra_fail("Clues do not fit the line",code="INVALID_STATE")
  for(c in d$columns) if(sum(unlist(c))+max(0,length(c)-1)>h) .ra_fail("Clues do not fit the line",code="INVALID_STATE")
  if(sum(unlist(d$rows))!=sum(unlist(d$columns))) .ra_fail("Row and column fill totals differ",code="INVALID_STATE")
}
.ra_nonogram_apply <- function(state,action) {
  d<-state$data;p<-action$params
  if(!d$configured) {
    if(action$type!="set_puzzle") .ra_fail("Supply the puzzle first")
    .ra_fields(p,list(rows=.ra_nonogram_clues,columns=.ra_nonogram_clues),"/params")
    d$configured<-TRUE;d$rows<-p$rows;d$columns<-p$columns;d$board<-as.list(rep(-1,length(p$rows)*length(p$columns)));return(d)
  }
  if(action$type!="write") .ra_fail("Unknown action")
  .ra_fields(p,list(cell=list(type="integer",minimum=0,maximum=length(d$board)-1),value=list(type="integer",minimum=-1,maximum=1)),"/params")
  i<-p$cell+1L
  if(d$board[[i]]==p$value) .ra_fail("Cell already contains this value")
  d$board[[i]]<-p$value;d
}
.ra_nonogram_legal <- function(state) {
  d<-state$data
  if(!d$configured) return(list(mode="event_types",actions=list(),action_types=list(list(type="set_puzzle",params_schema=list(rows=.ra_nonogram_clues,columns=.ra_nonogram_clues),preconditions=list("Clues fit each line and row and column fill totals agree."),external_observations=list("Supplied monochrome puzzle clues; an empty list denotes an empty line."))),constraints=structure(list(),names=character()),completeness=FALSE,reason="external_puzzle_required"))
  result<-list()
  for(i in seq_along(d$board)) for(v in -1:1) if(d$board[[i]]!=v) result[[length(result)+1L]]<-list(type="write",params=list(cell=i-1L,value=v))
  result
}
.ra_nonogram_outcome <- function(state,pack) {
  d<-state$data;w<-length(d$columns);solved<-d$configured && !(-1 %in% unlist(d$board))
  same<-function(a,b) length(a)==length(b) && all(a==unlist(b))
  if(solved) {
    for(r in seq_along(d$rows)) if(!same(.ra_nonogram_runs(d$board[((r-1L)*w+1L):(r*w)]),d$rows[[r]])) solved<-FALSE
    for(c in seq_along(d$columns)) if(!same(.ra_nonogram_runs(d$board[seq(c,length(d$board),by=w)]),d$columns[[c]])) solved<-FALSE
  }
  list(status=if(solved) "terminal" else "ongoing",result_kind=if(solved) "win" else "none",winners=if(solved) list(state$players[[1]]$id) else list(),losers=list(),placements=list(),reason=if(solved) "puzzle_completed" else if(d$configured) "ongoing" else "awaiting_puzzle",evidence_required=list(),rule_id=if(solved) paste0(pack$ruleset_id,"@",pack$ruleset_version) else NULL)
}
