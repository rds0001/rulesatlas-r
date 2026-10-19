.ra_sudoku_board <- list(type="array",items=list(type="integer",minimum=0,maximum=9),minItems=81,maxItems=81)
.ra_sudoku_groups <- c(lapply(0:8,function(r) r*9+0:8),lapply(0:8,function(c) (0:8)*9+c),unlist(lapply(c(0,3,6),function(r) lapply(c(0,3,6),function(c) unlist(lapply(0:2,function(dr) (r+dr)*9+c+0:2)))),recursive=FALSE))
.ra_sudoku_consistent <- function(board) all(vapply(.ra_sudoku_groups,function(group) {v<-unlist(board[group+1L]);v<-v[v!=0];!anyDuplicated(v)},logical(1)))
.ra_sudoku_validate <- function(d) {
  if(!.ra_sudoku_consistent(d$board) || !.ra_sudoku_consistent(d$givens)) .ra_fail("Repeated digit in a row, column or box",code="INVALID_STATE")
  g<-unlist(d$givens);b<-unlist(d$board)
  if(any(g!=0 & g!=b)) .ra_fail("A given was changed",code="INVALID_STATE")
  if(!d$configured && (any(b!=0) || any(g!=0))) .ra_fail("Puzzle is not configured",code="INVALID_STATE")
}
.ra_sudoku_candidates <- function(board,cell) {
  group<-unique(unlist(Filter(function(g) cell %in% g,.ra_sudoku_groups)));group<-group[group!=cell]
  setdiff(1:9,unlist(board[group+1L]))
}
.ra_sudoku_apply <- function(state,action) {
  d<-state$data;p<-action$params
  if(!d$configured) {
    if(action$type!="set_puzzle") .ra_fail("Supply the puzzle first")
    .ra_fields(p,list(givens=.ra_sudoku_board),"/params");d$givens<-p$givens;d$board<-p$givens;d$configured<-TRUE;return(d)
  }
  if(action$type!="write") .ra_fail("Unknown action")
  .ra_fields(p,list(cell=list(type="integer",minimum=0,maximum=80),value=list(type="integer",minimum=0,maximum=9)),"/params");i<-p$cell+1L;v<-p$value
  if(d$givens[[i]]!=0) .ra_fail("A given cannot be changed")
  if(v!=0 && !v %in% .ra_sudoku_candidates(d$board,i-1L)) .ra_fail("Digit conflicts with row, column or box")
  if(d$board[[i]]==v) .ra_fail("Cell already contains this value")
  d$board[[i]]<-v;d
}
.ra_sudoku_legal <- function(state) {
  d<-state$data
  if(!d$configured) return(list(mode="event_types",actions=list(),action_types=list(list(type="set_puzzle",params_schema=list(givens=.ra_sudoku_board),preconditions=list("Given digits must obey row, column and box constraints."),external_observations=list("A supplied puzzle; zero denotes an empty cell."))),constraints=structure(list(),names=character()),completeness=FALSE,reason="external_puzzle_required"))
  result<-list()
  for(i in 0:80) {
    if(d$givens[[i+1L]]!=0) next
    values<-c(if(d$board[[i+1L]]!=0) 0 else numeric(),.ra_sudoku_candidates(d$board,i))
    for(v in values) if(v!=d$board[[i+1L]]) result[[length(result)+1L]]<-list(type="write",params=list(cell=i,value=v))
  }
  result
}
.ra_sudoku_outcome <- function(state,pack) {
  solved<-state$data$configured && all(unlist(state$data$board)!=0)
  list(status=if(solved) "terminal" else "ongoing",result_kind=if(solved) "win" else "none",winners=if(solved) list(state$players[[1]]$id) else list(),losers=list(),placements=list(),reason=if(solved) "puzzle_completed" else if(state$data$configured) "ongoing" else "awaiting_puzzle",evidence_required=list(),rule_id=if(solved) paste0(pack$ruleset_id,"@",pack$ruleset_version) else NULL)
}
