test_prob <- function(){
  RUnit::checkTrue(p_overlap(10, 30, 20, 50) < 1)
}
