package com.gu.elections

case class Candidate(name: String)

case class Vote(ranking: Seq[Candidate]) {
  def firstChoiceFrom(validCandidates: Set[Candidate]) = ranking filter validCandidates headOption
}

class BallotCount(validCandidates: Set[Candidate], votes: Seq[Vote]) {
  val votesByCandidate = votes groupBy(_.firstChoiceFrom(validCandidates)) collect { case (Some(c), v) => (c, v) } toMap

  val allocatedVotes: Seq[Vote] =  votesByCandidate.values.flatten.toSeq

  def votesFor(candidate: Candidate) = votesByCandidate.getOrElse(candidate,List()).size

  val candidateRanking = Ordering[Int].on[Candidate]( votesFor(_) )

  val               forerunner = validCandidates max candidateRanking
  lazy val eliminatedCandidate = validCandidates min candidateRanking

  val foreRunnerHasMajority = votesFor(forerunner) > allocatedVotes.size / 2
}

object Election {

  def elect(validCandidates: Set[Candidate], votes: Seq[Vote]): Candidate = {
    val c = new BallotCount(validCandidates, votes)
    if (c.foreRunnerHasMajority || validCandidates.size <= 2) c.forerunner
    else elect(validCandidates - c.eliminatedCandidate, c.allocatedVotes)
  }
}


