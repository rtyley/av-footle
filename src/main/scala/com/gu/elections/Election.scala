package com.gu.elections

object Election {

  case class Candidate(name: String)

  case class Vote(ranking: Seq[Candidate]) {
    def firstChoiceFrom(validCandidates: Set[Candidate]) = ranking filter validCandidates headOption
  }

  class Count(validCandidates: Set[Candidate], votes: Seq[Vote]) {
    val votesByCandidate = (votes groupBy { _.firstChoiceFrom(validCandidates) }) mapValues { _.size }


    val candidateRanking = Ordering[Int].on[Candidate]( candidate => votesByCandidate.getOrElse(Some(candidate),0) )
  }

  def elect(validCandidates: Set[Candidate], votes: Seq[Vote]): Candidate = {
    val votesByCandidate = (votes groupBy { _.firstChoiceFrom(validCandidates) }) mapValues { _.size }
    
    println(votesByCandidate)

    val candidateRanking = Ordering[Int].on[Candidate]( candidate => votesByCandidate.getOrElse(Some(candidate),0) )

    val       mostVotesCandidate = validCandidates max candidateRanking
    lazy val leastVotesCandidate = validCandidates min candidateRanking

    if (votesByCandidate.getOrElse(Some(mostVotesCandidate), 0) >= votes.size / 2 || validCandidates.size <= 2) mostVotesCandidate
    else elect(validCandidates - leastVotesCandidate, votes)
  }



  def main(args: Array[String]) {
    println("Hello, world!")

    val dalek = Candidate("Dalek")
    val cyberman = Candidate("Cyberman")
    val sontaran = Candidate("Sontaran")
    val silurian = Candidate("Silurian")

    val doctorVote = Vote(List(dalek, cyberman))
    val amyVote = Vote(List(sontaran, cyberman))
    val roryVote = Vote(List(cyberman))

    val winner=elect(Set(dalek, cyberman, sontaran, silurian), List(doctorVote, amyVote, roryVote));
    println(winner)
  }
}


