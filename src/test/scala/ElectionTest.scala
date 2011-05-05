package com.gu.elections

import org.scalatest.FunSuite
import org.scalatest.matchers.ShouldMatchers

class ElectionTest extends FunSuite with ShouldMatchers {

  val dalek = Candidate("Dalek")
  val cyberman = Candidate("Cyberman")
  val sontaran = Candidate("Sontaran")
  val master = Candidate("The Master")
  val allMonsterCandidates = Set(dalek, cyberman, sontaran, master)

  test("Should elect the guy everyone likes") {
    val doctorVote = Vote(List(cyberman))
    val amyVote = Vote(List(cyberman))
    val roryVote = Vote(List(cyberman))

    val winner=Election.elect(allMonsterCandidates, List(doctorVote, amyVote, roryVote));
    winner should be(cyberman)
  }

  test("Having one vote does not win you the inital round if there are 3 votes") {
    val doctorVote = Vote(List(sontaran,master))
    val amyVote = Vote(List(dalek,master))
    val roryVote = Vote(List(master))

    val winner=Election.elect(allMonsterCandidates, List(doctorVote, amyVote, roryVote));
    winner should be(master)
  }


  test("1 vote out of 3 votes is not a majority") {
    val doctorVote = Vote(List(sontaran))
    val amyVote = Vote(List(dalek, sontaran))
    val roryVote = Vote(List(master))

    val c=new BallotCount(allMonsterCandidates, List(doctorVote, amyVote, roryVote));
    c.foreRunnerHasMajority should be(false)
  }

  test("2 votes out of 4 votes is not a majority") {
    val doctorVote = Vote(List(sontaran, master))
    val amyVote = Vote(List(sontaran, master))
    val roryVote = Vote(List(master))
    val sallyVote = Vote(List(dalek))

    val c=new BallotCount(allMonsterCandidates, List(doctorVote, amyVote, roryVote, sallyVote));
    c.foreRunnerHasMajority should be(false)
  }

  test("3 votes out of 4 votes is a majority") {
    val doctorVote = Vote(List(sontaran, master))
    val amyVote = Vote(List(sontaran, master))
    val roryVote = Vote(List(sontaran))
    val sallyVote = Vote(List(dalek))

    val c=new BallotCount(allMonsterCandidates, List(doctorVote, amyVote, roryVote, sallyVote));
    c.foreRunnerHasMajority should be(true)
  }
}