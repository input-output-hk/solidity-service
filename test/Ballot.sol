// Contract that host a running ballot.
contract Ballot {
	// Mapping from voter address to flag that ensures only registered voters can vote.
	mapping(address => bool) canVote;

	// Mapping from candidate address to current vote count for the candidate.
	mapping(address => uint) voteCount;

	// Mapping from voter address to flag that ensures no voter can vote twice.
	mapping(address => bool) voted;

	// Returns the vote count for the candidate with the given address.
	function getVoteCount(address addr) returns (uint retVoteCount) {
		return voteCount[addr];
	}

	// Registers the voter with the given address to be eligible to vote.
	function grantVoteRight(address addr) {
		canVote[addr] = true;
	}

	// Casts a vote for the candidate of the given address (vote) from the voter with
	// the given address (voter) as long as the voter is registered and has not voted
	// yet. Returns true on success and false if the vote was not casted for any of the
	// above reasons.
	function vote(address voter, address vote) returns (bool success) {
		if (!canVote[voter] || voted[voter]) return false;
		voted[voter] = true;
		voteCount[vote] = voteCount[vote] + 1;
		return true;
	}
}
