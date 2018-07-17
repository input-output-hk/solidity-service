// Simple contract that calculates and returns the n-th fibonacci number by calling
// its public function f with n.
contract test {
	function f(uint n) returns(uint nfac) {
		nfac = 1;
		var i = 2;
		do { nfac *= i++; } while (i <= n);
	}
}
