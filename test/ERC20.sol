pragma solidity ^0.4.22;

// This contract provides an abstract interface for an ERC-20-like token standard.
contract Token {
    event Transfer(address indexed _from, address indexed _to, uint256 _value);
    event Approval(address indexed _owner, address indexed _spender, uint256 _value);

    function totalSupply() constant public returns (uint256 supply);
    function balanceOf(address _owner) constant public returns (uint256 balance);
    function transfer(address _to, uint256 _value) public returns (bool success);
    function transferFrom(address _from, address _to, uint256 _value) public returns (bool success);
    function approve(address _spender, uint256 _value) public returns (bool success);
    function allowance(address _owner, address _spender) constant public returns (uint256 remaining);
}

// This contract implements the ERC-20 token standard.
contract StandardToken is Token {
    // Account storage location for the supply variable. This variable holds
    // the current total supply of the token.
    uint256 supply;

    // The balance mapping maps each account address to its balance.
    mapping (address => uint256) balance;

    // The allowance mapping maps each pair of owner and spender accounts to the
    // allowance that the owner approves to be transferred by the spender.
    mapping (address =>
        mapping (address => uint256)) m_allowance;

    // Constructor: initializes an ERC-20 compliant token with the given total supply
    // where the given initialOwner owns all the supply.
    constructor(address _initialOwner, uint256 _supply) public {
        supply = _supply;
        balance[_initialOwner] = _supply;
    }

    // Returns the current balance of an account.
    function balanceOf(address _account) constant public returns (uint256) {
        return balance[_account];
    }

    // Returns the total supply of the token.
    function totalSupply() constant public returns (uint256) {
        return supply;
    }

    // Transfers the given value from the caller to the target account.
    // Returns true on success or false if the caller's balance is lower than the
    // value to transferred.
    // Logs a transfer event on success.
    function transfer(address _to, uint256 _value) public returns (bool success) {
        return doTransfer(msg.sender, _to, _value);
    }

    // Transfers the given value from the given source to the given target account.
    // The approved allowance to be transferred from the source by the caller should
    // cover the value to be transferred. The value of the allowance is reduced by the
    // value that was transferred.
    // Returns true on success, or when the transfer failed due to insufficient balance
    // of the source account. Return false when the transfer failed due to insufficient
    // allowance.
    // Logs a transfer event on success.
    function transferFrom(address _from, address _to, uint256 _value) public returns (bool) {
        if (m_allowance[_from][msg.sender] >= _value) {
            if (doTransfer(_from, _to, _value)) {
                m_allowance[_from][msg.sender] -= _value;
            }
            return true;
        } else {
            return false;
        }
    }

    // Helper internal function that updates the balances for a transfer and logs a
    // transfer event.
    function doTransfer(address _from, address _to, uint256 _value) internal returns (bool success) {
        if (balance[_from] >= _value && balance[_to] + _value >= balance[_to]) {
            balance[_from] -= _value;
            balance[_to] += _value;
            emit Transfer(_from, _to, _value);
            return true;
        } else {
            return false;
        }
    }

    // Approves the given value as allowance to be transferred from the caller by the
    // spender.
    // Returns true.
    // Logs an approval event.
    function approve(address _spender, uint256 _value) public returns (bool success) {
        m_allowance[msg.sender][_spender] = _value;
        emit Approval(msg.sender, _spender, _value);
        return true;
    }

    // Returns the currently approved allowance to be transferred from the owner
    // by the spender.
    function allowance(address _owner, address _spender) constant public returns (uint256) {
        return m_allowance[_owner][_spender];
    }
}
