{
  class: "account-with-password",
  acct:  {
           class: "account-with-limit",
           acct:  {
                    class:   "account",
                    balance: 480.0,
                    name:    "A. Thrifty Spender"
                  },
           limit: 100.0
         },
  password:   "pass"
}

class:    account-with-password
acct:     class:   account
          balance: 500.0
          name:    "A. User"
password: "secret"


[account-with-password]
acct     = [account]
           balance = 500.0
           name    =  "A. User"
password = "secret"


{
  [account-with-password]
  acct     = {
               [account]
               balance = 500.0
               name    = "A. User"
             }
  password = "secret"
}
