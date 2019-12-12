-define(STRING, <<"TEST">>).
-define(BIN, <<"424242">>).
-define(MASKED_PAN, <<?BIN/binary, "******4242">>).

-define(BANK_CARD, #'domain_BankCard'{
    token = ?STRING,
    payment_system = visa,
    masked_pan = ?MASKED_PAN,
    bin = ?BIN
}).

-define(PUT_CARD_DATA_RESULT, #'PutCardDataResult'{
    bank_card = ?BANK_CARD,
    session_id = ?STRING
}).
