-define(STRING, <<"TEST">>).
-define(INTEGER, 10000).
-define(TIMESTAMP, <<"2016-03-22T06:12:27Z">>).

-define(BANK_CARD, #cds_BankCard{
    token = ?STRING,
    bin = <<"411111">>,
    last_digits = <<"1111">>
}).

-define(BINBASE_LOOKUP_RESULT, ?BINBASE_LOOKUP_RESULT(<<"MASTERCARD">>)).
-define(BINBASE_LOOKUP_RESULT(PaymentSystem), #'binbase_ResponseData'{
    bin_data = #'binbase_BinData' {
        payment_system = PaymentSystem,
        bank_name = ?STRING,
        iso_country_code = <<"KAZ">>,
        card_type = debit,
        bin_data_id = {i, 123}
    },
    version = ?INTEGER
}).

-define(PUT_CARD_RESULT, #'cds_PutCardResult'{
    bank_card = ?BANK_CARD
}).
