module Domain.Utils.SignedUserAgreement exposing (SignedUserAgreement, makeSignedUserAgreement, signedUserAgreementToString)


type SignedUserAgreement
    = SignedUserAgreement


makeSignedUserAgreement : Bool -> Result String SignedUserAgreement
makeSignedUserAgreement bool =
    if bool then
        Ok SignedUserAgreement

    else
        Err "E nevoie să acceptați Termenii de Utilizare."


signedUserAgreementToString : SignedUserAgreement -> String
signedUserAgreementToString _ =
    "on"
