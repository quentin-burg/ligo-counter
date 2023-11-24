#import "../vendors/breathalyzer/lib/lib.mligo" "B"
#import "../sum.mligo" "Sum"

let originate level owner =
  let default_storage = {
    counter = 0n;
    owner = owner
  }
  in
  B.Contract.originate level "contract" (contract_of Sum) default_storage 0tez

let call contract act () =
  B.Contract.transfer_to contract act 0tez

let case_add_correct_owner =
  B.Model.case
    "add"
    "when called with the correct owner, changes the storage"
    (fun level ->
      let (_, (alice, _, _)) = B.Context.init_default () in
      let contract = originate level alice.address in
      B.Result.reduce [
        B.Context.act_as alice (
          call contract (Add 1n)
        )
      ])

let case_add_wrong_owner =
  B.Model.case
    "add"
    "when called with the correct owner, changes the storage"
    (fun level ->
      let (_, (alice, bob, _)) = B.Context.init_default () in
      let contract = originate level alice.address in
      B.Result.reduce [
        B.Expect.fail_with_value "bad sender"
        (
          B.Context.act_as bob (
            call contract (Add 1n)
          )
        )
      ])

let () =
  B.Model.run_suites Trace [
    B.Model.suite "Basic test suite" [
      case_add_correct_owner;
      case_add_wrong_owner;
    ]
  ]
