#import "../src/main.mligo" "Voting"

include Test.Next


(* Test initial storage setup *)
let test_dapp =

  (* define some test players and voters *)
  let players = Map.empty
    |> Map.add 1 { name = "Lionel Messi"; year = "2023"; votes = 0n }
    |> Map.add 2 { name = "Sadio Mane"; year = "2023"; votes = 0n } in

  let voters = Set.empty
    |> Set.add ("tz1VSUr8wwNhLAzempoch5d6hLRiTh8Cjcjb" : address)
    |> Set.add ("tz1aWXP237BLwNHJcCD4b3DutCevhqq2T1Z9" : address) in

  (* initialize storage *)
  let store = Voting.Voting.initial_storage players voters in

  let orig = Test.Next.Originate.contract (contract_of Voting.Voting) store 0tez in
  // test that contract originated with expected storage
  Test.Next.Compare.eq (Test.Next.Typed_address.get_storage(orig.taddr)) store 