#import "../src/main.mligo" "Voting"

include Test.Next
include Test.Next.IO


(* Test initial storage setup *)
let test_origination =

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
  (* test that contract originated with expected storage *)
  // let updated_store = Typed_address.get_storage(orig.taddr) in
  match Test.Next.Compare.eq (Typed_address.get_storage(orig.taddr)) store with
  | true -> "Pass"
  | false -> "Fail"
  
  

  // let test_accounts =
  //   let initial_balances : tez list = [] in
  //   let () = Test.Next.State.reset 3n initial_balances in
  //   let voter_account1 = Test.Next.Account.address(0n) in
  //   Test.Next.IO.log (Test.Next.Address.get_balance voter_account1) 

let test_voting =

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
  
    
  (* Set up test accounts *)
  // let () = State.reset 3n [] in
  let voter_account1 = Account.address(0n) in
  // let voter_account2 = Account.address(1n) in

  (* Test increase votes *)
  let () = State.set_source voter_account1 in
  // let  = Voting.Voting.increase_votes 1 in 
  let _result = Contract.transfer_exn (Typed_address.get_entrypoint "increase_votes" orig.taddr) 1 0tez in

  let updated_store = Typed_address.get_storage(orig.taddr) in
  let player1 = Map.find_opt 1 updated_store.players in
  // let voting_result = (Compare.eq (player1.votes) 1) in
  let voting_result = match player1 with
    | Some p -> (p.votes = 1n)
    | None -> false
  in

  //test if voter can vote again
  
  (* Test if voter can vote again (should fail) *)
  let test_double_vote = 
    let second_vote_result = Contract.transfer (Typed_address.get_entrypoint "increase_votes" orig.taddr) 1 0tez in
    match second_vote_result with
    | Fail _x -> true (* If we reach here, the test failed because the second vote succeeded *)
    | Success _s  -> false (* The test passed because the second vote failed *)
  in

  (* Log results *)
  let () = log("First vote counted correctly: ", voting_result) in
  let () = log("Double voting prevented: ", test_double_vote) in

  (* Assert results *)
  Assert.assert(voting_result && test_double_vote ) 

