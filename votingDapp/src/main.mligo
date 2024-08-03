module Voting = struct
  type player_record =
    {
     name : string;
     year : string;
     votes : nat
    }

  type players_type = (int, player_record) map

  type storage =
    {
     players : players_type;
     votersWalletAddresses : address set;
     dummy : string
    }

  let initial_storage (players : players_type) (votersWalletAddresses : address set)
  : storage =   {
     players = players;
     votersWalletAddresses = votersWalletAddresses;
     dummy = ""
    }

    let increase_votes(params: int) (store: storage) : operation list * storage = 
      let sender = Tezos.get_sender () in

      (* Ensure voter has not already voted*)
      let () = match Set.mem sender store.votersWalletAddresses with
        | true  -> failwith "You already voted."
        | false -> () in

      (* Ensure Player exists*)
      let player = match Map.find_opt params store.players with
        | Some player -> player
        | None -> failwith "Player Id not found." in

      (* Increase votes and update storage *)
      let updated_players = Map.add params { player with votes = player.votes + 1n } store.players in
      let updated_voters = Set.add sender store.votersWalletAddresses in
      ([], { store with players = updated_players; votersWalletAddresses = updated_voters})

  (* dummy entry point *)
  [@entry]
  let dummy (_unit : unit) (store: storage) : operation list * storage =
    ([], { store with dummy = ""}) 

end

(* test *)
let test_dapp () = 

  //define some test players and voters
  let players = Map.empty
    |> Map.add 1 { name = "Lionel Messi"; year = "2023"; votes = 0n}
    |> Map.add 2 { name = "Sadio Mane"; year = "2023"; votes = 0n} in

  let voters = Set.empty
    |> Set.add ("tz1zenoaddress" : address)
    |> Set.add ("tz1senecaaddress" : address) in

  // initialize storage
  let store = Voting.initial_storage players voters in
  
  //simulate a vote from a new voter
  let (_ops, updated_store) = Voting.increase_votes 2 store in 
  match Map.find_opt 2 updated_store.players with
  | Some player ->
    if player.votes = 1n then
      "Test Passed: Player 2 vote count is 1.\n"
    else
      "Test Failed: Player 2 vote count is not 1. Found: %s\n" 
  | None -> "Test Failed: Player 2 not found in the players map.\n"
