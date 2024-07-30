type player_record =
  {
   name : string;
   year : string;
   votes : nat
  }

type players_type = (int, player_record) map

type storage = {
    players : players_type;
    votersWalletAddresses : address set;
    dummy : string
}

let main (players : players_type) (votersWalletAddresses : address set)
: storage =
  {
   players = players;
   votersWalletAddresses = votersWalletAddresses;
   dummy = ""
  }
