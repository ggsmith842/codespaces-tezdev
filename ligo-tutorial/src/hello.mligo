(** Module defining a smart contract to store a "Hello" message *)

module Hello = struct
  (* Define the type of storage as string *)
  type storage = string

  (* Define the entrypoint for the contract that sets the storage to "Hello, web3" *)
  (* _delta: int      : An ignored parameter, typically for input *)
  (* _store: storage  : The current storage of the contract  *)
  (* Returns an empty list of operations and the updated storage *)
  [@entry]
  let store_hello (_delta : int) (_store : storage) : operation list * storage =
    [], "Hello, web3"
end

(* Define the test for the Hello module *)
let test_hello = 
    let initial_storage = "" in
    let (_operations, new_storage) = Hello.store_hello 0 initial_storage in
    Assert.assert (new_storage = "Hello, web3")

(* run test with `ligo run test src/hello.mligo *)
