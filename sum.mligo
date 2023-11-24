type storage =
  {
   counter : nat;
   owner : address
  }

type return = operation list * storage

[@entry]
let add m (storage : storage) : return =
  let sender = Tezos.get_sender () in
  if sender <> storage.owner
  then failwith "bad sender"
  else
    let new_storage = {storage with counter = storage.counter + m} in
    [], new_storage

[@entry]
let set_owner owner (storage : storage) : return =
  let sender = Tezos.get_sender () in
  if sender <> storage.owner
  then failwith "sender is not the current owner"
  else
    let new_storage = {storage with owner = owner} in
    [], new_storage

[@view]
let get_storage () (storage : storage) : nat = storage.counter
