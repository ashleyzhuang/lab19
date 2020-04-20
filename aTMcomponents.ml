open Printf ;;
open Scanf ;;

(* Customer account identifiers *)
type id = int

(* Possible actions that an ATM customer can perform *)
type action =
  | Balance           (* balance inquiry *)
  | Withdraw of int   (* withdraw an amount *)
  | Deposit of int    (* deposit an amount *)
  | Next              (* finish this customer and move on to the next one *)
  | Finished          (* shut down the ATM and exit entirely *)
;;

(* A specification of a customer name and initial balance for
   initializing the account database *)
type account_spec = {name : string; id : id; balance : int} ;;

let accts = ref [] ;;

let initialize (lst : account_spec list) : unit =
    accts := List.map (fun x -> (x.id, (x.name, x.balance))) lst ;;

let rec acquire_id () : int =
  printf "Enter customer id: ";
  try
    let id = read_int () in
      if List.mem_assoc id !accts then id
      else raise Not_found
  with
    | Not_found
    | Failure _ -> printf "Invalid id \n"; acquire_id () ;;

let rec acquire_amount () : int =
  printf "Enter amount: ";
  try
    let amt = read_int () in
    if amt <= 0 then raise (Failure "amount non-positive");
    amt
  with
    Failure _ -> printf "Invalid amount \n"; acquire_amount () ;;

let rec acquire_act () : action =
  printf "Enter action: (B) Balance (-) Withdraw (+) Deposit (=) Done (X) Exit: ";
  scanf " %c" (fun char -> match char with
                           | 'b' | 'B'        -> Balance
                           | '/' | 'x' | 'X'  -> Finished
                           | '='              -> Next
                           | 'w' | 'W' | '-'  -> Withdraw (acquire_amount ())
                           | 'd' | 'D' | '+'  -> Deposit (acquire_amount ())
                           | _                -> printf "  invalid choice\n";
                                                 acquire_act () ) ;;

let get_balance (i : id) : int = snd (List.assoc i !accts) ;;

let get_name (i : id) : string = fst (List.assoc i !accts) ;;

let update_balance (i : id) (amt : int) : unit =
  let name = get_name i in
  accts := List.remove_assoc i !accts;
  accts := (i, (name, amt)) :: !accts ;;

let present_message (str : string) : unit =
  printf "%s\n" str ;;

let deliver_cash (amount : int) : unit =
  printf "Here's your cash: ";
  (* dispense some "20's" *)
  for _i = 1 to (amount / 20) do
    printf "[20 @ 20]"
  done;
  (* dispense the rest of the cash *)
  printf " and %d more\n" (amount mod 20) ;;

