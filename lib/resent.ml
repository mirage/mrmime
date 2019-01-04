type field = Rfc5322.resent

type t =
  { date : Date.t option
  ; from : Mailbox.t list
  ; sender : Mailbox.t option
  ; too : Address.t list
  ; cc : Address.t list
  ; bcc : Address.t list
  ; msg_id : Msg_id.t option
  ; reply_to : Address.t list }

let default =
  { date = None
  ; from = []
  ; sender = None
  ; too = []
  ; cc = []
  ; bcc = []
  ; msg_id = None
  ; reply_to = [] }

let pp ppf _resent =
  Fmt.string ppf "#resent"

let ( <$> ) f x y = f y x

let with_date t v = { t with date = Some v }
let with_from t v = { t with from = v }
let with_sender t v = { t with sender = Some v }
let with_to t v = { t with too = t.too @ v }
let with_cc t v = { t with cc = t.cc @ v }
let with_bcc t v = { t with bcc = t.bcc @ v }
let with_msg_id t v = { t with msg_id = Some v }
let with_reply_to t v = { t with reply_to = t.reply_to @ v }

let fold : ([> field ] as 'a) list -> t list -> (t list * 'a list) = fun fields resents ->
  let head f = function x :: r -> (f x) :: r | [] -> [] in

  List.fold_left
    (fun (resents, rest) field -> match field, resents with
       | `ResentDate v, { date = None; _ } :: _ -> head (with_date <$> v) resents, rest
       | `ResentDate v, resents -> with_date default v :: resents, rest
       | `ResentFrom v, { from = []; _ } :: _ -> head (with_from <$> v) resents, rest
       | `ResentFrom v, resents -> with_from default v :: resents, rest
       | `ResentSender v, resents -> head (with_sender <$> v) resents, rest
       | `ResentTo v, resents -> head (with_to <$> v) resents, rest
       | `ResentCc v, resents -> head (with_cc <$> v) resents, rest
       | `ResentBcc v, resents -> head (with_bcc <$> v) resents, rest
       | `ResentMessageID v, resents -> head (with_msg_id <$> v) resents, rest
       | `ResentReplyTo v, resents -> head (with_reply_to <$> v) resents, rest
       | field, resents -> resents, field :: rest)
    (resents, []) fields
  |> fun (resents, fields) -> resents, List.rev fields
