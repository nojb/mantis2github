module List = ListLabels

let ostr = function
  | Some x -> `String x
  | None -> `Null

let hashtbl_of_list l =
  let h = Hashtbl.create (List.length l) in
  List.iter ~f:(fun (id, x) -> Hashtbl.add h id x) l;
  h

let user_map =
  let h = Hashtbl.create 17 in
  let ic = open_in "user_map.txt" in
  let rec loop () =
    match String.trim (input_line ic) with
    | s when s = "" || s.[0] = '#' ->
        loop ()
    | s ->
        let s1, s2 =
          try
            Scanf.sscanf s "%S %s%!" (fun s1 s2 -> s1, s2)
          with _ ->
            failwith (Printf.sprintf "Error while parsing users map: %S" s)
        in
        Hashtbl.add h s1 s2;
        loop ()
    | exception End_of_file ->
        h
  in
  loop ()

module Status = struct
  type t =
    | New
    | Feedback
    | Acknowledged
    | Confirmed
    | Assigned
    | Resolved
    | Closed

  let of_int = function
    | 10 -> New
    | 20 -> Feedback
    | 30 -> Acknowledged
    | 40 -> Confirmed
    | 50 -> Assigned
    | 80 -> Resolved
    | 90 -> Closed
    | n -> failwith (Printf.sprintf "Unexpected status code: %d" n)

  let to_string = function
    | New -> "new"
    | Feedback -> "feedback"
    | Acknowledged -> "acknowledged"
    | Confirmed -> "confirmed"
    | Assigned -> "assigned"
    | Resolved -> "resolved"
    | Closed -> "closed"

  let to_json st =
    let module J = Yojson.Basic in
    `String (to_string st)
end

module Note = struct
  type t =
    {
      reporter: string option;
      text: string;
      last_modified: string;
      date_submitted: string;
    }

  let to_json {reporter = _; text; last_modified = _; date_submitted} =
    let module J = Yojson.Basic in
    `Assoc
      [
        "body", `String text;
        "created_at", `String date_submitted;
      ]
end

module Issue = struct
  type t =
    {
      id: int;
      summary: string;
      priority: string;
      category: string;
      date_submitted: string;
      last_updated: string;
      reporter: string option;
      handler: string option;
      description: string;
      steps_to_reproduce: string;
      additional_information: string;
      target_version: string;
      notes: Note.t list;
      status: Status.t;
      closed_at: string option;
    }

  let to_json
      {
        id = _;
        summary;
        priority = _;
        category = _;
        date_submitted;
        last_updated;
        reporter = _;
        handler;
        description;
        steps_to_reproduce;
        additional_information;
        target_version = _;
        notes;
        status = _;
        closed_at;
      }
    =
    let module J = Yojson.Basic in
    let issue =
      [
        "title", `String summary;
        "body", `String description;
        "created_at", `String date_submitted;
        "closed_at", ostr closed_at;
        "updated_at", `String last_updated;
        "asignee", ostr handler;
        "steps_to_reproduce", `String steps_to_reproduce;
        "additional_information", `String additional_information;
      ]
    in
    let comments = List.map ~f:Note.to_json notes in
    `Assoc ["issue", `Assoc issue; "comments", `List comments]
end

open Mysql

let db =
  { dbhost = Some "127.0.0.1";
    dbname = Some "db";
    dbport = None;
    dbpwd = Some "";
    dbuser = Some "root";
    dbsocket = None }

let exec dbd ~f query =
  Mysql.map (Mysql.exec dbd query) ~f

let timestamp s =
  let {Unix.tm_year; tm_mon; tm_mday; tm_hour; tm_min; tm_sec; _} =
    Unix.gmtime (float_of_string s)
  in
  Printf.sprintf "%04d-%02d-%02dT%02d:%02d:%02dZ"
    (tm_year + 1900) (tm_mon + 1) tm_mday tm_hour tm_min tm_sec

let main dbd =
  let categories =
    let f = function
      | [|Some id; Some name|] ->
          int_of_string id, name
      | _ ->
          failwith "Unexpected response when querying categories"
    in
    let r = exec dbd ~f "SELECT id, name FROM mantis_category_table;" in
    Hashtbl.of_seq (List.to_seq r)
  in
  let users =
    let f = function
      | [|Some id; Some user_name|] ->
          let gh_user =
            match Hashtbl.find_opt user_map user_name with
            | Some s -> s
            | None ->
                Printf.eprintf "WARNING: user_map: missing user: %S\n%!"
                  user_name;
                user_name ^ "@Mantis"
          in
          int_of_string id, gh_user
      | _ ->
          failwith "Unexpected response when querying users"
    in
    let r =
      exec dbd ~f "SELECT id, username FROM mantis_user_table;"
    in
    hashtbl_of_list r
  in
  let texts =
    let f = function
      | [|Some id; Some description; Some steps_to_reproduce; Some additional_information|] ->
          int_of_string id, (description, steps_to_reproduce, additional_information)
      | _ ->
          failwith "Unexpected response when querying bug texts"
    in
    let r =
      exec dbd ~f "SELECT id, description, steps_to_reproduce, additional_information \
                   FROM mantis_bug_text_table;"
    in
    hashtbl_of_list r
  in
  let notes =
    let texts =
      let f = function
        | [|Some id; Some note|] ->
            int_of_string id, note
        | _ ->
            failwith "Unexpected response when querying notes texts"
      in
      let r = exec dbd ~f "SELECT id, note FROM mantis_bugnote_text_table;" in
      hashtbl_of_list r
    in
    let f = function
      | [|Some bug_id; Some reporter_id; Some bugnote_text_id;
          Some last_modified; Some date_submitted|] ->
          let reporter = Hashtbl.find_opt users (int_of_string reporter_id) in
          let text = Hashtbl.find texts (int_of_string bugnote_text_id) in
          let last_modified = timestamp last_modified in
          let date_submitted = timestamp date_submitted in
          int_of_string bug_id, {Note.reporter; text; last_modified; date_submitted}
      | _ ->
          failwith "Unexpected response when querying notes"
    in
    let r =
      exec dbd ~f "SELECT bug_id, reporter_id, bugnote_text_id, \
                   last_modified, date_submitted \
                   FROM mantis_bugnote_table ORDER BY date_submitted DESC;"
    in
    hashtbl_of_list r
  in
  let statuses =
    let f = function
      | [|Some bug_id; Some date_modified; Some new_value|] ->
          int_of_string bug_id,
          (timestamp date_modified, Status.of_int (int_of_string new_value))
      | _ ->
          failwith "Unexpected response when querying history"
    in
    let r =
      exec dbd ~f
        "SELECT bug_id, date_modified, new_value FROM mantis_bug_history_table \
         WHERE field_name = 'status' ORDER BY date_modified ASC;"
    in
    let h = Hashtbl.create (List.length r) in
    List.iter ~f:(fun (id, x) -> Hashtbl.replace h id x) r;
    h
  in
  let query =
    "SELECT id, summary, priority, category_id, date_submitted, last_updated, \
     reporter_id, handler_id, bug_text_id, target_version, status \
     FROM mantis_bug_table ORDER BY id;"
  in
  let f = function
    | [|Some id; Some summary; Some priority; Some category_id;
        Some date_submitted; Some last_updated; Some reporter_id;
        Some handler_id; Some bug_text_id; Some target_version;
        Some status|] ->
        let id = int_of_string id in
        let category = Hashtbl.find categories (int_of_string category_id) in
        let reporter = Hashtbl.find_opt users (int_of_string reporter_id) in
        let handler = Hashtbl.find_opt users (int_of_string handler_id) in
        let description, steps_to_reproduce, additional_information =
          Hashtbl.find texts (int_of_string bug_text_id)
        in
        let notes = Hashtbl.find_all notes id in
        let status = Status.of_int (int_of_string status) in
        let closed_at =
          match Hashtbl.find_opt statuses id with
          | None -> None
          | Some (closed_at, st) ->
              assert (st = status);
              begin match status with
              | Status.Resolved | Status.Closed -> Some closed_at
              | _ -> None
              end
        in
        { Issue.id; summary; priority; category;
          date_submitted = timestamp date_submitted;
          last_updated = timestamp last_updated; reporter; handler;
          description; steps_to_reproduce; additional_information; target_version;
          notes; status; closed_at }
    | _ ->
        failwith "Unexpected response when querying bugs"
  in
  exec dbd ~f query

let connect () =
  try
    let dbd = Mysql.connect db in
    Mysql.set_charset dbd "utf8";
    dbd
  with Mysql.Error s ->
    prerr_endline s;
    exit 2

let () =
  let dbd = connect () in
  match main dbd with
  | issues ->
      Mysql.disconnect dbd;
      let f issue =
        let json = Issue.to_json issue in
        Printf.printf "%a\n" (Yojson.pretty_to_channel ~std:true) json
      in
      List.iter ~f issues
  | exception e ->
      Printf.eprintf "ERROR: %s\n%!" (Printexc.to_string e);
      Mysql.disconnect dbd;
      exit 2
