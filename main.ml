module List = ListLabels

let omap x ~f = function
  | Some x -> f x
  | None -> x

let hashtbl_of_list l =
  let h = Hashtbl.create (List.length l) in
  List.iter ~f:(fun (id, x) -> Hashtbl.add h id x) l;
  h

module User = struct
  type t =
    {
      user_name: string;
      real_name: string;
      email: string;
    }

  let to_json {user_name; real_name; email} =
    let module J = Yojson.Basic in
    `Assoc
      [
        "user_name", `String user_name;
        "real_name", `String real_name;
        "email", `String email;
      ]
end

module Note = struct
  type t =
    {
      reporter: User.t option;
      text: string;
      last_modified: string;
      date_submitted: string;
    }

  let to_json {reporter; text; last_modified; date_submitted} =
    let module J = Yojson.Basic in
    `Assoc
      [
        "reporter", omap `Null ~f:User.to_json reporter;
        "text", `String text;
        "last_modified", `String last_modified;
        "date_submitted", `String date_submitted;
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
      reporter: User.t option;
      handler: User.t option;
      description: string;
      steps_to_reproduce: string;
      additional_information: string;
      notes: Note.t list;
    }

  let to_json
      {
        id;
        summary;
        priority;
        category;
        date_submitted;
        last_updated;
        reporter;
        handler;
        description;
        steps_to_reproduce;
        additional_information;
        notes;
      }
    =
    let module J = Yojson.Basic in
    `Assoc
      [
        "id", `Int id;
        "summary", `String summary;
        "priority", `String priority;
        "category", `String category;
        "date_submitted", `String date_submitted;
        "last_updated", `String last_updated;
        "reporter", omap `Null ~f:User.to_json reporter;
        "handler", omap `Null ~f:User.to_json handler;
        "description", `String description;
        "steps_to_reproduce", `String steps_to_reproduce;
        "additional_information", `String additional_information;
        "notes", `List (List.map ~f:Note.to_json notes);
      ]
end

open Mysql

let db =
  { dbhost = Some "127.0.0.1";
    dbname = Some "db";
    dbport = None;
    dbpwd = Some "";
    dbuser = Some "root";
    dbsocket = None }

let connect () =
  try
    Mysql.connect db
  with Mysql.Error s ->
    prerr_endline s;
    exit 2

let exec dbd ~f query =
  match Mysql.exec dbd query with
  | exception Mysql.Error s ->
      prerr_endline s;
      Mysql.disconnect dbd;
      exit 2
  | result ->
      Mysql.map result ~f

let timestamp s =
  let {Unix.tm_year; tm_mon; tm_mday; tm_hour; tm_min; tm_sec; _} =
    Unix.gmtime (float_of_string s)
  in
  Printf.sprintf "%04d-%02d-%02dT%02d:%02d:%02dZ"
    (tm_year + 1900) (tm_mon + 1) tm_mday tm_hour tm_min tm_sec

let () =
  let dbd = connect () in
  let categories =
    let f = function
      | [|Some id; Some name|] ->
          (int_of_string id, name)
      | _ ->
          failwith ""
    in
    let r = exec dbd ~f "SELECT id, name FROM mantis_category_table;" in
    Hashtbl.of_seq (List.to_seq r)
  in
  let users =
    let f = function
      | [|Some id; Some user_name; Some real_name; Some email|] ->
          (int_of_string id, {User.user_name; real_name; email})
      | _ ->
          failwith ""
    in
    let r =
      exec dbd ~f "SELECT id, username, realname, email FROM mantis_user_table;"
    in
    hashtbl_of_list r
  in
  let texts =
    let f = function
      | [|Some id; Some description; Some steps_to_reproduce; Some additional_information|] ->
          (int_of_string id, (description, steps_to_reproduce, additional_information))
      | _ ->
          failwith ""
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
            (int_of_string id, note)
        | _ ->
            failwith ""
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
          failwith ""
    in
    let r =
      exec dbd ~f "SELECT bug_id, reporter_id, bugnote_text_id, \
                   last_modified, date_submitted \
                   FROM mantis_bugnote_table;"
    in
    hashtbl_of_list r
  in
  let query =
    "SELECT id, summary, priority, category_id, date_submitted, last_updated, \
     reporter_id, handler_id, bug_text_id \
     FROM mantis_bug_table ORDER BY id;"
  in
  let f = function
    | [|Some id; Some summary; Some priority; Some category_id;
        Some date_submitted; Some last_updated; Some reporter_id;
        Some handler_id; Some bug_text_id|] ->
        let id = int_of_string id in
        let category = Hashtbl.find categories (int_of_string category_id) in
        let reporter = Hashtbl.find_opt users (int_of_string reporter_id) in
        let handler = Hashtbl.find_opt users (int_of_string handler_id) in
        let description, steps_to_reproduce, additional_information =
          Hashtbl.find texts (int_of_string bug_text_id)
        in
        let notes = Hashtbl.find_all notes id in
        { Issue.id; summary; priority; category;
          date_submitted = timestamp date_submitted;
          last_updated = timestamp last_updated; reporter; handler;
          description; steps_to_reproduce; additional_information; notes }
    | _ ->
        failwith "unxpected"
  in
  let issues = exec dbd ~f query in
  let f issue =
    let json = Issue.to_json issue in
    Printf.printf "%a\n" (Yojson.pretty_to_channel ~std:true) json
  in
  List.iter ~f issues
